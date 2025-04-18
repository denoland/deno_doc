// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use crate::decorators::decorators_to_defs;
use crate::decorators::DecoratorDef;
use crate::display::display_optional;
use crate::display::SliceDisplayer;
use crate::ts_type::TsTypeDef;

use deno_ast::swc::ast::ObjectPatProp;
use deno_ast::swc::ast::Pat;
use deno_ast::swc::ast::TsFnParam;
use deno_ast::SourceRangedForSpanned;
use deno_graph::symbols::EsModuleInfo;
use serde::Deserialize;
use serde::Serialize;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "kind")]
pub enum ParamPatternDef {
  Array {
    elements: Vec<Option<ParamDef>>,
    optional: bool,
  },
  Assign {
    left: Box<ParamDef>,
    right: String,
  },
  Identifier {
    name: String,
    optional: bool,
  },
  Object {
    props: Vec<ObjectPatPropDef>,
    optional: bool,
  },
  Rest {
    arg: Box<ParamDef>,
  },
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct ParamDef {
  #[serde(flatten)]
  pub pattern: ParamPatternDef,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub decorators: Box<[DecoratorDef]>,
  pub ts_type: Option<TsTypeDef>,
}

impl Display for ParamDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    for decorator in self.decorators.iter() {
      write!(f, "{} ", decorator)?;
    }
    match &self.pattern {
      ParamPatternDef::Array { elements, optional } => {
        write!(f, "[")?;
        if !elements.is_empty() {
          if let Some(v) = &elements[0] {
            write!(f, "{}", v)?;
          }
          for maybe_v in &elements[1..] {
            write!(f, ", ")?;
            if let Some(v) = maybe_v {
              write!(f, "{}", v)?;
            }
          }
        }
        write!(f, "]")?;
        write!(f, "{}", display_optional(*optional))?;
        if let Some(ts_type) = &self.ts_type {
          write!(f, ": {}", ts_type)?;
        }
        Ok(())
      }
      ParamPatternDef::Assign { left, .. } => {
        write!(f, "{}", left)?;
        if let Some(ts_type) = &self.ts_type {
          write!(f, ": {}", ts_type)?;
        }
        // TODO(SyrupThinker) As we cannot display expressions the value is just omitted
        // write!(f, " = {}", right)?;
        Ok(())
      }
      ParamPatternDef::Identifier { name, optional } => {
        write!(f, "{}{}", name, display_optional(*optional))?;
        if let Some(ts_type) = &self.ts_type {
          write!(f, ": {}", ts_type)?;
        }
        Ok(())
      }
      ParamPatternDef::Object { props, optional } => {
        write!(
          f,
          "{{{}}}{}",
          SliceDisplayer::new(props, ", ", false),
          display_optional(*optional)
        )?;
        if let Some(ts_type) = &self.ts_type {
          write!(f, ": {}", ts_type)?;
        }
        Ok(())
      }
      ParamPatternDef::Rest { arg } => {
        write!(f, "...{}", arg)?;
        if let Some(ts_type) = &self.ts_type {
          write!(f, ": {}", ts_type)?;
        }
        Ok(())
      }
    }
  }
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
#[serde(tag = "kind")]
pub enum ObjectPatPropDef {
  Assign { key: String, value: Option<String> },
  KeyValue { key: String, value: Box<ParamDef> },
  Rest { arg: Box<ParamDef> },
}

impl Display for ObjectPatPropDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    match self {
      ObjectPatPropDef::KeyValue { key, .. } => {
        // The internal identifier does not need to be exposed
        write!(f, "{}", key)
      }
      ObjectPatPropDef::Assign { key, .. } => {
        // TODO(SyrupThinker) As we cannot display expressions the value is just omitted
        write!(f, "{}", key)
      }
      ObjectPatPropDef::Rest { arg } => write!(f, "...{}", arg),
    }
  }
}

pub fn ident_to_param_def(
  module_info: &EsModuleInfo,
  ident: &deno_ast::swc::ast::BindingIdent,
) -> ParamDef {
  let ts_type = ident
    .type_ann
    .as_deref()
    .map(|type_ann| TsTypeDef::new(module_info, &type_ann.type_ann));

  ParamDef {
    pattern: ParamPatternDef::Identifier {
      name: ident.id.sym.to_string(),
      optional: ident.id.optional,
    },
    decorators: Box::new([]),
    ts_type,
  }
}

fn rest_pat_to_param_def(
  module_info: &EsModuleInfo,
  rest_pat: &deno_ast::swc::ast::RestPat,
) -> ParamDef {
  let ts_type = rest_pat
    .type_ann
    .as_deref()
    .map(|type_ann| TsTypeDef::new(module_info, &type_ann.type_ann));

  ParamDef {
    pattern: ParamPatternDef::Rest {
      arg: Box::new(pat_to_param_def(module_info, &rest_pat.arg)),
    },
    decorators: Box::new([]),
    ts_type,
  }
}

fn object_pat_prop_to_def(
  module_info: &EsModuleInfo,
  object_pat_prop: &ObjectPatProp,
) -> ObjectPatPropDef {
  match object_pat_prop {
    ObjectPatProp::Assign(assign) => ObjectPatPropDef::Assign {
      key: assign.key.sym.to_string(),
      value: assign.value.as_ref().map(|_| "[UNSUPPORTED]".to_string()),
    },
    ObjectPatProp::KeyValue(keyvalue) => ObjectPatPropDef::KeyValue {
      key: prop_name_to_string(module_info, &keyvalue.key),
      value: Box::new(pat_to_param_def(module_info, &keyvalue.value)),
    },
    ObjectPatProp::Rest(rest) => ObjectPatPropDef::Rest {
      arg: Box::new(pat_to_param_def(module_info, &rest.arg)),
    },
  }
}

fn object_pat_to_param_def(
  module_info: &EsModuleInfo,
  object_pat: &deno_ast::swc::ast::ObjectPat,
) -> ParamDef {
  let props = object_pat
    .props
    .iter()
    .map(|prop| object_pat_prop_to_def(module_info, prop))
    .collect::<Vec<_>>();
  let ts_type = object_pat
    .type_ann
    .as_deref()
    .map(|type_ann| TsTypeDef::new(module_info, &type_ann.type_ann));

  ParamDef {
    pattern: ParamPatternDef::Object {
      props,
      optional: object_pat.optional,
    },
    decorators: Box::new([]),
    ts_type,
  }
}

fn array_pat_to_param_def(
  module_info: &EsModuleInfo,
  array_pat: &deno_ast::swc::ast::ArrayPat,
) -> ParamDef {
  let elements = array_pat
    .elems
    .iter()
    .map(|elem| elem.as_ref().map(|e| pat_to_param_def(module_info, e)))
    .collect::<Vec<Option<_>>>();
  let ts_type = array_pat
    .type_ann
    .as_deref()
    .map(|type_ann| TsTypeDef::new(module_info, &type_ann.type_ann));

  ParamDef {
    pattern: ParamPatternDef::Array {
      elements,
      optional: array_pat.optional,
    },
    decorators: Box::new([]),
    ts_type,
  }
}

pub fn assign_pat_to_param_def(
  module_info: &EsModuleInfo,
  assign_pat: &deno_ast::swc::ast::AssignPat,
) -> ParamDef {
  let mut left = pat_to_param_def(module_info, &assign_pat.left);

  if left.ts_type.is_none() {
    left.ts_type = crate::ts_type::infer_ts_type_from_expr(
      module_info,
      &assign_pat.right,
      false,
    );
  }

  ParamDef {
    pattern: ParamPatternDef::Assign {
      left: Box::new(left),
      right: crate::interface::expr_to_name(&assign_pat.right),
    },
    decorators: Box::new([]),
    ts_type: None,
  }
}

pub fn param_to_param_def(
  module_info: &EsModuleInfo,
  param: &deno_ast::swc::ast::Param,
) -> ParamDef {
  let mut def = pat_to_param_def(module_info, &param.pat);
  def.decorators = decorators_to_defs(module_info, &param.decorators);
  def
}

pub fn pat_to_param_def(
  module_info: &EsModuleInfo,
  pat: &deno_ast::swc::ast::Pat,
) -> ParamDef {
  match pat {
    Pat::Ident(ident) => ident_to_param_def(module_info, ident),
    Pat::Array(array_pat) => array_pat_to_param_def(module_info, array_pat),
    Pat::Rest(rest_pat) => rest_pat_to_param_def(module_info, rest_pat),
    Pat::Object(object_pat) => object_pat_to_param_def(module_info, object_pat),
    Pat::Assign(assign_pat) => assign_pat_to_param_def(module_info, assign_pat),
    _ => unreachable!(),
  }
}

pub fn ts_fn_param_to_param_def(
  module_info: &EsModuleInfo,
  ts_fn_param: &deno_ast::swc::ast::TsFnParam,
) -> ParamDef {
  match ts_fn_param {
    TsFnParam::Ident(ident) => ident_to_param_def(module_info, ident),
    TsFnParam::Array(array_pat) => {
      array_pat_to_param_def(module_info, array_pat)
    }
    TsFnParam::Rest(rest_pat) => rest_pat_to_param_def(module_info, rest_pat),
    TsFnParam::Object(object_pat) => {
      object_pat_to_param_def(module_info, object_pat)
    }
  }
}

pub fn prop_name_to_string(
  module_info: &EsModuleInfo,
  prop_name: &deno_ast::swc::ast::PropName,
) -> String {
  use deno_ast::swc::ast::PropName;
  match prop_name {
    PropName::Ident(ident) => ident.sym.to_string(),
    PropName::Str(str_) => str_.value.to_string(),
    PropName::Num(num) => num.value.to_string(),
    PropName::BigInt(num) => num.value.to_string(),
    PropName::Computed(comp_prop_name) => comp_prop_name
      .text_fast(module_info.source().text_info_lazy())
      .to_string(),
  }
}
