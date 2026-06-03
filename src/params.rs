// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use crate::decorators::DecoratorDef;
use crate::decorators::decorators_to_defs;
use crate::display::SliceDisplayer;
use crate::display::display_optional;
use crate::ts_type::TsTypeDef;

use deno_ast::oxc::ast::ast::ArrayPattern;
use deno_ast::oxc::ast::ast::AssignmentPattern;
use deno_ast::oxc::ast::ast::BindingIdentifier;
use deno_ast::oxc::ast::ast::BindingPattern;
use deno_ast::oxc::ast::ast::BindingProperty;
use deno_ast::oxc::ast::ast::BindingRestElement;
use deno_ast::oxc::ast::ast::FormalParameter;
use deno_ast::oxc::ast::ast::FormalParameters;
use deno_ast::oxc::ast::ast::ObjectPattern;
use deno_ast::oxc::ast::ast::PropertyKey;
use deno_ast::oxc::span::GetSpan;
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
  #[serde(skip_serializing_if = "Option::is_none", default)]
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
        write!(f, "{}", key)
      }
      ObjectPatPropDef::Assign { key, .. } => {
        write!(f, "{}", key)
      }
      ObjectPatPropDef::Rest { arg } => write!(f, "...{}", arg),
    }
  }
}

pub fn ident_to_param_def(
  module_info: &EsModuleInfo,
  ident: &BindingIdentifier,
  type_ann: Option<&deno_ast::oxc::ast::ast::TSTypeAnnotation>,
) -> ParamDef {
  let ts_type = type_ann
    .map(|type_ann| TsTypeDef::new(module_info, &type_ann.type_annotation));

  ParamDef {
    pattern: ParamPatternDef::Identifier {
      name: ident.name.to_string(),
      optional: false,
    },
    decorators: Box::new([]),
    ts_type,
  }
}

pub fn rest_element_to_param_def(
  module_info: &EsModuleInfo,
  rest: &BindingRestElement,
  type_ann: Option<&deno_ast::oxc::ast::ast::TSTypeAnnotation>,
) -> ParamDef {
  let ts_type = type_ann
    .map(|type_ann| TsTypeDef::new(module_info, &type_ann.type_annotation));

  ParamDef {
    pattern: ParamPatternDef::Rest {
      arg: Box::new(binding_pat_to_param_def(
        module_info,
        &rest.argument,
        None,
      )),
    },
    decorators: Box::new([]),
    ts_type,
  }
}

fn binding_prop_to_def(
  module_info: &EsModuleInfo,
  prop: &BindingProperty,
) -> ObjectPatPropDef {
  if prop.shorthand {
    // Shorthand: `{ x }` or `{ x = default }`
    let key = match &prop.value {
      BindingPattern::BindingIdentifier(ident) => ident.name.to_string(),
      BindingPattern::AssignmentPattern(assign) => match &assign.left {
        BindingPattern::BindingIdentifier(ident) => ident.name.to_string(),
        _ => "[UNSUPPORTED]".to_string(),
      },
      _ => "[UNSUPPORTED]".to_string(),
    };
    let value = match &prop.value {
      BindingPattern::AssignmentPattern(_) => Some("[UNSUPPORTED]".to_string()),
      _ => None,
    };
    ObjectPatPropDef::Assign { key, value }
  } else {
    // KeyValue: `{ key: value }`
    ObjectPatPropDef::KeyValue {
      key: prop_name_to_string(module_info, &prop.key),
      value: Box::new(binding_pat_to_param_def(module_info, &prop.value, None)),
    }
  }
}

fn object_pat_to_param_def(
  module_info: &EsModuleInfo,
  object_pat: &ObjectPattern,
  type_ann: Option<&deno_ast::oxc::ast::ast::TSTypeAnnotation>,
) -> ParamDef {
  let mut props = object_pat
    .properties
    .iter()
    .map(|prop| binding_prop_to_def(module_info, prop))
    .collect::<Vec<_>>();

  if let Some(rest) = &object_pat.rest {
    props.push(ObjectPatPropDef::Rest {
      arg: Box::new(binding_pat_to_param_def(
        module_info,
        &rest.argument,
        None,
      )),
    });
  }

  let ts_type = type_ann
    .map(|type_ann| TsTypeDef::new(module_info, &type_ann.type_annotation));

  ParamDef {
    pattern: ParamPatternDef::Object {
      props,
      optional: false,
    },
    decorators: Box::new([]),
    ts_type,
  }
}

fn array_pat_to_param_def(
  module_info: &EsModuleInfo,
  array_pat: &ArrayPattern,
  type_ann: Option<&deno_ast::oxc::ast::ast::TSTypeAnnotation>,
) -> ParamDef {
  let mut elements = array_pat
    .elements
    .iter()
    .map(|elem| {
      elem
        .as_ref()
        .map(|e| binding_pat_to_param_def(module_info, e, None))
    })
    .collect::<Vec<Option<_>>>();

  // If there's a rest element, add it
  if let Some(rest) = &array_pat.rest {
    elements.push(Some(ParamDef {
      pattern: ParamPatternDef::Rest {
        arg: Box::new(binding_pat_to_param_def(
          module_info,
          &rest.argument,
          None,
        )),
      },
      decorators: Box::new([]),
      ts_type: None,
    }));
  }

  let ts_type = type_ann
    .map(|type_ann| TsTypeDef::new(module_info, &type_ann.type_annotation));

  ParamDef {
    pattern: ParamPatternDef::Array {
      elements,
      optional: false,
    },
    decorators: Box::new([]),
    ts_type,
  }
}

pub fn assign_pat_to_param_def(
  module_info: &EsModuleInfo,
  assign_pat: &AssignmentPattern,
  type_ann: Option<&deno_ast::oxc::ast::ast::TSTypeAnnotation>,
) -> ParamDef {
  let mut left =
    binding_pat_to_param_def(module_info, &assign_pat.left, type_ann);

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
  param: &FormalParameter,
) -> ParamDef {
  let mut def = binding_pat_to_param_def(
    module_info,
    &param.pattern,
    param.type_annotation.as_deref(),
  );
  // Set optional from the FormalParameter (BindingIdentifier doesn't have it)
  if param.optional {
    match &mut def.pattern {
      ParamPatternDef::Identifier { optional, .. } => *optional = true,
      ParamPatternDef::Array { optional, .. } => *optional = true,
      ParamPatternDef::Object { optional, .. } => *optional = true,
      _ => {}
    }
  }
  def.decorators = decorators_to_defs(module_info, &param.decorators);

  // Wrap in Assign if the parameter has a default initializer
  if let Some(init) = &param.initializer {
    let right = crate::interface::expr_to_name(init);
    // Infer type from initializer if not already set
    if def.ts_type.is_none() {
      def.ts_type =
        crate::ts_type::infer_ts_type_from_expr(module_info, init, false);
    }
    def = ParamDef {
      pattern: ParamPatternDef::Assign {
        left: Box::new(def),
        right,
      },
      decorators: Box::new([]),
      ts_type: None,
    };
  }

  def
}

pub fn binding_pat_to_param_def(
  module_info: &EsModuleInfo,
  pat: &BindingPattern,
  type_ann: Option<&deno_ast::oxc::ast::ast::TSTypeAnnotation>,
) -> ParamDef {
  match pat {
    BindingPattern::BindingIdentifier(ident) => {
      ident_to_param_def(module_info, ident, type_ann)
    }
    BindingPattern::ArrayPattern(array_pat) => {
      array_pat_to_param_def(module_info, array_pat, type_ann)
    }
    BindingPattern::ObjectPattern(object_pat) => {
      object_pat_to_param_def(module_info, object_pat, type_ann)
    }
    BindingPattern::AssignmentPattern(assign_pat) => {
      assign_pat_to_param_def(module_info, assign_pat, type_ann)
    }
  }
}

/// Collect all params from FormalParameters, including the rest element.
pub fn formal_params_to_param_defs(
  module_info: &EsModuleInfo,
  params: &FormalParameters,
) -> Vec<ParamDef> {
  let mut result: Vec<ParamDef> = params
    .items
    .iter()
    .map(|param| param_to_param_def(module_info, param))
    .collect();
  if let Some(rest) = &params.rest {
    result.push(rest_element_to_param_def(
      module_info,
      &rest.rest,
      rest.type_annotation.as_deref(),
    ));
  }
  result
}

pub fn prop_name_to_string(
  module_info: &EsModuleInfo,
  prop_key: &PropertyKey,
) -> String {
  match prop_key {
    PropertyKey::StaticIdentifier(ident) => ident.name.to_string(),
    PropertyKey::StringLiteral(str_) => str_.value.to_string(),
    PropertyKey::NumericLiteral(num) => num.value.to_string(),
    PropertyKey::BigIntLiteral(num) => num
      .raw
      .as_ref()
      .map(|r| r.as_str().to_string())
      .unwrap_or_default(),
    // Computed expression keys - use the PropertyKey's expression nature
    // Member expressions like Symbol.iterator should produce [Symbol.iterator]
    PropertyKey::PrivateIdentifier(ident) => {
      format!("#{}", ident.name)
    }
    PropertyKey::StaticMemberExpression(member) => {
      let left: String = crate::interface::expr_to_name(&member.object);
      let right = member.property.name.to_string();
      format!("[{}.{}]", left, right)
    }
    _ => {
      let span = prop_key.span();
      module_info.source_text()[span.start as usize..span.end as usize]
        .to_string()
    }
  }
}
