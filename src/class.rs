// Copyright 2020-2021 the Deno authors. All rights reserved. MIT license.

use deno_ast::swc::common::Spanned;
use deno_ast::ParsedSource;
use serde::Deserialize;
use serde::Serialize;

use crate::decorators::decorators_to_defs;
use crate::decorators::DecoratorDef;
use crate::function::function_to_function_def;
use crate::function::FunctionDef;
use crate::interface::expr_to_name;
use crate::js_doc::JsDoc;
use crate::params::{
  assign_pat_to_param_def, ident_to_param_def, pat_to_param_def,
  prop_name_to_string, ts_fn_param_to_param_def,
};
use crate::swc_util::{get_location, js_doc_for_span};
use crate::ts_type::{
  maybe_type_param_instantiation_to_type_defs, ts_type_ann_to_def, TsTypeDef,
};
use crate::ts_type_param::maybe_type_param_decl_to_type_param_defs;
use crate::ts_type_param::TsTypeParamDef;
use crate::variable::VariableDef;
use crate::DocNode;
use crate::Location;
use crate::ParamDef;

cfg_if! {
  if #[cfg(feature = "rust")] {
    use crate::colors;
    use crate::display::display_abstract;
    use crate::display::display_accessibility;
    use crate::display::display_async;
    use crate::display::display_generator;
    use crate::display::display_method;
    use crate::display::display_optional;
    use crate::display::display_readonly;
    use crate::display::display_static;
    use crate::display::SliceDisplayer;

    use std::fmt::Display;
    use std::fmt::Formatter;
    use std::fmt::Result as FmtResult;
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ClassConstructorDef {
  #[serde(skip_serializing_if = "JsDoc::is_empty")]
  pub js_doc: JsDoc,
  pub accessibility: Option<deno_ast::swc::ast::Accessibility>,
  pub name: String,
  pub params: Vec<ParamDef>,
  pub location: Location,
}

#[cfg(feature = "rust")]
impl Display for ClassConstructorDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(
      f,
      "{}{}({})",
      display_accessibility(self.accessibility),
      colors::magenta("constructor"),
      SliceDisplayer::new(&self.params, ", ", false),
    )
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ClassPropertyDef {
  #[serde(skip_serializing_if = "JsDoc::is_empty")]
  pub js_doc: JsDoc,
  pub ts_type: Option<TsTypeDef>,
  pub readonly: bool,
  pub accessibility: Option<deno_ast::swc::ast::Accessibility>,
  #[serde(skip_serializing_if = "Vec::is_empty")]
  pub decorators: Vec<DecoratorDef>,
  pub optional: bool,
  pub is_abstract: bool,
  pub is_static: bool,
  pub name: String,
  pub location: Location,
}

impl From<ClassPropertyDef> for DocNode {
  fn from(def: ClassPropertyDef) -> DocNode {
    DocNode::variable(
      def.name,
      def.location,
      def.js_doc,
      VariableDef {
        ts_type: def.ts_type,
        kind: deno_ast::swc::ast::VarDeclKind::Const,
      },
    )
  }
}

#[cfg(feature = "rust")]
impl Display for ClassPropertyDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(
      f,
      "{}{}{}{}{}{}",
      display_abstract(self.is_abstract),
      display_accessibility(self.accessibility),
      display_static(self.is_static),
      display_readonly(self.readonly),
      colors::bold(&self.name),
      display_optional(self.optional),
    )?;
    if let Some(ts_type) = &self.ts_type {
      write!(f, ": {}", ts_type)?;
    }
    Ok(())
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ClassIndexSignatureDef {
  pub readonly: bool,
  pub params: Vec<ParamDef>,
  pub ts_type: Option<TsTypeDef>,
}

#[cfg(feature = "rust")]
impl Display for ClassIndexSignatureDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(
      f,
      "{}[{}]",
      display_readonly(self.readonly),
      SliceDisplayer::new(&self.params, ", ", false)
    )?;
    if let Some(ts_type) = &self.ts_type {
      write!(f, ": {}", ts_type)?;
    }
    Ok(())
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ClassMethodDef {
  #[serde(skip_serializing_if = "JsDoc::is_empty")]
  pub js_doc: JsDoc,
  pub accessibility: Option<deno_ast::swc::ast::Accessibility>,
  pub optional: bool,
  pub is_abstract: bool,
  pub is_static: bool,
  pub name: String,
  pub kind: deno_ast::swc::ast::MethodKind,
  pub function_def: FunctionDef,
  pub location: Location,
}

impl From<ClassMethodDef> for DocNode {
  fn from(def: ClassMethodDef) -> DocNode {
    DocNode::function(def.name, def.location, def.js_doc, def.function_def)
  }
}

#[cfg(feature = "rust")]
impl Display for ClassMethodDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(
      f,
      "{}{}{}{}{}{}{}{}({})",
      display_abstract(self.is_abstract),
      display_accessibility(self.accessibility),
      display_static(self.is_static),
      display_async(self.function_def.is_async),
      display_method(self.kind),
      display_generator(self.function_def.is_generator),
      colors::bold(&self.name),
      display_optional(self.optional),
      SliceDisplayer::new(&self.function_def.params, ", ", false),
    )?;
    if let Some(return_type) = &self.function_def.return_type {
      write!(f, ": {}", return_type)?;
    }
    Ok(())
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ClassDef {
  // TODO(bartlomieju): decorators
  pub is_abstract: bool,
  pub constructors: Vec<ClassConstructorDef>,
  pub properties: Vec<ClassPropertyDef>,
  pub index_signatures: Vec<ClassIndexSignatureDef>,
  pub methods: Vec<ClassMethodDef>,
  pub extends: Option<String>,
  pub implements: Vec<TsTypeDef>,
  pub type_params: Vec<TsTypeParamDef>,
  pub super_type_params: Vec<TsTypeDef>,
  #[serde(skip_serializing_if = "Vec::is_empty")]
  pub decorators: Vec<DecoratorDef>,
}

pub fn class_to_class_def(
  parsed_source: &ParsedSource,
  class: &deno_ast::swc::ast::Class,
) -> (ClassDef, JsDoc) {
  let mut constructors = vec![];
  let mut methods = vec![];
  let mut properties = vec![];
  let mut index_signatures = vec![];

  let extends: Option<String> = match &class.super_class {
    Some(boxed) => {
      use deno_ast::swc::ast::Expr;
      let expr: &Expr = &**boxed;
      match expr {
        Expr::Ident(ident) => Some(ident.sym.to_string()),
        _ => None,
      }
    }
    None => None,
  };

  let implements = class
    .implements
    .iter()
    .map(|expr| expr.into())
    .collect::<Vec<TsTypeDef>>();

  for member in &class.body {
    use deno_ast::swc::ast::ClassMember::*;

    match member {
      Constructor(ctor) => {
        let ctor_js_doc = js_doc_for_span(parsed_source, &ctor.span());
        let constructor_name =
          prop_name_to_string(Some(parsed_source), &ctor.key);

        let mut params = vec![];

        for param in &ctor.params {
          use deno_ast::swc::ast::ParamOrTsParamProp::*;

          let param_def = match param {
            Param(param) => pat_to_param_def(Some(parsed_source), &param.pat),
            TsParamProp(ts_param_prop) => {
              use deno_ast::swc::ast::TsParamPropParam;

              match &ts_param_prop.param {
                TsParamPropParam::Ident(ident) => {
                  ident_to_param_def(Some(parsed_source), ident)
                }
                TsParamPropParam::Assign(assign_pat) => {
                  assign_pat_to_param_def(Some(parsed_source), assign_pat)
                }
              }
            }
          };
          params.push(param_def);
        }

        let constructor_def = ClassConstructorDef {
          js_doc: ctor_js_doc,
          accessibility: ctor.accessibility,
          name: constructor_name,
          params,
          location: get_location(parsed_source, ctor.span.lo()),
        };
        constructors.push(constructor_def);
      }
      Method(class_method) => {
        let method_js_doc =
          js_doc_for_span(parsed_source, &class_method.span());
        let method_name =
          prop_name_to_string(Some(parsed_source), &class_method.key);
        let fn_def =
          function_to_function_def(parsed_source, &class_method.function);
        let method_def = ClassMethodDef {
          js_doc: method_js_doc,
          accessibility: class_method.accessibility,
          optional: class_method.is_optional,
          is_abstract: class_method.is_abstract,
          is_static: class_method.is_static,
          name: method_name,
          kind: class_method.kind,
          function_def: fn_def,
          location: get_location(parsed_source, class_method.span.lo()),
        };
        methods.push(method_def);
      }
      ClassProp(class_prop) => {
        let prop_js_doc = js_doc_for_span(parsed_source, &class_prop.span());

        let ts_type = class_prop
          .type_ann
          .as_ref()
          .map(|rt| ts_type_ann_to_def(rt));

        let prop_name = expr_to_name(&*class_prop.key);

        let decorators =
          decorators_to_defs(parsed_source, &class_prop.decorators);

        let prop_def = ClassPropertyDef {
          js_doc: prop_js_doc,
          ts_type,
          readonly: class_prop.readonly,
          optional: class_prop.is_optional,
          is_abstract: class_prop.is_abstract,
          is_static: class_prop.is_static,
          accessibility: class_prop.accessibility,
          name: prop_name,
          decorators,
          location: get_location(parsed_source, class_prop.span.lo()),
        };
        properties.push(prop_def);
      }
      TsIndexSignature(ts_index_sig) => {
        let mut params = vec![];
        for param in &ts_index_sig.params {
          // todo(kitsonk): investigate why `None` is provided here
          let param_def = ts_fn_param_to_param_def(None, param);
          params.push(param_def);
        }

        let ts_type = ts_index_sig
          .type_ann
          .as_ref()
          .map(|rt| (&*rt.type_ann).into());

        let index_sig_def = ClassIndexSignatureDef {
          readonly: ts_index_sig.readonly,
          params,
          ts_type,
        };
        index_signatures.push(index_sig_def);
      }
      // TODO(bartlomieju):
      PrivateMethod(_) => {}
      PrivateProp(_) => {}
      _ => {}
    }
  }

  let type_params =
    maybe_type_param_decl_to_type_param_defs(class.type_params.as_ref());

  let super_type_params = maybe_type_param_instantiation_to_type_defs(
    class.super_type_params.as_ref(),
  );

  let decorators = decorators_to_defs(parsed_source, &class.decorators);

  // JSDoc associated with the class may actually be a leading comment on a
  // decorator, and so we should parse out the JSDoc for the first decorator
  let js_doc = if !class.decorators.is_empty() {
    js_doc_for_span(parsed_source, &class.decorators[0].span)
  } else {
    JsDoc::default()
  };

  (
    ClassDef {
      is_abstract: class.is_abstract,
      extends,
      implements,
      constructors,
      properties,
      index_signatures,
      methods,
      type_params,
      super_type_params,
      decorators,
    },
    js_doc,
  )
}

pub fn get_doc_for_class_decl(
  parsed_source: &ParsedSource,
  class_decl: &deno_ast::swc::ast::ClassDecl,
) -> (String, ClassDef, JsDoc) {
  let class_name = class_decl.ident.sym.to_string();
  let (class_def, js_doc) =
    class_to_class_def(parsed_source, &class_decl.class);

  (class_name, class_def, js_doc)
}
