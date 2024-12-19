// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use deno_ast::ParsedSource;
use deno_ast::SourceRangedForSpanned;
use serde::Deserialize;
use serde::Serialize;

use crate::decorators::decorators_to_defs;
use crate::decorators::DecoratorDef;
use crate::function::function_to_function_def;
use crate::function::FunctionDef;
use crate::js_doc::JsDoc;
use crate::node::DeclarationKind;
use crate::params::assign_pat_to_param_def;
use crate::params::ident_to_param_def;
use crate::params::param_to_param_def;
use crate::params::prop_name_to_string;
use crate::params::ts_fn_param_to_param_def;
use crate::ts_type::infer_ts_type_from_expr;
use crate::ts_type::maybe_type_param_instantiation_to_type_defs;
use crate::ts_type::IndexSignatureDef;
use crate::ts_type::TsTypeDef;
use crate::ts_type_param::maybe_type_param_decl_to_type_param_defs;
use crate::ts_type_param::TsTypeParamDef;
use crate::util::swc::get_location;
use crate::util::swc::is_false;
use crate::util::swc::js_doc_for_range;
use crate::variable::VariableDef;
use crate::DocNode;
use crate::Location;
use crate::ParamDef;

cfg_if! {
  if #[cfg(feature = "rust")] {
    use deno_terminal::colors;
    use crate::display::display_abstract;
    use crate::display::display_accessibility;
    use crate::display::display_async;
    use crate::display::display_generator;
    use crate::display::display_method;
    use crate::display::display_optional;
    use crate::display::display_override;
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
pub struct ClassConstructorParamDef {
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub accessibility: Option<deno_ast::swc::ast::Accessibility>,
  #[serde(skip_serializing_if = "is_false", default)]
  pub is_override: bool,
  #[serde(flatten)]
  pub param: ParamDef,
  #[serde(skip_serializing_if = "is_false", default)]
  pub readonly: bool,
}

#[cfg(feature = "rust")]
impl Display for ClassConstructorParamDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(
      f,
      "{}{}{}{}",
      display_override(self.is_override),
      display_accessibility(self.accessibility, true),
      display_readonly(self.readonly),
      self.param
    )
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ClassConstructorDef {
  #[serde(skip_serializing_if = "JsDoc::is_empty", default)]
  pub js_doc: JsDoc,
  pub accessibility: Option<deno_ast::swc::ast::Accessibility>,
  #[serde(skip_serializing_if = "is_false", default)]
  pub is_optional: bool,
  #[serde(skip_serializing_if = "is_false", default)]
  pub has_body: bool,
  pub name: String,
  pub params: Vec<ClassConstructorParamDef>,
  pub location: Location,
}

#[cfg(feature = "rust")]
impl Display for ClassConstructorDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(
      f,
      "{}{}({})",
      display_accessibility(self.accessibility, false),
      colors::magenta("constructor"),
      SliceDisplayer::new(&self.params, ", ", false),
    )
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ClassPropertyDef {
  #[serde(skip_serializing_if = "JsDoc::is_empty", default)]
  pub js_doc: JsDoc,
  pub ts_type: Option<TsTypeDef>,
  pub readonly: bool,
  pub accessibility: Option<deno_ast::swc::ast::Accessibility>,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub decorators: Box<[DecoratorDef]>,
  pub optional: bool,
  pub is_abstract: bool,
  pub is_static: bool,
  #[serde(skip_serializing_if = "is_false", default)]
  pub is_override: bool,
  pub name: Box<str>,
  pub location: Location,
}

impl From<ClassPropertyDef> for DocNode {
  fn from(def: ClassPropertyDef) -> DocNode {
    DocNode::variable(
      def.name,
      false,
      def.location,
      DeclarationKind::Private,
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
      "{}{}{}{}{}{}{}",
      display_abstract(self.is_abstract),
      display_override(self.is_override),
      display_accessibility(self.accessibility, false),
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
pub struct ClassMethodDef {
  #[serde(skip_serializing_if = "JsDoc::is_empty", default)]
  pub js_doc: JsDoc,
  pub accessibility: Option<deno_ast::swc::ast::Accessibility>,
  pub optional: bool,
  pub is_abstract: bool,
  pub is_static: bool,
  #[serde(skip_serializing_if = "is_false", default)]
  pub is_override: bool,
  pub name: Box<str>,
  pub kind: deno_ast::swc::ast::MethodKind,
  pub function_def: FunctionDef,
  pub location: Location,
}

impl From<ClassMethodDef> for DocNode {
  fn from(def: ClassMethodDef) -> DocNode {
    DocNode::function(
      def.name,
      false,
      def.location,
      DeclarationKind::Private,
      def.js_doc,
      def.function_def,
    )
  }
}

#[cfg(feature = "rust")]
impl Display for ClassMethodDef {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    write!(
      f,
      "{}{}{}{}{}{}{}{}{}({})",
      display_abstract(self.is_abstract),
      display_override(self.is_override),
      display_accessibility(self.accessibility, false),
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
  #[serde(skip_serializing_if = "Option::is_none", default)]
  /// set when the class is a default export and has a name in its declaration
  pub def_name: Option<Box<str>>,
  pub is_abstract: bool,
  pub constructors: Box<[ClassConstructorDef]>,
  pub properties: Box<[ClassPropertyDef]>,
  pub index_signatures: Box<[IndexSignatureDef]>,
  pub methods: Box<[ClassMethodDef]>,
  pub extends: Option<Box<str>>,
  pub implements: Box<[TsTypeDef]>,
  pub type_params: Box<[TsTypeParamDef]>,
  pub super_type_params: Box<[TsTypeDef]>,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub decorators: Box<[DecoratorDef]>,
}

pub fn class_to_class_def(
  parsed_source: &ParsedSource,
  class: &deno_ast::swc::ast::Class,
  def_name: Option<Box<str>>,
) -> (ClassDef, JsDoc) {
  use deno_ast::swc::ast::Expr;

  let mut constructors = vec![];
  let mut methods = vec![];
  let mut properties = vec![];
  let mut index_signatures = vec![];

  fn walk_class_extends(expr: &Expr) -> Option<String> {
    match expr {
      Expr::Ident(ident) => Some(ident.sym.to_string()),
      Expr::Member(member_expr) => {
        let prop = &member_expr.prop.as_ident()?.sym;
        let mut string_path = walk_class_extends(&member_expr.obj)?;

        string_path.push('.');
        string_path.push_str(prop);

        Some(string_path)
      }
      _ => None,
    }
  }

  let extends: Option<Box<str>> = match &class.super_class {
    Some(boxed) => {
      let expr: &Expr = boxed;
      walk_class_extends(expr).map(|s| s.into_boxed_str())
    }
    None => None,
  };

  let implements = class
    .implements
    .iter()
    .map(|expr| TsTypeDef::ts_expr_with_type_args(parsed_source, expr))
    .collect::<Box<[_]>>();

  for member in &class.body {
    use deno_ast::swc::ast::ClassMember::*;

    match member {
      Constructor(ctor) => {
        if let Some(ctor_js_doc) =
          js_doc_for_range(parsed_source, &ctor.range())
        {
          let constructor_name = prop_name_to_string(parsed_source, &ctor.key);

          let mut params = vec![];

          for param in &ctor.params {
            use deno_ast::swc::ast::ParamOrTsParamProp::*;

            let param_def = match param {
              Param(param) => ClassConstructorParamDef {
                accessibility: None,
                is_override: false,
                param: param_to_param_def(parsed_source, param),
                readonly: false,
              },
              TsParamProp(ts_param_prop) => {
                use deno_ast::swc::ast::TsParamPropParam;

                let param = match &ts_param_prop.param {
                  TsParamPropParam::Ident(ident) => {
                    ident_to_param_def(parsed_source, ident)
                  }
                  TsParamPropParam::Assign(assign_pat) => {
                    assign_pat_to_param_def(parsed_source, assign_pat)
                  }
                };

                ClassConstructorParamDef {
                  accessibility: ts_param_prop.accessibility,
                  is_override: ts_param_prop.is_override,
                  param,
                  readonly: ts_param_prop.readonly,
                }
              }
            };
            params.push(param_def);
          }

          let constructor_def = ClassConstructorDef {
            js_doc: ctor_js_doc,
            accessibility: ctor.accessibility,
            is_optional: ctor.is_optional,
            has_body: ctor.body.is_some(),
            name: constructor_name,
            params,
            location: get_location(parsed_source, ctor.start()),
          };
          constructors.push(constructor_def);
        }
      }
      Method(class_method) => {
        if let Some(method_js_doc) =
          js_doc_for_range(parsed_source, &class_method.range())
        {
          let method_name =
            prop_name_to_string(parsed_source, &class_method.key);
          let fn_def = function_to_function_def(
            parsed_source,
            &class_method.function,
            None,
          );
          let method_def = ClassMethodDef {
            js_doc: method_js_doc,
            accessibility: class_method.accessibility,
            optional: class_method.is_optional,
            is_abstract: class_method.is_abstract,
            is_static: class_method.is_static,
            is_override: class_method.is_override,
            name: method_name.into_boxed_str(),
            kind: class_method.kind,
            function_def: fn_def,
            location: get_location(parsed_source, class_method.start()),
          };
          methods.push(method_def);
        }
      }
      ClassProp(class_prop) => {
        if let Some(prop_js_doc) =
          js_doc_for_range(parsed_source, &class_prop.range())
        {
          let ts_type = if let Some(type_ann) = &class_prop.type_ann {
            // if the property has a type annotation, use it
            Some(TsTypeDef::new(parsed_source, &type_ann.type_ann))
          } else if let Some(value) = &class_prop.value {
            // else, if it has an initializer, try to infer the type
            infer_ts_type_from_expr(parsed_source, value, false)
          } else {
            // else, none
            None
          };

          let prop_name = prop_name_to_string(parsed_source, &class_prop.key);

          let decorators =
            decorators_to_defs(parsed_source, &class_prop.decorators);

          let prop_def = ClassPropertyDef {
            js_doc: prop_js_doc,
            ts_type,
            readonly: class_prop.readonly,
            optional: class_prop.is_optional,
            is_abstract: class_prop.is_abstract,
            is_static: class_prop.is_static,
            is_override: class_prop.is_override,
            accessibility: class_prop.accessibility,
            name: prop_name.into_boxed_str(),
            decorators,
            location: get_location(parsed_source, class_prop.start()),
          };
          properties.push(prop_def);
        }
      }
      TsIndexSignature(ts_index_sig) => {
        if let Some(js_doc) =
          js_doc_for_range(parsed_source, &ts_index_sig.range())
        {
          let mut params = vec![];
          for param in &ts_index_sig.params {
            let param_def = ts_fn_param_to_param_def(parsed_source, param);
            params.push(param_def);
          }

          let ts_type = ts_index_sig
            .type_ann
            .as_ref()
            .map(|rt| TsTypeDef::new(parsed_source, &rt.type_ann));

          let index_sig_def = IndexSignatureDef {
            location: get_location(parsed_source, ts_index_sig.start()),
            js_doc,
            readonly: ts_index_sig.readonly,
            params,
            ts_type,
          };
          index_signatures.push(index_sig_def);
        }
      }
      // TODO(bartlomieju):
      PrivateMethod(_) => {}
      PrivateProp(_) => {}
      _ => {}
    }
  }

  let type_params = maybe_type_param_decl_to_type_param_defs(
    parsed_source,
    class.type_params.as_deref(),
  );

  let super_type_params = maybe_type_param_instantiation_to_type_defs(
    parsed_source,
    class.super_type_params.as_deref(),
  );

  let decorators = decorators_to_defs(parsed_source, &class.decorators);

  // JSDoc associated with the class may actually be a leading comment on a
  // decorator, and so we should parse out the JSDoc for the first decorator
  let js_doc = if !class.decorators.is_empty() {
    js_doc_for_range(parsed_source, &class.decorators[0].range()).unwrap()
  } else {
    JsDoc::default()
  };

  (
    ClassDef {
      def_name,
      is_abstract: class.is_abstract,
      extends,
      implements,
      constructors: constructors.into_boxed_slice(),
      properties: properties.into_boxed_slice(),
      index_signatures: index_signatures.into_boxed_slice(),
      methods: methods.into_boxed_slice(),
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
    class_to_class_def(parsed_source, &class_decl.class, None);

  (class_name, class_def, js_doc)
}
