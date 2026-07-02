// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use deno_ast::oxc::ast::ast::Class;
use deno_ast::oxc::ast::ast::ClassElement;
use deno_ast::oxc::ast::ast::Expression;
use deno_ast::oxc::ast::ast::MethodDefinitionKind;
use deno_ast::oxc::ast::ast::MethodDefinitionType;
use deno_ast::oxc::ast::ast::PropertyDefinitionType;
use deno_ast::oxc::span::GetSpan;
use deno_graph::symbols::EsModuleInfo;
use serde::Deserialize;
use serde::Serialize;

use crate::Location;
use crate::ParamDef;
use crate::decorators::DecoratorDef;
use crate::decorators::decorators_to_defs;
use crate::function::FunctionDef;
use crate::function::function_to_function_def;
use crate::js_doc::JsDoc;
use crate::node::DeclarationKind;
use crate::node::Symbol;
use crate::params::ParamPatternDef;
use crate::params::param_to_param_def;
use crate::params::prop_name_to_string;
use crate::ts_type::IndexSignatureDef;
use crate::ts_type::TsTypeDef;
use crate::ts_type::infer_ts_type_from_expr;
use crate::ts_type::maybe_type_param_instantiation_to_type_defs;
use crate::ts_type_param::TsTypeParamDef;
use crate::ts_type_param::maybe_type_param_decl_to_type_param_defs;
use crate::util::swc::get_location;
use crate::util::swc::is_false;
use crate::util::swc::js_doc_for_range;
use crate::util::types::Accessibility;
use crate::util::types::MethodKind;
use crate::util::types::VarDeclKind;
use crate::variable::VariableDef;

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
  pub accessibility: Option<Accessibility>,
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
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub accessibility: Option<Accessibility>,
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
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub ts_type: Option<TsTypeDef>,
  #[serde(skip_serializing_if = "is_false", default)]
  pub readonly: bool,
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub accessibility: Option<Accessibility>,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub decorators: Box<[DecoratorDef]>,
  #[serde(skip_serializing_if = "is_false", default)]
  pub optional: bool,
  #[serde(skip_serializing_if = "is_false", default)]
  pub is_abstract: bool,
  #[serde(skip_serializing_if = "is_false", default)]
  pub is_static: bool,
  #[serde(skip_serializing_if = "is_false", default)]
  pub is_override: bool,
  pub name: Box<str>,
  pub location: Location,
}

impl From<ClassPropertyDef> for Symbol {
  fn from(def: ClassPropertyDef) -> Symbol {
    Symbol::variable(
      def.name,
      false,
      def.location,
      DeclarationKind::Private,
      def.js_doc,
      VariableDef {
        ts_type: def.ts_type,
        kind: VarDeclKind::Const,
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
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub accessibility: Option<Accessibility>,
  #[serde(skip_serializing_if = "is_false", default)]
  pub optional: bool,
  #[serde(skip_serializing_if = "is_false", default)]
  pub is_abstract: bool,
  #[serde(skip_serializing_if = "is_false", default)]
  pub is_static: bool,
  #[serde(skip_serializing_if = "is_false", default)]
  pub is_override: bool,
  pub name: Box<str>,
  pub kind: MethodKind,
  pub function_def: FunctionDef,
  pub location: Location,
}

impl From<ClassMethodDef> for Symbol {
  fn from(def: ClassMethodDef) -> Symbol {
    Symbol::function(
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
  /// set when the class is a default export or a class expression, and has a name in its declaration
  pub def_name: Option<Box<str>>,
  #[serde(skip_serializing_if = "is_false", default)]
  pub is_abstract: bool,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub constructors: Box<[ClassConstructorDef]>,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub properties: Box<[ClassPropertyDef]>,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub index_signatures: Box<[IndexSignatureDef]>,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub methods: Box<[ClassMethodDef]>,
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub extends: Option<Box<str>>,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub implements: Box<[TsTypeDef]>,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub type_params: Box<[TsTypeParamDef]>,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub super_type_params: Box<[TsTypeDef]>,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub decorators: Box<[DecoratorDef]>,
}

pub fn class_to_class_def(
  module_info: &EsModuleInfo,
  class: &Class,
  def_name: Option<Box<str>>,
) -> (ClassDef, JsDoc) {
  let mut constructors = vec![];
  let mut methods = vec![];
  let mut properties = vec![];
  let mut index_signatures = vec![];

  fn walk_class_extends(expr: &Expression) -> Option<String> {
    match expr {
      Expression::Identifier(ident) => Some(ident.name.to_string()),
      Expression::StaticMemberExpression(member_expr) => {
        let prop = &member_expr.property.name;
        let mut string_path = walk_class_extends(&member_expr.object)?;

        string_path.push('.');
        string_path.push_str(prop);

        Some(string_path)
      }
      _ => None,
    }
  }

  let extends: Option<Box<str>> = match &class.super_class {
    Some(boxed) => {
      let expr: &Expression = boxed;
      walk_class_extends(expr).map(|s| s.into_boxed_str())
    }
    None => None,
  };

  let implements = class
    .implements
    .iter()
    .map(|expr| TsTypeDef::ts_class_implements(module_info, expr))
    .collect::<Box<[_]>>();

  for member in &class.body.body {
    match member {
      ClassElement::MethodDefinition(method_def)
        if method_def.kind == MethodDefinitionKind::Constructor =>
      {
        let ctor = method_def;
        if let Some(ctor_js_doc) = js_doc_for_range(module_info, ctor.span()) {
          let constructor_name = prop_name_to_string(module_info, &ctor.key);

          let mut params = vec![];

          for param in &ctor.value.params.items {
            let param_def = ClassConstructorParamDef {
              accessibility: Accessibility::from_oxc(param.accessibility),
              is_override: param.r#override,
              param: param_to_param_def(module_info, param),
              readonly: param.readonly,
            };
            params.push(param_def);
          }

          if let Some(rest) = &ctor.value.params.rest {
            let param_def = ClassConstructorParamDef {
              accessibility: None,
              is_override: false,
              param: crate::params::rest_element_to_param_def(
                module_info,
                &rest.rest,
                rest.type_annotation.as_deref(),
              ),
              readonly: false,
            };
            params.push(param_def);
          }

          let constructor_def = ClassConstructorDef {
            js_doc: ctor_js_doc,
            accessibility: Accessibility::from_oxc(ctor.accessibility),
            is_optional: ctor.optional,
            has_body: ctor.value.body.is_some(),
            name: constructor_name,
            params,
            location: get_location(module_info, ctor.span().start),
          };
          constructors.push(constructor_def);
        }
      }
      ClassElement::MethodDefinition(class_method) => {
        if let Some(method_js_doc) =
          js_doc_for_range(module_info, class_method.span())
        {
          let method_name = prop_name_to_string(module_info, &class_method.key);
          let mut fn_def =
            function_to_function_def(module_info, &class_method.value, None);
          fn_def.decorators =
            decorators_to_defs(module_info, &class_method.decorators);
          let method_def = ClassMethodDef {
            js_doc: method_js_doc,
            accessibility: Accessibility::from_oxc(class_method.accessibility),
            optional: class_method.optional,
            is_abstract: class_method.r#type
              == MethodDefinitionType::TSAbstractMethodDefinition,
            is_static: class_method.r#static,
            is_override: class_method.r#override,
            name: method_name.into_boxed_str(),
            kind: MethodKind::from(class_method.kind),
            function_def: fn_def,
            location: get_location(module_info, class_method.span().start),
          };
          methods.push(method_def);
        }
      }
      ClassElement::PropertyDefinition(class_prop) => {
        if class_prop.key.is_private_identifier() {
          continue;
        }
        if let Some(prop_js_doc) =
          js_doc_for_range(module_info, class_prop.span())
        {
          let ts_type = if let Some(type_ann) = &class_prop.type_annotation {
            // if the property has a type annotation, use it
            Some(TsTypeDef::new(module_info, &type_ann.type_annotation))
          } else if let Some(value) = &class_prop.value {
            // else, if it has an initializer, try to infer the type
            infer_ts_type_from_expr(module_info, value, false)
          } else {
            // else, none
            None
          };

          let prop_name = prop_name_to_string(module_info, &class_prop.key);

          let decorators =
            decorators_to_defs(module_info, &class_prop.decorators);

          let prop_def = ClassPropertyDef {
            js_doc: prop_js_doc,
            ts_type,
            readonly: class_prop.readonly,
            optional: class_prop.optional,
            is_abstract: class_prop.r#type
              == PropertyDefinitionType::TSAbstractPropertyDefinition,
            is_static: class_prop.r#static,
            is_override: class_prop.r#override,
            accessibility: Accessibility::from_oxc(class_prop.accessibility),
            name: prop_name.into_boxed_str(),
            decorators,
            location: get_location(module_info, class_prop.span().start),
          };
          properties.push(prop_def);
        }
      }
      ClassElement::TSIndexSignature(ts_index_sig) => {
        if let Some(js_doc) = js_doc_for_range(module_info, ts_index_sig.span())
        {
          let mut params = vec![];
          for param in &ts_index_sig.parameters {
            let ts_type = Some(TsTypeDef::new(
              module_info,
              &param.type_annotation.type_annotation,
            ));
            let param_def = ParamDef {
              pattern: ParamPatternDef::Identifier {
                name: param.name.to_string(),
                optional: false,
              },
              decorators: Box::new([]),
              ts_type,
            };
            params.push(param_def);
          }

          let ts_type = Some(TsTypeDef::new(
            module_info,
            &ts_index_sig.type_annotation.type_annotation,
          ));

          let index_sig_def = IndexSignatureDef {
            location: get_location(module_info, ts_index_sig.span().start),
            js_doc,
            readonly: ts_index_sig.readonly,
            params,
            ts_type,
          };
          index_signatures.push(index_sig_def);
        }
      }
      _ => {}
    }
  }

  let type_params = maybe_type_param_decl_to_type_param_defs(
    module_info,
    class.type_parameters.as_deref(),
  );

  let super_type_params = maybe_type_param_instantiation_to_type_defs(
    module_info,
    class.super_type_arguments.as_deref(),
  );

  let decorators = decorators_to_defs(module_info, &class.decorators);

  // JSDoc associated with the class may actually be a leading comment on a
  // decorator, and so we should parse out the JSDoc for the first decorator
  let js_doc = if !class.decorators.is_empty() {
    js_doc_for_range(module_info, class.decorators[0].span()).unwrap()
  } else {
    JsDoc::default()
  };

  (
    ClassDef {
      def_name,
      is_abstract: class.r#abstract,
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
  module_info: &EsModuleInfo,
  class: &Class,
) -> (ClassDef, JsDoc) {
  class_to_class_def(module_info, class, None)
}
