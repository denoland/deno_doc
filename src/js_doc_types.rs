// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

//! JavaScript files can't carry type annotations, so their types are declared
//! in JSDoc instead (`@param {string} name`, `@returns {number}`,
//! `@type {Foo}`), which TypeScript honours when checking JavaScript. This
//! module copies those types onto the declarations they describe, so a
//! JavaScript function documents the same signature TypeScript sees for it.

use crate::class::ClassDef;
use crate::function::FunctionDef;
use crate::js_doc::JsDoc;
use crate::js_doc::JsDocTag;
use crate::js_doc::parse_jsdoc_type;
use crate::node::Declaration;
use crate::node::DeclarationDef;
use crate::params::ParamDef;
use crate::params::ParamPatternDef;
use crate::ts_type::TsTypeDef;
use crate::ts_type::TsTypeDefKind;
use deno_ast::MediaType;
use deno_graph::symbols::EsModuleInfo;
use std::collections::HashSet;

/// Whether types in a module of this media type are declared through JSDoc.
/// TypeScript ignores JSDoc types in TypeScript files, so this is only true
/// for JavaScript.
pub(crate) fn declares_types_in_js_doc(media_type: MediaType) -> bool {
  matches!(
    media_type,
    MediaType::JavaScript | MediaType::Jsx | MediaType::Mjs | MediaType::Cjs
  )
}

/// Applies the types from `decl`'s JSDoc tags to its definition. In a
/// JavaScript file every type already on the definition was inferred (from a
/// default value or an initializer), so a type declared in JSDoc replaces it.
pub(crate) fn apply_js_doc_types(
  module_info: &EsModuleInfo,
  decl: &mut Declaration,
) {
  match &mut decl.def {
    DeclarationDef::Function(function_def) => {
      apply_to_function(module_info, &decl.js_doc, function_def);
    }
    DeclarationDef::Variable(variable_def) => {
      if let Some(ts_type) = type_tag(&decl.js_doc) {
        variable_def.ts_type = Some(ts_type.clone());
      }
    }
    DeclarationDef::Class(class_def) => {
      apply_to_class(module_info, &decl.js_doc, class_def);
    }
    DeclarationDef::Enum(_)
    | DeclarationDef::TypeAlias(_)
    | DeclarationDef::Namespace(_)
    | DeclarationDef::Interface(_)
    | DeclarationDef::Reference(_) => {}
  }
}

fn apply_to_function(
  module_info: &EsModuleInfo,
  js_doc: &JsDoc,
  function_def: &mut FunctionDef,
) {
  apply_to_params(
    module_info,
    js_doc,
    function_def.params.iter_mut().collect(),
  );

  let return_type = js_doc.tags.iter().find_map(|tag| match tag {
    JsDocTag::Return {
      ts_type: Some(ts_type),
      ..
    } => Some(ts_type),
    _ => None,
  });
  if let Some(return_type) = return_type {
    function_def.return_type = Some(return_type.clone());
  }
}

fn apply_to_class(
  module_info: &EsModuleInfo,
  js_doc: &JsDoc,
  class_def: &mut ClassDef,
) {
  for constructor in class_def.constructors.iter_mut() {
    apply_to_params(
      module_info,
      &constructor.js_doc,
      constructor
        .params
        .iter_mut()
        .map(|param| &mut param.param)
        .collect(),
    );
  }

  for method in class_def.methods.iter_mut() {
    apply_to_function(module_info, &method.js_doc, &mut method.function_def);
  }

  for property in class_def.properties.iter_mut() {
    let property_tag = js_doc.tags.iter().find_map(|tag| match tag {
      JsDocTag::Property { name, ts_type, doc } if *name == property.name => {
        Some((ts_type, doc))
      }
      _ => None,
    });

    // A property's own `@type` and description win over a `@property` tag
    // on the class.
    let ts_type =
      type_tag(&property.js_doc).or(property_tag.map(|(ts_type, _)| ts_type));
    if let Some(ts_type) = ts_type {
      property.ts_type = Some(ts_type.clone());
    }
    if property.js_doc.doc.is_none()
      && let Some((_, doc)) = property_tag
    {
      property.js_doc.doc = doc.clone();
    }
  }
}

/// Applies the types of the `@param` tags in `js_doc` to `params`. Tags are
/// matched by the name a parameter binds; a destructured parameter binds no
/// single name and so takes the tag at its position, unless that tag names
/// another parameter — the same matching the HTML parameter docs use.
fn apply_to_params(
  module_info: &EsModuleInfo,
  js_doc: &JsDoc,
  mut params: Vec<&mut ParamDef>,
) {
  // `@param options.field` documents a property of a parameter rather than a
  // parameter, so it never applies to one.
  let param_tags = js_doc
    .tags
    .iter()
    .filter_map(|tag| match tag {
      JsDocTag::Param {
        name,
        ts_type,
        optional,
        ..
      } if !name.contains('.') => Some((&**name, ts_type.as_ref(), *optional)),
      _ => None,
    })
    .collect::<Vec<_>>();
  if param_tags.is_empty() {
    return;
  }

  let bound_names = params
    .iter()
    .filter_map(|param| param.binding_name().map(str::to_owned))
    .collect::<HashSet<_>>();

  for (i, param) in params.iter_mut().enumerate() {
    let tag = match param.binding_name() {
      Some(binding_name) => {
        param_tags.iter().find(|(name, ..)| *name == binding_name)
      }
      None => param_tags
        .get(i)
        .filter(|(name, ..)| !bound_names.contains(*name)),
    };
    let Some((_, ts_type, optional)) = tag else {
      continue;
    };
    if let Some(ts_type) = ts_type {
      set_param_type(module_info, param, ts_type, *optional);
    }
  }
}

fn set_param_type(
  module_info: &EsModuleInfo,
  param: &mut ParamDef,
  ts_type: &TsTypeDef,
  is_optional: bool,
) {
  match &mut param.pattern {
    // A parameter with a default value is already optional, and its type
    // lives on the binding it wraps.
    ParamPatternDef::Assign { left, .. } => {
      left.ts_type = Some(ts_type.clone());
    }
    ParamPatternDef::Rest { .. } => {
      param.ts_type = Some(rest_param_type(module_info, ts_type));
    }
    ParamPatternDef::Identifier { optional, .. }
    | ParamPatternDef::Array { optional, .. }
    | ParamPatternDef::Object { optional, .. } => {
      param.ts_type = Some(ts_type.clone());
      // `@param {string} [name]` marks the parameter optional.
      *optional |= is_optional;
    }
  }
}

/// A rest parameter is documented either with its array type
/// (`@param {string[]} args`) or with JSDoc's variadic syntax
/// (`@param {...string} args`), which stands for an array of the element type.
fn rest_param_type(
  module_info: &EsModuleInfo,
  ts_type: &TsTypeDef,
) -> TsTypeDef {
  if let TsTypeDefKind::Unsupported = ts_type.kind
    && let Some(element_type) = ts_type.repr.strip_prefix("...")
    && let Some(element_type) = parse_jsdoc_type(module_info, element_type)
  {
    TsTypeDef {
      repr: String::new(),
      kind: TsTypeDefKind::Array(Box::new(element_type)),
    }
  } else {
    ts_type.clone()
  }
}

fn type_tag(js_doc: &JsDoc) -> Option<&TsTypeDef> {
  js_doc.tags.iter().find_map(|tag| match tag {
    JsDocTag::TypeRef { ts_type, .. } => Some(ts_type),
    _ => None,
  })
}
