// Copyright 2020-2021 the Deno authors. All rights reserved. MIT license.

use serde::Deserialize;
use serde::Serialize;

use crate::js_doc::JsDoc;

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub enum DocNodeKind {
  Function,
  Variable,
  Class,
  Enum,
  Interface,
  TypeAlias,
  Namespace,
  Import,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct Location {
  pub filename: String,
  pub line: usize,
  pub col: usize,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub enum ReexportKind {
  /// export * from "./path/to/module.js";
  All,
  /// export * as someNamespace from "./path/to/module.js";
  Namespace(String),
  /// (identifier, optional alias)
  /// export { foo } from "./path/to/module.js";
  /// export { foo as bar } from "./path/to/module.js";
  Named(String, Option<String>),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Reexport {
  pub kind: ReexportKind,
  pub src: String,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ModuleDoc {
  pub definitions: Vec<DocNode>,
  pub reexports: Vec<Reexport>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ImportDef {
  pub src: String,
  pub imported: Option<String>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct DocNode {
  pub kind: DocNodeKind,
  pub name: String,
  pub location: Location,
  #[serde(skip_serializing_if = "JsDoc::is_empty")]
  pub js_doc: JsDoc,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub function_def: Option<super::function::FunctionDef>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub variable_def: Option<super::variable::VariableDef>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub enum_def: Option<super::r#enum::EnumDef>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub class_def: Option<super::class::ClassDef>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_alias_def: Option<super::type_alias::TypeAliasDef>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub namespace_def: Option<super::namespace::NamespaceDef>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub interface_def: Option<super::interface::InterfaceDef>,

  #[serde(skip_serializing_if = "Option::is_none")]
  pub import_def: Option<ImportDef>,
}

impl DocNode {
  pub fn function(
    name: String,
    location: Location,
    js_doc: JsDoc,
    fn_def: super::function::FunctionDef,
  ) -> Self {
    Self {
      kind: DocNodeKind::Function,
      name,
      location,
      js_doc,
      function_def: Some(fn_def),
      variable_def: None,
      enum_def: None,
      class_def: None,
      type_alias_def: None,
      namespace_def: None,
      interface_def: None,
      import_def: None,
    }
  }

  pub fn variable(
    name: String,
    location: Location,
    js_doc: JsDoc,
    var_def: super::variable::VariableDef,
  ) -> Self {
    Self {
      kind: DocNodeKind::Variable,
      name,
      location,
      js_doc,
      function_def: None,
      variable_def: Some(var_def),
      enum_def: None,
      class_def: None,
      type_alias_def: None,
      namespace_def: None,
      interface_def: None,
      import_def: None,
    }
  }

  pub fn r#enum(
    name: String,
    location: Location,
    js_doc: JsDoc,
    enum_def: super::r#enum::EnumDef,
  ) -> Self {
    Self {
      kind: DocNodeKind::Enum,
      name,
      location,
      js_doc,
      function_def: None,
      variable_def: None,
      enum_def: Some(enum_def),
      class_def: None,
      type_alias_def: None,
      namespace_def: None,
      interface_def: None,
      import_def: None,
    }
  }

  pub fn class(
    name: String,
    location: Location,
    js_doc: JsDoc,
    class_def: super::class::ClassDef,
  ) -> Self {
    Self {
      kind: DocNodeKind::Class,
      name,
      location,
      js_doc,
      function_def: None,
      variable_def: None,
      enum_def: None,
      class_def: Some(class_def),
      type_alias_def: None,
      namespace_def: None,
      interface_def: None,
      import_def: None,
    }
  }

  pub fn type_alias(
    name: String,
    location: Location,
    js_doc: JsDoc,
    type_alias_def: super::type_alias::TypeAliasDef,
  ) -> Self {
    Self {
      kind: DocNodeKind::TypeAlias,
      name,
      location,
      js_doc,
      function_def: None,
      variable_def: None,
      enum_def: None,
      class_def: None,
      type_alias_def: Some(type_alias_def),
      namespace_def: None,
      interface_def: None,
      import_def: None,
    }
  }

  pub fn namespace(
    name: String,
    location: Location,
    js_doc: JsDoc,
    namespace_def: super::namespace::NamespaceDef,
  ) -> Self {
    Self {
      kind: DocNodeKind::Namespace,
      name,
      location,
      js_doc,
      function_def: None,
      variable_def: None,
      enum_def: None,
      class_def: None,
      type_alias_def: None,
      namespace_def: Some(namespace_def),
      interface_def: None,
      import_def: None,
    }
  }

  pub fn interface(
    name: String,
    location: Location,
    js_doc: JsDoc,
    interface_def: super::interface::InterfaceDef,
  ) -> Self {
    Self {
      kind: DocNodeKind::Interface,
      name,
      location,
      js_doc,
      function_def: None,
      variable_def: None,
      enum_def: None,
      class_def: None,
      type_alias_def: None,
      namespace_def: None,
      interface_def: Some(interface_def),
      import_def: None,
    }
  }

  pub fn import(
    name: String,
    location: Location,
    js_doc: JsDoc,
    import_def: ImportDef,
  ) -> Self {
    Self {
      kind: DocNodeKind::Import,
      name,
      location,
      js_doc,
      function_def: None,
      variable_def: None,
      enum_def: None,
      class_def: None,
      type_alias_def: None,
      namespace_def: None,
      interface_def: None,
      import_def: Some(import_def),
    }
  }
}
