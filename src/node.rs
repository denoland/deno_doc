// Copyright 2020-2022 the Deno authors. All rights reserved. MIT license.

use deno_graph::ModuleSpecifier;
use serde::Deserialize;
use serde::Serialize;

use crate::js_doc::JsDoc;

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub enum DocNodeKind {
  ModuleDoc,
  Function,
  Variable,
  Class,
  Enum,
  Interface,
  TypeAlias,
  Namespace,
  Import,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub struct Location {
  pub filename: String,
  /// The 1-indexed display line.
  /// todo(#150): why is one of these 0-indexed and the other 1-indexed?
  pub line: usize,
  /// The 0-indexed display column.
  pub col: usize,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub enum ReexportKind {
  /// export * as someNamespace from "./path/to/module.js";
  Namespace,
  /// (identifier, optional alias)
  /// export { foo } from "./path/to/module.js";
  /// export { foo as bar } from "./path/to/module.js";
  Named,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Reexport {
  pub name: String,
  pub kind: ReexportKind,
  pub locations: Vec<Location>,
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
pub enum DeclarationKind {
  Private,
  Declare,
  Export,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct DocNode {
  pub kind: DocNodeKind,
  pub name: String,
  pub location: Location,
  pub declaration_kind: DeclarationKind,
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

impl Default for DocNode {
  fn default() -> Self {
    Self {
      kind: DocNodeKind::ModuleDoc,
      name: "".to_string(),
      declaration_kind: DeclarationKind::Private,
      location: Location {
        filename: "".to_string(),
        line: 0,
        col: 0,
      },
      js_doc: JsDoc::default(),
      function_def: None,
      variable_def: None,
      enum_def: None,
      class_def: None,
      type_alias_def: None,
      namespace_def: None,
      interface_def: None,
      import_def: None,
    }
  }
}

impl DocNode {
  pub fn module_doc(location: Location, js_doc: JsDoc) -> Self {
    Self {
      kind: DocNodeKind::ModuleDoc,
      name: "".to_string(),
      location,
      declaration_kind: DeclarationKind::Export,
      js_doc,
      ..Default::default()
    }
  }

  pub fn function(
    name: String,
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    fn_def: super::function::FunctionDef,
  ) -> Self {
    Self {
      kind: DocNodeKind::Function,
      name,
      location,
      declaration_kind,
      js_doc,
      function_def: Some(fn_def),
      ..Default::default()
    }
  }

  pub fn variable(
    name: String,
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    var_def: super::variable::VariableDef,
  ) -> Self {
    Self {
      kind: DocNodeKind::Variable,
      name,
      declaration_kind,
      location,
      js_doc,
      variable_def: Some(var_def),
      ..Default::default()
    }
  }

  pub fn r#enum(
    name: String,
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    enum_def: super::r#enum::EnumDef,
  ) -> Self {
    Self {
      kind: DocNodeKind::Enum,
      name,
      declaration_kind,
      location,
      js_doc,
      enum_def: Some(enum_def),
      ..Default::default()
    }
  }

  pub fn class(
    name: String,
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    class_def: super::class::ClassDef,
  ) -> Self {
    Self {
      kind: DocNodeKind::Class,
      name,
      declaration_kind,
      location,
      js_doc,
      class_def: Some(class_def),
      ..Default::default()
    }
  }

  pub fn type_alias(
    name: String,
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    type_alias_def: super::type_alias::TypeAliasDef,
  ) -> Self {
    Self {
      kind: DocNodeKind::TypeAlias,
      name,
      declaration_kind,
      location,
      js_doc,
      type_alias_def: Some(type_alias_def),
      ..Default::default()
    }
  }

  pub fn namespace(
    name: String,
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    namespace_def: super::namespace::NamespaceDef,
  ) -> Self {
    Self {
      kind: DocNodeKind::Namespace,
      name,
      declaration_kind,
      location,
      js_doc,
      namespace_def: Some(namespace_def),
      ..Default::default()
    }
  }

  pub fn interface(
    name: String,
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    interface_def: super::interface::InterfaceDef,
  ) -> Self {
    Self {
      kind: DocNodeKind::Interface,
      name,
      declaration_kind,
      location,
      js_doc,
      interface_def: Some(interface_def),
      ..Default::default()
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
      declaration_kind: DeclarationKind::Private,
      location,
      js_doc,
      import_def: Some(import_def),
      ..Default::default()
    }
  }
}
