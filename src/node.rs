// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use serde::Deserialize;
use serde::Serialize;
use std::rc::Rc;

use crate::js_doc::JsDoc;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct NamespaceDef {
  pub elements: Vec<Rc<DocNode>>,
}

#[derive(
  Debug,
  PartialEq,
  Eq,
  PartialOrd,
  Ord,
  Hash,
  Serialize,
  Deserialize,
  Clone,
  Copy,
)]
#[serde(rename_all = "camelCase")]
pub enum DocNodeKind {
  // NOTE(bartlomieju): Because of `derive(Ord), we must keep the variants
  // in an alphabetical order.
  Class,
  Enum,
  Function,
  Import,
  Interface,
  ModuleDoc,
  Namespace,
  TypeAlias,
  Variable,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, Hash)]
#[serde(rename_all = "camelCase")]
pub struct Location {
  pub filename: String,
  /// The 1-indexed display line.
  /// todo(#150): why is one of these 0-indexed and the other 1-indexed?
  pub line: usize,
  /// The 0-indexed display column.
  pub col: usize,
  #[serde(default)]
  /// The 0-indexed byte offset in the source text.
  pub byte_index: usize,
}

impl Ord for Location {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    match self.filename.cmp(&other.filename) {
      core::cmp::Ordering::Equal => match self.line.cmp(&other.line) {
        core::cmp::Ordering::Equal => self.col.cmp(&other.col),
        ord => ord,
      },
      ord => ord,
    }
  }
}

impl PartialOrd for Location {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
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

#[derive(Debug, Serialize, Deserialize, Clone, Copy, PartialEq)]
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
  #[serde(skip_serializing_if = "JsDoc::is_empty", default)]
  pub js_doc: JsDoc,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub function_def: Option<super::function::FunctionDef>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub variable_def: Option<super::variable::VariableDef>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub enum_def: Option<super::r#enum::EnumDef>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub class_def: Option<super::class::ClassDef>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub type_alias_def: Option<super::type_alias::TypeAliasDef>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub namespace_def: Option<NamespaceDef>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub interface_def: Option<super::interface::InterfaceDef>,

  #[serde(skip_serializing_if = "Option::is_none", default)]
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
        byte_index: 0,
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
    namespace_def: NamespaceDef,
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

  pub fn get_name(&self) -> &str {
    let default_name = match self.kind {
      DocNodeKind::Class => self.class_def.as_ref().unwrap().def_name.as_ref(),
      DocNodeKind::Function => {
        self.function_def.as_ref().unwrap().def_name.as_ref()
      }
      DocNodeKind::Interface => {
        self.interface_def.as_ref().unwrap().def_name.as_ref()
      }
      DocNodeKind::Enum
      | DocNodeKind::Import
      | DocNodeKind::ModuleDoc
      | DocNodeKind::Namespace
      | DocNodeKind::TypeAlias
      | DocNodeKind::Variable => None,
    };

    default_name.unwrap_or(&self.name)
  }
}
