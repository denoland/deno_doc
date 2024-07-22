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

#[derive(
  Debug, Serialize, Deserialize, Clone, PartialEq, Eq, Hash, Default,
)]
#[serde(rename_all = "camelCase")]
pub struct Location {
  pub filename: Box<str>,
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
  pub name: Box<str>,
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub is_default: Option<bool>,
  pub location: Location,
  pub declaration_kind: DeclarationKind,
  #[serde(skip_serializing_if = "JsDoc::is_empty", default)]
  pub js_doc: JsDoc,
  #[serde(flatten)]
  pub def: DocNodeDef,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum DocNodeDef {
  #[serde(rename_all = "camelCase")]
  Function {
    function_def: super::function::FunctionDef,
  },
  #[serde(rename_all = "camelCase")]
  Variable {
    variable_def: super::variable::VariableDef,
  },
  #[serde(rename_all = "camelCase")]
  Enum {
    enum_def: super::r#enum::EnumDef,
  },
  #[serde(rename_all = "camelCase")]
  Class {
    class_def: super::class::ClassDef,
  },
  #[serde(rename_all = "camelCase")]
  TypeAlias {
    type_alias_def: super::type_alias::TypeAliasDef,
  },
  #[serde(rename_all = "camelCase")]
  Namespace {
    namespace_def: NamespaceDef,
  },
  #[serde(rename_all = "camelCase")]
  Interface {
    interface_def: super::interface::InterfaceDef,
  },
  #[serde(rename_all = "camelCase")]
  Import {
    import_def: ImportDef,
  },
  ModuleDoc,
}

impl Default for DocNode {
  fn default() -> Self {
    Self {
      is_default: None,
      name: "".into(),
      declaration_kind: DeclarationKind::Private,
      location: Location::default(),
      js_doc: JsDoc::default(),
      def: DocNodeDef::ModuleDoc,
    }
  }
}

impl DocNode {
  pub fn module_doc(location: Location, js_doc: JsDoc) -> Self {
    Self {
      name: "".into(),
      is_default: None,
      location,
      declaration_kind: DeclarationKind::Export,
      js_doc,
      def: DocNodeDef::ModuleDoc,
    }
  }

  pub fn function(
    name: Box<str>,
    is_default: bool,
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    fn_def: super::function::FunctionDef,
  ) -> Self {
    Self {
      name,
      is_default: Some(is_default),
      location,
      declaration_kind,
      js_doc,
      def: DocNodeDef::Function {
        function_def: fn_def,
      },
    }
  }

  pub fn variable(
    name: Box<str>,
    is_default: bool,
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    var_def: super::variable::VariableDef,
  ) -> Self {
    Self {
      name,
      is_default: Some(is_default),
      declaration_kind,
      location,
      js_doc,
      def: DocNodeDef::Variable {
        variable_def: var_def,
      },
    }
  }

  pub fn r#enum(
    name: Box<str>,
    is_default: bool,
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    enum_def: super::r#enum::EnumDef,
  ) -> Self {
    Self {
      name,
      is_default: Some(is_default),
      declaration_kind,
      location,
      js_doc,
      def: DocNodeDef::Enum { enum_def },
    }
  }

  pub fn class(
    name: Box<str>,
    is_default: bool,
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    class_def: super::class::ClassDef,
  ) -> Self {
    Self {
      name,
      is_default: Some(is_default),
      declaration_kind,
      location,
      js_doc,
      def: DocNodeDef::Class { class_def },
    }
  }

  pub fn type_alias(
    name: Box<str>,
    is_default: bool,
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    type_alias_def: super::type_alias::TypeAliasDef,
  ) -> Self {
    Self {
      name,
      is_default: Some(is_default),
      declaration_kind,
      location,
      js_doc,
      def: DocNodeDef::TypeAlias { type_alias_def },
    }
  }

  pub fn namespace(
    name: Box<str>,
    is_default: bool,
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    namespace_def: NamespaceDef,
  ) -> Self {
    Self {
      name,
      is_default: Some(is_default),
      declaration_kind,
      location,
      js_doc,
      def: DocNodeDef::Namespace { namespace_def },
    }
  }

  pub fn interface(
    name: Box<str>,
    is_default: bool,
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    interface_def: super::interface::InterfaceDef,
  ) -> Self {
    Self {
      name,
      is_default: Some(is_default),
      declaration_kind,
      location,
      js_doc,
      def: DocNodeDef::Interface { interface_def },
    }
  }

  pub fn import(
    name: Box<str>,
    location: Location,
    js_doc: JsDoc,
    import_def: ImportDef,
  ) -> Self {
    Self {
      name,
      is_default: None,
      declaration_kind: DeclarationKind::Private,
      location,
      js_doc,
      def: DocNodeDef::Import { import_def },
    }
  }

  pub fn get_name(&self) -> &str {
    let default_name = match &self.def {
      DocNodeDef::Class { class_def } => class_def.def_name.as_deref(),
      DocNodeDef::Function { function_def } => function_def.def_name.as_deref(),
      DocNodeDef::Interface { interface_def } => {
        interface_def.def_name.as_deref()
      }
      DocNodeDef::Enum { .. }
      | DocNodeDef::Import { .. }
      | DocNodeDef::ModuleDoc { .. }
      | DocNodeDef::Namespace { .. }
      | DocNodeDef::TypeAlias { .. }
      | DocNodeDef::Variable { .. } => None,
    };

    default_name.unwrap_or(&self.name)
  }

  pub fn kind(&self) -> DocNodeKind {
    match &self.def {
      DocNodeDef::Class { .. } => DocNodeKind::Class,
      DocNodeDef::Function { .. } => DocNodeKind::Function,
      DocNodeDef::Variable { .. } => DocNodeKind::Variable,
      DocNodeDef::Enum { .. } => DocNodeKind::Enum,
      DocNodeDef::TypeAlias { .. } => DocNodeKind::TypeAlias,
      DocNodeDef::Namespace { .. } => DocNodeKind::Namespace,
      DocNodeDef::Interface { .. } => DocNodeKind::Interface,
      DocNodeDef::Import { .. } => DocNodeKind::Import,
      DocNodeDef::ModuleDoc => DocNodeKind::ModuleDoc,
    }
  }

  pub fn class_def(&self) -> Option<&super::class::ClassDef> {
    match &self.def {
      DocNodeDef::Class { class_def } => Some(class_def),
      _ => None,
    }
  }

  pub fn function_def(&self) -> Option<&super::function::FunctionDef> {
    match &self.def {
      DocNodeDef::Function { function_def } => Some(function_def),
      _ => None,
    }
  }

  pub fn variable_def(&self) -> Option<&super::variable::VariableDef> {
    match &self.def {
      DocNodeDef::Variable { variable_def } => Some(variable_def),
      _ => None,
    }
  }

  pub fn enum_def(&self) -> Option<&super::r#enum::EnumDef> {
    match &self.def {
      DocNodeDef::Enum { enum_def } => Some(enum_def),
      _ => None,
    }
  }

  pub fn type_alias_def(&self) -> Option<&super::type_alias::TypeAliasDef> {
    match &self.def {
      DocNodeDef::TypeAlias { type_alias_def } => Some(type_alias_def),
      _ => None,
    }
  }

  pub fn namespace_def(&self) -> Option<&NamespaceDef> {
    match &self.def {
      DocNodeDef::Namespace { namespace_def } => Some(namespace_def),
      _ => None,
    }
  }

  pub fn interface_def(&self) -> Option<&super::interface::InterfaceDef> {
    match &self.def {
      DocNodeDef::Interface { interface_def } => Some(interface_def),
      _ => None,
    }
  }

  pub fn import_def(&self) -> Option<&ImportDef> {
    match &self.def {
      DocNodeDef::Import { import_def } => Some(import_def),
      _ => None,
    }
  }
}
