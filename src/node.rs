// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use crate::js_doc::JsDoc;
use serde::Deserialize;
use serde::Serialize;
use std::sync::Arc;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct NamespaceDef {
  pub elements: Vec<Arc<Symbol>>,
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
  Reference,
  TypeAlias,
  Variable,
}

impl From<&DeclarationDef> for DocNodeKind {
  fn from(def: &DeclarationDef) -> Self {
    match def {
      DeclarationDef::Class(..) => Self::Class,
      DeclarationDef::Enum(..) => Self::Enum,
      DeclarationDef::Function(..) => Self::Function,
      DeclarationDef::Import(..) => Self::Import,
      DeclarationDef::Interface(..) => Self::Interface,
      DeclarationDef::ModuleDoc => Self::ModuleDoc,
      DeclarationDef::Namespace(..) => Self::Namespace,
      DeclarationDef::Reference(..) => Self::Reference,
      DeclarationDef::TypeAlias(..) => Self::TypeAlias,
      DeclarationDef::Variable(..) => Self::Variable,
    }
  }
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
pub struct ImportDef {
  pub src: String,
  pub imported: Option<String>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ReferenceDef {
  pub target: Location,
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
pub struct Declaration {
  pub location: Location,
  pub declaration_kind: DeclarationKind,
  #[serde(skip_serializing_if = "JsDoc::is_empty", default)]
  pub js_doc: JsDoc,
  #[serde(flatten)]
  pub def: DeclarationDef,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Symbol {
  pub name: Box<str>,
  #[serde(skip_serializing_if = "std::ops::Not::not", default)]
  pub is_default: bool,
  pub declarations: Vec<Declaration>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(tag = "kind", content = "def", rename_all = "camelCase")]
pub enum DeclarationDef {
  Function(super::function::FunctionDef),
  Variable(super::variable::VariableDef),
  Enum(super::r#enum::EnumDef),
  Class(super::class::ClassDef),
  TypeAlias(super::type_alias::TypeAliasDef),
  Namespace(NamespaceDef),
  Interface(super::interface::InterfaceDef),
  Import(ImportDef),
  ModuleDoc,
  Reference(ReferenceDef),
}

impl DeclarationDef {
  pub fn to_kind(&self) -> DocNodeKind {
    match self {
      DeclarationDef::Function(_) => DocNodeKind::Function,
      DeclarationDef::Variable(_) => DocNodeKind::Variable,
      DeclarationDef::Enum(_) => DocNodeKind::Enum,
      DeclarationDef::Class(_) => DocNodeKind::Class,
      DeclarationDef::TypeAlias(_) => DocNodeKind::TypeAlias,
      DeclarationDef::Namespace(_) => DocNodeKind::Namespace,
      DeclarationDef::Interface(_) => DocNodeKind::Interface,
      DeclarationDef::Import(_) => DocNodeKind::Import,
      DeclarationDef::ModuleDoc => DocNodeKind::ModuleDoc,
      DeclarationDef::Reference(_) => DocNodeKind::Reference,
    }
  }
}

impl Declaration {
  pub fn class_def(&self) -> Option<&super::class::ClassDef> {
    match &self.def {
      DeclarationDef::Class(class_def) => Some(class_def),
      _ => None,
    }
  }

  pub fn function_def(&self) -> Option<&super::function::FunctionDef> {
    match &self.def {
      DeclarationDef::Function(function_def) => Some(function_def),
      _ => None,
    }
  }

  pub fn variable_def(&self) -> Option<&super::variable::VariableDef> {
    match &self.def {
      DeclarationDef::Variable(variable_def) => Some(variable_def),
      _ => None,
    }
  }

  pub fn enum_def(&self) -> Option<&super::r#enum::EnumDef> {
    match &self.def {
      DeclarationDef::Enum(enum_def) => Some(enum_def),
      _ => None,
    }
  }

  pub fn type_alias_def(&self) -> Option<&super::type_alias::TypeAliasDef> {
    match &self.def {
      DeclarationDef::TypeAlias(type_alias_def) => Some(type_alias_def),
      _ => None,
    }
  }

  pub fn namespace_def(&self) -> Option<&NamespaceDef> {
    match &self.def {
      DeclarationDef::Namespace(namespace_def) => Some(namespace_def),
      _ => None,
    }
  }

  pub fn interface_def(&self) -> Option<&super::interface::InterfaceDef> {
    match &self.def {
      DeclarationDef::Interface(interface_def) => Some(interface_def),
      _ => None,
    }
  }

  pub fn import_def(&self) -> Option<&ImportDef> {
    match &self.def {
      DeclarationDef::Import(import_def) => Some(import_def),
      _ => None,
    }
  }

  pub fn reference_def(&self) -> Option<&ReferenceDef> {
    match &self.def {
      DeclarationDef::Reference(reference_def) => Some(reference_def),
      _ => None,
    }
  }

  pub fn function(
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    fn_def: super::function::FunctionDef,
  ) -> Self {
    Self {
      location,
      declaration_kind,
      js_doc,
      def: DeclarationDef::Function(fn_def),
    }
  }

  pub fn variable(
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    var_def: super::variable::VariableDef,
  ) -> Self {
    Self {
      location,
      declaration_kind,
      js_doc,
      def: DeclarationDef::Variable(var_def),
    }
  }

  pub fn r#enum(
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    enum_def: super::r#enum::EnumDef,
  ) -> Self {
    Self {
      location,
      declaration_kind,
      js_doc,
      def: DeclarationDef::Enum(enum_def),
    }
  }

  pub fn class(
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    class_def: super::class::ClassDef,
  ) -> Self {
    Self {
      location,
      declaration_kind,
      js_doc,
      def: DeclarationDef::Class(class_def),
    }
  }

  pub fn type_alias(
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    type_alias_def: super::type_alias::TypeAliasDef,
  ) -> Self {
    Self {
      location,
      declaration_kind,
      js_doc,
      def: DeclarationDef::TypeAlias(type_alias_def),
    }
  }

  pub fn namespace(
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    namespace_def: NamespaceDef,
  ) -> Self {
    Self {
      location,
      declaration_kind,
      js_doc,
      def: DeclarationDef::Namespace(namespace_def),
    }
  }

  pub fn interface(
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    interface_def: super::interface::InterfaceDef,
  ) -> Self {
    Self {
      location,
      declaration_kind,
      js_doc,
      def: DeclarationDef::Interface(interface_def),
    }
  }

  pub fn import(
    location: Location,
    js_doc: JsDoc,
    import_def: ImportDef,
  ) -> Self {
    Self {
      declaration_kind: DeclarationKind::Private,
      location,
      js_doc,
      def: DeclarationDef::Import(import_def),
    }
  }

  pub fn reference(
    location: Location,
    js_doc: JsDoc,
    reference_def: ReferenceDef,
  ) -> Self {
    Self {
      declaration_kind: DeclarationKind::Private,
      location,
      js_doc,
      def: DeclarationDef::Reference(reference_def),
    }
  }

  pub fn module_doc(location: Location, js_doc: JsDoc) -> Self {
    Self {
      location,
      declaration_kind: DeclarationKind::Export,
      js_doc,
      def: DeclarationDef::ModuleDoc,
    }
  }
}

impl Default for Symbol {
  fn default() -> Self {
    Self {
      is_default: false,
      name: "".into(),
      declarations: vec![Declaration {
        declaration_kind: DeclarationKind::Private,
        location: Location::default(),
        js_doc: JsDoc::default(),
        def: DeclarationDef::ModuleDoc,
      }],
    }
  }
}

impl Symbol {
  fn single(
    name: Box<str>,
    is_default: bool,
    declaration: Declaration,
  ) -> Self {
    Self {
      name,
      is_default,
      declarations: vec![declaration],
    }
  }

  pub fn module_doc(location: Location, js_doc: JsDoc) -> Self {
    Self::single("".into(), false, Declaration::module_doc(location, js_doc))
  }

  pub fn function(
    name: Box<str>,
    is_default: bool,
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    fn_def: super::function::FunctionDef,
  ) -> Self {
    Self::single(
      name,
      is_default,
      Declaration::function(location, declaration_kind, js_doc, fn_def),
    )
  }

  pub fn variable(
    name: Box<str>,
    is_default: bool,
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    var_def: super::variable::VariableDef,
  ) -> Self {
    Self::single(
      name,
      is_default,
      Declaration::variable(location, declaration_kind, js_doc, var_def),
    )
  }

  pub fn r#enum(
    name: Box<str>,
    is_default: bool,
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    enum_def: super::r#enum::EnumDef,
  ) -> Self {
    Self::single(
      name,
      is_default,
      Declaration::r#enum(location, declaration_kind, js_doc, enum_def),
    )
  }

  pub fn class(
    name: Box<str>,
    is_default: bool,
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    class_def: super::class::ClassDef,
  ) -> Self {
    Self::single(
      name,
      is_default,
      Declaration::class(location, declaration_kind, js_doc, class_def),
    )
  }

  pub fn type_alias(
    name: Box<str>,
    is_default: bool,
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    type_alias_def: super::type_alias::TypeAliasDef,
  ) -> Self {
    Self::single(
      name,
      is_default,
      Declaration::type_alias(
        location,
        declaration_kind,
        js_doc,
        type_alias_def,
      ),
    )
  }

  pub fn namespace(
    name: Box<str>,
    is_default: bool,
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    namespace_def: NamespaceDef,
  ) -> Self {
    Self::single(
      name,
      is_default,
      Declaration::namespace(location, declaration_kind, js_doc, namespace_def),
    )
  }

  pub fn interface(
    name: Box<str>,
    is_default: bool,
    location: Location,
    declaration_kind: DeclarationKind,
    js_doc: JsDoc,
    interface_def: super::interface::InterfaceDef,
  ) -> Self {
    Self::single(
      name,
      is_default,
      Declaration::interface(location, declaration_kind, js_doc, interface_def),
    )
  }

  pub fn import(
    name: Box<str>,
    location: Location,
    js_doc: JsDoc,
    import_def: ImportDef,
  ) -> Self {
    Self::single(
      name,
      false,
      Declaration::import(location, js_doc, import_def),
    )
  }

  pub fn reference(
    name: Box<str>,
    location: Location,
    js_doc: JsDoc,
    reference_def: ReferenceDef,
  ) -> Self {
    Self::single(
      name,
      false,
      Declaration::reference(location, js_doc, reference_def),
    )
  }

  pub fn get_name(&self) -> &str {
    let default_name =
      self.declarations.iter().find_map(|decl| match &decl.def {
        DeclarationDef::Class(class_def) => class_def.def_name.as_deref(),
        DeclarationDef::Function(function_def) => {
          function_def.def_name.as_deref()
        }
        DeclarationDef::Interface(interface_def) => {
          interface_def.def_name.as_deref()
        }
        DeclarationDef::Enum(..)
        | DeclarationDef::Import(..)
        | DeclarationDef::ModuleDoc
        | DeclarationDef::Namespace(..)
        | DeclarationDef::TypeAlias(..)
        | DeclarationDef::Variable(..)
        | DeclarationDef::Reference(..) => None,
      });

    default_name.unwrap_or(self.name.as_ref())
  }
}
