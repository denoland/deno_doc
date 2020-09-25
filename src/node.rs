// Copyright 2018-2020 the Deno authors. All rights reserved. MIT license.
use serde::{Deserialize, Serialize};

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

impl Into<Location> for swc_common::Loc {
  fn into(self) -> Location {
    use swc_common::FileName::*;

    let filename = match &self.file.name {
      Real(path_buf) => path_buf.to_string_lossy().to_string(),
      Custom(str_) => str_.to_string(),
      _ => panic!("invalid filename"),
    };

    Location {
      filename,
      line: self.line,
      col: self.col_display,
    }
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub enum ReexportKind {
  /// export * from "./path/to/module.js";
  All,
  /// export * as someNamespace from "./path/to/module.js";
  Namespace(String),
  /// export default from "./path/to/module.js";
  Default,
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
  pub js_doc: Option<String>,

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
