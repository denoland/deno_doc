// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

//! Local wrapper enums for OXC types that need serde support
//! with serialization compatible with the old SWC types.

use deno_ast::oxc::ast::ast::MethodDefinitionKind;
use deno_ast::oxc::ast::ast::TSAccessibility;
use deno_ast::oxc::ast::ast::TSMethodSignatureKind;
use deno_ast::oxc::ast::ast::VariableDeclarationKind;
use serde::Deserialize;
use serde::Serialize;

/// Mirrors the old SWC `Accessibility` enum with matching serde.
#[derive(Debug, Serialize, Deserialize, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum Accessibility {
  Public,
  Protected,
  Private,
}

impl From<TSAccessibility> for Accessibility {
  fn from(a: TSAccessibility) -> Self {
    match a {
      TSAccessibility::Public => Accessibility::Public,
      TSAccessibility::Protected => Accessibility::Protected,
      TSAccessibility::Private => Accessibility::Private,
    }
  }
}

impl Accessibility {
  pub fn from_oxc(a: Option<TSAccessibility>) -> Option<Self> {
    a.map(Self::from)
  }
}

/// Mirrors the old SWC `MethodKind` enum with matching serde.
#[derive(Debug, Serialize, Deserialize, Clone, Copy, PartialEq, Eq, Hash)]
#[serde(rename_all = "camelCase")]
pub enum MethodKind {
  Method,
  Getter,
  Setter,
}

impl From<MethodDefinitionKind> for MethodKind {
  fn from(k: MethodDefinitionKind) -> Self {
    match k {
      MethodDefinitionKind::Method | MethodDefinitionKind::Constructor => {
        MethodKind::Method
      }
      MethodDefinitionKind::Get => MethodKind::Getter,
      MethodDefinitionKind::Set => MethodKind::Setter,
    }
  }
}

impl From<TSMethodSignatureKind> for MethodKind {
  fn from(k: TSMethodSignatureKind) -> Self {
    match k {
      TSMethodSignatureKind::Method => MethodKind::Method,
      TSMethodSignatureKind::Get => MethodKind::Getter,
      TSMethodSignatureKind::Set => MethodKind::Setter,
    }
  }
}

/// Mirrors the old SWC `VarDeclKind` enum with matching serde.
#[derive(Debug, Serialize, Deserialize, Clone, Copy, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum VarDeclKind {
  Var,
  Let,
  Const,
}

impl From<VariableDeclarationKind> for VarDeclKind {
  fn from(k: VariableDeclarationKind) -> Self {
    match k {
      VariableDeclarationKind::Var => VarDeclKind::Var,
      VariableDeclarationKind::Let => VarDeclKind::Let,
      VariableDeclarationKind::Const
      | VariableDeclarationKind::Using
      | VariableDeclarationKind::AwaitUsing => VarDeclKind::Const,
    }
  }
}
