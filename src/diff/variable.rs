// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use super::DiffEntry;
use super::ts_type::TsTypeDiff;
use crate::variable::VariableDef;
use deno_ast::swc::ast::VarDeclKind;
use serde::Deserialize;
use serde::Serialize;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct VariableDiff {
  #[serde(skip_serializing_if = "Option::is_none")]
  pub ts_type_change: Option<TsTypeDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub kind_change: Option<DiffEntry<VarDeclKind>>,
}

impl VariableDiff {
  pub fn diff(old: &VariableDef, new: &VariableDef) -> Option<Self> {
    let ts_type_change = match (&old.ts_type, &new.ts_type) {
      (Some(old_type), Some(new_type)) => TsTypeDiff::diff(old_type, new_type),
      (None, None) => None,
      (Some(old_type), None) => Some(TsTypeDiff {
        old: old_type.clone(),
        new: crate::ts_type::TsTypeDef::keyword("unknown"),
      }),
      (None, Some(new_type)) => Some(TsTypeDiff {
        old: crate::ts_type::TsTypeDef::keyword("unknown"),
        new: new_type.clone(),
      }),
    };

    let kind_change = if old.kind != new.kind {
      Some(DiffEntry::modified(old.kind, new.kind))
    } else {
      None
    };

    if ts_type_change.is_none() && kind_change.is_none() {
      return None;
    }

    Some(VariableDiff {
      ts_type_change,
      kind_change,
    })
  }
}
