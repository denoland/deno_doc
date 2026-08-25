// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use super::ts_type::TsTypeDiff;
use super::ts_type::TypeParamsDiff;
use crate::type_alias::TypeAliasDef;
use serde::Deserialize;
use serde::Serialize;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TypeAliasDiff {
  #[serde(skip_serializing_if = "Option::is_none")]
  pub ts_type_change: Option<TsTypeDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_params_change: Option<TypeParamsDiff>,
}

impl TypeAliasDiff {
  pub fn diff(old: &TypeAliasDef, new: &TypeAliasDef) -> Option<Self> {
    let TypeAliasDef {
      ts_type: old_ts_type,
      type_params: old_type_params,
    } = old;
    let TypeAliasDef {
      ts_type: new_ts_type,
      type_params: new_type_params,
    } = new;

    let ts_type_change = TsTypeDiff::diff(old_ts_type, new_ts_type);
    let type_params_change =
      TypeParamsDiff::diff(old_type_params, new_type_params);

    if ts_type_change.is_none() && type_params_change.is_none() {
      return None;
    }

    Some(TypeAliasDiff {
      ts_type_change,
      type_params_change,
    })
  }

  pub fn change_percentage(&self) -> f64 {
    let total = 2.0;
    let changed = self.ts_type_change.is_some() as u8
      + self.type_params_change.is_some() as u8;
    changed as f64 / total
  }
}
