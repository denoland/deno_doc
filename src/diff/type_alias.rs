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
    let ts_type_change = TsTypeDiff::diff(&old.ts_type, &new.ts_type);
    let type_params_change =
      TypeParamsDiff::diff(&old.type_params, &new.type_params);

    if ts_type_change.is_none() && type_params_change.is_none() {
      return None;
    }

    Some(TypeAliasDiff {
      ts_type_change,
      type_params_change,
    })
  }
}
