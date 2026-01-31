// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use super::Change;
use crate::ts_type::TsTypeDef;
use crate::ts_type_param::TsTypeParamDef;
use serde::Deserialize;
use serde::Serialize;
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TsTypeDiff {
  pub old: TsTypeDef,
  pub new: TsTypeDef,
}

impl TsTypeDiff {
  pub fn diff(old: &TsTypeDef, new: &TsTypeDef) -> Option<Self> {
    if old.repr == new.repr {
      return None;
    }

    Some(TsTypeDiff {
      old: old.clone(),
      new: new.clone(),
    })
  }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TsTypeParamDiff {
  pub name: String,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub constraint_change: Option<Change<Option<TsTypeDef>>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub default_change: Option<Change<Option<TsTypeDef>>>,
}

impl TsTypeParamDiff {
  pub fn diff(old: &TsTypeParamDef, new: &TsTypeParamDef) -> Option<Self> {
    let constraint_changed = match (&old.constraint, &new.constraint) {
      (Some(old_c), Some(new_c)) => old_c.repr != new_c.repr,
      (None, None) => false,
      _ => true,
    };

    let default_changed = match (&old.default, &new.default) {
      (Some(old_d), Some(new_d)) => old_d.repr != new_d.repr,
      (None, None) => false,
      _ => true,
    };

    if !constraint_changed && !default_changed {
      return None;
    }

    let constraint_change = if constraint_changed {
      Some(Change::new(old.constraint.clone(), new.constraint.clone()))
    } else {
      None
    };

    let default_change = if default_changed {
      Some(Change::new(old.default.clone(), new.default.clone()))
    } else {
      None
    };

    Some(TsTypeParamDiff {
      name: old.name.clone(),
      constraint_change,
      default_change,
    })
  }
}

pub fn types_equal(a: &Option<TsTypeDef>, b: &Option<TsTypeDef>) -> bool {
  match (a, b) {
    (Some(a), Some(b)) => a.repr == b.repr,
    (None, None) => true,
    _ => false,
  }
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct TypeParamsDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added: Vec<TsTypeParamDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed: Vec<TsTypeParamDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub modified: Vec<TsTypeParamDiff>,
}

impl TypeParamsDiff {
  pub fn diff(old: &[TsTypeParamDef], new: &[TsTypeParamDef]) -> Option<Self> {
    let old_map = old
      .iter()
      .map(|p| (p.name.as_str(), p))
      .collect::<HashMap<_, _>>();
    let new_map = new
      .iter()
      .map(|p| (p.name.as_str(), p))
      .collect::<HashMap<_, _>>();

    let mut added = Vec::new();
    let mut removed = Vec::new();
    let mut modified = Vec::new();

    for (name, param) in &new_map {
      if !old_map.contains_key(name) {
        added.push((*param).clone());
      }
    }

    for (name, param) in &old_map {
      if !new_map.contains_key(name) {
        removed.push((*param).clone());
      }
    }

    for (name, old_param) in &old_map {
      if let Some(new_param) = new_map.get(name)
        && let Some(diff) = TsTypeParamDiff::diff(old_param, new_param)
      {
        modified.push(diff);
      }
    }

    if added.is_empty() && removed.is_empty() && modified.is_empty() {
      return None;
    }

    Some(TypeParamsDiff {
      added,
      removed,
      modified,
    })
  }
}
