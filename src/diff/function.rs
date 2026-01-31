// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use super::DiffEntry;
use super::ts_type::TsTypeDiff;
use super::ts_type::TypeParamsDiff;
use super::ts_type::types_equal;
use crate::decorators::DecoratorDef;
use crate::function::FunctionDef;
use crate::params::ParamDef;
use crate::ts_type::TsTypeDef;
use serde::Deserialize;
use serde::Serialize;
use std::collections::HashSet;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FunctionDiff {
  #[serde(skip_serializing_if = "Option::is_none")]
  pub params_change: Option<ParamsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub return_type_change: Option<TsTypeDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_async_change: Option<DiffEntry<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_generator_change: Option<DiffEntry<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_params_change: Option<TypeParamsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub decorators_change: Option<DecoratorsDiff>,
}

impl FunctionDiff {
  pub fn diff(old: &FunctionDef, new: &FunctionDef) -> Option<Self> {
    let params_change = ParamsDiff::diff(&old.params, &new.params);

    let return_type_change = if !types_equal(&old.return_type, &new.return_type)
    {
      match (&old.return_type, &new.return_type) {
        (Some(old_type), Some(new_type)) => {
          TsTypeDiff::diff(old_type, new_type)
        }
        (Some(old_type), None) => Some(TsTypeDiff {
          old: old_type.clone(),
          new: TsTypeDef::keyword("void"),
        }),
        (None, Some(new_type)) => Some(TsTypeDiff {
          old: TsTypeDef::keyword("void"),
          new: new_type.clone(),
        }),
        (None, None) => None,
      }
    } else {
      None
    };

    let is_async_change = if old.is_async != new.is_async {
      Some(DiffEntry::modified(old.is_async, new.is_async))
    } else {
      None
    };

    let is_generator_change = if old.is_generator != new.is_generator {
      Some(DiffEntry::modified(old.is_generator, new.is_generator))
    } else {
      None
    };

    let type_params_change =
      TypeParamsDiff::diff(&old.type_params, &new.type_params);

    let decorators_change =
      DecoratorsDiff::diff(&old.decorators, &new.decorators);

    if params_change.is_none()
      && return_type_change.is_none()
      && is_async_change.is_none()
      && is_generator_change.is_none()
      && type_params_change.is_none()
      && decorators_change.is_none()
    {
      return None;
    }

    Some(FunctionDiff {
      params_change,
      return_type_change,
      is_async_change,
      is_generator_change,
      type_params_change,
      decorators_change,
    })
  }
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct ParamsDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added: Vec<ParamDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed: Vec<ParamDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub modified: Vec<ParamDiff>,
}

impl ParamsDiff {
  pub fn diff(old: &[ParamDef], new: &[ParamDef]) -> Option<Self> {
    let max_len = old.len().max(new.len());
    let mut added = Vec::new();
    let mut removed = Vec::new();
    let mut modified = Vec::new();

    for i in 0..max_len {
      match (old.get(i), new.get(i)) {
        (Some(old_param), Some(new_param)) => {
          if let Some(diff) = ParamDiff::diff(i, old_param, new_param) {
            modified.push(diff);
          }
        }
        (Some(old_param), None) => {
          removed.push(old_param.clone());
        }
        (None, Some(new_param)) => {
          added.push(new_param.clone());
        }
        (None, None) => unreachable!(),
      }
    }

    if added.is_empty() && removed.is_empty() && modified.is_empty() {
      return None;
    }

    Some(ParamsDiff {
      added,
      removed,
      modified,
    })
  }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ParamDiff {
  pub index: usize,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub pattern_change: Option<DiffEntry<ParamDef>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_change: Option<TsTypeDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub decorators_change: Option<DecoratorsDiff>,
}

impl ParamDiff {
  pub fn diff(index: usize, old: &ParamDef, new: &ParamDef) -> Option<Self> {
    let pattern_changed = old.pattern != new.pattern;
    let type_changed = !types_equal(&old.ts_type, &new.ts_type);
    let decorators_change =
      DecoratorsDiff::diff(&old.decorators, &new.decorators);

    if !pattern_changed && !type_changed && decorators_change.is_none() {
      return None;
    }

    let pattern_change = if pattern_changed {
      Some(DiffEntry::modified(old.clone(), new.clone()))
    } else {
      None
    };

    let type_change = if type_changed {
      match (&old.ts_type, &new.ts_type) {
        (Some(old_type), Some(new_type)) => {
          TsTypeDiff::diff(old_type, new_type)
        }
        (Some(old_type), None) => Some(TsTypeDiff {
          old: old_type.clone(),
          new: TsTypeDef::keyword("unknown"),
        }),
        (None, Some(new_type)) => Some(TsTypeDiff {
          old: TsTypeDef::keyword("unknown"),
          new: new_type.clone(),
        }),
        (None, None) => None,
      }
    } else {
      None
    };

    Some(ParamDiff {
      index,
      pattern_change,
      type_change,
      decorators_change,
    })
  }
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct DecoratorsDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added: Vec<DecoratorDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed: Vec<DecoratorDef>,
}

impl DecoratorsDiff {
  pub fn diff(old: &[DecoratorDef], new: &[DecoratorDef]) -> Option<Self> {
    let old_set = old.iter().map(|d| &d.name).collect::<HashSet<_>>();
    let new_set = new.iter().map(|d| &d.name).collect::<HashSet<_>>();

    let added: Vec<_> = new
      .iter()
      .filter(|d| !old_set.contains(&d.name))
      .cloned()
      .collect();
    let removed: Vec<_> = old
      .iter()
      .filter(|d| !new_set.contains(&d.name))
      .cloned()
      .collect();

    if added.is_empty() && removed.is_empty() {
      return None;
    }

    Some(DecoratorsDiff { added, removed })
  }
}
