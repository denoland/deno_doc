// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use super::Change;
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
  pub is_async_change: Option<Change<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_generator_change: Option<Change<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_params_change: Option<TypeParamsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub decorators_change: Option<DecoratorsDiff>,
}

impl FunctionDiff {
  pub fn diff(old: &FunctionDef, new: &FunctionDef) -> Option<Self> {
    let FunctionDef {
      def_name: _, // internal, not part of public API
      params: old_params,
      return_type: old_return_type,
      has_body: _, // implementation detail, not diffed
      is_async: old_is_async,
      is_generator: old_is_generator,
      type_params: old_type_params,
      decorators: old_decorators,
    } = old;
    let FunctionDef {
      def_name: _,
      params: new_params,
      return_type: new_return_type,
      has_body: _,
      is_async: new_is_async,
      is_generator: new_is_generator,
      type_params: new_type_params,
      decorators: new_decorators,
    } = new;

    let params_change = ParamsDiff::diff(old_params, new_params);

    let return_type_change = if !types_equal(old_return_type, new_return_type) {
      match (old_return_type, new_return_type) {
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

    let is_async_change = if old_is_async != new_is_async {
      Some(Change::new(*old_is_async, *new_is_async))
    } else {
      None
    };

    let is_generator_change = if old_is_generator != new_is_generator {
      Some(Change::new(*old_is_generator, *new_is_generator))
    } else {
      None
    };

    let type_params_change =
      TypeParamsDiff::diff(old_type_params, new_type_params);
    let decorators_change =
      DecoratorsDiff::diff(old_decorators, new_decorators);

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
  pub pattern_change: Option<Change<ParamDef>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_change: Option<TsTypeDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub decorators_change: Option<DecoratorsDiff>,
}

impl ParamDiff {
  pub fn diff(index: usize, old: &ParamDef, new: &ParamDef) -> Option<Self> {
    let ParamDef {
      pattern: old_pattern,
      decorators: old_decorators,
      ts_type: old_ts_type,
    } = old;
    let ParamDef {
      pattern: new_pattern,
      decorators: new_decorators,
      ts_type: new_ts_type,
    } = new;

    let pattern_changed = old_pattern != new_pattern;
    let type_changed = !types_equal(old_ts_type, new_ts_type);
    let decorators_change =
      DecoratorsDiff::diff(old_decorators, new_decorators);

    if !pattern_changed && !type_changed && decorators_change.is_none() {
      return None;
    }

    let pattern_change = if pattern_changed {
      Some(Change::new(old.clone(), new.clone()))
    } else {
      None
    };

    let type_change = if type_changed {
      match (old_ts_type, new_ts_type) {
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
