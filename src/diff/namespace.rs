// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use super::SymbolDiff;
use crate::Symbol;
use crate::node::NamespaceDef;
use indexmap::IndexMap;
use serde::Deserialize;
use serde::Serialize;
use std::sync::Arc;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct NamespaceDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added_elements: Vec<Arc<Symbol>>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed_elements: Vec<Arc<Symbol>>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub modified_elements: Vec<SymbolDiff>,
}

impl NamespaceDiff {
  pub fn diff(old: &NamespaceDef, new: &NamespaceDef) -> Option<Self> {
    let NamespaceDef {
      elements: old_elements,
    } = old;
    let NamespaceDef {
      elements: new_elements,
    } = new;

    let old_map: IndexMap<&str, &Arc<Symbol>> =
      old_elements.iter().map(|n| (n.name.as_ref(), n)).collect();
    let new_map: IndexMap<&str, &Arc<Symbol>> =
      new_elements.iter().map(|n| (n.name.as_ref(), n)).collect();

    let mut added_elements = Vec::new();
    let mut removed_elements = Vec::new();
    let mut modified_elements = Vec::new();

    for (name, node) in &new_map {
      if !old_map.contains_key(name) {
        added_elements.push((*node).clone());
      }
    }

    for (name, node) in &old_map {
      if !new_map.contains_key(name) {
        removed_elements.push((*node).clone());
      }
    }

    for (name, old_node) in &old_map {
      if let Some(new_node) = new_map.get(name) {
        if let Some(diff) = SymbolDiff::diff(old_node, new_node) {
          modified_elements.push(diff);
        }
      }
    }

    if added_elements.is_empty()
      && removed_elements.is_empty()
      && modified_elements.is_empty()
    {
      return None;
    }

    Some(NamespaceDiff {
      added_elements,
      removed_elements,
      modified_elements,
    })
  }

  pub fn change_percentage(
    &self,
    old: &NamespaceDef,
    new: &NamespaceDef,
  ) -> f64 {
    let total = old.elements.len().max(new.elements.len());
    if total == 0 {
      return 0.0;
    }
    let changed = self.added_elements.len()
      + self.removed_elements.len()
      + self.modified_elements.len();
    (changed as f64 / total as f64).min(1.0)
  }
}
