// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use super::DocNodeDiff;
use crate::DocNode;
use crate::node::NamespaceDef;
use serde::Deserialize;
use serde::Serialize;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct NamespaceDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added_elements: Vec<Arc<DocNode>>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed_elements: Vec<Arc<DocNode>>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub modified_elements: Vec<DocNodeDiff>,
}

impl NamespaceDiff {
  pub fn diff(old: &NamespaceDef, new: &NamespaceDef) -> Option<Self> {
    let NamespaceDef {
      elements: old_elements,
    } = old;
    let NamespaceDef {
      elements: new_elements,
    } = new;

    let old_map = old_elements
      .iter()
      .map(|n| ((n.name.to_string(), n.def.to_kind()), n))
      .collect::<HashMap<_, _>>();
    let new_map = new_elements
      .iter()
      .map(|n| ((n.name.to_string(), n.def.to_kind()), n))
      .collect::<HashMap<_, _>>();

    let mut added_elements = Vec::new();
    let mut removed_elements = Vec::new();
    let mut modified_elements = Vec::new();

    for ((name, kind), node) in &new_map {
      if !old_map.contains_key(&(name.clone(), *kind)) {
        added_elements.push((*node).clone());
      }
    }

    for ((name, kind), node) in &old_map {
      if !new_map.contains_key(&(name.clone(), *kind)) {
        removed_elements.push((*node).clone());
      }
    }

    for ((name, kind), old_node) in &old_map {
      if let Some(new_node) = new_map.get(&(name.clone(), *kind))
        && let Some(diff) = DocNodeDiff::diff(old_node, new_node)
      {
        modified_elements.push(diff);
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
}
