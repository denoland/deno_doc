// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use crate::DocNode;
use crate::diff::DocDiff;
use crate::diff::DocNodeDiff;
use crate::diff::ModuleDiff;
use crate::node::DocNodeKind as AstDocNodeKind;
use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use serde::Deserialize;
use serde::Serialize;
use std::collections::HashMap;

/// Diff status for a symbol or member in the rendered output.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum DiffStatus {
  Added,
  Removed,
  Modified,
  Renamed { old_name: String },
}

/// Pre-indexed lookup for a node's diff status.
pub struct NodeDiffInfo {
  pub status: DiffStatus,
  pub diff: Option<DocNodeDiff>,
}

/// Pre-indexed diff data for fast lookup during rendering.
pub struct DiffIndex {
  added_modules: Vec<ModuleSpecifier>,
  removed_modules: Vec<ModuleSpecifier>,
  /// Per-module diff data, keyed by specifier.
  module_diffs: IndexMap<ModuleSpecifier, ModuleDiff>,
  /// Per-node lookup: (specifier, name, kind) -> NodeDiffInfo
  node_index:
    HashMap<(ModuleSpecifier, Box<str>, AstDocNodeKind), NodeDiffInfo>,
}

impl DiffIndex {
  pub fn new(doc_diff: DocDiff) -> Self {
    let mut node_index = HashMap::new();

    for (specifier, module_diff) in &doc_diff.modified_modules {
      // Index added nodes
      for node in &module_diff.added {
        let kind = node.def.to_kind();
        node_index.insert(
          (specifier.clone(), node.name.clone(), kind),
          NodeDiffInfo {
            status: DiffStatus::Added,
            diff: None,
          },
        );
      }

      // Index removed nodes
      for node in &module_diff.removed {
        let kind = node.def.to_kind();
        node_index.insert(
          (specifier.clone(), node.name.clone(), kind),
          NodeDiffInfo {
            status: DiffStatus::Removed,
            diff: None,
          },
        );
      }

      // Index modified nodes
      for node_diff in &module_diff.modified {
        let kind = node_diff.kind;
        let status = if let Some(name_change) = &node_diff.name_change {
          DiffStatus::Renamed {
            old_name: name_change.old.to_string(),
          }
        } else if node_diff.def_changes.is_some()
          || node_diff.js_doc_changes.is_some()
          || node_diff.declaration_kind_change.is_some()
          || node_diff.is_default_change.is_some()
        {
          DiffStatus::Modified
        } else {
          continue;
        };

        node_index.insert(
          (specifier.clone(), node_diff.name.clone(), kind),
          NodeDiffInfo {
            status,
            diff: Some(node_diff.clone()),
          },
        );
      }
    }

    DiffIndex {
      added_modules: doc_diff.added_modules,
      removed_modules: doc_diff.removed_modules,
      module_diffs: doc_diff.modified_modules,
      node_index,
    }
  }

  pub fn is_added_module(&self, specifier: &ModuleSpecifier) -> bool {
    self.added_modules.contains(specifier)
  }

  pub fn is_removed_module(&self, specifier: &ModuleSpecifier) -> bool {
    self.removed_modules.contains(specifier)
  }

  pub fn get_module_diff(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<&ModuleDiff> {
    self.module_diffs.get(specifier)
  }

  pub fn get_node_diff(
    &self,
    specifier: &ModuleSpecifier,
    name: &str,
    kind: AstDocNodeKind,
  ) -> Option<&NodeDiffInfo> {
    self.node_index.get(&(specifier.clone(), name.into(), kind))
  }

  /// Get the typed def diff for a node, combining node lookup + def_changes
  /// extraction.
  pub fn get_def_diff(
    &self,
    specifier: &ModuleSpecifier,
    name: &str,
    kind: AstDocNodeKind,
  ) -> Option<&crate::diff::DocNodeDefDiff> {
    let info = self.get_node_diff(specifier, name, kind)?;
    info.diff.as_ref()?.def_changes.as_ref()
  }

  /// Get removed nodes for a module (for injecting into listings).
  pub fn get_removed_nodes(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<&[DocNode]> {
    self
      .module_diffs
      .get(specifier)
      .map(|diff| diff.removed.as_slice())
  }
}
