// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use crate::DocNode;
use crate::diff::DocDiff;
use crate::diff::DocNodeDiff;
use crate::diff::ModuleDiff;
use crate::html::DocNodeWithContext;
use crate::html::util::{SectionContentCtx, SectionCtx};
use crate::node::DocNodeKind as AstDocNodeKind;
use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use serde::Deserialize;
use serde::Serialize;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum DiffStatus {
  Added,
  Removed,
  Modified,
  Renamed { old_name: String },
}

pub struct NodeDiffInfo {
  pub status: DiffStatus,
  pub diff: Option<DocNodeDiff>,
}

pub struct DiffIndex {
  added_modules: Vec<ModuleSpecifier>,
  removed_modules: Vec<ModuleSpecifier>,
  pub module_diffs: IndexMap<ModuleSpecifier, ModuleDiff>,
  node_index:
    HashMap<(ModuleSpecifier, Box<str>, AstDocNodeKind), NodeDiffInfo>,
}

impl DiffIndex {
  pub fn new(doc_diff: DocDiff) -> Self {
    let mut node_index = HashMap::new();

    for (specifier, module_diff) in &doc_diff.modified_modules {
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

  pub fn get_def_diff(
    &self,
    specifier: &ModuleSpecifier,
    name: &str,
    kind: AstDocNodeKind,
  ) -> Option<&crate::diff::DocNodeDefDiff> {
    let info = self.get_node_diff(specifier, name, kind)?;
    info.diff.as_ref()?.def_changes.as_ref()
  }

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

pub(crate) fn is_symbol_added(doc_node: &DocNodeWithContext) -> bool {
  matches!(doc_node.diff_status, Some(DiffStatus::Added))
}

pub(crate) fn is_symbol_removed(doc_node: &DocNodeWithContext) -> bool {
  matches!(doc_node.diff_status, Some(DiffStatus::Removed))
}

pub(crate) fn filter_sections_diff_only(
  sections: &mut Vec<SectionCtx>,
  toc: &crate::html::render_context::HeadingToCAdapter,
) {
  sections.retain_mut(|section| match &mut section.content {
    SectionContentCtx::DocEntry(entries) => {
      entries.retain_mut(|e| {
        if e.diff_status.is_some() {
          if matches!(e.diff_status, Some(DiffStatus::Modified)) {
            e.tags.retain(|t| t.diff.is_some());
          }
          true
        } else {
          false
        }
      });
      !entries.is_empty()
    }
    SectionContentCtx::IndexSignature(entries) => {
      entries.retain(|e| e.diff_status.is_some());
      !entries.is_empty()
    }
    SectionContentCtx::NamespaceSection(entries) => {
      for entry in entries.iter_mut() {
        if !matches!(
          entry.diff_status,
          Some(DiffStatus::Added) | Some(DiffStatus::Removed)
        ) {
          entry.subitems.retain(|s| s.diff_status.is_some());
        }
      }
      entries.retain_mut(|e| {
        if e.diff_status.is_some()
          || e.subitems.iter().any(|s| s.diff_status.is_some())
        {
          if matches!(e.diff_status, Some(DiffStatus::Modified)) {
            e.tags.retain(|t| t.diff.is_some());
          }
          true
        } else {
          false
        }
      });
      !entries.is_empty()
    }
    SectionContentCtx::Example(_)
    | SectionContentCtx::See(_)
    | SectionContentCtx::Empty => false,
  });

  // Collect surviving anchor IDs from remaining sections
  let mut surviving_anchors = std::collections::HashSet::<&str>::new();
  for section in sections.iter() {
    if let Some(header) = &section.header {
      surviving_anchors.insert(header.anchor.id.as_str());
    }
    match &section.content {
      SectionContentCtx::DocEntry(entries) => {
        for entry in entries {
          surviving_anchors.insert(entry.anchor.id.as_str());
        }
      }
      SectionContentCtx::NamespaceSection(nodes) => {
        for node in nodes {
          surviving_anchors.insert(node.anchor.id.as_str());
        }
      }
      _ => {}
    }
  }

  // Remove ToC entries whose anchors no longer exist
  let mut toc_entries = toc.toc.lock().unwrap();
  toc_entries.retain(|entry| surviving_anchors.contains(entry.anchor.as_str()));
}
