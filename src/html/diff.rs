// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use crate::Symbol;
use crate::diff::DeclarationDefDiff;
use crate::diff::DeclarationDiff;
use crate::diff::DocDiff;
use crate::diff::ModuleDiff;
use crate::diff::SymbolDiff;
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

pub struct SymbolDiffInfo {
  pub status: DiffStatus,
  pub diff: Option<SymbolDiff>,
}

pub struct DiffIndex {
  added_modules: Vec<ModuleSpecifier>,
  removed_modules: Vec<ModuleSpecifier>,
  pub module_diffs: IndexMap<ModuleSpecifier, ModuleDiff>,
  symbol_index: HashMap<(ModuleSpecifier, Box<str>), SymbolDiffInfo>,
}

impl DiffIndex {
  pub fn new(doc_diff: DocDiff) -> Self {
    let mut symbol_index = HashMap::new();

    for (specifier, module_diff) in &doc_diff.modified_modules {
      for symbol in &module_diff.added {
        symbol_index.insert(
          (specifier.clone(), symbol.name.clone()),
          SymbolDiffInfo {
            status: DiffStatus::Added,
            diff: None,
          },
        );
      }

      for symbol in &module_diff.removed {
        symbol_index.insert(
          (specifier.clone(), symbol.name.clone()),
          SymbolDiffInfo {
            status: DiffStatus::Removed,
            diff: None,
          },
        );
      }

      for symbol_diff in &module_diff.modified {
        let status = if let Some(name_change) = &symbol_diff.name_change {
          DiffStatus::Renamed {
            old_name: name_change.old.to_string(),
          }
        } else if symbol_diff.declarations.is_some()
          || symbol_diff.is_default_change.is_some()
        {
          DiffStatus::Modified
        } else {
          continue;
        };

        symbol_index.insert(
          (specifier.clone(), symbol_diff.name.clone()),
          SymbolDiffInfo {
            status,
            diff: Some(symbol_diff.clone()),
          },
        );
      }
    }

    DiffIndex {
      added_modules: doc_diff.added_modules,
      removed_modules: doc_diff.removed_modules,
      module_diffs: doc_diff.modified_modules,
      symbol_index,
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

  pub fn get_symbol_diff(
    &self,
    specifier: &ModuleSpecifier,
    name: &str,
  ) -> Option<&SymbolDiffInfo> {
    self.symbol_index.get(&(specifier.clone(), name.into()))
  }

  pub fn get_declaration_diff(
    &self,
    specifier: &ModuleSpecifier,
    name: &str,
    kind: AstDocNodeKind,
  ) -> Option<&DeclarationDiff> {
    let info = self.get_symbol_diff(specifier, name)?;
    let declarations = info.diff.as_ref()?.declarations.as_ref()?;
    declarations.modified.iter().find(|d| d.kind == kind)
  }

  pub fn get_def_diff(
    &self,
    specifier: &ModuleSpecifier,
    name: &str,
    kind: AstDocNodeKind,
  ) -> Option<&DeclarationDefDiff> {
    self
      .get_declaration_diff(specifier, name, kind)?
      .def_changes
      .as_ref()
  }

  pub fn get_removed_symbols(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Option<&[Symbol]> {
    self
      .module_diffs
      .get(specifier)
      .map(|diff| diff.removed.as_slice())
  }
}

pub(crate) fn is_symbol_added(symbol: &DocNodeWithContext) -> bool {
  matches!(symbol.diff_status, Some(DiffStatus::Added))
}

pub(crate) fn is_symbol_removed(symbol: &DocNodeWithContext) -> bool {
  matches!(symbol.diff_status, Some(DiffStatus::Removed))
}

/// Check if a specific declaration kind was added within a symbol.
/// Returns true if the whole symbol is added, or if this kind is in
/// the DeclarationsDiff.added list.
pub(crate) fn is_decl_added(
  symbol: &DocNodeWithContext,
  kind: crate::node::DocNodeKind,
  diff: &Option<DiffIndex>,
) -> bool {
  if is_symbol_added(symbol) {
    return true;
  }
  diff
    .as_ref()
    .and_then(|d| d.get_symbol_diff(&symbol.origin.specifier, &symbol.name))
    .and_then(|info| info.diff.as_ref())
    .and_then(|sd| sd.declarations.as_ref())
    .is_some_and(|dd| dd.added.iter().any(|a| a.def.to_kind() == kind))
}

/// Check if a specific declaration kind was removed within a symbol.
/// Returns true if the whole symbol is removed, or if this kind is in
/// the DeclarationsDiff.removed list.
pub(crate) fn is_decl_removed(
  symbol: &DocNodeWithContext,
  kind: crate::node::DocNodeKind,
  diff: &Option<DiffIndex>,
) -> bool {
  if is_symbol_removed(symbol) {
    return true;
  }
  diff
    .as_ref()
    .and_then(|d| d.get_symbol_diff(&symbol.origin.specifier, &symbol.name))
    .and_then(|info| info.diff.as_ref())
    .and_then(|sd| sd.declarations.as_ref())
    .is_some_and(|dd| dd.removed.iter().any(|a| a.def.to_kind() == kind))
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
      SectionContentCtx::NamespaceSection(symbols) => {
        for symbol in symbols {
          surviving_anchors.insert(symbol.anchor.id.as_str());
        }
      }
      _ => {}
    }
  }

  // Remove ToC entries whose anchors no longer exist
  let mut toc_entries = toc.toc.lock().unwrap();
  toc_entries.retain(|entry| surviving_anchors.contains(entry.anchor.as_str()));
}
