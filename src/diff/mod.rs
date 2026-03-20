// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

mod class;
mod r#enum;
mod function;
mod interface;
mod js_doc;
mod namespace;
mod ts_type;
mod type_alias;
mod variable;

use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use indexmap::IndexSet;
use serde::Deserialize;
use serde::Serialize;

use std::sync::Arc;

use crate::Symbol;
use crate::node::Declaration;
use crate::node::DeclarationDef;
use crate::node::DeclarationKind;
use crate::node::DocNodeKind;
use crate::parser::ParseOutput;

pub use class::ClassDiff;
pub use class::ConstructorDiff;
pub use class::ConstructorsDiff;
pub use class::IndexSignatureDiff;
pub use class::IndexSignaturesDiff;
pub use class::MethodDiff;
pub use class::MethodsDiff;
pub use class::PropertiesDiff;
pub use class::PropertyDiff;
pub use r#enum::EnumDiff;
pub use function::FunctionDiff;
pub use function::ParamsDiff;
pub use interface::CallSignaturesDiff;
pub use interface::InterfaceConstructorsDiff;
pub use interface::InterfaceDiff;
pub use interface::InterfaceIndexSignatureDiff;
pub use interface::InterfaceIndexSignaturesDiff;
pub use interface::InterfaceMethodDiff;
pub use interface::InterfaceMethodsDiff;
pub use interface::InterfacePropertiesDiff;
pub use interface::InterfacePropertyDiff;
pub use js_doc::JsDocDiff;
pub use js_doc::TagsDiff;
pub use namespace::NamespaceDiff;
pub use ts_type::TsTypeDiff;
pub use ts_type::TypeParamsDiff;
pub use type_alias::TypeAliasDiff;
pub use variable::VariableDiff;

/// Maximum change percentage for two items to be considered a rename.
pub(crate) const RENAME_THRESHOLD: f64 = 0.10;

/// Trait for diff types that support rename detection via change percentage.
pub(crate) trait RenameCandidate: Sized {
  type Item;

  /// Whether two items are structurally compatible for a rename
  /// (e.g. same kind, same is_static).
  fn is_candidate(old: &Self::Item, new: &Self::Item) -> bool;

  /// Compute the diff between old and new items.
  fn compute(old: &Self::Item, new: &Self::Item) -> Option<Self>;

  /// The fraction of fields that changed (0.0 = identical, 1.0 = everything).
  fn delta(&self) -> f64;

  /// Wrap an optional diff as a rename entry (setting name and name_change).
  fn with_rename(
    diff: Option<Self>,
    old: &Self::Item,
    new: &Self::Item,
  ) -> Self;
}

/// Detect renames among added/removed pairs using diff delta threshold.
/// Matched pairs are moved into `modified` and removed from `added`/`removed`.
pub(crate) fn detect_renames<D: RenameCandidate>(
  added: &mut Vec<D::Item>,
  removed: &mut Vec<D::Item>,
  modified: &mut Vec<D>,
) {
  let mut matched_added = IndexSet::new();
  let mut matched_removed = IndexSet::new();

  for (r_idx, removed_item) in removed.iter().enumerate() {
    for (a_idx, added_item) in added.iter().enumerate() {
      if matched_added.contains(&a_idx) {
        continue;
      }

      if !D::is_candidate(removed_item, added_item) {
        continue;
      }

      let diff = D::compute(removed_item, added_item);
      let pct = diff.as_ref().map_or(0.0, |d| d.delta());
      if pct <= RENAME_THRESHOLD {
        modified.push(D::with_rename(diff, removed_item, added_item));
        matched_added.insert(a_idx);
        matched_removed.insert(r_idx);
        break;
      }
    }
  }

  let mut i = 0;
  added.retain(|_| {
    let keep = !matched_added.contains(&i);
    i += 1;
    keep
  });
  let mut i = 0;
  removed.retain(|_| {
    let keep = !matched_removed.contains(&i);
    i += 1;
    keep
  });
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Change<T> {
  pub old: T,
  pub new: T,
}

impl<T> Change<T> {
  pub fn new(old: T, new: T) -> Self {
    Self { old, new }
  }
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct DocDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added_modules: Vec<ModuleSpecifier>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed_modules: Vec<ModuleSpecifier>,
  #[serde(skip_serializing_if = "IndexMap::is_empty", default)]
  pub modified_modules: IndexMap<ModuleSpecifier, ModuleDiff>,
}

impl DocDiff {
  pub fn diff(old: &ParseOutput, new: &ParseOutput) -> Self {
    let old_modules = old.keys().collect::<IndexSet<_>>();
    let new_modules = new.keys().collect::<IndexSet<_>>();

    let added_modules = new_modules
      .difference(&old_modules)
      .map(|s| (*s).clone())
      .collect::<Vec<_>>();
    let removed_modules = old_modules
      .difference(&new_modules)
      .map(|s| (*s).clone())
      .collect::<Vec<_>>();

    let mut modified_modules = IndexMap::new();

    for specifier in old_modules.intersection(&new_modules) {
      let old_doc = old.get(*specifier).unwrap();
      let new_doc = new.get(*specifier).unwrap();

      let module_diff = ModuleDiff::diff(&old_doc.symbols, &new_doc.symbols);

      if module_diff.has_changes() {
        modified_modules.insert((*specifier).clone(), module_diff);
      }
    }

    DocDiff {
      added_modules,
      removed_modules,
      modified_modules,
    }
  }

  pub fn has_changes(&self) -> bool {
    !self.added_modules.is_empty()
      || !self.removed_modules.is_empty()
      || !self.modified_modules.is_empty()
  }
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct ModuleDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added: Vec<Symbol>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed: Vec<Symbol>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub modified: Vec<SymbolDiff>,
}

impl ModuleDiff {
  pub fn diff(old_nodes: &[Arc<Symbol>], new_nodes: &[Arc<Symbol>]) -> Self {
    let old_map = build_name_map(old_nodes);
    let new_map = build_name_map(new_nodes);

    let mut added: Vec<Symbol> = Vec::new();
    let mut removed: Vec<Symbol> = Vec::new();
    let mut modified = Vec::new();

    for (name, node) in &new_map {
      if !old_map.contains_key(name) {
        added.push((*node).clone());
      }
    }

    for (name, node) in &old_map {
      if !new_map.contains_key(name) {
        removed.push((*node).clone());
      }
    }

    // Rename detection among added/removed Symbols
    let mut matched_added = IndexSet::new();
    let mut matched_removed = IndexSet::new();

    for (r_idx, removed_node) in removed.iter().enumerate() {
      for (a_idx, added_node) in added.iter().enumerate() {
        if matched_added.contains(&a_idx) {
          continue;
        }

        if let Some(rename_diff) = try_detect_rename(removed_node, added_node) {
          modified.push(rename_diff);
          matched_added.insert(a_idx);
          matched_removed.insert(r_idx);
          break;
        }
      }
    }

    let mut i = 0;
    added.retain(|_| {
      let keep = !matched_added.contains(&i);
      i += 1;
      keep
    });
    let mut i = 0;
    removed.retain(|_| {
      let keep = !matched_removed.contains(&i);
      i += 1;
      keep
    });

    // Modifications: same name in both
    for (name, old_node) in &old_map {
      if let Some(new_node) = new_map.get(name)
        && let Some(diff) = SymbolDiff::diff(old_node, new_node)
      {
        modified.push(diff);
      }
    }

    ModuleDiff {
      added,
      removed,
      modified,
    }
  }

  pub fn has_changes(&self) -> bool {
    !self.added.is_empty()
      || !self.removed.is_empty()
      || !self.modified.is_empty()
  }
}

/// Detect if two Symbols are a rename of each other.
/// Returns a SymbolDiff with name_change set if below threshold.
fn try_detect_rename(old: &Symbol, new: &Symbol) -> Option<SymbolDiff> {
  let old_kinds: IndexSet<DocNodeKind> =
    old.declarations.iter().map(|d| d.def.to_kind()).collect();
  let new_kinds: IndexSet<DocNodeKind> =
    new.declarations.iter().map(|d| d.def.to_kind()).collect();

  if old_kinds.is_disjoint(&new_kinds) {
    return None;
  }

  // Use first common kind to check rename threshold
  let first_kind = *old_kinds.intersection(&new_kinds).next()?;
  let old_decl = old
    .declarations
    .iter()
    .find(|d| d.def.to_kind() == first_kind)?;
  let new_decl = new
    .declarations
    .iter()
    .find(|d| d.def.to_kind() == first_kind)?;

  let def_diff = DeclarationDefDiff::diff(&old_decl.def, &new_decl.def);
  let pct = def_diff
    .as_ref()
    .map_or(0.0, |d| d.change_percentage(&old_decl.def, &new_decl.def));

  if pct > RENAME_THRESHOLD {
    return None;
  }

  let declarations =
    DeclarationsDiff::diff(&old.declarations, &new.declarations);

  Some(SymbolDiff {
    name: new.name.clone(),
    name_change: Some(Change::new(old.name.clone(), new.name.clone())),
    is_default_change: if old.is_default != new.is_default {
      Some(Change::new(old.is_default, new.is_default))
    } else {
      None
    },
    declarations,
  })
}

fn build_name_map(nodes: &[Arc<Symbol>]) -> IndexMap<&str, &Symbol> {
  nodes.iter().map(|n| (n.name.as_ref(), n.as_ref())).collect()
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SymbolDiff {
  pub name: Box<str>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub name_change: Option<Change<Box<str>>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_default_change: Option<Change<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub declarations: Option<DeclarationsDiff>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct DeclarationsDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added: Vec<Declaration>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed: Vec<Declaration>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub modified: Vec<DeclarationDiff>,
}

impl DeclarationsDiff {
  pub fn diff(old: &[Declaration], new: &[Declaration]) -> Option<Self> {
    let old_map = old
      .iter()
      .map(|d| (d.def.to_kind(), d))
      .collect::<IndexMap<_, _>>();
    let new_map = new
      .iter()
      .map(|d| (d.def.to_kind(), d))
      .collect::<IndexMap<_, _>>();

    let mut added = Vec::new();
    let mut removed = Vec::new();
    let mut modified = Vec::new();

    for (kind, new_decl) in &new_map {
      match old_map.get(kind) {
        Some(old_decl) => {
          if let Some(diff) = DeclarationDiff::diff(old_decl, new_decl) {
            modified.push(diff);
          }
        }
        None => {
          added.push((*new_decl).clone());
        }
      }
    }

    for (kind, old_decl) in &old_map {
      if !new_map.contains_key(kind) {
        removed.push((*old_decl).clone());
      }
    }

    if added.is_empty() && removed.is_empty() && modified.is_empty() {
      return None;
    }

    Some(DeclarationsDiff {
      added,
      removed,
      modified,
    })
  }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DeclarationDiff {
  pub kind: DocNodeKind,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub def_changes: Option<DeclarationDefDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub js_doc_changes: Option<JsDocDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub declaration_kind_change: Option<Change<DeclarationKind>>,
}

impl DeclarationDiff {
  pub fn diff(old: &Declaration, new: &Declaration) -> Option<Self> {
    let def_changes = DeclarationDefDiff::diff(&old.def, &new.def);

    let js_doc_changes = JsDocDiff::diff(&old.js_doc, &new.js_doc);

    let declaration_kind_change =
      if old.declaration_kind != new.declaration_kind {
        Some(Change::new(old.declaration_kind, new.declaration_kind))
      } else {
        None
      };

    if def_changes.is_none()
      && js_doc_changes.is_none()
      && declaration_kind_change.is_none()
    {
      return None;
    }

    Some(DeclarationDiff {
      kind: old.def.to_kind(),
      def_changes,
      js_doc_changes,
      declaration_kind_change,
    })
  }
}

impl SymbolDiff {
  pub fn diff(old: &Symbol, new: &Symbol) -> Option<Self> {
    let declarations =
      DeclarationsDiff::diff(&old.declarations, &new.declarations);

    let is_default_change = if old.is_default != new.is_default {
      Some(Change::new(old.is_default, new.is_default))
    } else {
      None
    };

    if declarations.is_none() && is_default_change.is_none() {
      return None;
    }

    Some(SymbolDiff {
      name: old.name.clone(),
      name_change: None,
      is_default_change,
      declarations,
    })
  }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum DeclarationDefDiff {
  Function(FunctionDiff),
  Variable(VariableDiff),
  Enum(EnumDiff),
  Class(ClassDiff),
  TypeAlias(TypeAliasDiff),
  Namespace(NamespaceDiff),
  Interface(InterfaceDiff),
  // ignore Reference
}

impl DeclarationDefDiff {
  pub fn change_percentage(
    &self,
    old: &DeclarationDef,
    new: &DeclarationDef,
  ) -> f64 {
    match (self, old, new) {
      (DeclarationDefDiff::Function(d), _, _) => d.change_percentage(),
      (DeclarationDefDiff::Variable(d), _, _) => d.change_percentage(),
      (DeclarationDefDiff::TypeAlias(d), _, _) => d.change_percentage(),
      (
        DeclarationDefDiff::Enum(d),
        DeclarationDef::Enum(old_def),
        DeclarationDef::Enum(new_def),
      ) => d.change_percentage(old_def, new_def),
      (
        DeclarationDefDiff::Class(d),
        DeclarationDef::Class(old_def),
        DeclarationDef::Class(new_def),
      ) => d.change_percentage(old_def, new_def),
      (
        DeclarationDefDiff::Interface(d),
        DeclarationDef::Interface(old_def),
        DeclarationDef::Interface(new_def),
      ) => d.change_percentage(old_def, new_def),
      (
        DeclarationDefDiff::Namespace(d),
        DeclarationDef::Namespace(old_def),
        DeclarationDef::Namespace(new_def),
      ) => d.change_percentage(old_def, new_def),
      _ => 1.0,
    }
  }

  pub fn as_function(&self) -> Option<&FunctionDiff> {
    match self {
      DeclarationDefDiff::Function(d) => Some(d),
      _ => None,
    }
  }

  pub fn as_variable(&self) -> Option<&VariableDiff> {
    match self {
      DeclarationDefDiff::Variable(d) => Some(d),
      _ => None,
    }
  }

  pub fn as_enum(&self) -> Option<&EnumDiff> {
    match self {
      DeclarationDefDiff::Enum(d) => Some(d),
      _ => None,
    }
  }

  pub fn as_class(&self) -> Option<&ClassDiff> {
    match self {
      DeclarationDefDiff::Class(d) => Some(d),
      _ => None,
    }
  }

  pub fn as_type_alias(&self) -> Option<&TypeAliasDiff> {
    match self {
      DeclarationDefDiff::TypeAlias(d) => Some(d),
      _ => None,
    }
  }

  pub fn as_namespace(&self) -> Option<&NamespaceDiff> {
    match self {
      DeclarationDefDiff::Namespace(d) => Some(d),
      _ => None,
    }
  }

  pub fn as_interface(&self) -> Option<&InterfaceDiff> {
    match self {
      DeclarationDefDiff::Interface(d) => Some(d),
      _ => None,
    }
  }

  pub fn diff(old: &DeclarationDef, new: &DeclarationDef) -> Option<Self> {
    match (old, new) {
      (
        DeclarationDef::Function(old_def),
        DeclarationDef::Function(new_def),
      ) => {
        FunctionDiff::diff(old_def, new_def).map(DeclarationDefDiff::Function)
      }

      (
        DeclarationDef::Variable(old_def),
        DeclarationDef::Variable(new_def),
      ) => {
        VariableDiff::diff(old_def, new_def).map(DeclarationDefDiff::Variable)
      }

      (DeclarationDef::Enum(old_def), DeclarationDef::Enum(new_def)) => {
        EnumDiff::diff(old_def, new_def).map(DeclarationDefDiff::Enum)
      }

      (DeclarationDef::Class(old_def), DeclarationDef::Class(new_def)) => {
        ClassDiff::diff(old_def, new_def).map(DeclarationDefDiff::Class)
      }

      (
        DeclarationDef::TypeAlias(old_def),
        DeclarationDef::TypeAlias(new_def),
      ) => {
        TypeAliasDiff::diff(old_def, new_def).map(DeclarationDefDiff::TypeAlias)
      }

      (
        DeclarationDef::Namespace(old_def),
        DeclarationDef::Namespace(new_def),
      ) => {
        NamespaceDiff::diff(old_def, new_def).map(DeclarationDefDiff::Namespace)
      }

      (
        DeclarationDef::Interface(old_def),
        DeclarationDef::Interface(new_def),
      ) => {
        InterfaceDiff::diff(old_def, new_def).map(DeclarationDefDiff::Interface)
      }

      (DeclarationDef::Reference(..), DeclarationDef::Reference(..)) => None,

      _ => unreachable!(),
    }
  }
}
