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

use crate::DocNode;
use crate::DocNodeDef;
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
      let old_nodes = old.get(*specifier).unwrap();
      let new_nodes = new.get(*specifier).unwrap();

      let module_diff = ModuleDiff::diff(old_nodes, new_nodes);

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
  pub added: Vec<DocNode>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed: Vec<DocNode>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub modified: Vec<DocNodeDiff>,
}

impl ModuleDiff {
  pub fn diff(old_nodes: &[DocNode], new_nodes: &[DocNode]) -> Self {
    let old_map = build_node_map(old_nodes);
    let new_map = build_node_map(new_nodes);

    let mut added = Vec::new();
    let mut removed = Vec::new();
    let mut modified = Vec::new();

    for ((name, kind), node) in &new_map {
      if !old_map.contains_key(&(name.clone(), *kind)) {
        added.push((*node).clone());
      }
    }

    for ((name, kind), node) in &old_map {
      if !new_map.contains_key(&(name.clone(), *kind)) {
        removed.push((*node).clone());
      }
    }

    let mut matched_added = IndexSet::new();
    let mut matched_removed = IndexSet::new();

    for (r_idx, removed_node) in removed.iter().enumerate() {
      for (a_idx, added_node) in added.iter().enumerate() {
        if matched_added.contains(&a_idx) {
          continue;
        }

        if let Some(def_changes) = try_detect_rename(removed_node, added_node) {
          let js_doc_changes =
            JsDocDiff::diff(&removed_node.js_doc, &added_node.js_doc);
          let declaration_kind_change =
            if removed_node.declaration_kind != added_node.declaration_kind {
              Some(Change::new(
                removed_node.declaration_kind,
                added_node.declaration_kind,
              ))
            } else {
              None
            };

          let is_default_change = if removed_node.is_default.unwrap_or(false)
            != added_node.is_default.unwrap_or(false)
          {
            Some(Change::new(
              removed_node.is_default.unwrap_or(false),
              added_node.is_default.unwrap_or(false),
            ))
          } else {
            None
          };

          modified.push(DocNodeDiff {
            name: added_node.name.clone(),
            kind: added_node.def.to_kind(),
            name_change: Some(Change::new(
              removed_node.name.clone(),
              added_node.name.clone(),
            )),
            def_changes,
            js_doc_changes,
            declaration_kind_change,
            is_default_change,
          });

          matched_added.insert(a_idx);
          matched_removed.insert(r_idx);
          break;
        }
      }
    }

    let added = added
      .into_iter()
      .enumerate()
      .filter(|(i, _)| !matched_added.contains(i))
      .map(|(_, n)| n)
      .collect::<Vec<_>>();
    let removed = removed
      .into_iter()
      .enumerate()
      .filter(|(i, _)| !matched_removed.contains(i))
      .map(|(_, n)| n)
      .collect::<Vec<_>>();

    for ((name, kind), old_node) in &old_map {
      if let Some(new_node) = new_map.get(&(name.clone(), *kind))
        && let Some(symbol_diff) = DocNodeDiff::diff(old_node, new_node)
      {
        modified.push(symbol_diff);
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

/// None = not a rename.
/// Some(None) = rename detected, no diff.
/// Some(Some(def_diff)) = rename detected, with diff.
fn try_detect_rename(
  old: &DocNode,
  new: &DocNode,
) -> Option<Option<DocNodeDefDiff>> {
  if old.def.to_kind() != new.def.to_kind() {
    return None;
  }

  let def_diff = DocNodeDefDiff::diff(&old.def, &new.def);
  let pct = def_diff
    .as_ref()
    .map_or(0.0, |d| d.change_percentage(&old.def, &new.def));

  if pct <= RENAME_THRESHOLD {
    Some(def_diff)
  } else {
    None
  }
}

fn build_node_map(
  nodes: &[DocNode],
) -> IndexMap<(String, DocNodeKind), &DocNode> {
  let mut map = IndexMap::new();
  for node in nodes {
    let kind = node.def.to_kind();
    map.insert((node.name.to_string(), kind), node);
  }
  map
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DocNodeDiff {
  pub name: Box<str>,
  pub kind: DocNodeKind,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub name_change: Option<Change<Box<str>>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub def_changes: Option<DocNodeDefDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub js_doc_changes: Option<JsDocDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub declaration_kind_change: Option<Change<DeclarationKind>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_default_change: Option<Change<bool>>,
}

impl DocNodeDiff {
  pub fn diff(old: &DocNode, new: &DocNode) -> Option<Self> {
    let def_changes = DocNodeDefDiff::diff(&old.def, &new.def);
    let js_doc_changes = JsDocDiff::diff(&old.js_doc, &new.js_doc);

    let declaration_kind_change =
      if old.declaration_kind != new.declaration_kind {
        Some(Change::new(old.declaration_kind, new.declaration_kind))
      } else {
        None
      };

    let is_default_change =
      if old.is_default.unwrap_or(false) != new.is_default.unwrap_or(false) {
        Some(Change::new(
          old.is_default.unwrap_or(false),
          new.is_default.unwrap_or(false),
        ))
      } else {
        None
      };

    if def_changes.is_none()
      && js_doc_changes.is_none()
      && declaration_kind_change.is_none()
      && is_default_change.is_none()
    {
      return None;
    }

    Some(DocNodeDiff {
      name: old.name.clone(),
      kind: old.def.to_kind(),
      name_change: None,
      def_changes,
      js_doc_changes,
      declaration_kind_change,
      is_default_change,
    })
  }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum DocNodeDefDiff {
  Function(FunctionDiff),
  Variable(VariableDiff),
  Enum(EnumDiff),
  Class(ClassDiff),
  TypeAlias(TypeAliasDiff),
  Namespace(NamespaceDiff),
  Interface(InterfaceDiff),
  // ignore Import & Reference
}

impl DocNodeDefDiff {
  pub fn change_percentage(&self, old: &DocNodeDef, new: &DocNodeDef) -> f64 {
    match (self, old, new) {
      (DocNodeDefDiff::Function(d), _, _) => d.change_percentage(),
      (DocNodeDefDiff::Variable(d), _, _) => d.change_percentage(),
      (DocNodeDefDiff::TypeAlias(d), _, _) => d.change_percentage(),
      (
        DocNodeDefDiff::Enum(d),
        DocNodeDef::Enum { enum_def: old_def },
        DocNodeDef::Enum { enum_def: new_def },
      ) => d.change_percentage(old_def, new_def),
      (
        DocNodeDefDiff::Class(d),
        DocNodeDef::Class { class_def: old_def },
        DocNodeDef::Class { class_def: new_def },
      ) => d.change_percentage(old_def, new_def),
      (
        DocNodeDefDiff::Interface(d),
        DocNodeDef::Interface {
          interface_def: old_def,
        },
        DocNodeDef::Interface {
          interface_def: new_def,
        },
      ) => d.change_percentage(old_def, new_def),
      (
        DocNodeDefDiff::Namespace(d),
        DocNodeDef::Namespace {
          namespace_def: old_def,
        },
        DocNodeDef::Namespace {
          namespace_def: new_def,
        },
      ) => d.change_percentage(old_def, new_def),
      _ => 1.0,
    }
  }

  pub fn as_function(&self) -> Option<&FunctionDiff> {
    match self {
      DocNodeDefDiff::Function(d) => Some(d),
      _ => None,
    }
  }

  pub fn as_variable(&self) -> Option<&VariableDiff> {
    match self {
      DocNodeDefDiff::Variable(d) => Some(d),
      _ => None,
    }
  }

  pub fn as_enum(&self) -> Option<&EnumDiff> {
    match self {
      DocNodeDefDiff::Enum(d) => Some(d),
      _ => None,
    }
  }

  pub fn as_class(&self) -> Option<&ClassDiff> {
    match self {
      DocNodeDefDiff::Class(d) => Some(d),
      _ => None,
    }
  }

  pub fn as_type_alias(&self) -> Option<&TypeAliasDiff> {
    match self {
      DocNodeDefDiff::TypeAlias(d) => Some(d),
      _ => None,
    }
  }

  pub fn as_namespace(&self) -> Option<&NamespaceDiff> {
    match self {
      DocNodeDefDiff::Namespace(d) => Some(d),
      _ => None,
    }
  }

  pub fn as_interface(&self) -> Option<&InterfaceDiff> {
    match self {
      DocNodeDefDiff::Interface(d) => Some(d),
      _ => None,
    }
  }

  pub fn diff(old: &DocNodeDef, new: &DocNodeDef) -> Option<Self> {
    match (old, new) {
      (
        DocNodeDef::Function {
          function_def: old_def,
        },
        DocNodeDef::Function {
          function_def: new_def,
        },
      ) => FunctionDiff::diff(old_def, new_def).map(DocNodeDefDiff::Function),

      (
        DocNodeDef::Variable {
          variable_def: old_def,
        },
        DocNodeDef::Variable {
          variable_def: new_def,
        },
      ) => VariableDiff::diff(old_def, new_def).map(DocNodeDefDiff::Variable),

      (
        DocNodeDef::Enum { enum_def: old_def },
        DocNodeDef::Enum { enum_def: new_def },
      ) => EnumDiff::diff(old_def, new_def).map(DocNodeDefDiff::Enum),

      (
        DocNodeDef::Class { class_def: old_def },
        DocNodeDef::Class { class_def: new_def },
      ) => ClassDiff::diff(old_def, new_def).map(DocNodeDefDiff::Class),

      (
        DocNodeDef::TypeAlias {
          type_alias_def: old_def,
        },
        DocNodeDef::TypeAlias {
          type_alias_def: new_def,
        },
      ) => TypeAliasDiff::diff(old_def, new_def).map(DocNodeDefDiff::TypeAlias),

      (
        DocNodeDef::Namespace {
          namespace_def: old_def,
        },
        DocNodeDef::Namespace {
          namespace_def: new_def,
        },
      ) => NamespaceDiff::diff(old_def, new_def).map(DocNodeDefDiff::Namespace),

      (
        DocNodeDef::Interface {
          interface_def: old_def,
        },
        DocNodeDef::Interface {
          interface_def: new_def,
        },
      ) => InterfaceDiff::diff(old_def, new_def).map(DocNodeDefDiff::Interface),

      (DocNodeDef::Import { .. }, DocNodeDef::Import { .. }) => None,
      (DocNodeDef::Reference { .. }, DocNodeDef::Reference { .. }) => None,
      (DocNodeDef::ModuleDoc, DocNodeDef::ModuleDoc) => None,

      _ => unreachable!(),
    }
  }
}
