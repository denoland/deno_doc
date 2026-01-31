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

use std::collections::HashMap;

use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use serde::Deserialize;
use serde::Serialize;

use crate::DocNode;
use crate::DocNodeDef;
use crate::node::DeclarationKind;
use crate::node::DocNodeKind;
use crate::parser::ParseOutput;

pub use class::ClassDiff;
pub use r#enum::EnumDiff;
pub use function::FunctionDiff;
pub use interface::InterfaceDiff;
pub use js_doc::JsDocDiff;
pub use namespace::NamespaceDiff;
pub use ts_type::TsTypeDiff;
pub use type_alias::TypeAliasDiff;
pub use variable::VariableDiff;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum DiffKind {
  Added,
  Removed,
  Modified,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct DiffEntry<T> {
  pub kind: DiffKind,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub old: Option<T>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub new: Option<T>,
}

impl<T> DiffEntry<T> {
  pub fn added(value: T) -> Self {
    Self {
      kind: DiffKind::Added,
      old: None,
      new: Some(value),
    }
  }

  pub fn removed(value: T) -> Self {
    Self {
      kind: DiffKind::Removed,
      old: Some(value),
      new: None,
    }
  }

  pub fn modified(old: T, new: T) -> Self {
    Self {
      kind: DiffKind::Modified,
      old: Some(old),
      new: Some(new),
    }
  }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct NameChange {
  pub old: Box<str>,
  pub new: Box<str>,
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
    let old_modules = old.keys().collect::<std::collections::HashSet<_>>();
    let new_modules = new.keys().collect::<std::collections::HashSet<_>>();

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

    // Find potentially added symbols
    for ((name, kind), node) in &new_map {
      if !old_map.contains_key(&(name.clone(), *kind)) {
        added.push((*node).clone());
      }
    }

    // Find potentially removed symbols
    for ((name, kind), node) in &old_map {
      if !new_map.contains_key(&(name.clone(), *kind)) {
        removed.push((*node).clone());
      }
    }

    let mut matched_added = std::collections::HashSet::new();
    let mut matched_removed = std::collections::HashSet::new();

    for (r_idx, removed_node) in removed.iter().enumerate() {
      for (a_idx, added_node) in added.iter().enumerate() {
        if matched_added.contains(&a_idx) {
          continue;
        }

        if is_likely_rename(removed_node, added_node) {
          let def_changes =
            DocNodeDefDiff::diff(&removed_node.def, &added_node.def);
          let js_doc_changes =
            JsDocDiff::diff(&removed_node.js_doc, &added_node.js_doc);
          let declaration_kind_change =
            if removed_node.declaration_kind != added_node.declaration_kind {
              Some(DiffEntry::modified(
                removed_node.declaration_kind,
                added_node.declaration_kind,
              ))
            } else {
              None
            };

          let is_default_change = if removed_node.is_default.unwrap_or(false)
            != added_node.is_default.unwrap_or(false)
          {
            Some(DiffEntry::modified(
              removed_node.is_default.unwrap_or(false),
              added_node.is_default.unwrap_or(false),
            ))
          } else {
            None
          };

          modified.push(DocNodeDiff {
            name: added_node.name.clone(),
            kind: added_node.def.to_kind(),
            name_change: Some(NameChange {
              old: removed_node.name.clone(),
              new: added_node.name.clone(),
            }),
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

    // Remove matched items from added/removed
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

    // Find modified symbols
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

/// Check if two nodes are likely a rename (same definition, different name).
/// Uses JSON serialization for comparison since not all types implement PartialEq.
fn is_likely_rename(old: &DocNode, new: &DocNode) -> bool {
  use crate::node::DocNodeDef;

  if old.def.to_kind() != new.def.to_kind() {
    return false;
  }

  // Helper to compare via JSON serialization
  fn json_eq<T: serde::Serialize>(a: &T, b: &T) -> bool {
    match (serde_json::to_value(a), serde_json::to_value(b)) {
      (Ok(a_json), Ok(b_json)) => a_json == b_json,
      _ => false,
    }
  }

  // Compare definitions by serializing to JSON and comparing
  // This is a simple but effective way to check structural equality
  match (&old.def, &new.def) {
    (
      DocNodeDef::Function {
        function_def: old_fn,
      },
      DocNodeDef::Function {
        function_def: new_fn,
      },
    ) => json_eq(old_fn, new_fn),
    (
      DocNodeDef::Variable {
        variable_def: old_var,
      },
      DocNodeDef::Variable {
        variable_def: new_var,
      },
    ) => json_eq(old_var, new_var),
    (
      DocNodeDef::Class {
        class_def: old_class,
      },
      DocNodeDef::Class {
        class_def: new_class,
      },
    ) => json_eq(old_class, new_class),
    (
      DocNodeDef::Interface {
        interface_def: old_iface,
      },
      DocNodeDef::Interface {
        interface_def: new_iface,
      },
    ) => json_eq(old_iface, new_iface),
    (
      DocNodeDef::Enum { enum_def: old_enum },
      DocNodeDef::Enum { enum_def: new_enum },
    ) => json_eq(old_enum, new_enum),
    (
      DocNodeDef::TypeAlias {
        type_alias_def: old_alias,
      },
      DocNodeDef::TypeAlias {
        type_alias_def: new_alias,
      },
    ) => json_eq(old_alias, new_alias),
    (
      DocNodeDef::Namespace {
        namespace_def: old_ns,
      },
      DocNodeDef::Namespace {
        namespace_def: new_ns,
      },
    ) => {
      // For namespaces, just check element count as a basic heuristic
      // (full comparison would need recursive rename detection)
      old_ns.elements.len() == new_ns.elements.len()
    }
    _ => false,
  }
}

fn build_node_map(
  nodes: &[DocNode],
) -> HashMap<(String, DocNodeKind), &DocNode> {
  let mut map = HashMap::new();
  for node in nodes {
    let kind = node.def.to_kind();
    map.insert((node.name.to_string(), kind), node);
  }
  map
}

/// Detailed diff for a modified symbol.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DocNodeDiff {
  pub name: Box<str>,
  pub kind: DocNodeKind,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub name_change: Option<NameChange>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub def_changes: Option<DocNodeDefDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub js_doc_changes: Option<JsDocDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub declaration_kind_change: Option<DiffEntry<DeclarationKind>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_default_change: Option<DiffEntry<bool>>,
}

impl DocNodeDiff {
  pub fn diff(old: &DocNode, new: &DocNode) -> Option<Self> {
    let def_changes = DocNodeDefDiff::diff(&old.def, &new.def);
    let js_doc_changes = JsDocDiff::diff(&old.js_doc, &new.js_doc);

    let declaration_kind_change =
      if old.declaration_kind != new.declaration_kind {
        Some(DiffEntry::modified(
          old.declaration_kind,
          new.declaration_kind,
        ))
      } else {
        None
      };

    let is_default_change =
      if old.is_default.unwrap_or(false) != new.is_default.unwrap_or(false) {
        Some(DiffEntry::modified(
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
