// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use deno_ast::swc::ast::Accessibility;
use serde::Deserialize;
use serde::Serialize;

use crate::class::ClassConstructorDef;
use crate::class::ClassConstructorParamDef;
use crate::class::ClassDef;
use crate::class::ClassMethodDef;
use crate::class::ClassPropertyDef;
use crate::ts_type::IndexSignatureDef;
use crate::ts_type::TsTypeDef;

use super::DiffEntry;
use super::function::DecoratorsDiff;
use super::function::FunctionDiff;
use super::function::ParamsDiff;
use super::js_doc::JsDocDiff;
use super::ts_type::TsTypeDiff;
use super::ts_type::TypeParamsDiff;
use super::ts_type::types_equal;

/// Diff for a class definition.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ClassDiff {
  /// Change in abstract modifier.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_abstract_change: Option<DiffEntry<bool>>,
  /// Change in extends clause.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub extends_change: Option<DiffEntry<Option<Box<str>>>>,
  /// Changes to implements list.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub implements_change: Option<ImplementsDiff>,
  /// Changes to type parameters.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_params_change: Option<TypeParamsDiff>,
  /// Changes to super type parameters.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub super_type_params_change: Option<SuperTypeParamsDiff>,
  /// Constructor changes.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub constructor_changes: Option<ConstructorsDiff>,
  /// Method changes.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub method_changes: Option<MethodsDiff>,
  /// Property changes.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub property_changes: Option<PropertiesDiff>,
  /// Index signature changes.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub index_signature_changes: Option<IndexSignaturesDiff>,
  /// Decorator changes.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub decorators_change: Option<DecoratorsDiff>,
}

impl ClassDiff {
  /// Compare two class definitions.
  pub fn diff(old: &ClassDef, new: &ClassDef) -> Option<Self> {
    let is_abstract_change = if old.is_abstract != new.is_abstract {
      Some(DiffEntry::modified(old.is_abstract, new.is_abstract))
    } else {
      None
    };

    let extends_change = if old.extends != new.extends {
      Some(DiffEntry::modified(
        old.extends.clone(),
        new.extends.clone(),
      ))
    } else {
      None
    };

    let implements_change =
      ImplementsDiff::diff(&old.implements, &new.implements);
    let type_params_change =
      TypeParamsDiff::diff(&old.type_params, &new.type_params);
    let super_type_params_change =
      SuperTypeParamsDiff::diff(&old.super_type_params, &new.super_type_params);
    let constructor_changes =
      ConstructorsDiff::diff(&old.constructors, &new.constructors);
    let method_changes = MethodsDiff::diff(&old.methods, &new.methods);
    let property_changes =
      PropertiesDiff::diff(&old.properties, &new.properties);
    let index_signature_changes =
      IndexSignaturesDiff::diff(&old.index_signatures, &new.index_signatures);
    let decorators_change =
      DecoratorsDiff::diff(&old.decorators, &new.decorators);

    if is_abstract_change.is_none()
      && extends_change.is_none()
      && implements_change.is_none()
      && type_params_change.is_none()
      && super_type_params_change.is_none()
      && constructor_changes.is_none()
      && method_changes.is_none()
      && property_changes.is_none()
      && index_signature_changes.is_none()
      && decorators_change.is_none()
    {
      return None;
    }

    Some(ClassDiff {
      is_abstract_change,
      extends_change,
      implements_change,
      type_params_change,
      super_type_params_change,
      constructor_changes,
      method_changes,
      property_changes,
      index_signature_changes,
      decorators_change,
    })
  }
}

/// Diff for implements list.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct ImplementsDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added: Vec<TsTypeDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed: Vec<TsTypeDef>,
}

impl ImplementsDiff {
  pub fn diff(old: &[TsTypeDef], new: &[TsTypeDef]) -> Option<Self> {
    use std::collections::HashSet;

    let old_set: HashSet<_> = old.iter().map(|t| &t.repr).collect();
    let new_set: HashSet<_> = new.iter().map(|t| &t.repr).collect();

    let added: Vec<_> = new
      .iter()
      .filter(|t| !old_set.contains(&t.repr))
      .cloned()
      .collect();
    let removed: Vec<_> = old
      .iter()
      .filter(|t| !new_set.contains(&t.repr))
      .cloned()
      .collect();

    if added.is_empty() && removed.is_empty() {
      return None;
    }

    Some(ImplementsDiff { added, removed })
  }
}

/// Diff for super type parameters (generic arguments to the extended class).
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct SuperTypeParamsDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added: Vec<TsTypeDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed: Vec<TsTypeDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub modified: Vec<TsTypeDiff>,
}

impl SuperTypeParamsDiff {
  pub fn diff(old: &[TsTypeDef], new: &[TsTypeDef]) -> Option<Self> {
    let max_len = old.len().max(new.len());
    let mut added = Vec::new();
    let mut removed = Vec::new();
    let mut modified = Vec::new();

    for i in 0..max_len {
      match (old.get(i), new.get(i)) {
        (Some(old_type), Some(new_type)) => {
          if let Some(diff) = TsTypeDiff::diff(old_type, new_type) {
            modified.push(diff);
          }
        }
        (Some(old_type), None) => {
          removed.push(old_type.clone());
        }
        (None, Some(new_type)) => {
          added.push(new_type.clone());
        }
        (None, None) => unreachable!(),
      }
    }

    if added.is_empty() && removed.is_empty() && modified.is_empty() {
      return None;
    }

    Some(SuperTypeParamsDiff {
      added,
      removed,
      modified,
    })
  }
}

/// Diff for class constructors.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct ConstructorsDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added: Vec<ClassConstructorDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed: Vec<ClassConstructorDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub modified: Vec<ConstructorDiff>,
}

impl ConstructorsDiff {
  pub fn diff(
    old: &[ClassConstructorDef],
    new: &[ClassConstructorDef],
  ) -> Option<Self> {
    // Match constructors by parameter count (simple heuristic)
    // In practice, most classes have 0 or 1 constructor
    let mut added = Vec::new();
    let mut removed = Vec::new();
    let mut modified = Vec::new();

    // Group by parameter count
    use std::collections::HashMap;
    let old_by_param_count: HashMap<_, Vec<_>> =
      old.iter().fold(HashMap::new(), |mut acc, c| {
        acc.entry(c.params.len()).or_default().push(c);
        acc
      });
    let new_by_param_count: HashMap<_, Vec<_>> =
      new.iter().fold(HashMap::new(), |mut acc, c| {
        acc.entry(c.params.len()).or_default().push(c);
        acc
      });

    for (param_count, new_ctors) in &new_by_param_count {
      match old_by_param_count.get(param_count) {
        Some(old_ctors) => {
          // Compare the first constructor of each (simple case)
          if let (Some(old_ctor), Some(new_ctor)) =
            (old_ctors.first(), new_ctors.first())
          {
            if let Some(diff) = ConstructorDiff::diff(old_ctor, new_ctor) {
              modified.push(diff);
            }
          }
          // Handle extra constructors
          for new_ctor in new_ctors.iter().skip(old_ctors.len()) {
            added.push((*new_ctor).clone());
          }
        }
        None => {
          for new_ctor in new_ctors {
            added.push((*new_ctor).clone());
          }
        }
      }
    }

    for (param_count, old_ctors) in &old_by_param_count {
      if !new_by_param_count.contains_key(param_count) {
        for old_ctor in old_ctors {
          removed.push((*old_ctor).clone());
        }
      } else if let Some(new_ctors) = new_by_param_count.get(param_count) {
        for old_ctor in old_ctors.iter().skip(new_ctors.len()) {
          removed.push((*old_ctor).clone());
        }
      }
    }

    if added.is_empty() && removed.is_empty() && modified.is_empty() {
      return None;
    }

    Some(ConstructorsDiff {
      added,
      removed,
      modified,
    })
  }
}

/// Diff for a single constructor.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ConstructorDiff {
  #[serde(skip_serializing_if = "Option::is_none")]
  pub accessibility_change: Option<DiffEntry<Option<Accessibility>>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_optional_change: Option<DiffEntry<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub params_change: Option<ConstructorParamsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub js_doc_change: Option<JsDocDiff>,
}

impl ConstructorDiff {
  pub fn diff(
    old: &ClassConstructorDef,
    new: &ClassConstructorDef,
  ) -> Option<Self> {
    let accessibility_change = if old.accessibility != new.accessibility {
      Some(DiffEntry::modified(old.accessibility, new.accessibility))
    } else {
      None
    };

    let is_optional_change = if old.is_optional != new.is_optional {
      Some(DiffEntry::modified(old.is_optional, new.is_optional))
    } else {
      None
    };

    let params_change = ConstructorParamsDiff::diff(&old.params, &new.params);
    let js_doc_change = JsDocDiff::diff(&old.js_doc, &new.js_doc);

    if accessibility_change.is_none()
      && is_optional_change.is_none()
      && params_change.is_none()
      && js_doc_change.is_none()
    {
      return None;
    }

    Some(ConstructorDiff {
      accessibility_change,
      is_optional_change,
      params_change,
      js_doc_change,
    })
  }
}

/// Diff for constructor parameters.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct ConstructorParamsDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added: Vec<ClassConstructorParamDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed: Vec<ClassConstructorParamDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub modified: Vec<ConstructorParamDiff>,
}

impl ConstructorParamsDiff {
  pub fn diff(
    old: &[ClassConstructorParamDef],
    new: &[ClassConstructorParamDef],
  ) -> Option<Self> {
    let max_len = old.len().max(new.len());
    let mut added = Vec::new();
    let mut removed = Vec::new();
    let mut modified = Vec::new();

    for i in 0..max_len {
      match (old.get(i), new.get(i)) {
        (Some(old_param), Some(new_param)) => {
          if let Some(diff) =
            ConstructorParamDiff::diff(i, old_param, new_param)
          {
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

    Some(ConstructorParamsDiff {
      added,
      removed,
      modified,
    })
  }
}

/// Diff for a single constructor parameter.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ConstructorParamDiff {
  pub index: usize,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub accessibility_change: Option<DiffEntry<Option<Accessibility>>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_override_change: Option<DiffEntry<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub readonly_change: Option<DiffEntry<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub param_change: Option<ParamsDiff>,
}

impl ConstructorParamDiff {
  pub fn diff(
    index: usize,
    old: &ClassConstructorParamDef,
    new: &ClassConstructorParamDef,
  ) -> Option<Self> {
    let accessibility_change = if old.accessibility != new.accessibility {
      Some(DiffEntry::modified(old.accessibility, new.accessibility))
    } else {
      None
    };

    let is_override_change = if old.is_override != new.is_override {
      Some(DiffEntry::modified(old.is_override, new.is_override))
    } else {
      None
    };

    let readonly_change = if old.readonly != new.readonly {
      Some(DiffEntry::modified(old.readonly, new.readonly))
    } else {
      None
    };

    let param_change = ParamsDiff::diff(
      std::slice::from_ref(&old.param),
      std::slice::from_ref(&new.param),
    );

    if accessibility_change.is_none()
      && is_override_change.is_none()
      && readonly_change.is_none()
      && param_change.is_none()
    {
      return None;
    }

    Some(ConstructorParamDiff {
      index,
      accessibility_change,
      is_override_change,
      readonly_change,
      param_change,
    })
  }
}

/// Diff for class methods.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct MethodsDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added: Vec<ClassMethodDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed: Vec<ClassMethodDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub modified: Vec<MethodDiff>,
}

impl MethodsDiff {
  pub fn diff(old: &[ClassMethodDef], new: &[ClassMethodDef]) -> Option<Self> {
    use std::collections::HashMap;

    // Key by (name, is_static, kind) to handle method overloads
    let old_map: HashMap<_, _> = old
      .iter()
      .map(|m| ((&*m.name, m.is_static, m.kind), m))
      .collect();
    let new_map: HashMap<_, _> = new
      .iter()
      .map(|m| ((&*m.name, m.is_static, m.kind), m))
      .collect();

    let mut added = Vec::new();
    let mut removed = Vec::new();
    let mut modified = Vec::new();

    for (key, new_method) in &new_map {
      match old_map.get(key) {
        Some(old_method) => {
          if let Some(diff) = MethodDiff::diff(old_method, new_method) {
            modified.push(diff);
          }
        }
        None => {
          added.push((*new_method).clone());
        }
      }
    }

    for (key, old_method) in &old_map {
      if !new_map.contains_key(key) {
        removed.push((*old_method).clone());
      }
    }

    if added.is_empty() && removed.is_empty() && modified.is_empty() {
      return None;
    }

    Some(MethodsDiff {
      added,
      removed,
      modified,
    })
  }
}

/// Diff for a single class method.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MethodDiff {
  pub name: Box<str>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub accessibility_change: Option<DiffEntry<Option<Accessibility>>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_static_change: Option<DiffEntry<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_abstract_change: Option<DiffEntry<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_override_change: Option<DiffEntry<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub optional_change: Option<DiffEntry<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub function_diff: Option<FunctionDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub js_doc_change: Option<JsDocDiff>,
}

impl MethodDiff {
  pub fn diff(old: &ClassMethodDef, new: &ClassMethodDef) -> Option<Self> {
    let accessibility_change = if old.accessibility != new.accessibility {
      Some(DiffEntry::modified(old.accessibility, new.accessibility))
    } else {
      None
    };

    let is_static_change = if old.is_static != new.is_static {
      Some(DiffEntry::modified(old.is_static, new.is_static))
    } else {
      None
    };

    let is_abstract_change = if old.is_abstract != new.is_abstract {
      Some(DiffEntry::modified(old.is_abstract, new.is_abstract))
    } else {
      None
    };

    let is_override_change = if old.is_override != new.is_override {
      Some(DiffEntry::modified(old.is_override, new.is_override))
    } else {
      None
    };

    let optional_change = if old.optional != new.optional {
      Some(DiffEntry::modified(old.optional, new.optional))
    } else {
      None
    };

    let function_diff =
      FunctionDiff::diff(&old.function_def, &new.function_def);
    let js_doc_change = JsDocDiff::diff(&old.js_doc, &new.js_doc);

    if accessibility_change.is_none()
      && is_static_change.is_none()
      && is_abstract_change.is_none()
      && is_override_change.is_none()
      && optional_change.is_none()
      && function_diff.is_none()
      && js_doc_change.is_none()
    {
      return None;
    }

    Some(MethodDiff {
      name: old.name.clone(),
      accessibility_change,
      is_static_change,
      is_abstract_change,
      is_override_change,
      optional_change,
      function_diff,
      js_doc_change,
    })
  }
}

/// Diff for class properties.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct PropertiesDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added: Vec<ClassPropertyDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed: Vec<ClassPropertyDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub modified: Vec<PropertyDiff>,
}

impl PropertiesDiff {
  pub fn diff(
    old: &[ClassPropertyDef],
    new: &[ClassPropertyDef],
  ) -> Option<Self> {
    use std::collections::HashMap;

    // Key by (name, is_static)
    let old_map: HashMap<_, _> =
      old.iter().map(|p| ((&*p.name, p.is_static), p)).collect();
    let new_map: HashMap<_, _> =
      new.iter().map(|p| ((&*p.name, p.is_static), p)).collect();

    let mut added = Vec::new();
    let mut removed = Vec::new();
    let mut modified = Vec::new();

    for (key, new_prop) in &new_map {
      match old_map.get(key) {
        Some(old_prop) => {
          if let Some(diff) = PropertyDiff::diff(old_prop, new_prop) {
            modified.push(diff);
          }
        }
        None => {
          added.push((*new_prop).clone());
        }
      }
    }

    for (key, old_prop) in &old_map {
      if !new_map.contains_key(key) {
        removed.push((*old_prop).clone());
      }
    }

    if added.is_empty() && removed.is_empty() && modified.is_empty() {
      return None;
    }

    Some(PropertiesDiff {
      added,
      removed,
      modified,
    })
  }
}

/// Diff for a single class property.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PropertyDiff {
  pub name: Box<str>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub accessibility_change: Option<DiffEntry<Option<Accessibility>>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub readonly_change: Option<DiffEntry<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_static_change: Option<DiffEntry<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_abstract_change: Option<DiffEntry<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_override_change: Option<DiffEntry<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub optional_change: Option<DiffEntry<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_change: Option<TsTypeDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub js_doc_change: Option<JsDocDiff>,
}

impl PropertyDiff {
  pub fn diff(old: &ClassPropertyDef, new: &ClassPropertyDef) -> Option<Self> {
    let accessibility_change = if old.accessibility != new.accessibility {
      Some(DiffEntry::modified(old.accessibility, new.accessibility))
    } else {
      None
    };

    let readonly_change = if old.readonly != new.readonly {
      Some(DiffEntry::modified(old.readonly, new.readonly))
    } else {
      None
    };

    let is_static_change = if old.is_static != new.is_static {
      Some(DiffEntry::modified(old.is_static, new.is_static))
    } else {
      None
    };

    let is_abstract_change = if old.is_abstract != new.is_abstract {
      Some(DiffEntry::modified(old.is_abstract, new.is_abstract))
    } else {
      None
    };

    let is_override_change = if old.is_override != new.is_override {
      Some(DiffEntry::modified(old.is_override, new.is_override))
    } else {
      None
    };

    let optional_change = if old.optional != new.optional {
      Some(DiffEntry::modified(old.optional, new.optional))
    } else {
      None
    };

    let type_change = if !types_equal(&old.ts_type, &new.ts_type) {
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

    let js_doc_change = JsDocDiff::diff(&old.js_doc, &new.js_doc);

    if accessibility_change.is_none()
      && readonly_change.is_none()
      && is_static_change.is_none()
      && is_abstract_change.is_none()
      && is_override_change.is_none()
      && optional_change.is_none()
      && type_change.is_none()
      && js_doc_change.is_none()
    {
      return None;
    }

    Some(PropertyDiff {
      name: old.name.clone(),
      accessibility_change,
      readonly_change,
      is_static_change,
      is_abstract_change,
      is_override_change,
      optional_change,
      type_change,
      js_doc_change,
    })
  }
}

/// Diff for index signatures.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct IndexSignaturesDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added: Vec<IndexSignatureDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed: Vec<IndexSignatureDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub modified: Vec<IndexSignatureDiff>,
}

impl IndexSignaturesDiff {
  pub fn diff(
    old: &[IndexSignatureDef],
    new: &[IndexSignatureDef],
  ) -> Option<Self> {
    // Index signatures are tricky to match - use position-based comparison
    let max_len = old.len().max(new.len());
    let mut added = Vec::new();
    let mut removed = Vec::new();
    let mut modified = Vec::new();

    for i in 0..max_len {
      match (old.get(i), new.get(i)) {
        (Some(old_sig), Some(new_sig)) => {
          if let Some(diff) = IndexSignatureDiff::diff(old_sig, new_sig) {
            modified.push(diff);
          }
        }
        (Some(old_sig), None) => {
          removed.push(old_sig.clone());
        }
        (None, Some(new_sig)) => {
          added.push(new_sig.clone());
        }
        (None, None) => unreachable!(),
      }
    }

    if added.is_empty() && removed.is_empty() && modified.is_empty() {
      return None;
    }

    Some(IndexSignaturesDiff {
      added,
      removed,
      modified,
    })
  }
}

/// Diff for a single index signature.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct IndexSignatureDiff {
  #[serde(skip_serializing_if = "Option::is_none")]
  pub readonly_change: Option<DiffEntry<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub params_change: Option<ParamsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_change: Option<TsTypeDiff>,
}

impl IndexSignatureDiff {
  pub fn diff(
    old: &IndexSignatureDef,
    new: &IndexSignatureDef,
  ) -> Option<Self> {
    let readonly_change = if old.readonly != new.readonly {
      Some(DiffEntry::modified(old.readonly, new.readonly))
    } else {
      None
    };

    let params_change = ParamsDiff::diff(&old.params, &new.params);

    let type_change = if !types_equal(&old.ts_type, &new.ts_type) {
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

    if readonly_change.is_none()
      && params_change.is_none()
      && type_change.is_none()
    {
      return None;
    }

    Some(IndexSignatureDiff {
      readonly_change,
      params_change,
      type_change,
    })
  }
}
