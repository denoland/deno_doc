// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use serde::Deserialize;
use serde::Serialize;
use std::collections::HashMap;
use crate::interface::InterfaceDef;
use crate::ts_type::CallSignatureDef;
use crate::ts_type::ConstructorDef;
use crate::ts_type::IndexSignatureDef;
use crate::ts_type::MethodDef;
use crate::ts_type::PropertyDef;
use crate::ts_type::TsTypeDef;
use super::DiffEntry;
use super::js_doc::JsDocDiff;
use super::ts_type::TsTypeDiff;
use super::ts_type::TypeParamsDiff;
use super::ts_type::types_equal;

/// Diff for an interface definition.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InterfaceDiff {
  /// Changes to extends list.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub extends_change: Option<ExtendsDiff>,
  /// Type parameter changes.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_params_change: Option<TypeParamsDiff>,
  /// Constructor changes.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub constructor_changes: Option<InterfaceConstructorsDiff>,
  /// Method changes.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub method_changes: Option<InterfaceMethodsDiff>,
  /// Property changes.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub property_changes: Option<InterfacePropertiesDiff>,
  /// Call signature changes.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub call_signature_changes: Option<CallSignaturesDiff>,
  /// Index signature changes.
  #[serde(skip_serializing_if = "Option::is_none")]
  pub index_signature_changes: Option<InterfaceIndexSignaturesDiff>,
}

impl InterfaceDiff {
  /// Compare two interface definitions.
  pub fn diff(old: &InterfaceDef, new: &InterfaceDef) -> Option<Self> {
    let extends_change = ExtendsDiff::diff(&old.extends, &new.extends);
    let type_params_change =
      TypeParamsDiff::diff(&old.type_params, &new.type_params);
    let constructor_changes =
      InterfaceConstructorsDiff::diff(&old.constructors, &new.constructors);
    let method_changes = InterfaceMethodsDiff::diff(&old.methods, &new.methods);
    let property_changes =
      InterfacePropertiesDiff::diff(&old.properties, &new.properties);
    let call_signature_changes =
      CallSignaturesDiff::diff(&old.call_signatures, &new.call_signatures);
    let index_signature_changes = InterfaceIndexSignaturesDiff::diff(
      &old.index_signatures,
      &new.index_signatures,
    );

    if extends_change.is_none()
      && type_params_change.is_none()
      && constructor_changes.is_none()
      && method_changes.is_none()
      && property_changes.is_none()
      && call_signature_changes.is_none()
      && index_signature_changes.is_none()
    {
      return None;
    }

    Some(InterfaceDiff {
      extends_change,
      type_params_change,
      constructor_changes,
      method_changes,
      property_changes,
      call_signature_changes,
      index_signature_changes,
    })
  }
}

/// Diff for extends list.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct ExtendsDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added: Vec<TsTypeDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed: Vec<TsTypeDef>,
}

impl ExtendsDiff {
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

    Some(ExtendsDiff { added, removed })
  }
}

/// Diff for interface constructors.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct InterfaceConstructorsDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added: Vec<ConstructorDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed: Vec<ConstructorDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub modified: Vec<InterfaceConstructorDiff>,
}

impl InterfaceConstructorsDiff {
  pub fn diff(old: &[ConstructorDef], new: &[ConstructorDef]) -> Option<Self> {
    // Match by parameter count
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

    let mut added = Vec::new();
    let mut removed = Vec::new();
    let mut modified = Vec::new();

    for (param_count, new_ctors) in &new_by_param_count {
      match old_by_param_count.get(param_count) {
        Some(old_ctors) => {
          if let (Some(old_ctor), Some(new_ctor)) =
            (old_ctors.first(), new_ctors.first())
            && let Some(diff) =
              InterfaceConstructorDiff::diff(old_ctor, new_ctor)
          {
            modified.push(diff);
          }

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

    Some(InterfaceConstructorsDiff {
      added,
      removed,
      modified,
    })
  }
}

/// Diff for a single interface constructor.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InterfaceConstructorDiff {
  #[serde(skip_serializing_if = "Option::is_none")]
  pub return_type_change: Option<TsTypeDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub js_doc_change: Option<JsDocDiff>,
  pub old: ConstructorDef,
  pub new: ConstructorDef,
}

impl InterfaceConstructorDiff {
  pub fn diff(old: &ConstructorDef, new: &ConstructorDef) -> Option<Self> {
    let return_type_change = if !types_equal(&old.return_type, &new.return_type)
    {
      match (&old.return_type, &new.return_type) {
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

    let js_doc_change = JsDocDiff::diff(&old.js_doc, &new.js_doc);

    let params_changed =
      old.params.len() != new.params.len() || old.params != new.params;

    if return_type_change.is_none()
      && js_doc_change.is_none()
      && !params_changed
    {
      return None;
    }

    Some(InterfaceConstructorDiff {
      return_type_change,
      js_doc_change,
      old: old.clone(),
      new: new.clone(),
    })
  }
}

/// Diff for interface methods.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct InterfaceMethodsDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added: Vec<MethodDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed: Vec<MethodDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub modified: Vec<InterfaceMethodDiff>,
}

impl InterfaceMethodsDiff {
  pub fn diff(old: &[MethodDef], new: &[MethodDef]) -> Option<Self> {
    use std::collections::HashMap;

    // Key by (name, kind)
    let old_map: HashMap<_, _> =
      old.iter().map(|m| ((&*m.name, m.kind), m)).collect();
    let new_map: HashMap<_, _> =
      new.iter().map(|m| ((&*m.name, m.kind), m)).collect();

    let mut added = Vec::new();
    let mut removed = Vec::new();
    let mut modified = Vec::new();

    for (key, new_method) in &new_map {
      match old_map.get(key) {
        Some(old_method) => {
          if let Some(diff) = InterfaceMethodDiff::diff(old_method, new_method)
          {
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

    Some(InterfaceMethodsDiff {
      added,
      removed,
      modified,
    })
  }
}

/// Diff for a single interface method.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InterfaceMethodDiff {
  pub name: String,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub optional_change: Option<DiffEntry<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub return_type_change: Option<TsTypeDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_params_change: Option<TypeParamsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub js_doc_change: Option<JsDocDiff>,
  pub old: MethodDef,
  pub new: MethodDef,
}

impl InterfaceMethodDiff {
  pub fn diff(old: &MethodDef, new: &MethodDef) -> Option<Self> {
    let optional_change = if old.optional != new.optional {
      Some(DiffEntry::modified(old.optional, new.optional))
    } else {
      None
    };

    let return_type_change = if !types_equal(&old.return_type, &new.return_type)
    {
      match (&old.return_type, &new.return_type) {
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

    let type_params_change =
      TypeParamsDiff::diff(&old.type_params, &new.type_params);
    let js_doc_change = JsDocDiff::diff(&old.js_doc, &new.js_doc);

    let params_changed =
      old.params.len() != new.params.len() || old.params != new.params;

    if optional_change.is_none()
      && return_type_change.is_none()
      && type_params_change.is_none()
      && js_doc_change.is_none()
      && !params_changed
    {
      return None;
    }

    Some(InterfaceMethodDiff {
      name: old.name.clone(),
      optional_change,
      return_type_change,
      type_params_change,
      js_doc_change,
      old: old.clone(),
      new: new.clone(),
    })
  }
}

/// Diff for interface properties.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct InterfacePropertiesDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added: Vec<PropertyDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed: Vec<PropertyDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub modified: Vec<InterfacePropertyDiff>,
}

impl InterfacePropertiesDiff {
  pub fn diff(old: &[PropertyDef], new: &[PropertyDef]) -> Option<Self> {
    use std::collections::HashMap;

    let old_map: HashMap<_, _> = old.iter().map(|p| (&*p.name, p)).collect();
    let new_map: HashMap<_, _> = new.iter().map(|p| (&*p.name, p)).collect();

    let mut added = Vec::new();
    let mut removed = Vec::new();
    let mut modified = Vec::new();

    for (key, new_prop) in &new_map {
      match old_map.get(key) {
        Some(old_prop) => {
          if let Some(diff) = InterfacePropertyDiff::diff(old_prop, new_prop) {
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

    Some(InterfacePropertiesDiff {
      added,
      removed,
      modified,
    })
  }
}

/// Diff for a single interface property.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InterfacePropertyDiff {
  pub name: String,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub readonly_change: Option<DiffEntry<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub optional_change: Option<DiffEntry<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_change: Option<TsTypeDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub js_doc_change: Option<JsDocDiff>,
}

impl InterfacePropertyDiff {
  pub fn diff(old: &PropertyDef, new: &PropertyDef) -> Option<Self> {
    let readonly_change = if old.readonly != new.readonly {
      Some(DiffEntry::modified(old.readonly, new.readonly))
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

    if readonly_change.is_none()
      && optional_change.is_none()
      && type_change.is_none()
      && js_doc_change.is_none()
    {
      return None;
    }

    Some(InterfacePropertyDiff {
      name: old.name.clone(),
      readonly_change,
      optional_change,
      type_change,
      js_doc_change,
    })
  }
}

/// Diff for call signatures.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct CallSignaturesDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added: Vec<CallSignatureDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed: Vec<CallSignatureDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub modified: Vec<CallSignatureDiff>,
}

impl CallSignaturesDiff {
  pub fn diff(
    old: &[CallSignatureDef],
    new: &[CallSignatureDef],
  ) -> Option<Self> {
    // Position-based comparison
    let max_len = old.len().max(new.len());
    let mut added = Vec::new();
    let mut removed = Vec::new();
    let mut modified = Vec::new();

    for i in 0..max_len {
      match (old.get(i), new.get(i)) {
        (Some(old_sig), Some(new_sig)) => {
          if let Some(diff) = CallSignatureDiff::diff(old_sig, new_sig) {
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

    Some(CallSignaturesDiff {
      added,
      removed,
      modified,
    })
  }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CallSignatureDiff {
  #[serde(skip_serializing_if = "Option::is_none")]
  pub return_type_change: Option<TsTypeDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub js_doc_change: Option<JsDocDiff>,
  pub old: CallSignatureDef,
  pub new: CallSignatureDef,
}

impl CallSignatureDiff {
  pub fn diff(old: &CallSignatureDef, new: &CallSignatureDef) -> Option<Self> {
    let return_type_change = if !types_equal(&old.ts_type, &new.ts_type) {
      match (&old.ts_type, &new.ts_type) {
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

    let js_doc_change = JsDocDiff::diff(&old.js_doc, &new.js_doc);

    let params_changed =
      old.params.len() != new.params.len() || old.params != new.params;

    if return_type_change.is_none()
      && js_doc_change.is_none()
      && !params_changed
    {
      return None;
    }

    Some(CallSignatureDiff {
      return_type_change,
      js_doc_change,
      old: old.clone(),
      new: new.clone(),
    })
  }
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct InterfaceIndexSignaturesDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added: Vec<IndexSignatureDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed: Vec<IndexSignatureDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub modified: Vec<InterfaceIndexSignatureDiff>,
}

impl InterfaceIndexSignaturesDiff {
  pub fn diff(
    old: &[IndexSignatureDef],
    new: &[IndexSignatureDef],
  ) -> Option<Self> {
    let max_len = old.len().max(new.len());
    let mut added = Vec::new();
    let mut removed = Vec::new();
    let mut modified = Vec::new();

    for i in 0..max_len {
      match (old.get(i), new.get(i)) {
        (Some(old_sig), Some(new_sig)) => {
          if let Some(diff) =
            InterfaceIndexSignatureDiff::diff(old_sig, new_sig)
          {
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

    Some(InterfaceIndexSignaturesDiff {
      added,
      removed,
      modified,
    })
  }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InterfaceIndexSignatureDiff {
  #[serde(skip_serializing_if = "Option::is_none")]
  pub readonly_change: Option<DiffEntry<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_change: Option<TsTypeDiff>,
  pub old: IndexSignatureDef,
  pub new: IndexSignatureDef,
}

impl InterfaceIndexSignatureDiff {
  pub fn diff(
    old: &IndexSignatureDef,
    new: &IndexSignatureDef,
  ) -> Option<Self> {
    let readonly_change = if old.readonly != new.readonly {
      Some(DiffEntry::modified(old.readonly, new.readonly))
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

    let params_changed =
      old.params.len() != new.params.len() || old.params != new.params;

    if readonly_change.is_none() && type_change.is_none() && !params_changed {
      return None;
    }

    Some(InterfaceIndexSignatureDiff {
      readonly_change,
      type_change,
      old: old.clone(),
      new: new.clone(),
    })
  }
}
