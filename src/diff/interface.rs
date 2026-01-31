// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use super::Change;
use super::function::ParamsDiff;
use super::js_doc::JsDocDiff;
use super::ts_type::TsTypeDiff;
use super::ts_type::TypeParamsDiff;
use super::ts_type::types_equal;
use crate::interface::InterfaceDef;
use crate::ts_type::CallSignatureDef;
use crate::ts_type::ConstructorDef;
use crate::ts_type::IndexSignatureDef;
use crate::ts_type::MethodDef;
use crate::ts_type::PropertyDef;
use crate::ts_type::TsTypeDef;
use serde::Deserialize;
use serde::Serialize;
use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InterfaceDiff {
  #[serde(skip_serializing_if = "Option::is_none")]
  pub extends_change: Option<ExtendsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_params_change: Option<TypeParamsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub constructor_changes: Option<InterfaceConstructorsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub method_changes: Option<InterfaceMethodsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub property_changes: Option<InterfacePropertiesDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub call_signature_changes: Option<CallSignaturesDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub index_signature_changes: Option<InterfaceIndexSignaturesDiff>,
}

impl InterfaceDiff {
  pub fn diff(old: &InterfaceDef, new: &InterfaceDef) -> Option<Self> {
    let InterfaceDef {
      def_name: _, // internal, not part of public API
      extends: old_extends,
      constructors: old_constructors,
      methods: old_methods,
      properties: old_properties,
      call_signatures: old_call_signatures,
      index_signatures: old_index_signatures,
      type_params: old_type_params,
    } = old;
    let InterfaceDef {
      def_name: _,
      extends: new_extends,
      constructors: new_constructors,
      methods: new_methods,
      properties: new_properties,
      call_signatures: new_call_signatures,
      index_signatures: new_index_signatures,
      type_params: new_type_params,
    } = new;

    let extends_change = ExtendsDiff::diff(old_extends, new_extends);
    let type_params_change =
      TypeParamsDiff::diff(old_type_params, new_type_params);
    let constructor_changes =
      InterfaceConstructorsDiff::diff(old_constructors, new_constructors);
    let method_changes = InterfaceMethodsDiff::diff(old_methods, new_methods);
    let property_changes =
      InterfacePropertiesDiff::diff(old_properties, new_properties);
    let call_signature_changes =
      CallSignaturesDiff::diff(old_call_signatures, new_call_signatures);
    let index_signature_changes = InterfaceIndexSignaturesDiff::diff(
      old_index_signatures,
      new_index_signatures,
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
    let old_set = old.iter().map(|t| &t.repr).collect::<HashSet<_>>();
    let new_set = new.iter().map(|t| &t.repr).collect::<HashSet<_>>();

    let added = new
      .iter()
      .filter(|t| !old_set.contains(&t.repr))
      .cloned()
      .collect::<Vec<_>>();
    let removed = old
      .iter()
      .filter(|t| !new_set.contains(&t.repr))
      .cloned()
      .collect::<Vec<_>>();

    if added.is_empty() && removed.is_empty() {
      return None;
    }

    Some(ExtendsDiff { added, removed })
  }
}

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
    let old_by_param_count =
      old.iter().fold(HashMap::<_, Vec<_>>::new(), |mut acc, c| {
        acc.entry(c.params.len()).or_default().push(c);
        acc
      });
    let new_by_param_count =
      new.iter().fold(HashMap::<_, Vec<_>>::new(), |mut acc, c| {
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

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InterfaceConstructorDiff {
  #[serde(skip_serializing_if = "Option::is_none")]
  pub params_change: Option<ParamsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_params_change: Option<TypeParamsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub return_type_change: Option<TsTypeDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub js_doc_change: Option<JsDocDiff>,
}

impl InterfaceConstructorDiff {
  pub fn diff(old: &ConstructorDef, new: &ConstructorDef) -> Option<Self> {
    let ConstructorDef {
      js_doc: old_js_doc,
      params: old_params,
      return_type: old_return_type,
      type_params: old_type_params,
      location: _, // internal, not diffed
    } = old;
    let ConstructorDef {
      js_doc: new_js_doc,
      params: new_params,
      return_type: new_return_type,
      type_params: new_type_params,
      location: _,
    } = new;

    let params_change = ParamsDiff::diff(old_params, new_params);
    let type_params_change =
      TypeParamsDiff::diff(old_type_params, new_type_params);

    let return_type_change =
      TsTypeDiff::diff_optional(old_return_type, new_return_type, "void");

    let js_doc_change = JsDocDiff::diff(old_js_doc, new_js_doc);

    if params_change.is_none()
      && type_params_change.is_none()
      && return_type_change.is_none()
      && js_doc_change.is_none()
    {
      return None;
    }

    Some(InterfaceConstructorDiff {
      params_change,
      type_params_change,
      return_type_change,
      js_doc_change,
    })
  }
}

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
    let old_map = old
      .iter()
      .map(|m| ((&*m.name, m.kind), m))
      .collect::<HashMap<_, _>>();
    let new_map = new
      .iter()
      .map(|m| ((&*m.name, m.kind), m))
      .collect::<HashMap<_, _>>();

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

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InterfaceMethodDiff {
  pub name: String,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub optional_change: Option<Change<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub params_change: Option<ParamsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub return_type_change: Option<TsTypeDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_params_change: Option<TypeParamsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub js_doc_change: Option<JsDocDiff>,
}

impl InterfaceMethodDiff {
  pub fn diff(old: &MethodDef, new: &MethodDef) -> Option<Self> {
    let MethodDef {
      name: old_name,
      js_doc: old_js_doc,
      kind: _,     // methods matched by kind in caller
      location: _, // internal, not diffed
      params: old_params,
      computed: _, // not useful for diffing
      optional: old_optional,
      return_type: old_return_type,
      type_params: old_type_params,
    } = old;
    let MethodDef {
      name: _,
      js_doc: new_js_doc,
      kind: _,
      location: _,
      params: new_params,
      computed: _,
      optional: new_optional,
      return_type: new_return_type,
      type_params: new_type_params,
    } = new;

    let optional_change = if old_optional != new_optional {
      Some(Change::new(*old_optional, *new_optional))
    } else {
      None
    };

    let params_change = ParamsDiff::diff(old_params, new_params);

    let return_type_change =
      TsTypeDiff::diff_optional(old_return_type, new_return_type, "void");

    let type_params_change =
      TypeParamsDiff::diff(old_type_params, new_type_params);
    let js_doc_change = JsDocDiff::diff(old_js_doc, new_js_doc);

    if optional_change.is_none()
      && params_change.is_none()
      && return_type_change.is_none()
      && type_params_change.is_none()
      && js_doc_change.is_none()
    {
      return None;
    }

    Some(InterfaceMethodDiff {
      name: old_name.clone(),
      optional_change,
      params_change,
      return_type_change,
      type_params_change,
      js_doc_change,
    })
  }
}

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
    let old_map = old.iter().map(|p| (&*p.name, p)).collect::<HashMap<_, _>>();
    let new_map = new.iter().map(|p| (&*p.name, p)).collect::<HashMap<_, _>>();

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

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InterfacePropertyDiff {
  pub name: String,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub readonly_change: Option<Change<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub optional_change: Option<Change<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_change: Option<TsTypeDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub params_change: Option<ParamsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_params_change: Option<TypeParamsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub js_doc_change: Option<JsDocDiff>,
}

impl InterfacePropertyDiff {
  pub fn diff(old: &PropertyDef, new: &PropertyDef) -> Option<Self> {
    let PropertyDef {
      name: old_name,
      js_doc: old_js_doc,
      location: _, // internal, not diffed
      params: old_params,
      readonly: old_readonly,
      computed: _, // not useful for diffing
      optional: old_optional,
      ts_type: old_ts_type,
      type_params: old_type_params,
    } = old;
    let PropertyDef {
      name: _,
      js_doc: new_js_doc,
      location: _,
      params: new_params,
      readonly: new_readonly,
      computed: _,
      optional: new_optional,
      ts_type: new_ts_type,
      type_params: new_type_params,
    } = new;

    let readonly_change = if old_readonly != new_readonly {
      Some(Change::new(*old_readonly, *new_readonly))
    } else {
      None
    };

    let optional_change = if old_optional != new_optional {
      Some(Change::new(*old_optional, *new_optional))
    } else {
      None
    };

    let type_change =
      TsTypeDiff::diff_optional(old_ts_type, new_ts_type, "unknown");

    let params_change = ParamsDiff::diff(old_params, new_params);
    let type_params_change =
      TypeParamsDiff::diff(old_type_params, new_type_params);
    let js_doc_change = JsDocDiff::diff(old_js_doc, new_js_doc);

    if readonly_change.is_none()
      && optional_change.is_none()
      && type_change.is_none()
      && params_change.is_none()
      && type_params_change.is_none()
      && js_doc_change.is_none()
    {
      return None;
    }

    Some(InterfacePropertyDiff {
      name: old_name.clone(),
      readonly_change,
      optional_change,
      type_change,
      params_change,
      type_params_change,
      js_doc_change,
    })
  }
}

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
  pub params_change: Option<ParamsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_params_change: Option<TypeParamsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub ts_type_change: Option<TsTypeDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub js_doc_change: Option<JsDocDiff>,
}

impl CallSignatureDiff {
  pub fn diff(old: &CallSignatureDef, new: &CallSignatureDef) -> Option<Self> {
    let CallSignatureDef {
      js_doc: old_js_doc,
      location: _, // internal, not diffed
      params: old_params,
      ts_type: old_ts_type,
      type_params: old_type_params,
    } = old;
    let CallSignatureDef {
      js_doc: new_js_doc,
      location: _,
      params: new_params,
      ts_type: new_ts_type,
      type_params: new_type_params,
    } = new;

    let params_change = ParamsDiff::diff(old_params, new_params);
    let type_params_change =
      TypeParamsDiff::diff(old_type_params, new_type_params);

    let ts_type_change =
      TsTypeDiff::diff_optional(old_ts_type, new_ts_type, "void");

    let js_doc_change = JsDocDiff::diff(old_js_doc, new_js_doc);

    if params_change.is_none()
      && type_params_change.is_none()
      && ts_type_change.is_none()
      && js_doc_change.is_none()
    {
      return None;
    }

    Some(CallSignatureDiff {
      params_change,
      type_params_change,
      ts_type_change,
      js_doc_change,
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
  pub readonly_change: Option<Change<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub params_change: Option<ParamsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_change: Option<TsTypeDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub js_doc_change: Option<JsDocDiff>,
}

impl InterfaceIndexSignatureDiff {
  pub fn diff(
    old: &IndexSignatureDef,
    new: &IndexSignatureDef,
  ) -> Option<Self> {
    let IndexSignatureDef {
      js_doc: old_js_doc,
      readonly: old_readonly,
      params: old_params,
      ts_type: old_ts_type,
      location: _, // internal, not diffed
    } = old;
    let IndexSignatureDef {
      js_doc: new_js_doc,
      readonly: new_readonly,
      params: new_params,
      ts_type: new_ts_type,
      location: _,
    } = new;

    let readonly_change = if old_readonly != new_readonly {
      Some(Change::new(*old_readonly, *new_readonly))
    } else {
      None
    };

    let params_change = ParamsDiff::diff(old_params, new_params);

    let type_change =
      TsTypeDiff::diff_optional(old_ts_type, new_ts_type, "unknown");

    let js_doc_change = JsDocDiff::diff(old_js_doc, new_js_doc);

    if readonly_change.is_none()
      && params_change.is_none()
      && type_change.is_none()
      && js_doc_change.is_none()
    {
      return None;
    }

    Some(InterfaceIndexSignatureDiff {
      readonly_change,
      params_change,
      type_change,
      js_doc_change,
    })
  }
}
