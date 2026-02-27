// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use super::Change;
use super::RenameCandidate;
use super::detect_renames;
use super::function::ParamsDiff;
use super::js_doc::JsDocDiff;
use super::ts_type::TsTypeDiff;
use super::ts_type::TypeParamsDiff;
use crate::interface::InterfaceDef;
use crate::ts_type::CallSignatureDef;
use crate::ts_type::ConstructorDef;
use crate::ts_type::IndexSignatureDef;
use crate::ts_type::MethodDef;
use crate::ts_type::PropertyDef;
use crate::ts_type::TsTypeDef;
use indexmap::IndexMap;
use indexmap::IndexSet;
use serde::Deserialize;
use serde::Serialize;

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

  pub fn diff_type_literal(
    old: &crate::ts_type::TsTypeLiteralDef,
    new: &crate::ts_type::TsTypeLiteralDef,
  ) -> Option<Self> {
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

    if constructor_changes.is_none()
      && method_changes.is_none()
      && property_changes.is_none()
      && call_signature_changes.is_none()
      && index_signature_changes.is_none()
    {
      return None;
    }

    Some(InterfaceDiff {
      extends_change: None,
      type_params_change: None,
      constructor_changes,
      method_changes,
      property_changes,
      call_signature_changes,
      index_signature_changes,
    })
  }

  pub fn change_percentage(
    &self,
    old: &InterfaceDef,
    new: &InterfaceDef,
  ) -> f64 {
    // Fields: extends, type_params = 2
    let field_total = 2;
    let field_changed = self.extends_change.is_some() as usize
      + self.type_params_change.is_some() as usize;

    // Collection items
    let constructor_total = old.constructors.len().max(new.constructors.len());
    let constructor_changed = self
      .constructor_changes
      .as_ref()
      .map_or(0, |c| c.added.len() + c.removed.len() + c.modified.len());

    let method_total = old.methods.len().max(new.methods.len());
    let method_changed = self
      .method_changes
      .as_ref()
      .map_or(0, |c| c.added.len() + c.removed.len() + c.modified.len());

    let property_total = old.properties.len().max(new.properties.len());
    let property_changed = self
      .property_changes
      .as_ref()
      .map_or(0, |c| c.added.len() + c.removed.len() + c.modified.len());

    let call_sig_total =
      old.call_signatures.len().max(new.call_signatures.len());
    let call_sig_changed = self
      .call_signature_changes
      .as_ref()
      .map_or(0, |c| c.added.len() + c.removed.len() + c.modified.len());

    let index_sig_total =
      old.index_signatures.len().max(new.index_signatures.len());
    let index_sig_changed = self
      .index_signature_changes
      .as_ref()
      .map_or(0, |c| c.added.len() + c.removed.len() + c.modified.len());

    let total = field_total
      + constructor_total
      + method_total
      + property_total
      + call_sig_total
      + index_sig_total;
    let changed = field_changed
      + constructor_changed
      + method_changed
      + property_changed
      + call_sig_changed
      + index_sig_changed;

    if total == 0 {
      return 0.0;
    }
    (changed as f64 / total as f64).min(1.0)
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
    let old_set = old.iter().map(|t| &t.repr).collect::<IndexSet<_>>();
    let new_set = new.iter().map(|t| &t.repr).collect::<IndexSet<_>>();

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
      old.iter().fold(IndexMap::<_, Vec<_>>::new(), |mut acc, c| {
        acc.entry(c.params.len()).or_default().push(c);
        acc
      });
    let new_by_param_count =
      new.iter().fold(IndexMap::<_, Vec<_>>::new(), |mut acc, c| {
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
  pub param_count: usize,
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
      param_count: new_params.len(),
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
      .collect::<IndexMap<_, _>>();
    let new_map = new
      .iter()
      .map(|m| ((&*m.name, m.kind), m))
      .collect::<IndexMap<_, _>>();

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

    detect_renames::<InterfaceMethodDiff>(
      &mut added,
      &mut removed,
      &mut modified,
    );

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
  pub name_change: Option<Change<String>>,
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
      name_change: None,
      optional_change,
      params_change,
      return_type_change,
      type_params_change,
      js_doc_change,
    })
  }

  pub fn change_percentage(&self) -> f64 {
    let changed = self.optional_change.is_some() as usize
      + self.params_change.is_some() as usize
      + self.return_type_change.is_some() as usize
      + self.type_params_change.is_some() as usize;
    changed as f64 / 4.0
  }
}

impl RenameCandidate for InterfaceMethodDiff {
  type Item = MethodDef;

  fn is_candidate(old: &MethodDef, new: &MethodDef) -> bool {
    old.kind == new.kind
  }

  fn compute(old: &MethodDef, new: &MethodDef) -> Option<Self> {
    InterfaceMethodDiff::diff(old, new)
  }

  fn delta(&self) -> f64 {
    self.change_percentage()
  }

  fn with_rename(diff: Option<Self>, old: &MethodDef, new: &MethodDef) -> Self {
    let mut d = diff.unwrap_or_else(|| InterfaceMethodDiff {
      name: new.name.clone(),
      name_change: None,
      optional_change: None,
      params_change: None,
      return_type_change: None,
      type_params_change: None,
      js_doc_change: None,
    });
    d.name = new.name.clone();
    d.name_change = Some(Change::new(old.name.clone(), new.name.clone()));
    d
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
    let old_map = old
      .iter()
      .map(|p| (&*p.name, p))
      .collect::<IndexMap<_, _>>();
    let new_map = new
      .iter()
      .map(|p| (&*p.name, p))
      .collect::<IndexMap<_, _>>();

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

    detect_renames::<InterfacePropertyDiff>(
      &mut added,
      &mut removed,
      &mut modified,
    );

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
  pub name_change: Option<Change<String>>,
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
      name_change: None,
      readonly_change,
      optional_change,
      type_change,
      params_change,
      type_params_change,
      js_doc_change,
    })
  }

  pub fn change_percentage(&self) -> f64 {
    let changed = self.readonly_change.is_some() as usize
      + self.optional_change.is_some() as usize
      + self.type_change.is_some() as usize
      + self.params_change.is_some() as usize
      + self.type_params_change.is_some() as usize;
    changed as f64 / 5.0
  }
}

impl RenameCandidate for InterfacePropertyDiff {
  type Item = PropertyDef;

  fn is_candidate(_old: &PropertyDef, _new: &PropertyDef) -> bool {
    true
  }

  fn compute(old: &PropertyDef, new: &PropertyDef) -> Option<Self> {
    InterfacePropertyDiff::diff(old, new)
  }

  fn delta(&self) -> f64 {
    self.change_percentage()
  }

  fn with_rename(
    diff: Option<Self>,
    old: &PropertyDef,
    new: &PropertyDef,
  ) -> Self {
    let mut d = diff.unwrap_or_else(|| InterfacePropertyDiff {
      name: new.name.clone(),
      name_change: None,
      readonly_change: None,
      optional_change: None,
      type_change: None,
      params_change: None,
      type_params_change: None,
      js_doc_change: None,
    });
    d.name = new.name.clone();
    d.name_change = Some(Change::new(old.name.clone(), new.name.clone()));
    d
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
          if let Some(diff) = CallSignatureDiff::diff(i, old_sig, new_sig) {
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
  pub index: usize,
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
  pub fn diff(
    index: usize,
    old: &CallSignatureDef,
    new: &CallSignatureDef,
  ) -> Option<Self> {
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
      index,
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
            InterfaceIndexSignatureDiff::diff(i, old_sig, new_sig)
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
  pub index: usize,
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
    index: usize,
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
      index,
      readonly_change,
      params_change,
      type_change,
      js_doc_change,
    })
  }
}
