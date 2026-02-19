// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use super::Change;
use super::RENAME_THRESHOLD;
use super::function::DecoratorsDiff;
use super::function::FunctionDiff;
use super::function::ParamsDiff;
use super::js_doc::JsDocDiff;
use super::ts_type::TsTypeDiff;
use super::ts_type::TypeParamsDiff;
use crate::class::ClassConstructorDef;
use crate::class::ClassConstructorParamDef;
use crate::class::ClassDef;
use crate::class::ClassMethodDef;
use crate::class::ClassPropertyDef;
use crate::ts_type::IndexSignatureDef;
use crate::ts_type::TsTypeDef;
use deno_ast::swc::ast::Accessibility;
use indexmap::IndexMap;
use indexmap::IndexSet;
use serde::Deserialize;
use serde::Serialize;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ClassDiff {
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_abstract_change: Option<Change<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub extends_change: Option<Change<Option<Box<str>>>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub implements_change: Option<ImplementsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_params_change: Option<TypeParamsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub super_type_params_change: Option<SuperTypeParamsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub constructor_changes: Option<ConstructorsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub method_changes: Option<MethodsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub property_changes: Option<PropertiesDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub index_signature_changes: Option<IndexSignaturesDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub decorators_change: Option<DecoratorsDiff>,
}

impl ClassDiff {
  pub fn diff(old: &ClassDef, new: &ClassDef) -> Option<Self> {
    let ClassDef {
      def_name: _, // internal, not part of public API
      is_abstract: old_is_abstract,
      constructors: old_constructors,
      properties: old_properties,
      index_signatures: old_index_signatures,
      methods: old_methods,
      extends: old_extends,
      implements: old_implements,
      type_params: old_type_params,
      super_type_params: old_super_type_params,
      decorators: old_decorators,
    } = old;
    let ClassDef {
      def_name: _,
      is_abstract: new_is_abstract,
      constructors: new_constructors,
      properties: new_properties,
      index_signatures: new_index_signatures,
      methods: new_methods,
      extends: new_extends,
      implements: new_implements,
      type_params: new_type_params,
      super_type_params: new_super_type_params,
      decorators: new_decorators,
    } = new;

    let is_abstract_change = if old_is_abstract != new_is_abstract {
      Some(Change::new(*old_is_abstract, *new_is_abstract))
    } else {
      None
    };

    let extends_change = if old_extends != new_extends {
      Some(Change::new(old_extends.clone(), new_extends.clone()))
    } else {
      None
    };

    let implements_change =
      ImplementsDiff::diff(old_implements, new_implements);
    let type_params_change =
      TypeParamsDiff::diff(old_type_params, new_type_params);
    let super_type_params_change =
      SuperTypeParamsDiff::diff(old_super_type_params, new_super_type_params);
    let constructor_changes =
      ConstructorsDiff::diff(old_constructors, new_constructors);
    let method_changes = MethodsDiff::diff(old_methods, new_methods);
    let property_changes = PropertiesDiff::diff(old_properties, new_properties);
    let index_signature_changes =
      IndexSignaturesDiff::diff(old_index_signatures, new_index_signatures);
    let decorators_change =
      DecoratorsDiff::diff(old_decorators, new_decorators);

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

  pub fn change_percentage(&self, old: &ClassDef, new: &ClassDef) -> f64 {
    // Fields: is_abstract, extends, implements, type_params,
    // super_type_params, decorators = 6
    let field_total = 6;
    let field_changed = self.is_abstract_change.is_some() as usize
      + self.extends_change.is_some() as usize
      + self.implements_change.is_some() as usize
      + self.type_params_change.is_some() as usize
      + self.super_type_params_change.is_some() as usize
      + self.decorators_change.is_some() as usize;

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
      + index_sig_total;
    let changed = field_changed
      + constructor_changed
      + method_changed
      + property_changed
      + index_sig_changed;

    if total == 0 {
      return 0.0;
    }
    (changed as f64 / total as f64).min(1.0)
  }
}

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

    Some(ImplementsDiff { added, removed })
  }
}

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
    let mut added = Vec::new();
    let mut removed = Vec::new();
    let mut modified = Vec::new();

    // Group by parameter count
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

    for (param_count, new_ctors) in &new_by_param_count {
      match old_by_param_count.get(param_count) {
        Some(old_ctors) => {
          // Compare the first constructor of each (simple case)
          if let (Some(old_ctor), Some(new_ctor)) =
            (old_ctors.first(), new_ctors.first())
            && let Some(diff) = ConstructorDiff::diff(old_ctor, new_ctor)
          {
            modified.push(diff);
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

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ConstructorDiff {
  #[serde(skip_serializing_if = "Option::is_none")]
  pub accessibility_change: Option<Change<Option<Accessibility>>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_optional_change: Option<Change<bool>>,
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
    let ClassConstructorDef {
      js_doc: old_js_doc,
      accessibility: old_accessibility,
      is_optional: old_is_optional,
      has_body: _, // implementation detail, not diffed
      name: _,     // constructors identified by position, not name
      params: old_params,
      location: _, // internal, not diffed
    } = old;
    let ClassConstructorDef {
      js_doc: new_js_doc,
      accessibility: new_accessibility,
      is_optional: new_is_optional,
      has_body: _,
      name: _,
      params: new_params,
      location: _,
    } = new;

    let accessibility_change = if old_accessibility != new_accessibility {
      Some(Change::new(*old_accessibility, *new_accessibility))
    } else {
      None
    };

    let is_optional_change = if old_is_optional != new_is_optional {
      Some(Change::new(*old_is_optional, *new_is_optional))
    } else {
      None
    };

    let params_change = ConstructorParamsDiff::diff(old_params, new_params);
    let js_doc_change = JsDocDiff::diff(old_js_doc, new_js_doc);

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

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ConstructorParamDiff {
  pub index: usize,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub accessibility_change: Option<Change<Option<Accessibility>>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_override_change: Option<Change<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub readonly_change: Option<Change<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub param_change: Option<ParamsDiff>,
}

impl ConstructorParamDiff {
  pub fn diff(
    index: usize,
    old: &ClassConstructorParamDef,
    new: &ClassConstructorParamDef,
  ) -> Option<Self> {
    let ClassConstructorParamDef {
      accessibility: old_accessibility,
      is_override: old_is_override,
      param: old_param,
      readonly: old_readonly,
    } = old;
    let ClassConstructorParamDef {
      accessibility: new_accessibility,
      is_override: new_is_override,
      param: new_param,
      readonly: new_readonly,
    } = new;

    let accessibility_change = if old_accessibility != new_accessibility {
      Some(Change::new(*old_accessibility, *new_accessibility))
    } else {
      None
    };

    let is_override_change = if old_is_override != new_is_override {
      Some(Change::new(*old_is_override, *new_is_override))
    } else {
      None
    };

    let readonly_change = if old_readonly != new_readonly {
      Some(Change::new(*old_readonly, *new_readonly))
    } else {
      None
    };

    let param_change = ParamsDiff::diff(
      std::slice::from_ref(old_param),
      std::slice::from_ref(new_param),
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
    let old_map = old
      .iter()
      .map(|m| ((&*m.name, m.is_static, m.kind), m))
      .collect::<IndexMap<_, _>>();
    let new_map = new
      .iter()
      .map(|m| ((&*m.name, m.is_static, m.kind), m))
      .collect::<IndexMap<_, _>>();

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

    let mut matched_added = IndexSet::new();
    let mut matched_removed = IndexSet::new();

    for (r_idx, removed_method) in removed.iter().enumerate() {
      for (a_idx, added_method) in added.iter().enumerate() {
        if matched_added.contains(&a_idx) {
          continue;
        }

        if removed_method.is_static == added_method.is_static
          && removed_method.kind == added_method.kind
        {
          let method_diff = MethodDiff::diff(removed_method, added_method);
          let pct = method_diff.as_ref().map_or(0.0, |d| d.change_percentage());
          if pct <= RENAME_THRESHOLD {
            let mut diff = method_diff.unwrap_or_else(|| MethodDiff {
              name: added_method.name.clone(),
              name_change: None,
              accessibility_change: None,
              is_static_change: None,
              is_abstract_change: None,
              is_override_change: None,
              optional_change: None,
              function_diff: None,
              js_doc_change: None,
            });
            diff.name = added_method.name.clone();
            diff.name_change = Some(Change::new(
              removed_method.name.clone(),
              added_method.name.clone(),
            ));

            modified.push(diff);
            matched_added.insert(a_idx);
            matched_removed.insert(r_idx);
            break;
          }
        }
      }
    }

    let added = added
      .into_iter()
      .enumerate()
      .filter(|(i, _)| !matched_added.contains(i))
      .map(|(_, m)| m)
      .collect::<Vec<_>>();
    let removed = removed
      .into_iter()
      .enumerate()
      .filter(|(i, _)| !matched_removed.contains(i))
      .map(|(_, m)| m)
      .collect::<Vec<_>>();

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

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MethodDiff {
  pub name: Box<str>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub name_change: Option<Change<Box<str>>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub accessibility_change: Option<Change<Option<Accessibility>>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_static_change: Option<Change<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_abstract_change: Option<Change<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_override_change: Option<Change<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub optional_change: Option<Change<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub function_diff: Option<FunctionDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub js_doc_change: Option<JsDocDiff>,
}

impl MethodDiff {
  pub fn diff(old: &ClassMethodDef, new: &ClassMethodDef) -> Option<Self> {
    let ClassMethodDef {
      js_doc: old_js_doc,
      accessibility: old_accessibility,
      optional: old_optional,
      is_abstract: old_is_abstract,
      is_static: old_is_static,
      is_override: old_is_override,
      name: old_name,
      kind: _, // methods are matched by kind in the caller
      function_def: old_function_def,
      location: _, // internal, not diffed
    } = old;
    let ClassMethodDef {
      js_doc: new_js_doc,
      accessibility: new_accessibility,
      optional: new_optional,
      is_abstract: new_is_abstract,
      is_static: new_is_static,
      is_override: new_is_override,
      name: _,
      kind: _,
      function_def: new_function_def,
      location: _,
    } = new;

    let accessibility_change = if old_accessibility != new_accessibility {
      Some(Change::new(*old_accessibility, *new_accessibility))
    } else {
      None
    };

    let is_static_change = if old_is_static != new_is_static {
      Some(Change::new(*old_is_static, *new_is_static))
    } else {
      None
    };

    let is_abstract_change = if old_is_abstract != new_is_abstract {
      Some(Change::new(*old_is_abstract, *new_is_abstract))
    } else {
      None
    };

    let is_override_change = if old_is_override != new_is_override {
      Some(Change::new(*old_is_override, *new_is_override))
    } else {
      None
    };

    let optional_change = if old_optional != new_optional {
      Some(Change::new(*old_optional, *new_optional))
    } else {
      None
    };

    let function_diff = FunctionDiff::diff(old_function_def, new_function_def);
    let js_doc_change = JsDocDiff::diff(old_js_doc, new_js_doc);

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
      name: old_name.clone(),
      name_change: None,
      accessibility_change,
      is_static_change,
      is_abstract_change,
      is_override_change,
      optional_change,
      function_diff,
      js_doc_change,
    })
  }

  pub fn change_percentage(&self) -> f64 {
    let fn_pct = self
      .function_diff
      .as_ref()
      .map_or(0.0, |f| f.change_percentage());
    let field_changed = self.accessibility_change.is_some() as u8
      + self.is_static_change.is_some() as u8
      + self.is_abstract_change.is_some() as u8
      + self.is_override_change.is_some() as u8
      + self.optional_change.is_some() as u8;
    // Average of method-level field percentage and function percentage
    ((field_changed as f64 / 5.0) + fn_pct) / 2.0
  }
}

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
    let old_map = old
      .iter()
      .map(|p| ((&*p.name, p.is_static), p))
      .collect::<IndexMap<_, _>>();
    let new_map = new
      .iter()
      .map(|p| ((&*p.name, p.is_static), p))
      .collect::<IndexMap<_, _>>();

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

    let mut matched_added = IndexSet::new();
    let mut matched_removed = IndexSet::new();

    for (r_idx, removed_prop) in removed.iter().enumerate() {
      for (a_idx, added_prop) in added.iter().enumerate() {
        if matched_added.contains(&a_idx) {
          continue;
        }

        if removed_prop.is_static == added_prop.is_static {
          let prop_diff = PropertyDiff::diff(removed_prop, added_prop);
          let pct = prop_diff.as_ref().map_or(0.0, |d| d.change_percentage());
          if pct <= RENAME_THRESHOLD {
            let mut diff = prop_diff.unwrap_or_else(|| PropertyDiff {
              name: added_prop.name.clone(),
              name_change: None,
              accessibility_change: None,
              readonly_change: None,
              is_static_change: None,
              is_abstract_change: None,
              is_override_change: None,
              optional_change: None,
              type_change: None,
              decorators_change: None,
              js_doc_change: None,
            });
            diff.name = added_prop.name.clone();
            diff.name_change = Some(Change::new(
              removed_prop.name.clone(),
              added_prop.name.clone(),
            ));

            modified.push(diff);
            matched_added.insert(a_idx);
            matched_removed.insert(r_idx);
            break;
          }
        }
      }
    }

    let added = added
      .into_iter()
      .enumerate()
      .filter(|(i, _)| !matched_added.contains(i))
      .map(|(_, p)| p)
      .collect::<Vec<_>>();
    let removed = removed
      .into_iter()
      .enumerate()
      .filter(|(i, _)| !matched_removed.contains(i))
      .map(|(_, p)| p)
      .collect::<Vec<_>>();

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

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PropertyDiff {
  pub name: Box<str>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub name_change: Option<Change<Box<str>>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub accessibility_change: Option<Change<Option<Accessibility>>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub readonly_change: Option<Change<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_static_change: Option<Change<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_abstract_change: Option<Change<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub is_override_change: Option<Change<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub optional_change: Option<Change<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_change: Option<TsTypeDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub decorators_change: Option<DecoratorsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub js_doc_change: Option<JsDocDiff>,
}

impl PropertyDiff {
  pub fn diff(old: &ClassPropertyDef, new: &ClassPropertyDef) -> Option<Self> {
    let ClassPropertyDef {
      js_doc: old_js_doc,
      ts_type: old_ts_type,
      readonly: old_readonly,
      accessibility: old_accessibility,
      decorators: old_decorators,
      optional: old_optional,
      is_abstract: old_is_abstract,
      is_static: old_is_static,
      is_override: old_is_override,
      name: old_name,
      location: _, // internal, not diffed
    } = old;
    let ClassPropertyDef {
      js_doc: new_js_doc,
      ts_type: new_ts_type,
      readonly: new_readonly,
      accessibility: new_accessibility,
      decorators: new_decorators,
      optional: new_optional,
      is_abstract: new_is_abstract,
      is_static: new_is_static,
      is_override: new_is_override,
      name: _,
      location: _,
    } = new;

    let accessibility_change = if old_accessibility != new_accessibility {
      Some(Change::new(*old_accessibility, *new_accessibility))
    } else {
      None
    };

    let readonly_change = if old_readonly != new_readonly {
      Some(Change::new(*old_readonly, *new_readonly))
    } else {
      None
    };

    let is_static_change = if old_is_static != new_is_static {
      Some(Change::new(*old_is_static, *new_is_static))
    } else {
      None
    };

    let is_abstract_change = if old_is_abstract != new_is_abstract {
      Some(Change::new(*old_is_abstract, *new_is_abstract))
    } else {
      None
    };

    let is_override_change = if old_is_override != new_is_override {
      Some(Change::new(*old_is_override, *new_is_override))
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

    let js_doc_change = JsDocDiff::diff(old_js_doc, new_js_doc);
    let decorators_change =
      DecoratorsDiff::diff(old_decorators, new_decorators);

    if accessibility_change.is_none()
      && readonly_change.is_none()
      && is_static_change.is_none()
      && is_abstract_change.is_none()
      && is_override_change.is_none()
      && optional_change.is_none()
      && type_change.is_none()
      && js_doc_change.is_none()
      && decorators_change.is_none()
    {
      return None;
    }

    Some(PropertyDiff {
      name: old_name.clone(),
      name_change: None,
      accessibility_change,
      readonly_change,
      is_static_change,
      is_abstract_change,
      is_override_change,
      optional_change,
      type_change,
      decorators_change,
      js_doc_change,
    })
  }

  pub fn change_percentage(&self) -> f64 {
    let changed = self.accessibility_change.is_some() as usize
      + self.readonly_change.is_some() as usize
      + self.is_static_change.is_some() as usize
      + self.is_abstract_change.is_some() as usize
      + self.is_override_change.is_some() as usize
      + self.optional_change.is_some() as usize
      + self.type_change.is_some() as usize
      + self.decorators_change.is_some() as usize;
    changed as f64 / 8.0
  }
}

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

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct IndexSignatureDiff {
  #[serde(skip_serializing_if = "Option::is_none")]
  pub readonly_change: Option<Change<bool>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub params_change: Option<ParamsDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub type_change: Option<TsTypeDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub js_doc_change: Option<JsDocDiff>,
}

impl IndexSignatureDiff {
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

    Some(IndexSignatureDiff {
      readonly_change,
      params_change,
      type_change,
      js_doc_change,
    })
  }
}
