// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use super::js_doc::JsDocDiff;
use super::ts_type::TsTypeDiff;
use crate::r#enum::EnumDef;
use crate::r#enum::EnumMemberDef;
use serde::Deserialize;
use serde::Serialize;
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EnumDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added_members: Vec<EnumMemberDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed_members: Vec<EnumMemberDef>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub modified_members: Vec<EnumMemberDiff>,
}

impl EnumDiff {
  pub fn diff(old: &EnumDef, new: &EnumDef) -> Option<Self> {
    let EnumDef {
      members: old_members,
    } = old;
    let EnumDef {
      members: new_members,
    } = new;

    let old_map = old_members
      .iter()
      .map(|m| (m.name.as_str(), m))
      .collect::<HashMap<_, _>>();
    let new_map = new_members
      .iter()
      .map(|m| (m.name.as_str(), m))
      .collect::<HashMap<_, _>>();

    let mut added_members = Vec::new();
    let mut removed_members = Vec::new();
    let mut modified_members = Vec::new();

    for (name, member) in &new_map {
      if !old_map.contains_key(name) {
        added_members.push((*member).clone());
      }
    }

    for (name, member) in &old_map {
      if !new_map.contains_key(name) {
        removed_members.push((*member).clone());
      }
    }

    for (name, old_member) in &old_map {
      if let Some(new_member) = new_map.get(name)
        && let Some(diff) = EnumMemberDiff::diff(old_member, new_member)
      {
        modified_members.push(diff);
      }
    }

    if added_members.is_empty()
      && removed_members.is_empty()
      && modified_members.is_empty()
    {
      return None;
    }

    Some(EnumDiff {
      added_members,
      removed_members,
      modified_members,
    })
  }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EnumMemberDiff {
  pub name: String,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub init_change: Option<TsTypeDiff>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub js_doc_change: Option<JsDocDiff>,
}

impl EnumMemberDiff {
  pub fn diff(old: &EnumMemberDef, new: &EnumMemberDef) -> Option<Self> {
    let EnumMemberDef {
      name: old_name,
      init: old_init,
      js_doc: old_js_doc,
      location: _, // internal, not diffed
    } = old;
    let EnumMemberDef {
      name: _,
      init: new_init,
      js_doc: new_js_doc,
      location: _,
    } = new;

    let init_change = match (old_init, new_init) {
      (Some(old_init), Some(new_init)) => TsTypeDiff::diff(old_init, new_init),
      (None, None) => None,
      (Some(old_init), None) => Some(TsTypeDiff {
        old: old_init.clone(),
        new: crate::ts_type::TsTypeDef::keyword("undefined"),
      }),
      (None, Some(new_init)) => Some(TsTypeDiff {
        old: crate::ts_type::TsTypeDef::keyword("undefined"),
        new: new_init.clone(),
      }),
    };

    let js_doc_change = JsDocDiff::diff(old_js_doc, new_js_doc);

    if init_change.is_none() && js_doc_change.is_none() {
      return None;
    }

    Some(EnumMemberDiff {
      name: old_name.clone(),
      init_change,
      js_doc_change,
    })
  }
}
