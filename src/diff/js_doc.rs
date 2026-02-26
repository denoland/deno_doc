// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use super::Change;
use crate::js_doc::JsDoc;
use crate::js_doc::JsDocTag;
use serde::Deserialize;
use serde::Serialize;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct JsDocDiff {
  #[serde(skip_serializing_if = "Option::is_none")]
  pub doc_change: Option<Change<Option<Box<str>>>>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub tags_change: Option<TagsDiff>,
}

impl JsDocDiff {
  pub fn diff(old: &JsDoc, new: &JsDoc) -> Option<Self> {
    let doc_change = if old.doc != new.doc {
      Some(Change::new(old.doc.clone(), new.doc.clone()))
    } else {
      None
    };

    let tags_change = TagsDiff::diff(&old.tags, &new.tags);

    if doc_change.is_none() && tags_change.is_none() {
      return None;
    }

    Some(JsDocDiff {
      doc_change,
      tags_change,
    })
  }
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
pub struct TagsDiff {
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub added: Vec<JsDocTag>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub removed: Vec<JsDocTag>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
  pub modified: Vec<TagDiff>,
}

impl TagsDiff {
  pub fn diff(old: &[JsDocTag], new: &[JsDocTag]) -> Option<Self> {
    let mut added = Vec::new();
    let mut removed = Vec::new();
    let mut modified = Vec::new();

    for new_tag in new.iter() {
      let matching_old = old.iter().find(|t| tags_same_kind(t, new_tag));
      match matching_old {
        Some(old_tag) if old_tag != new_tag => {
          modified.push(TagDiff {
            old: old_tag.clone(),
            new: new_tag.clone(),
          });
        }
        None => {
          added.push(new_tag.clone());
        }
        _ => {}
      }
    }

    for old_tag in old.iter() {
      let matching_new = new.iter().find(|t| tags_same_kind(t, old_tag));
      if matching_new.is_none() {
        removed.push(old_tag.clone());
      }
    }

    if added.is_empty() && removed.is_empty() && modified.is_empty() {
      return None;
    }

    Some(TagsDiff {
      added,
      removed,
      modified,
    })
  }
}

fn tags_same_kind(a: &JsDocTag, b: &JsDocTag) -> bool {
  use JsDocTag::*;

  match (a, b) {
    (Callback { name: n1, .. }, Callback { name: n2, .. }) => n1 == n2,
    (Category { .. }, Category { .. }) => true,
    (Constructor, Constructor) => true,
    (Default { value: v1, .. }, Default { value: v2, .. }) => v1 == v2,
    (Deprecated { .. }, Deprecated { .. }) => true,
    (Enum { type_ref: t1, .. }, Enum { type_ref: t2, .. }) => t1 == t2,
    (Example { .. }, Example { .. }) => true,
    (Experimental, Experimental) => true,
    (Extends { type_ref: t1, .. }, Extends { type_ref: t2, .. }) => t1 == t2,
    (Ignore, Ignore) => true,
    (Internal, Internal) => true,
    (Module { .. }, Module { .. }) => true,
    (Param { name: n1, .. }, Param { name: n2, .. }) => n1 == n2,
    (Public, Public) => true,
    (Private, Private) => true,
    (Property { name: n1, .. }, Property { name: n2, .. }) => n1 == n2,
    (Protected, Protected) => true,
    (ReadOnly, ReadOnly) => true,
    (Return { .. }, Return { .. }) => true,
    (Tags { .. }, Tags { .. }) => true,
    (Template { name: n1, .. }, Template { name: n2, .. }) => n1 == n2,
    (This { type_ref: t1, .. }, This { type_ref: t2, .. }) => t1 == t2,
    (Throws { type_ref: t1, .. }, Throws { type_ref: t2, .. }) => t1 == t2,
    (TypeDef { name: n1, .. }, TypeDef { name: n2, .. }) => n1 == n2,
    (TypeRef { type_ref: t1, .. }, TypeRef { type_ref: t2, .. }) => t1 == t2,
    (See { .. }, See { .. }) => true,
    (Since { .. }, Since { .. }) => true,
    (Priority { .. }, Priority { .. }) => true,
    (Unsupported { .. }, Unsupported { .. }) => true,
    _ => false,
  }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TagDiff {
  pub old: JsDocTag,
  pub new: JsDocTag,
}
