// Copyright 2020-2021 the Deno authors. All rights reserved. MIT license.

use regex::Regex;
use serde::Deserialize;
use serde::Serialize;

lazy_static! {
  static ref JS_DOC_TAG_RE: Regex = Regex::new(r#"^\s*@\S+"#).unwrap();
  static ref JS_DOC_TAG_PARAM_RE: Regex = Regex::new(
    r#"(?s)^\s*@(?:param|arg|argument)(?:\s+\{([^}]+)\})?\s+([a-zA-Z_$]\S*)(?:\s+(.+))?"#
  )
  .unwrap();
  static ref JS_DOC_TAG_RETURN_RE: Regex = Regex::new(r#"(?s)^\s*@returns?(?:\s+\{([^}]+)\})?(?:\s+(.+))?"#).unwrap();
}

#[derive(Debug, Default, Clone, Deserialize, Serialize)]
pub struct JsDoc {
  #[serde(skip_serializing_if = "Option::is_none")]
  pub doc: Option<String>,
  #[serde(skip_serializing_if = "Vec::is_empty")]
  pub tags: Vec<JsDocTag>,
}

impl JsDoc {
  pub fn is_empty(&self) -> bool {
    self.doc.is_none() && self.tags.is_empty()
  }
}

impl From<String> for JsDoc {
  fn from(value: String) -> Self {
    let mut tags = Vec::new();
    let mut doc_lines = Vec::new();
    let mut is_tag = false;
    let mut current_tag: Vec<&str> = Vec::new();
    for line in value.lines() {
      let is_match = JS_DOC_TAG_RE.is_match(line);
      if is_tag || is_match {
        if !is_tag {
          is_tag = true;
          assert!(current_tag.is_empty());
        }
        if is_match && !current_tag.is_empty() {
          tags.push(current_tag.join("\n").into());
          current_tag.clear();
        }
        current_tag.push(line.trim());
      } else {
        doc_lines.push(line);
      }
    }
    if !current_tag.is_empty() {
      tags.push(current_tag.join("\n").into());
    }
    let doc = if doc_lines.is_empty() {
      None
    } else {
      Some(doc_lines.join("\n"))
    };
    Self { doc, tags }
  }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "lowercase")]
pub enum JsDocTag {
  /// `@callback Predicate comment`
  Callback {
    name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    doc: Option<String>,
  },
  /// `@constructor comment` or `@class comment`
  Constructor {
    #[serde(skip_serializing_if = "Option::is_none")]
    doc: Option<String>,
  },
  /// `@deprecated comment`
  Deprecated {
    #[serde(skip_serializing_if = "Option::is_none")]
    doc: Option<String>,
  },
  /// `@enum {type} comment`
  Enum {
    #[serde(rename = "type")]
    type_ref: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    doc: Option<String>,
  },
  /// `@extends {type} comment`
  Extends {
    #[serde(rename = "type")]
    type_ref: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    doc: Option<String>,
  },
  /// `@param {type} name comment` or `@arg {type} name comment` or
  /// `@argument {type} name comment`
  Param {
    name: String,
    #[serde(rename = "type", skip_serializing_if = "Option::is_none")]
    type_ref: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    doc: Option<String>,
  },
  /// `@public`
  Public,
  /// `@private`
  Private,
  /// `@property {type} name comment` or `@prop {type} name comment`
  Property {
    name: String,
    #[serde(rename = "type")]
    type_ref: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    doc: Option<String>,
  },
  /// `@protected`
  Protected,
  /// `@readonly`
  ReadOnly,
  /// `@return {type} comment` or `@returns {type} comment`
  Return {
    #[serde(rename = "type", skip_serializing_if = "Option::is_none")]
    type_ref: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    doc: Option<String>,
  },
  /// `@template T comment`
  Template {
    name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    doc: Option<String>,
  },
  /// `@this {type} comment`
  This {
    #[serde(rename = "type")]
    type_ref: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    doc: Option<String>,
  },
  /// `@typedef {type} name comment`
  TypeDef {
    name: String,
    #[serde(rename = "type")]
    type_ref: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    doc: Option<String>,
  },
  /// `@type {type} comment`
  #[serde(rename = "type")]
  TypeRef {
    #[serde(rename = "type")]
    type_ref: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    doc: Option<String>,
  },
  Unsupported {
    value: String,
  },
}

impl From<String> for JsDocTag {
  fn from(value: String) -> Self {
    if let Some(caps) = JS_DOC_TAG_PARAM_RE.captures(&value) {
      let name = caps.get(2).unwrap().as_str().to_string();
      let type_ref = caps.get(1).map(|m| m.as_str().to_string());
      let doc = caps.get(3).map(|m| m.as_str().to_string());
      Self::Param {
        name,
        type_ref,
        doc,
      }
    } else if let Some(caps) = JS_DOC_TAG_RETURN_RE.captures(&value) {
      let type_ref = caps.get(1).map(|m| m.as_str().to_string());
      let doc = caps.get(2).map(|m| m.as_str().to_string());
      Self::Return { type_ref, doc }
    } else {
      Self::Unsupported { value }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_js_doc_from_str() {
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "Line 1
Line 2

@param {string} a comment
@param {string} b comment
multi-line
@returns {Promise<T>} nothing
"
        .to_string()
      ))
      .unwrap(),
      json!({
        "doc": "Line 1\nLine 2\n",
        "tags": [
          {
            "kind": "param",
            "name": "a",
            "type": "string",
            "doc": "comment",
          },
          {
            "kind": "param",
            "name": "b",
            "type": "string",
            "doc": "comment\nmulti-line",
          },
          {
            "kind": "return",
            "type": "Promise<T>",
            "doc": "nothing"
          }
        ]
      })
    );
  }

  #[test]
  fn test_js_doc_tag_serialization() {
    assert_eq!(
      serde_json::to_value(JsDocTag::Callback {
        name: "Predicate".to_string(),
        doc: None,
      })
      .unwrap(),
      json!({
        "kind": "callback",
        "name": "Predicate",
      })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::Constructor { doc: None }).unwrap(),
      json!({
        "kind": "constructor",
      })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::Deprecated {
        doc: Some("comment".to_string()),
      })
      .unwrap(),
      json!({
        "kind": "deprecated",
        "doc": "comment",
      })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::Enum {
        type_ref: "number".to_string(),
        doc: None,
      })
      .unwrap(),
      json!({
        "kind": "enum",
        "type": "number",
      })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::Extends {
        type_ref: "OtherType<T>".to_string(),
        doc: None,
      })
      .unwrap(),
      json!({
        "kind": "extends",
        "type": "OtherType<T>",
      })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::Param {
        name: "arg".to_string(),
        type_ref: Some("number".to_string()),
        doc: Some("comment".to_string()),
      })
      .unwrap(),
      json!({
        "kind": "param",
        "name": "arg",
        "type": "number",
        "doc": "comment",
      })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::Param {
        name: "arg".to_string(),
        type_ref: None,
        doc: Some("comment".to_string()),
      })
      .unwrap(),
      json!({
        "kind": "param",
        "name": "arg",
        "doc": "comment",
      })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::Public).unwrap(),
      json!({ "kind": "public" })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::Private).unwrap(),
      json!({ "kind": "private" })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::Property {
        name: "prop".to_string(),
        type_ref: "string".to_string(),
        doc: None,
      })
      .unwrap(),
      json!({
        "kind": "property",
        "name": "prop",
        "type": "string",
      })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::Protected).unwrap(),
      json!({ "kind": "protected" })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::ReadOnly).unwrap(),
      json!({ "kind": "readonly" })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::Return {
        type_ref: Some("string".to_string()),
        doc: Some("comment".to_string()),
      })
      .unwrap(),
      json!({
        "kind": "return",
        "type": "string",
        "doc": "comment",
      })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::Template {
        name: "T".to_string(),
        doc: None,
      })
      .unwrap(),
      json!({
        "kind": "template",
        "name": "T",
      })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::This {
        type_ref: "Record<string, unknown>".to_string(),
        doc: None,
      })
      .unwrap(),
      json!({
        "kind": "this",
        "type": "Record<string, unknown>",
      })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::TypeDef {
        name: "Interface".to_string(),
        type_ref: "object".to_string(),
        doc: None,
      })
      .unwrap(),
      json!({
        "kind": "typedef",
        "name": "Interface",
        "type": "object",
      })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::TypeRef {
        type_ref: "Map<string, string>".to_string(),
        doc: None,
      })
      .unwrap(),
      json!({
        "kind": "type",
        "type": "Map<string, string>",
      })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::Unsupported {
        value: "unsupported".to_string()
      })
      .unwrap(),
      json!({
        "kind": "unsupported",
        "value": "unsupported",
      })
    );
  }
}
