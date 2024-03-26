// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use regex::Regex;
use serde::Deserialize;
use serde::Serialize;

lazy_static! {
  static ref JS_DOC_TAG_MAYBE_DOC_RE: Regex = Regex::new(r"(?s)^\s*@(deprecated)(?:\s+(.+))?").unwrap();
  static ref JS_DOC_TAG_DOC_RE: Regex = Regex::new(r"(?s)^\s*@(category|see|example|tags)(?:\s+(.+))").unwrap();
  static ref JS_DOC_TAG_NAMED_RE: Regex = Regex::new(r"(?s)^\s*@(callback|template|typeparam|typeParam)\s+([a-zA-Z_$]\S*)(?:\s+(.+))?").unwrap();
  static ref JS_DOC_TAG_NAMED_TYPED_RE: Regex = Regex::new(r"(?s)^\s*@(prop(?:erty)?|typedef)\s+\{([^}]+)\}\s+([a-zA-Z_$]\S*)(?:\s+(.+))?").unwrap();
  static ref JS_DOC_TAG_ONLY_RE: Regex = Regex::new(r"^\s*@(constructor|class|ignore|module|public|private|protected|readonly)").unwrap();
  static ref JS_DOC_TAG_PARAM_RE: Regex = Regex::new(
    r"(?s)^\s*@(?:param|arg(?:ument)?)(?:\s+\{(?P<type>[^}]+)\})?\s+(?:(?:\[(?P<nameWithDefault>[a-zA-Z_$]\S*?)(?:\s*=\s*(?P<default>[^]]+))?\])|(?P<name>[a-zA-Z_$]\S*))(?:\s+(?P<doc>.+))?"
  )
  .unwrap();
  static ref JS_DOC_TAG_RE: Regex = Regex::new(r"(?s)^\s*@(\S+)").unwrap();
  static ref JS_DOC_TAG_RETURN_RE: Regex = Regex::new(r"(?s)^\s*@returns?(?:\s+\{([^}]+)\})?(?:\s+(.+))?").unwrap();
  static ref JS_DOC_TAG_TYPED_RE: Regex = Regex::new(r"(?s)^\s*@(enum|extends|augments|this|type|default)\s+\{([^}]+)\}(?:\s+(.+))?").unwrap();
}

#[derive(Debug, Default, Clone, Deserialize, Serialize)]
pub struct JsDoc {
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub doc: Option<String>,
  #[serde(skip_serializing_if = "Vec::is_empty", default)]
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
    let mut current_tag_name = "";
    for line in value.lines() {
      let caps = JS_DOC_TAG_RE.captures(line);
      if is_tag || caps.is_some() {
        if !is_tag {
          is_tag = true;
          assert!(current_tag.is_empty());
        }
        if caps.is_some() && !current_tag.is_empty() {
          tags.push(current_tag.join("\n").into());
          current_tag.clear();
        }
        if let Some(caps) = caps {
          current_tag_name = caps.get(1).unwrap().as_str();
        }
        // certain tags, we want to preserve any leading whitespace
        if matches!(current_tag_name, "example") {
          current_tag.push(line.trim_end());
        } else {
          current_tag.push(line.trim());
        }
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

#[derive(Debug, Clone, Deserialize, Serialize, Eq, PartialEq)]
#[serde(tag = "kind", rename_all = "lowercase")]
pub enum JsDocTag {
  /// `@callback Predicate comment`
  Callback {
    name: String,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<String>,
  },
  /// `@category comment`
  Category {
    #[serde(default)]
    doc: String,
  },
  /// `@constructor` or `@class`
  Constructor,
  /// `@default {value} comment`
  Default {
    value: String,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<String>,
  },
  /// `@deprecated comment`
  Deprecated {
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<String>,
  },
  /// `@enum {type} comment`
  Enum {
    #[serde(rename = "type")]
    type_ref: String,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<String>,
  },
  /// `@example comment`
  Example {
    #[serde(default)]
    doc: String,
  },
  /// `@extends {type} comment`
  Extends {
    #[serde(rename = "type")]
    type_ref: String,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<String>,
  },
  /// `@ignore`
  Ignore,
  /// `@module`
  Module,
  /// `@param`, `@arg` or `argument`, in format of `@param {type} name comment`
  /// or `@param {type} [name=default] comment`
  /// or `@param {type} [name] comment`
  Param {
    name: String,
    #[serde(rename = "type", skip_serializing_if = "Option::is_none", default)]
    type_ref: Option<String>,
    #[serde(skip_serializing_if = "core::ops::Not::not", default)]
    optional: bool,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    default: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<String>,
  },
  /// `@public`
  Public,
  /// `@private`
  Private,
  /// `@property {type} name comment` or `@prop {type} name comment`
  Property {
    name: String,
    #[serde(rename = "type", default)]
    type_ref: String,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<String>,
  },
  /// `@protected`
  Protected,
  /// `@readonly`
  ReadOnly,
  /// `@return {type} comment` or `@returns {type} comment`
  Return {
    #[serde(rename = "type", skip_serializing_if = "Option::is_none", default)]
    type_ref: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<String>,
  },
  /// `@tags allow-read, allow-write`
  Tags {
    tags: Vec<String>,
  },
  /// `@template T comment`
  /// `@typeparam T comment`
  /// `@typeParam T comment`
  Template {
    name: String,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<String>,
  },
  /// `@this {type} comment`
  This {
    #[serde(rename = "type")]
    type_ref: String,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<String>,
  },
  /// `@typedef {type} name comment`
  TypeDef {
    name: String,
    #[serde(rename = "type")]
    type_ref: String,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<String>,
  },
  /// `@type {type} comment`
  #[serde(rename = "type")]
  TypeRef {
    #[serde(rename = "type")]
    type_ref: String,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<String>,
  },
  /// `@see comment`
  See {
    doc: String,
  },
  Unsupported {
    value: String,
  },
}

impl From<String> for JsDocTag {
  fn from(value: String) -> Self {
    if let Some(caps) = JS_DOC_TAG_ONLY_RE.captures(&value) {
      let kind = caps.get(1).unwrap().as_str();
      match kind {
        "constructor" | "class" => Self::Constructor,
        "ignore" => Self::Ignore,
        "module" => Self::Module,
        "public" => Self::Public,
        "private" => Self::Private,
        "protected" => Self::Protected,
        "readonly" => Self::ReadOnly,
        _ => unreachable!("kind unexpected: {}", kind),
      }
    } else if let Some(caps) = JS_DOC_TAG_NAMED_RE.captures(&value) {
      let kind = caps.get(1).unwrap().as_str();
      let name = caps.get(2).unwrap().as_str().to_string();
      let doc = caps.get(3).map(|m| m.as_str().to_string());
      match kind {
        "callback" => Self::Callback { name, doc },
        "template" | "typeparam" | "typeParam" => Self::Template { name, doc },
        _ => unreachable!("kind unexpected: {}", kind),
      }
    } else if let Some(caps) = JS_DOC_TAG_TYPED_RE.captures(&value) {
      let kind = caps.get(1).unwrap().as_str();
      let type_ref = caps.get(2).unwrap().as_str().to_string();
      let doc = caps.get(3).map(|m| m.as_str().to_string());
      match kind {
        "enum" => Self::Enum { type_ref, doc },
        "extends" | "augments" => Self::Extends { type_ref, doc },
        "this" => Self::This { type_ref, doc },
        "type" => Self::TypeRef { type_ref, doc },
        "default" => Self::Default {
          value: type_ref,
          doc,
        },
        _ => unreachable!("kind unexpected: {}", kind),
      }
    } else if let Some(caps) = JS_DOC_TAG_NAMED_TYPED_RE.captures(&value) {
      let kind = caps.get(1).unwrap().as_str();
      let type_ref = caps.get(2).unwrap().as_str().to_string();
      let name = caps.get(3).unwrap().as_str().to_string();
      let doc = caps.get(4).map(|m| m.as_str().to_string());
      match kind {
        "prop" | "property" => Self::Property {
          name,
          type_ref,
          doc,
        },
        "typedef" => Self::TypeDef {
          name,
          type_ref,
          doc,
        },
        _ => unreachable!("kind unexpected: {}", kind),
      }
    } else if let Some(caps) = JS_DOC_TAG_MAYBE_DOC_RE.captures(&value) {
      let kind = caps.get(1).unwrap().as_str();
      let doc = caps.get(2).map(|m| m.as_str().to_string());
      match kind {
        "deprecated" => Self::Deprecated { doc },
        _ => unreachable!("kind unexpected: {}", kind),
      }
    } else if let Some(caps) = JS_DOC_TAG_DOC_RE.captures(&value) {
      let kind = caps.get(1).unwrap().as_str();
      let doc = caps.get(2).unwrap().as_str().to_string();
      match kind {
        "category" => Self::Category { doc },
        "example" => Self::Example { doc },
        "tags" => Self::Tags {
          tags: doc.split(',').map(|i| i.trim().to_string()).collect(),
        },
        "see" => Self::See { doc },
        _ => unreachable!("kind unexpected: {}", kind),
      }
    } else if let Some(caps) = JS_DOC_TAG_PARAM_RE.captures(&value) {
      let name_with_maybe_default = caps.name("nameWithDefault");
      let name = caps
        .name("name")
        .or(name_with_maybe_default)
        .unwrap()
        .as_str()
        .to_string();
      let type_ref = caps.name("type").map(|m| m.as_str().to_string());
      let default = caps.name("default").map(|m| m.as_str().to_string());
      let doc = caps.name("doc").map(|m| m.as_str().to_string());
      Self::Param {
        name,
        type_ref,
        optional: name_with_maybe_default.is_some() && default.is_none(),
        default,
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
  fn test_js_doc_tag_only() {
    assert_eq!(
      serde_json::to_value(JsDoc::from("@constructor more".to_string()))
        .unwrap(),
      json!({ "tags": [ { "kind": "constructor" } ] }),
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from("@class more".to_string())).unwrap(),
      json!({ "tags": [ { "kind": "constructor" } ] }),
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from("@ignore more".to_string())).unwrap(),
      json!({ "tags": [ { "kind": "ignore" } ] }),
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from("@module more".to_string())).unwrap(),
      json!({ "tags": [ { "kind": "module" } ] }),
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from("@public more".to_string())).unwrap(),
      json!({ "tags": [ { "kind": "public" } ] }),
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from("@private more".to_string())).unwrap(),
      json!({ "tags": [ { "kind": "private" } ] }),
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from("@protected more".to_string())).unwrap(),
      json!({ "tags": [ { "kind": "protected" } ] }),
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from("@readonly more".to_string())).unwrap(),
      json!({ "tags": [ { "kind": "readonly" } ] }),
    );
  }

  #[test]
  fn test_js_doc_preserves_leading_whitespace() {
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        r#"
Some JSDoc goes here

@example something like this

explain

```ts
if (true) {
  console.log("hello");
}
```

@param a an example of a multi-line
         indented comment
@returns nothing
"#
        .to_string()
      ))
      .unwrap(),
      json!({
        "doc": "\nSome JSDoc goes here\n",
        "tags": [
          {
            "kind": "example",
            "doc": "something like this\n\nexplain\n\n```ts\nif (true) {\n  console.log(\"hello\");\n}\n```\n"
          },
          {
            "kind": "param",
            "name": "a",
            "doc": "an example of a multi-line\nindented comment"
          },
          {
            "kind": "return",
            "doc": "nothing"
          }
        ]
      })
    );
  }

  #[test]
  fn test_js_doc_tag_named() {
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@callback name more docs\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [
          {
            "kind": "callback",
            "name": "name",
            "doc": "more docs\n\nnew paragraph",
          }
        ]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@template T more docs\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [
          {
            "kind": "template",
            "name": "T",
            "doc": "more docs\n\nnew paragraph",
          }
        ]
      })
    );
  }

  #[test]
  fn test_js_doc_tag_typed() {
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@default {true} more doc\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "default",
          "value": "true",
          "doc": "more doc\n\nnew paragraph"
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@enum {string} more doc\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "enum",
          "type": "string",
          "doc": "more doc\n\nnew paragraph"
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@extends {string} more doc\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "extends",
          "type": "string",
          "doc": "more doc\n\nnew paragraph"
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@augments {string} more doc\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "extends",
          "type": "string",
          "doc": "more doc\n\nnew paragraph"
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@this {string} more doc\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "this",
          "type": "string",
          "doc": "more doc\n\nnew paragraph"
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@type {string} more doc\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "type",
          "type": "string",
          "doc": "more doc\n\nnew paragraph"
        }]
      })
    );
  }

  #[test]
  fn test_js_doc_tag_named_typed() {
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@prop {string} a more doc\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "property",
          "name": "a",
          "type": "string",
          "doc": "more doc\n\nnew paragraph"
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@property {string} a more doc\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "property",
          "name": "a",
          "type": "string",
          "doc": "more doc\n\nnew paragraph"
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@typedef {object} Interface more doc\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "typedef",
          "name": "Interface",
          "type": "object",
          "doc": "more doc\n\nnew paragraph"
        }]
      })
    );
  }

  #[test]
  fn test_js_doc_tag_maybe_doc() {
    assert_eq!(
      serde_json::to_value(JsDoc::from("@deprecated".to_string())).unwrap(),
      json!({
        "tags": [{
          "kind": "deprecated",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@deprecated maybe doc\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "deprecated",
          "doc": "maybe doc\n\nnew paragraph",
        }]
      })
    );
  }

  #[test]
  fn test_js_doc_tag_doc() {
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@category Functional Components".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "category",
          "doc": "Functional Components",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@example\n\nconst a = \"a\";\n".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "example",
          "doc": "const a = \"a\";"
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@tags allow-read, allow-write".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "tags",
          "tags": ["allow-read", "allow-write"],
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from("@see foo".to_string())).unwrap(),
      json!({
        "tags": [{
          "kind": "see",
          "doc": "foo"
        }]
      })
    );

    assert_eq!(
      serde_json::to_value(JsDoc::from(
        r#"@tags allow-read, allow-write
@example some example
const a = "a";
@category foo
@see bar
"#
        .to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "tags",
          "tags": ["allow-read", "allow-write"]
        }, {
          "kind": "example",
          "doc": "some example\nconst a = \"a\";"
        }, {
          "kind": "category",
          "doc": "foo"
        }, {
          "kind": "see",
          "doc": "bar"
        }]

      })
    );
  }

  #[test]
  fn test_js_doc_tag_param() {
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@param a maybe doc\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "param",
          "name": "a",
          "doc": "maybe doc\n\nnew paragraph",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@param {string} a maybe doc\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "param",
          "name": "a",
          "type": "string",
          "doc": "maybe doc\n\nnew paragraph",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from("@param {string} a".to_string()))
        .unwrap(),
      json!({
        "tags": [{
          "kind": "param",
          "name": "a",
          "type": "string",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        r#"@param {string} [a="foo"]"#.to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "param",
          "name": "a",
          "type": "string",
          "default": "\"foo\"",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from(r#"@param {string} [a]"#.to_string()))
        .unwrap(),
      json!({
        "tags": [{
          "kind": "param",
          "name": "a",
          "type": "string",
          "optional": true,
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@arg {string} a maybe doc\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "param",
          "name": "a",
          "type": "string",
          "doc": "maybe doc\n\nnew paragraph",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@argument {string} a maybe doc\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "param",
          "name": "a",
          "type": "string",
          "doc": "maybe doc\n\nnew paragraph",
        }]
      })
    );
  }

  #[test]
  fn test_js_doc_tag_returns() {
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@return {string} maybe doc\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "return",
          "type": "string",
          "doc": "maybe doc\n\nnew paragraph",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@return maybe doc\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "return",
          "doc": "maybe doc\n\nnew paragraph",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@returns {string} maybe doc\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "return",
          "type": "string",
          "doc": "maybe doc\n\nnew paragraph",
        }]
      })
    );
  }

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
      serde_json::to_value(JsDocTag::Constructor).unwrap(),
      json!({
        "kind": "constructor",
      })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::Default {
        value: "true".to_string(),
        doc: None,
      })
      .unwrap(),
      json!({
        "kind": "default",
        "value": "true",
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
        optional: false,
        default: Some("1".to_string()),
        doc: Some("comment".to_string()),
      })
      .unwrap(),
      json!({
        "kind": "param",
        "name": "arg",
        "type": "number",
        "default": "1",
        "doc": "comment",
      })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::Param {
        name: "arg".to_string(),
        type_ref: None,
        optional: false,
        default: None,
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
