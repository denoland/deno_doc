// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use regex::Regex;
use serde::Deserialize;
use serde::Serialize;

lazy_static! {
  static ref JS_DOC_TAG_RE: Regex = Regex::new(r"(?s)^\s*@(\S+)").unwrap();
  /// @tag
  static ref JS_DOC_TAG_WITHOUT_VALUE_RE: Regex = Regex::new(r"^\s*@(constructor|class|ignore|internal|public|private|protected|readonly|experimental)").unwrap();
  /// @tag maybe_value
  static ref JS_DOC_TAG_WITH_MAYBE_VALUE_RE: Regex = Regex::new(r"(?s)^\s*@(deprecated|module)(?:\s+(.+))?").unwrap();
  /// @tag value
  static ref JS_DOC_TAG_WITH_VALUE_RE: Regex = Regex::new(r"(?s)^\s*@(category|group|see|example|tags|since)(?:\s+(.+))").unwrap();
  /// @tag name maybe_value
  static ref JS_DOC_TAG_NAMED_WITH_MAYBE_VALUE_RE: Regex = Regex::new(r"(?s)^\s*@(callback|template|typeparam|typeParam)\s+([a-zA-Z_$]\S*)(?:\s+(.+))?").unwrap();
  static ref JS_DOC_TAG_NAMED_TYPED_RE: Regex = Regex::new(r"(?s)^\s*@(prop(?:erty)?|typedef)\s+\{([^}]+)\}\s+([a-zA-Z_$]\S*)(?:\s+(.+))?").unwrap();
  static ref JS_DOC_TAG_PARAM_RE: Regex = Regex::new(
    r"(?s)^\s*@(?:param|arg(?:ument)?)(?:\s+\{(?P<type>[^}]+)\})?\s+(?:(?:\[(?P<nameWithDefault>[a-zA-Z_$]\S*?)(?:\s*=\s*(?P<default>[^]]+))?\])|(?P<name>[a-zA-Z_$]\S*))(?:\s+(?P<doc>.+))?"
  )
  .unwrap();
  static ref JS_DOC_TAG_OPTIONAL_TYPE_AND_DOC_RE: Regex = Regex::new(r"(?s)^\s*@(returns?|throws|exception)(?:\s+\{([^}]+)\})?(?:\s+(.+))?").unwrap();
  static ref JS_DOC_TAG_TYPED_RE: Regex = Regex::new(r"(?s)^\s*@(enum|extends|augments|this|type|default)\s+\{([^}]+)\}(?:\s+(.+))?").unwrap();
}

#[derive(Debug, Default, Clone, Deserialize, Serialize, PartialEq)]
pub struct JsDoc {
  #[serde(skip_serializing_if = "Option::is_none", default)]
  pub doc: Option<Box<str>>,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub tags: Box<[JsDocTag]>,
}

impl JsDoc {
  pub fn is_empty(&self) -> bool {
    self.doc.is_none() && self.tags.is_empty()
  }
}

fn handle_codeblock<'a>(
  line: &'a str,
  is_codeblock: &mut bool,
) -> Option<&'a str> {
  if *is_codeblock && line.starts_with("# ") {
    if line.contains("```") {
      *is_codeblock = !*is_codeblock;
      Some("```")
    } else {
      None
    }
  } else {
    if line.contains("```") {
      *is_codeblock = !*is_codeblock;
    }
    Some(line)
  }
}

impl From<String> for JsDoc {
  fn from(value: String) -> Self {
    let mut tags = Vec::new();
    let mut doc_lines: Option<String> = None;
    let mut is_tag = false;
    let mut is_codeblock = false;
    let mut tag_is_codeblock = false;
    let mut current_tag: Option<String> = None;
    let mut current_tag_name = "";
    for line in value.lines() {
      let caps = JS_DOC_TAG_RE.captures(line);
      if is_tag || caps.is_some() {
        if !is_tag {
          is_tag = true;
          assert!(current_tag.is_none());
        }
        if caps.is_some() {
          tag_is_codeblock = false;
          let current_tag = std::mem::take(&mut current_tag);
          if let Some(current_tag) = current_tag {
            tags.push(current_tag.into());
          }
        }
        if let Some(caps) = caps {
          current_tag_name = caps.get(1).unwrap().as_str();
        }
        if let Some(line) = handle_codeblock(line, &mut tag_is_codeblock) {
          let current_tag = if let Some(current_tag) = &mut current_tag {
            current_tag.push('\n');
            current_tag
          } else {
            current_tag = Some(String::new());
            current_tag.as_mut().unwrap()
          };

          // certain tags, we want to preserve any leading whitespace
          if matches!(current_tag_name, "example") {
            current_tag.push_str(line.trim_end());
          } else {
            current_tag.push_str(line.trim());
          }
        }
      } else if let Some(doc_lines) = &mut doc_lines {
        if let Some(line) = handle_codeblock(line, &mut is_codeblock) {
          doc_lines.push('\n');
          doc_lines.push_str(line);
        }
      } else {
        doc_lines = Some(String::new());
        doc_lines.as_mut().unwrap().push_str(line);
        if line.contains("```") {
          is_codeblock = !is_codeblock;
        }
      }
    }
    if let Some(current_tag) = current_tag {
      tags.push(current_tag.into());
    }
    let doc = doc_lines.map(|doc_lines| doc_lines.into_boxed_str());
    Self {
      doc,
      tags: tags.into_boxed_slice(),
    }
  }
}

#[derive(Debug, Clone, Deserialize, Serialize, Eq, PartialEq)]
#[serde(tag = "kind", rename_all = "lowercase")]
pub enum JsDocTag {
  /// `@callback Predicate comment`
  Callback {
    name: Box<str>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<Box<str>>,
  },
  /// `@category comment`
  /// `@group comment`
  Category {
    #[serde(default)]
    doc: Box<str>,
  },
  /// `@constructor` or `@class`
  Constructor,
  /// `@default {value} comment`
  Default {
    value: Box<str>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<Box<str>>,
  },
  /// `@deprecated comment`
  Deprecated {
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<Box<str>>,
  },
  /// `@enum {type} comment`
  Enum {
    #[serde(rename = "type")]
    type_ref: Box<str>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<Box<str>>,
  },
  /// `@example comment`
  Example {
    #[serde(default)]
    doc: Box<str>,
  },
  /// `@experimental`
  Experimental,
  /// `@extends {type} comment`
  Extends {
    #[serde(rename = "type")]
    type_ref: Box<str>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<Box<str>>,
  },
  /// `@ignore`
  Ignore,
  /// `@internal`
  Internal,
  /// `@module`
  /// `@module name`
  Module {
    #[serde(skip_serializing_if = "Option::is_none", default)]
    name: Option<Box<str>>,
  },
  /// `@param`, `@arg` or `argument`, in format of `@param {type} name comment`
  /// or `@param {type} [name=default] comment`
  /// or `@param {type} [name] comment`
  Param {
    name: Box<str>,
    #[serde(rename = "type", skip_serializing_if = "Option::is_none", default)]
    type_ref: Option<Box<str>>,
    #[serde(skip_serializing_if = "core::ops::Not::not", default)]
    optional: bool,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    default: Option<Box<str>>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<Box<str>>,
  },
  /// `@public`
  Public,
  /// `@private`
  Private,
  /// `@property {type} name comment` or `@prop {type} name comment`
  Property {
    name: Box<str>,
    #[serde(rename = "type", default)]
    type_ref: Box<str>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<Box<str>>,
  },
  /// `@protected`
  Protected,
  /// `@readonly`
  ReadOnly,
  /// `@return {type} comment` or `@returns {type} comment`
  Return {
    #[serde(rename = "type", skip_serializing_if = "Option::is_none", default)]
    type_ref: Option<Box<str>>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<Box<str>>,
  },
  /// `@tags allow-read, allow-write`
  Tags {
    tags: Box<[Box<str>]>,
  },
  /// `@template T comment`
  /// `@typeparam T comment`
  /// `@typeParam T comment`
  Template {
    name: Box<str>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<Box<str>>,
  },
  /// `@this {type} comment`
  This {
    #[serde(rename = "type")]
    type_ref: Box<str>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<Box<str>>,
  },
  /// `@throws {type} comment` or `@exception {type} comment`
  Throws {
    #[serde(rename = "type")]
    type_ref: Option<Box<str>>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<Box<str>>,
  },
  /// `@typedef {type} name comment`
  TypeDef {
    name: Box<str>,
    #[serde(rename = "type")]
    type_ref: Box<str>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<Box<str>>,
  },
  /// `@type {type} comment`
  #[serde(rename = "type")]
  TypeRef {
    #[serde(rename = "type")]
    type_ref: Box<str>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<Box<str>>,
  },
  /// `@see comment`
  See {
    doc: Box<str>,
  },
  /// `@since version`
  Since {
    doc: Box<str>,
  },
  Unsupported {
    value: Box<str>,
  },
}

impl From<String> for JsDocTag {
  fn from(value: String) -> Self {
    if let Some(caps) = JS_DOC_TAG_WITHOUT_VALUE_RE.captures(&value) {
      let kind = caps.get(1).unwrap().as_str();
      match kind {
        "constructor" | "class" => Self::Constructor,
        "experimental" => Self::Experimental,
        "ignore" => Self::Ignore,
        "internal" => Self::Internal,
        "public" => Self::Public,
        "private" => Self::Private,
        "protected" => Self::Protected,
        "readonly" => Self::ReadOnly,
        _ => unreachable!("kind unexpected: {}", kind),
      }
    } else if let Some(caps) =
      JS_DOC_TAG_NAMED_WITH_MAYBE_VALUE_RE.captures(&value)
    {
      let kind = caps.get(1).unwrap().as_str();
      let name = caps.get(2).unwrap().as_str().into();
      let doc = caps.get(3).map(|m| m.as_str().into());
      match kind {
        "callback" => Self::Callback { name, doc },
        "template" | "typeparam" | "typeParam" => Self::Template { name, doc },
        _ => unreachable!("kind unexpected: {}", kind),
      }
    } else if let Some(caps) = JS_DOC_TAG_TYPED_RE.captures(&value) {
      let kind = caps.get(1).unwrap().as_str();
      let type_ref = caps.get(2).unwrap().as_str().into();
      let doc = caps.get(3).map(|m| m.as_str().into());
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
      let type_ref = caps.get(2).unwrap().as_str().into();
      let name = caps.get(3).unwrap().as_str().into();
      let doc = caps.get(4).map(|m| m.as_str().into());
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
    } else if let Some(caps) = JS_DOC_TAG_WITH_MAYBE_VALUE_RE.captures(&value) {
      let kind = caps.get(1).unwrap().as_str();
      let doc = caps.get(2).map(|m| m.as_str().into());
      match kind {
        "deprecated" => Self::Deprecated { doc },
        "module" => Self::Module { name: doc },
        _ => unreachable!("kind unexpected: {}", kind),
      }
    } else if let Some(caps) = JS_DOC_TAG_WITH_VALUE_RE.captures(&value) {
      let kind = caps.get(1).unwrap().as_str();
      let doc = caps.get(2).unwrap().as_str().into();
      match kind {
        "category" | "group" => Self::Category { doc },
        "example" => Self::Example { doc },
        "tags" => Self::Tags {
          tags: doc.split(',').map(|i| i.trim().into()).collect(),
        },
        "see" => Self::See { doc },
        "since" => Self::Since { doc },
        _ => unreachable!("kind unexpected: {}", kind),
      }
    } else if let Some(caps) = JS_DOC_TAG_PARAM_RE.captures(&value) {
      let name_with_maybe_default = caps.name("nameWithDefault");
      let name = caps
        .name("name")
        .or(name_with_maybe_default)
        .unwrap()
        .as_str()
        .into();
      let type_ref = caps.name("type").map(|m| m.as_str().into());
      let default = caps.name("default").map(|m| m.as_str().into());
      let doc = caps.name("doc").map(|m| m.as_str().into());
      Self::Param {
        name,
        type_ref,
        optional: name_with_maybe_default.is_some() && default.is_none(),
        default,
        doc,
      }
    } else if let Some(caps) =
      JS_DOC_TAG_OPTIONAL_TYPE_AND_DOC_RE.captures(&value)
    {
      let kind = caps.get(1).unwrap().as_str();
      let type_ref = caps.get(2).map(|m| m.as_str().into());
      let doc = caps.get(3).map(|m| m.as_str().into());
      match kind {
        "return" | "returns" => Self::Return { type_ref, doc },
        "throws" | "exception" => Self::Throws { type_ref, doc },
        _ => unreachable!("kind unexpected: {}", kind),
      }
    } else {
      Self::Unsupported {
        value: value.into(),
      }
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
      serde_json::to_value(JsDoc::from("@experimental more".to_string()))
        .unwrap(),
      json!({ "tags": [ { "kind": "experimental" } ] }),
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from("@ignore more".to_string())).unwrap(),
      json!({ "tags": [ { "kind": "ignore" } ] }),
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
    assert_eq!(
      serde_json::to_value(JsDoc::from("@module".to_string())).unwrap(),
      json!({
        "tags": [{
          "kind": "module",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@module maybe doc\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "module",
          "name": "maybe doc\n\nnew paragraph",
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
        "@group Functional Components".to_string()
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
      serde_json::to_value(JsDoc::from("@since 1.0.0".to_string())).unwrap(),
      json!({
        "tags": [{
          "kind": "since",
          "doc": "1.0.0"
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
@since 1.0.0
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
        }, {
          "kind": "since",
          "doc": "1.0.0"
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
  fn test_js_doc_tag_throws() {
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@throws {string} maybe doc\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "throws",
          "type": "string",
          "doc": "maybe doc\n\nnew paragraph",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@throws maybe doc\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "throws",
          "type": null,
          "doc": "maybe doc\n\nnew paragraph",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(JsDoc::from(
        "@throws {string} maybe doc\n\nnew paragraph".to_string()
      ))
      .unwrap(),
      json!({
        "tags": [{
          "kind": "throws",
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
        name: "Predicate".into(),
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
        value: "true".into(),
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
        doc: Some("comment".into()),
      })
      .unwrap(),
      json!({
        "kind": "deprecated",
        "doc": "comment",
      })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::Enum {
        type_ref: "number".into(),
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
        type_ref: "OtherType<T>".into(),
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
        name: "arg".into(),
        type_ref: Some("number".into()),
        optional: false,
        default: Some("1".into()),
        doc: Some("comment".into()),
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
        name: "arg".into(),
        type_ref: None,
        optional: false,
        default: None,
        doc: Some("comment".into()),
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
        name: "prop".into(),
        type_ref: "string".into(),
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
        type_ref: Some("string".into()),
        doc: Some("comment".into()),
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
        name: "T".into(),
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
        type_ref: "Record<string, unknown>".into(),
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
        name: "Interface".into(),
        type_ref: "object".into(),
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
        type_ref: "Map<string, string>".into(),
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
        value: "unsupported".into()
      })
      .unwrap(),
      json!({
        "kind": "unsupported",
        "value": "unsupported",
      })
    );
  }
}
