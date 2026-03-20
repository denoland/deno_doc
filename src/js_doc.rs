// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::sync::Arc;

use crate::ts_type::TsTypeDef;
use crate::ts_type::TsTypeDefKind;
use deno_ast::MediaType;
use deno_ast::ModuleSpecifier;
use deno_ast::ParseParams;
use deno_graph::symbols::EsModuleInfo;
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
  static ref JS_DOC_TAG_WITH_VALUE_RE: Regex = Regex::new(r"(?s)^\s*@(category|group|see|example|tags|since|priority|summary|description)(?:\s+(.+))").unwrap();
  /// @tag name maybe_value
  static ref JS_DOC_TAG_NAMED_WITH_MAYBE_VALUE_RE: Regex = Regex::new(r"(?s)^\s*@(callback|template|typeparam|typeParam)\s+([a-zA-Z_$]\S*)(?:\s+(?:-\s+)?(.+))?").unwrap();
  /// @tag {type} name maybe_value
  static ref JS_DOC_TAG_NAMED_TYPED_RE: Regex = Regex::new(r"(?s)^\s*@(prop(?:erty)?|typedef)\s+\{([^}]+)\}\s+([a-zA-Z_$]\S*)(?:\s+(?:-\s+)?(.+))?").unwrap();
  /// @tag {type} name maybe_value
  /// @tag {type} [name] maybe_value
  /// @tag {type} [name=default] maybe_value
  static ref JS_DOC_TAG_PARAM_RE: Regex = Regex::new(
    r"(?s)^\s*@(?:param|arg(?:ument)?)(?:\s+\{(?P<type>[^}]+)\})?\s+(?:(?:\[(?P<nameWithDefault>[a-zA-Z_$]\S*?)(?:\s*=\s*(?P<default>[^]]+))?\])|(?P<name>[a-zA-Z_$]\S*))(?:\s+(?:-\s+)?(?P<doc>.+))?"
  )
  .unwrap();
  /// @tag {maybe_type} maybe_value
  static ref JS_DOC_TAG_WITH_MAYBE_TYPE_AND_MAYBE_VALUE_RE: Regex = Regex::new(r"(?s)^\s*@(returns?|throws|exception)(?:\s+\{([^}]+)\})?(?:\s+(.+))?").unwrap();
  /// @tag {maybe_type} value
  static ref JS_DOC_TAG_WITH_TYPE_AND_MAYBE_VALUE_RE: Regex = Regex::new(r"(?s)^\s*@(enum|extends|augments|this|type|default)\s+\{([^}]+)\}(?:\s+(.+))?").unwrap();
}

fn make_ts_type(type_str: &str, module_info: &EsModuleInfo) -> TsTypeDef {
  if let Some(mut parsed) = parse_jsdoc_type(module_info, type_str) {
    parsed.repr = type_str.to_string();
    parsed
  } else {
    TsTypeDef {
      repr: type_str.to_string(),
      kind: TsTypeDefKind::Unsupported,
    }
  }
}

fn parse_jsdoc_type_source(type_str: &str) -> Option<deno_ast::ParsedSource> {
  let source = format!("type _temp = {type_str}");
  let specifier = ModuleSpecifier::parse("file:///jsdoc_type.ts").unwrap();
  let parsed = deno_ast::parse_module(ParseParams {
    specifier,
    text: Arc::from(source.as_str()),
    media_type: MediaType::TypeScript,
    capture_tokens: false,
    scope_analysis: false,
    maybe_syntax: None,
  })
  .ok()?;

  let program_ref = parsed.program_ref();
  let module = program_ref.unwrap_module();
  let type_alias = module.body.first()?;
  if !matches!(
    type_alias,
    deno_ast::swc::ast::ModuleItem::Stmt(deno_ast::swc::ast::Stmt::Decl(
      deno_ast::swc::ast::Decl::TsTypeAlias(_),
    ))
  ) {
    return None;
  }

  Some(parsed)
}

pub fn parse_jsdoc_type(
  module_info: &EsModuleInfo,
  type_str: &str,
) -> Option<TsTypeDef> {
  let parsed = parse_jsdoc_type_source(type_str)?;
  let program_ref = parsed.program_ref();
  let module = program_ref.unwrap_module();
  let type_alias = module.body.first()?;
  if let deno_ast::swc::ast::ModuleItem::Stmt(deno_ast::swc::ast::Stmt::Decl(
    deno_ast::swc::ast::Decl::TsTypeAlias(type_alias),
  )) = type_alias
  {
    Some(TsTypeDef::new(module_info, &type_alias.type_ann))
  } else {
    None
  }
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

  pub fn new(value: String, module_info: &EsModuleInfo) -> Self {
    let mut tags = Vec::new();
    let mut doc_lines: Option<String> = None;
    let mut is_tag = false;
    let mut is_codeblock = false;
    let mut tag_is_codeblock = false;
    let mut current_tag: Option<String> = None;
    let mut current_tag_name = "";
    let mut description_override: Option<String> = None;
    for line in value.lines() {
      let caps = if tag_is_codeblock || is_codeblock {
        None
      } else {
        JS_DOC_TAG_RE.captures(line)
      };
      if is_tag || caps.is_some() {
        if !is_tag {
          is_tag = true;
          assert!(current_tag.is_none());
        }
        if caps.is_some() {
          tag_is_codeblock = false;
          let current_tag = std::mem::take(&mut current_tag);
          if let Some(current_tag) = current_tag {
            if current_tag_name == "description" {
              if let Some(caps) =
                JS_DOC_TAG_WITH_VALUE_RE.captures(&current_tag)
                && let Some(m) = caps.get(2)
              {
                description_override = Some(m.as_str().to_string());
              }
            } else {
              tags.push(JsDocTag::new(current_tag, module_info));
            }
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
      if current_tag_name == "description" {
        if let Some(rest) = current_tag.strip_prefix("@description") {
          let desc = rest.trim_start();
          if !desc.is_empty() {
            description_override = Some(desc.to_string());
          }
        }
      } else {
        tags.push(JsDocTag::new(current_tag, module_info));
      }
    }
    let doc = if let Some(desc) = description_override {
      Some(desc.into_boxed_str())
    } else {
      doc_lines.map(|doc_lines| doc_lines.into_boxed_str())
    };
    Self {
      doc,
      tags: tags.into_boxed_slice(),
    }
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

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
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
  #[serde(rename_all = "camelCase")]
  Enum {
    ts_type: TsTypeDef,
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
  #[serde(rename_all = "camelCase")]
  Extends {
    ts_type: TsTypeDef,
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
  #[serde(rename_all = "camelCase")]
  Param {
    name: Box<str>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    ts_type: Option<TsTypeDef>,
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
  #[serde(rename_all = "camelCase")]
  Property {
    name: Box<str>,
    ts_type: TsTypeDef,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<Box<str>>,
  },
  /// `@protected`
  Protected,
  /// `@readonly`
  ReadOnly,
  /// `@return {type} comment` or `@returns {type} comment`
  #[serde(rename_all = "camelCase")]
  Return {
    #[serde(skip_serializing_if = "Option::is_none", default)]
    ts_type: Option<TsTypeDef>,
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
  #[serde(rename_all = "camelCase")]
  This {
    ts_type: TsTypeDef,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<Box<str>>,
  },
  /// `@throws {type} comment` or `@exception {type} comment`
  #[serde(rename_all = "camelCase")]
  Throws {
    #[serde(skip_serializing_if = "Option::is_none", default)]
    ts_type: Option<TsTypeDef>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<Box<str>>,
  },
  /// `@typedef {type} name comment`
  #[serde(rename_all = "camelCase")]
  TypeDef {
    name: Box<str>,
    ts_type: TsTypeDef,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<Box<str>>,
  },
  /// `@type {type} comment`
  #[serde(rename = "type", rename_all = "camelCase")]
  TypeRef {
    ts_type: TsTypeDef,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    doc: Option<Box<str>>,
  },
  /// `@see comment`
  See {
    doc: Box<str>,
  },
  /// `@summary comment`
  Summary {
    #[serde(default)]
    doc: Box<str>,
  },
  /// `@since version`
  Since {
    doc: Box<str>,
  },
  /// `@priority 1`
  Priority {
    priority: i32,
  },
  Unsupported {
    value: Box<str>,
  },
}

impl JsDocTag {
  pub fn new(value: String, module_info: &EsModuleInfo) -> Self {
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
    } else if let Some(caps) =
      JS_DOC_TAG_WITH_TYPE_AND_MAYBE_VALUE_RE.captures(&value)
    {
      let kind = caps.get(1).unwrap().as_str();
      let type_str = caps.get(2).unwrap().as_str();
      let doc = caps.get(3).map(|m| m.as_str().into());
      match kind {
        "enum" => Self::Enum {
          ts_type: make_ts_type(type_str, module_info),
          doc,
        },
        "extends" | "augments" => Self::Extends {
          ts_type: make_ts_type(type_str, module_info),
          doc,
        },
        "this" => Self::This {
          ts_type: make_ts_type(type_str, module_info),
          doc,
        },
        "type" => Self::TypeRef {
          ts_type: make_ts_type(type_str, module_info),
          doc,
        },
        "default" => Self::Default {
          value: type_str.into(),
          doc,
        },
        _ => unreachable!("kind unexpected: {}", kind),
      }
    } else if let Some(caps) = JS_DOC_TAG_NAMED_TYPED_RE.captures(&value) {
      let kind = caps.get(1).unwrap().as_str();
      let type_str = caps.get(2).unwrap().as_str();
      let name = caps.get(3).unwrap().as_str().into();
      let doc = caps.get(4).map(|m| m.as_str().into());
      match kind {
        "prop" | "property" => Self::Property {
          name,
          ts_type: make_ts_type(type_str, module_info),
          doc,
        },
        "typedef" => Self::TypeDef {
          name,
          ts_type: make_ts_type(type_str, module_info),
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
        "summary" => Self::Summary { doc },
        "priority" => {
          let Ok(priority) = doc.parse() else {
            return Self::Unsupported {
              value: value.into(),
            };
          };
          Self::Priority { priority }
        }
        "description" => unreachable!("@description is handled earlier"),
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
      let ts_type = caps
        .name("type")
        .map(|m| make_ts_type(m.as_str(), module_info));
      let default = caps.name("default").map(|m| m.as_str().into());
      let doc = caps.name("doc").map(|m| m.as_str().into());
      Self::Param {
        name,
        ts_type,
        optional: name_with_maybe_default.is_some() && default.is_none(),
        default,
        doc,
      }
    } else if let Some(caps) =
      JS_DOC_TAG_WITH_MAYBE_TYPE_AND_MAYBE_VALUE_RE.captures(&value)
    {
      let kind = caps.get(1).unwrap().as_str();
      let ts_type = caps.get(2).map(|m| make_ts_type(m.as_str(), module_info));
      let doc = caps.get(3).map(|m| m.as_str().into());
      match kind {
        "return" | "returns" => Self::Return { ts_type, doc },
        "throws" | "exception" => Self::Throws { ts_type, doc },
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

  fn parse_jsdoc(input: &str) -> JsDoc {
    let rt = tokio::runtime::Runtime::new().unwrap();
    rt.block_on(async {
      let specifier =
        deno_ast::ModuleSpecifier::parse("file:///test.ts").unwrap();
      let mut loader = deno_graph::source::MemoryLoader::default();
      loader.add_source_with_text(specifier.as_str(), "export {};");
      let analyzer = deno_graph::ast::CapturingModuleAnalyzer::default();
      let mut graph =
        deno_graph::ModuleGraph::new(deno_graph::GraphKind::TypesOnly);
      graph
        .build(
          vec![specifier.clone()],
          Vec::new(),
          &loader,
          deno_graph::BuildOptions {
            module_analyzer: &analyzer,
            ..Default::default()
          },
        )
        .await;
      let root_symbol = deno_graph::symbols::RootSymbol::new(&graph, &analyzer);
      let module_info = root_symbol.module_from_specifier(&specifier).unwrap();
      let esm = match module_info {
        deno_graph::symbols::ModuleInfoRef::Esm(esm) => esm,
        _ => panic!("expected esm module"),
      };
      JsDoc::new(input.to_string(), esm)
    })
  }

  fn ts_keyword(name: &str) -> serde_json::Value {
    serde_json::json!({
      "repr": name,
      "kind": "keyword",
      "value": name,
    })
  }

  fn ts_type_ref(
    name: &str,
    repr: &str,
    type_params: Option<Vec<serde_json::Value>>,
  ) -> serde_json::Value {
    let mut value = serde_json::json!({
      "typeName": name,
      "resolution": { "kind": "typeParam" },
    });
    if let Some(tp) = type_params {
      value["typeParams"] = serde_json::json!(tp);
    }
    serde_json::json!({
      "repr": repr,
      "kind": "typeRef",
      "value": value,
    })
  }

  #[test]
  fn test_js_doc_tag_only() {
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@constructor more")).unwrap(),
      serde_json::json!({ "tags": [ { "kind": "constructor" } ] }),
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@class more")).unwrap(),
      serde_json::json!({ "tags": [ { "kind": "constructor" } ] }),
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@experimental more")).unwrap(),
      serde_json::json!({ "tags": [ { "kind": "experimental" } ] }),
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@ignore more")).unwrap(),
      serde_json::json!({ "tags": [ { "kind": "ignore" } ] }),
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@public more")).unwrap(),
      serde_json::json!({ "tags": [ { "kind": "public" } ] }),
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@private more")).unwrap(),
      serde_json::json!({ "tags": [ { "kind": "private" } ] }),
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@protected more")).unwrap(),
      serde_json::json!({ "tags": [ { "kind": "protected" } ] }),
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@readonly more")).unwrap(),
      serde_json::json!({ "tags": [ { "kind": "readonly" } ] }),
    );
  }

  #[test]
  fn test_js_doc_preserves_leading_whitespace() {
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
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
      ))
      .unwrap(),
      serde_json::json!({
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
  fn test_js_doc_example_with_decorator() {
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        r#"
Some JSDoc goes here

@example Usage with decorators
```ts
const migrations = new MigrationRegistry();

@migrations.register()
class MyMigration {
  up() {}
  down() {}
}
```

@param a some param
"#
      ))
      .unwrap(),
      serde_json::json!({
        "doc": "\nSome JSDoc goes here\n",
        "tags": [
          {
            "kind": "example",
            "doc": "Usage with decorators\n```ts\nconst migrations = new MigrationRegistry();\n\n@migrations.register()\nclass MyMigration {\n  up() {}\n  down() {}\n}\n```\n"
          },
          {
            "kind": "param",
            "name": "a",
            "doc": "some param"
          }
        ]
      })
    );
  }

  #[test]
  fn test_js_doc_doc_codeblock_with_at_symbol() {
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        r#"
Some description

```ts
@decorator
class Foo {}
```

@param a some param
"#
      ))
      .unwrap(),
      serde_json::json!({
        "doc": "\nSome description\n\n```ts\n@decorator\nclass Foo {}\n```\n",
        "tags": [
          {
            "kind": "param",
            "name": "a",
            "doc": "some param"
          }
        ]
      })
    );
  }

  #[test]
  fn test_js_doc_tag_named() {
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        "@callback name more docs\n\nnew paragraph"
      ))
      .unwrap(),
      serde_json::json!({
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
      serde_json::to_value(parse_jsdoc(
        "@template T more docs\n\nnew paragraph"
      ))
      .unwrap(),
      serde_json::json!({
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
      serde_json::to_value(parse_jsdoc(
        "@default {true} more doc\n\nnew paragraph"
      ))
      .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "default",
          "value": "true",
          "doc": "more doc\n\nnew paragraph"
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        "@enum {string} more doc\n\nnew paragraph"
      ))
      .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "enum",
          "tsType": ts_keyword("string"),
          "doc": "more doc\n\nnew paragraph"
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        "@extends {string} more doc\n\nnew paragraph"
      ))
      .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "extends",
          "tsType": ts_keyword("string"),
          "doc": "more doc\n\nnew paragraph"
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        "@augments {string} more doc\n\nnew paragraph"
      ))
      .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "extends",
          "tsType": ts_keyword("string"),
          "doc": "more doc\n\nnew paragraph"
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        "@this {string} more doc\n\nnew paragraph"
      ))
      .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "this",
          "tsType": ts_keyword("string"),
          "doc": "more doc\n\nnew paragraph"
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        "@type {string} more doc\n\nnew paragraph"
      ))
      .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "type",
          "tsType": ts_keyword("string"),
          "doc": "more doc\n\nnew paragraph"
        }]
      })
    );
  }

  #[test]
  fn test_js_doc_tag_named_typed() {
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        "@prop {string} a more doc\n\nnew paragraph"
      ))
      .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "property",
          "name": "a",
          "tsType": ts_keyword("string"),
          "doc": "more doc\n\nnew paragraph"
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        "@property {string} a more doc\n\nnew paragraph"
      ))
      .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "property",
          "name": "a",
          "tsType": ts_keyword("string"),
          "doc": "more doc\n\nnew paragraph"
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        "@typedef {object} Interface more doc\n\nnew paragraph"
      ))
      .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "typedef",
          "name": "Interface",
          "tsType": ts_keyword("object"),
          "doc": "more doc\n\nnew paragraph"
        }]
      })
    );
  }

  #[test]
  fn test_js_doc_tag_maybe_doc() {
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@deprecated")).unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "deprecated",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        "@deprecated maybe doc\n\nnew paragraph"
      ))
      .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "deprecated",
          "doc": "maybe doc\n\nnew paragraph",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@module")).unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "module",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@module maybe doc\n\nnew paragraph"))
        .unwrap(),
      serde_json::json!({
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
      serde_json::to_value(parse_jsdoc("@category Functional Components"))
        .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "category",
          "doc": "Functional Components",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@group Functional Components"))
        .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "category",
          "doc": "Functional Components",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@example\n\nconst a = \"a\";\n"))
        .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "example",
          "doc": "const a = \"a\";"
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@tags allow-read, allow-write"))
        .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "tags",
          "tags": ["allow-read", "allow-write"],
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@see foo")).unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "see",
          "doc": "foo"
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@since 1.0.0")).unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "since",
          "doc": "1.0.0"
        }]
      })
    );

    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        r#"@tags allow-read, allow-write
@example some example
const a = "a";
@category foo
@see bar
@since 1.0.0
"#
      ))
      .unwrap(),
      serde_json::json!({
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
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@summary A brief summary")).unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "summary",
          "doc": "A brief summary",
        }]
      })
    );
  }

  #[test]
  fn test_js_doc_description_tag() {
    // @description overrides the doc field
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        "Normal doc text\n@description Override description"
      ))
      .unwrap(),
      serde_json::json!({
        "doc": "Override description",
      })
    );
    // @description without preceding doc text
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@description The description"))
        .unwrap(),
      serde_json::json!({
        "doc": "The description",
      })
    );
    // @description with other tags
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        "Normal doc\n@description Override\n@param {string} a a param"
      ))
      .unwrap(),
      serde_json::json!({
        "doc": "Override",
        "tags": [{
          "kind": "param",
          "name": "a",
          "tsType": ts_keyword("string"),
          "doc": "a param",
        }]
      })
    );
    // multi-line @description
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@description Line 1\nLine 2")).unwrap(),
      serde_json::json!({
        "doc": "Line 1\nLine 2",
      })
    );
  }

  #[test]
  fn test_js_doc_tag_param() {
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@param a maybe doc\n\nnew paragraph"))
        .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "param",
          "name": "a",
          "doc": "maybe doc\n\nnew paragraph",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        "@param {string} a maybe doc\n\nnew paragraph"
      ))
      .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "param",
          "name": "a",
          "tsType": ts_keyword("string"),
          "doc": "maybe doc\n\nnew paragraph",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@param {string} a")).unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "param",
          "name": "a",
          "tsType": ts_keyword("string"),
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc(r#"@param {string} [a="foo"]"#))
        .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "param",
          "name": "a",
          "tsType": ts_keyword("string"),
          "default": "\"foo\"",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc(r#"@param {string} [a]"#)).unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "param",
          "name": "a",
          "tsType": ts_keyword("string"),
          "optional": true,
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        "@arg {string} a maybe doc\n\nnew paragraph"
      ))
      .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "param",
          "name": "a",
          "tsType": ts_keyword("string"),
          "doc": "maybe doc\n\nnew paragraph",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        "@argument {string} a maybe doc\n\nnew paragraph"
      ))
      .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "param",
          "name": "a",
          "tsType": ts_keyword("string"),
          "doc": "maybe doc\n\nnew paragraph",
        }]
      })
    );
    // hyphen separator should be stripped
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@param foo - The foo")).unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "param",
          "name": "foo",
          "doc": "The foo",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@param {string} foo - The foo"))
        .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "param",
          "name": "foo",
          "tsType": ts_keyword("string"),
          "doc": "The foo",
        }]
      })
    );
  }

  #[test]
  fn test_js_doc_tag_returns() {
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        "@return {string} maybe doc\n\nnew paragraph"
      ))
      .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "return",
          "tsType": ts_keyword("string"),
          "doc": "maybe doc\n\nnew paragraph",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@return maybe doc\n\nnew paragraph"))
        .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "return",
          "doc": "maybe doc\n\nnew paragraph",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        "@returns {string} maybe doc\n\nnew paragraph"
      ))
      .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "return",
          "tsType": ts_keyword("string"),
          "doc": "maybe doc\n\nnew paragraph",
        }]
      })
    );
  }

  #[test]
  fn test_js_doc_tag_throws() {
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        "@throws {string} maybe doc\n\nnew paragraph"
      ))
      .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "throws",
          "tsType": ts_keyword("string"),
          "doc": "maybe doc\n\nnew paragraph",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc("@throws maybe doc\n\nnew paragraph"))
        .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "throws",
          "doc": "maybe doc\n\nnew paragraph",
        }]
      })
    );
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        "@throws {string} maybe doc\n\nnew paragraph"
      ))
      .unwrap(),
      serde_json::json!({
        "tags": [{
          "kind": "throws",
          "tsType": ts_keyword("string"),
          "doc": "maybe doc\n\nnew paragraph",
        }]
      })
    );
  }

  #[test]
  fn test_js_doc_from_str() {
    assert_eq!(
      serde_json::to_value(parse_jsdoc(
        "Line 1
Line 2

@param {string} a comment
@param {string} b comment
multi-line
@returns {Promise<T>} nothing
"
      ))
      .unwrap(),
      serde_json::json!({
        "doc": "Line 1\nLine 2\n",
        "tags": [
          {
            "kind": "param",
            "name": "a",
            "tsType": ts_keyword("string"),
            "doc": "comment",
          },
          {
            "kind": "param",
            "name": "b",
            "tsType": ts_keyword("string"),
            "doc": "comment\nmulti-line",
          },
          {
            "kind": "return",
            "tsType": ts_type_ref("Promise", "Promise<T>", Some(vec![
              ts_type_ref("T", "T", None)
            ])),
            "doc": "nothing"
          }
        ]
      })
    );
  }

  #[test]
  fn test_parse_jsdoc_type_source() {
    // valid types
    assert!(parse_jsdoc_type_source("string").is_some());
    assert!(parse_jsdoc_type_source("number").is_some());
    assert!(parse_jsdoc_type_source("boolean").is_some());
    assert!(parse_jsdoc_type_source("void").is_some());
    assert!(parse_jsdoc_type_source("any").is_some());
    assert!(parse_jsdoc_type_source("unknown").is_some());
    assert!(parse_jsdoc_type_source("never").is_some());
    assert!(parse_jsdoc_type_source("null").is_some());
    assert!(parse_jsdoc_type_source("undefined").is_some());
    assert!(parse_jsdoc_type_source("bigint").is_some());
    assert!(parse_jsdoc_type_source("symbol").is_some());
    assert!(parse_jsdoc_type_source("object").is_some());

    // type references
    assert!(parse_jsdoc_type_source("Promise<string>").is_some());
    assert!(parse_jsdoc_type_source("Map<string, Set<number>>").is_some());
    assert!(parse_jsdoc_type_source("Record<string, unknown>").is_some());
    assert!(
      parse_jsdoc_type_source("Promise<Map<string, Array<number>>>").is_some()
    );

    // unions and intersections
    assert!(parse_jsdoc_type_source("string | number").is_some());
    assert!(parse_jsdoc_type_source("A & B").is_some());
    assert!(parse_jsdoc_type_source("string | null | undefined").is_some());
    assert!(parse_jsdoc_type_source("string[] | number[]").is_some());

    // arrays, tuples
    assert!(parse_jsdoc_type_source("string[]").is_some());
    assert!(parse_jsdoc_type_source("[string, number]").is_some());
    assert!(parse_jsdoc_type_source("[string?]").is_some());
    assert!(parse_jsdoc_type_source("[...string[]]").is_some());

    // function types
    assert!(parse_jsdoc_type_source("(a: string) => void").is_some());
    assert!(parse_jsdoc_type_source("(x: string) => number | null").is_some());
    assert!(parse_jsdoc_type_source("new (x: number) => Foo").is_some());

    // object literals
    assert!(parse_jsdoc_type_source("{ foo: string }").is_some());
    assert!(
      parse_jsdoc_type_source("{ name: string; age: number; tags: string[] }")
        .is_some()
    );

    // type operators
    assert!(parse_jsdoc_type_source("keyof T").is_some());
    assert!(parse_jsdoc_type_source("readonly string[]").is_some());
    assert!(parse_jsdoc_type_source("unique symbol").is_some());

    // conditional, mapped, indexed access
    assert!(parse_jsdoc_type_source("T extends string ? T : never").is_some());
    assert!(
      parse_jsdoc_type_source("T extends Promise<infer U> ? U : never")
        .is_some()
    );
    assert!(parse_jsdoc_type_source("{ [K in keyof T]: T[K] }").is_some());
    assert!(parse_jsdoc_type_source("T[K]").is_some());

    // other valid types
    assert!(parse_jsdoc_type_source("this").is_some());
    assert!(parse_jsdoc_type_source("typeof Array").is_some());
    assert!(parse_jsdoc_type_source("`hello${string}`").is_some());
    assert!(parse_jsdoc_type_source("(string | number)").is_some());
    assert!(parse_jsdoc_type_source("import('foo').Bar").is_some());

    // literals
    assert!(parse_jsdoc_type_source("\"hello\"").is_some());
    assert!(parse_jsdoc_type_source("42").is_some());
    assert!(parse_jsdoc_type_source("true").is_some());
    assert!(parse_jsdoc_type_source("false").is_some());

    // invalid types
    assert!(parse_jsdoc_type_source("???invalid!!!").is_none());
    assert!(parse_jsdoc_type_source("").is_none());
    assert!(parse_jsdoc_type_source("   ").is_none());
    assert!(parse_jsdoc_type_source("@#$%").is_none());
    assert!(parse_jsdoc_type_source("if (true) {}").is_none());
    assert!(parse_jsdoc_type_source("class Foo {}").is_none());
    assert!(parse_jsdoc_type_source("x is string").is_none());
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
        ts_type: TsTypeDef {
          repr: "number".to_string(),
          kind: TsTypeDefKind::Unsupported
        },
        doc: None,
      })
      .unwrap(),
      json!({
        "kind": "enum",
        "tsType": { "repr": "number", "kind": "unsupported" },
      })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::Extends {
        ts_type: TsTypeDef {
          repr: "OtherType<T>".to_string(),
          kind: TsTypeDefKind::Unsupported
        },
        doc: None,
      })
      .unwrap(),
      json!({
        "kind": "extends",
        "tsType": { "repr": "OtherType<T>", "kind": "unsupported" },
      })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::Param {
        name: "arg".into(),
        ts_type: Some(TsTypeDef {
          repr: "number".to_string(),
          kind: TsTypeDefKind::Unsupported
        }),
        optional: false,
        default: Some("1".into()),
        doc: Some("comment".into()),
      })
      .unwrap(),
      json!({
        "kind": "param",
        "name": "arg",
        "tsType": { "repr": "number", "kind": "unsupported" },
        "default": "1",
        "doc": "comment",
      })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::Param {
        name: "arg".into(),
        ts_type: None,
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
        ts_type: TsTypeDef {
          repr: "string".to_string(),
          kind: TsTypeDefKind::Unsupported
        },
        doc: None,
      })
      .unwrap(),
      json!({
        "kind": "property",
        "name": "prop",
        "tsType": { "repr": "string", "kind": "unsupported" },
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
        ts_type: Some(TsTypeDef {
          repr: "string".to_string(),
          kind: TsTypeDefKind::Unsupported
        }),
        doc: Some("comment".into()),
      })
      .unwrap(),
      json!({
        "kind": "return",
        "tsType": { "repr": "string", "kind": "unsupported" },
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
        ts_type: TsTypeDef {
          repr: "Record<string, unknown>".to_string(),
          kind: TsTypeDefKind::Unsupported
        },
        doc: None,
      })
      .unwrap(),
      json!({
        "kind": "this",
        "tsType": { "repr": "Record<string, unknown>", "kind": "unsupported" },
      })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::TypeDef {
        name: "Interface".into(),
        ts_type: TsTypeDef {
          repr: "object".to_string(),
          kind: TsTypeDefKind::Unsupported
        },
        doc: None,
      })
      .unwrap(),
      json!({
        "kind": "typedef",
        "name": "Interface",
        "tsType": { "repr": "object", "kind": "unsupported" },
      })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::TypeRef {
        ts_type: TsTypeDef {
          repr: "Map<string, string>".to_string(),
          kind: TsTypeDefKind::Unsupported
        },
        doc: None,
      })
      .unwrap(),
      json!({
        "kind": "type",
        "tsType": { "repr": "Map<string, string>", "kind": "unsupported" },
      })
    );
    assert_eq!(
      serde_json::to_value(JsDocTag::Summary {
        doc: "A brief summary".into(),
      })
      .unwrap(),
      json!({
        "kind": "summary",
        "doc": "A brief summary",
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
