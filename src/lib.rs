// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

#![recursion_limit = "256"]
#![deny(clippy::disallowed_methods)]
#![deny(clippy::disallowed_types)]
#![deny(clippy::print_stderr)]
#![deny(clippy::print_stdout)]

#[macro_use]
extern crate cfg_if;
#[macro_use]
extern crate lazy_static;
#[cfg(test)]
#[macro_use]
extern crate serde_json;

pub mod class;
mod decorators;
mod diagnostics;
pub mod diff;
mod display;
pub mod r#enum;
pub mod function;
pub mod html;
pub mod interface;
pub mod js_doc;
pub mod node;
mod params;
mod parser;
pub mod ts_type;
pub mod ts_type_param;
pub mod type_alias;
mod util;
pub mod variable;
mod visibility;

pub use node::Declaration;
pub use node::DeclarationDef;
pub use node::Document;
pub use node::Location;
pub use node::Symbol;

use params::ParamDef;

cfg_if! {
  if #[cfg(feature = "rust")] {
    mod printer;
    pub use diagnostics::DocDiagnostic;
    pub use diagnostics::DocDiagnosticKind;
    pub use printer::DocPrinter;
  }
}

pub use parser::DocError;
pub use parser::DocParser;
pub use parser::DocParserOptions;
pub use parser::ParseOutput;

#[cfg(test)]
mod tests;

#[cfg(feature = "rust")]
pub fn find_nodes_by_name_recursively(
  symbols: Vec<Symbol>,
  name: &str,
) -> Vec<Symbol> {
  let mut parts = name.splitn(2, '.');
  let name = parts.next();
  let leftover = parts.next();
  if name.is_none() {
    return symbols;
  }

  let name = name.unwrap();
  let symbol = symbols.into_iter().find(|symbol| &*symbol.name == name);

  let mut found: Vec<Symbol> = vec![];

  if let Some(symbol) = symbol {
    match leftover {
      Some(leftover) => {
        let children = get_children_of_node(symbol);
        found.extend(find_nodes_by_name_recursively(children, leftover));
      }
      None => found.push(symbol),
    }
  }

  found
}

#[cfg(feature = "rust")]
fn get_children_of_node(node: Symbol) -> Vec<Symbol> {
  use node::DeclarationDef;

  let mut doc_nodes: Vec<Symbol> = vec![];
  for decl in node.declarations {
    match decl.def {
      DeclarationDef::Namespace(namespace_def) => {
        doc_nodes.extend(
          namespace_def
            .elements
            .into_iter()
            .map(std::sync::Arc::unwrap_or_clone),
        );
      }
      DeclarationDef::Interface(interface_def) => {
        for method in interface_def.methods {
          doc_nodes.push(method.into());
        }
        for property in interface_def.properties {
          doc_nodes.push(property.into());
        }
      }
      DeclarationDef::Class(class_def) => {
        for method in class_def.methods.into_vec().into_iter() {
          doc_nodes.push(method.into());
        }
        for property in class_def.properties.into_vec().into_iter() {
          doc_nodes.push(property.into());
        }
      }
      _ => {}
    }
  }
  doc_nodes
}

pub fn docnodes_v1_to_v2(value: serde_json::Value) -> Document {
  let serde_json::Value::Array(arr) = value else {
    return Document::default();
  };

  // v1 format: flat array where each entry has "name", "kind", "location",
  // "declarationKind", "jsDoc", and def fields all at the top level.
  // v2 format: Document { module_doc, imports, symbols }
  // where symbols are Symbol { name, isDefault, declarations: [...] }.
  let mut module_doc = js_doc::JsDoc::default();
  let mut imports = Vec::new();
  let mut symbols: indexmap::IndexMap<Box<str>, Symbol> =
    indexmap::IndexMap::new();

  for item in arr {
    let serde_json::Value::Object(mut obj) = item else {
      continue;
    };

    let kind = obj
      .get("kind")
      .and_then(|v| v.as_str())
      .map(|s| s.to_string());

    // v1 moduleDoc nodes become Document.module_doc
    if kind.as_deref() == Some("moduleDoc") {
      if let Some(mut js_doc_val) = obj.remove("jsDoc") {
        migrate_js_doc_tags(&mut js_doc_val);
        if let Ok(js_doc) = serde_json::from_value::<js_doc::JsDoc>(js_doc_val)
        {
          module_doc = js_doc;
        }
      }
      continue;
    }

    // v1 import nodes become Document.imports
    if kind.as_deref() == Some("import") {
      let imported_name: Box<str> = obj
        .remove("name")
        .and_then(|v| v.as_str().map(|s| s.into()))
        .unwrap_or_else(|| "".into());
      let import_def = obj.remove("importDef").unwrap_or_default();
      let src = import_def
        .get("src")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();
      let original_name = import_def
        .get("imported")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());
      let js_doc = obj
        .remove("jsDoc")
        .and_then(|mut v| {
          migrate_js_doc_tags(&mut v);
          serde_json::from_value::<js_doc::JsDoc>(v).ok()
        })
        .unwrap_or_default();
      imports.push(node::Import {
        imported_name,
        original_name,
        src,
        js_doc,
      });
      continue;
    }

    let name: Box<str> = obj
      .remove("name")
      .and_then(|v| v.as_str().map(|s| s.into()))
      .unwrap_or_else(|| "".into());

    let is_default = obj
      .remove("isDefault")
      .and_then(|v| v.as_bool())
      .unwrap_or(false);

    // v1 had kind-specific def fields (e.g. "functionDef", "variableDef")
    // at the top level. v2 uses adjacently-tagged "def" for all kinds.
    for field in [
      "functionDef",
      "variableDef",
      "enumDef",
      "classDef",
      "typeAliasDef",
      "namespaceDef",
      "interfaceDef",
      "referenceDef",
    ] {
      if let Some(val) = obj.remove(field) {
        obj.insert("def".to_string(), val);
        break;
      }
    }

    // v1 TsTypeDef used variant-name content keys (e.g. "keyword": "string"),
    // v2 uses "value" as a uniform content key.
    // v1 JsDocTag used "type": "<string>" for type refs,
    // v2 uses "tsType": { TsTypeDef object }.
    let mut declaration = serde_json::Value::Object(obj);
    migrate_ts_type_defs(&mut declaration);
    migrate_js_doc_tags(&mut declaration);

    let symbol = symbols.entry(name.clone()).or_insert_with(|| Symbol {
      name,
      is_default,
      declarations: vec![],
    });
    // If any entry is marked default, the symbol is default
    if is_default {
      symbol.is_default = true;
    }
    if let Ok(decl) = serde_json::from_value::<Declaration>(declaration) {
      symbol.declarations.push(decl);
    }
  }

  Document {
    module_doc,
    imports,
    symbols: symbols.into_values().collect(),
  }
}

/// Recursively walk JSON and convert v1 TsTypeDef objects (which use
/// variant-name content keys like `"keyword": "string"`) to v2 format
/// (which uses `"value"` as the content key).
fn migrate_ts_type_defs(value: &mut serde_json::Value) {
  match value {
    serde_json::Value::Object(obj) => {
      // If this looks like a v1 TsTypeDef (has "repr" and "kind"), rename
      // the kind-named content key to "value".
      if obj.contains_key("repr")
        && let Some(kind_str) = obj
          .get("kind")
          .and_then(|k| k.as_str())
          .map(|s| s.to_string())
        && let Some(content) = obj.remove(&kind_str)
      {
        obj.insert("value".to_string(), content);
      }

      for val in obj.values_mut() {
        migrate_ts_type_defs(val);
      }
    }
    serde_json::Value::Array(arr) => {
      for val in arr.iter_mut() {
        migrate_ts_type_defs(val);
      }
    }
    _ => {}
  }
}

/// Recursively walk JSON and convert v1 JsDocTag objects that had
/// `"type": "<string>"` (the old `type_ref` field) to v2 format
/// with `"tsType": { "repr": "<string>", "kind": "unsupported" }`.
fn migrate_js_doc_tags(value: &mut serde_json::Value) {
  match value {
    serde_json::Value::Object(obj) => {
      // Check if this looks like a v1 JsDocTag with a "type" field that
      // should be migrated to "tsType". We distinguish from TsTypeDef
      // objects (which also have "kind") by checking that "repr" is absent.
      if !obj.contains_key("repr") {
        let dominated_kind =
          obj.get("kind").and_then(|k| k.as_str()).is_some_and(|s| {
            matches!(
              s,
              "enum"
                | "extends"
                | "param"
                | "property"
                | "return"
                | "this"
                | "throws"
                | "typedef"
                | "type"
            )
          });

        if dominated_kind && let Some(type_val) = obj.remove("type") {
          match type_val {
            serde_json::Value::String(s) => {
              obj.insert(
                "tsType".to_string(),
                serde_json::json!({
                  "repr": s,
                  "kind": "unsupported"
                }),
              );
            }
            serde_json::Value::Null => {
              // Optional type that was null - leave tsType absent
            }
            other => {
              // Unexpected value, preserve it
              obj.insert("type".to_string(), other);
            }
          }
        }
      }

      for val in obj.values_mut() {
        migrate_js_doc_tags(val);
      }
    }
    serde_json::Value::Array(arr) => {
      for val in arr.iter_mut() {
        migrate_js_doc_tags(val);
      }
    }
    _ => {}
  }
}

#[cfg(test)]
mod v1_to_v2_tests {
  use super::*;
  use serde_json::json;

  #[test]
  fn non_array_returns_default() {
    let doc = docnodes_v1_to_v2(json!({}));
    assert!(doc.symbols.is_empty());
    assert!(doc.imports.is_empty());
    assert!(doc.module_doc.is_empty());
  }

  #[test]
  fn empty_array() {
    let doc = docnodes_v1_to_v2(json!([]));
    assert!(doc.symbols.is_empty());
  }

  #[test]
  fn module_doc() {
    let doc = docnodes_v1_to_v2(json!([
      {
        "kind": "moduleDoc",
        "jsDoc": {
          "doc": "Module documentation"
        }
      }
    ]));
    assert_eq!(doc.module_doc.doc.as_deref(), Some("Module documentation"));
    assert!(doc.symbols.is_empty());
  }

  #[test]
  fn import_node() {
    let doc = docnodes_v1_to_v2(json!([
      {
        "kind": "import",
        "name": "Foo",
        "importDef": {
          "src": "./foo.ts",
          "imported": "Foo"
        }
      }
    ]));
    assert_eq!(doc.imports.len(), 1);
    assert_eq!(&*doc.imports[0].imported_name, "Foo");
    assert_eq!(doc.imports[0].src, "./foo.ts");
    assert_eq!(doc.imports[0].original_name.as_deref(), Some("Foo"));
  }

  #[test]
  fn variable_declaration() {
    let doc = docnodes_v1_to_v2(json!([
      {
        "name": "myVar",
        "kind": "variable",
        "location": { "filename": "test.ts", "line": 1, "col": 0, "byteIndex": 0 },
        "declarationKind": "export",
        "variableDef": {
          "kind": "const"
        }
      }
    ]));
    assert_eq!(doc.symbols.len(), 1);
    assert_eq!(&*doc.symbols[0].name, "myVar");
    assert_eq!(doc.symbols[0].declarations.len(), 1);
    assert!(matches!(
      doc.symbols[0].declarations[0].def,
      node::DeclarationDef::Variable(_)
    ));
  }

  #[test]
  fn function_declaration() {
    let doc = docnodes_v1_to_v2(json!([
      {
        "name": "myFunc",
        "kind": "function",
        "location": { "filename": "test.ts", "line": 1, "col": 0, "byteIndex": 0 },
        "declarationKind": "export",
        "functionDef": {
          "params": [],
          "returnType": {
            "repr": "string",
            "kind": "keyword",
            "keyword": "string"
          }
        }
      }
    ]));
    assert_eq!(doc.symbols.len(), 1);
    assert_eq!(&*doc.symbols[0].name, "myFunc");
    let decl = &doc.symbols[0].declarations[0];
    match &decl.def {
      node::DeclarationDef::Function(f) => {
        let rt = f.return_type.as_ref().unwrap();
        assert_eq!(rt.repr, "string");
      }
      other => panic!("expected Function, got {:?}", other),
    }
  }

  #[test]
  fn ts_type_def_migration() {
    // v1 uses kind-named content key, v2 uses "value"
    let doc = docnodes_v1_to_v2(json!([
      {
        "name": "x",
        "kind": "variable",
        "location": { "filename": "test.ts", "line": 1, "col": 0, "byteIndex": 0 },
        "declarationKind": "export",
        "variableDef": {
          "kind": "const",
          "tsType": {
            "repr": "string",
            "kind": "keyword",
            "keyword": "string"
          }
        }
      }
    ]));
    let decl = &doc.symbols[0].declarations[0];
    match &decl.def {
      node::DeclarationDef::Variable(v) => {
        let ts_type = v.ts_type.as_ref().unwrap();
        assert_eq!(ts_type.repr, "string");
      }
      other => panic!("expected Variable, got {:?}", other),
    }
  }

  #[test]
  fn is_default_flag() {
    let doc = docnodes_v1_to_v2(json!([
      {
        "name": "default",
        "isDefault": true,
        "kind": "variable",
        "location": { "filename": "test.ts", "line": 1, "col": 0, "byteIndex": 0 },
        "declarationKind": "export",
        "variableDef": { "kind": "const" }
      }
    ]));
    assert!(doc.symbols[0].is_default);
  }

  #[test]
  fn multiple_declarations_same_name() {
    let doc = docnodes_v1_to_v2(json!([
      {
        "name": "myFunc",
        "kind": "function",
        "location": { "filename": "test.ts", "line": 1, "col": 0, "byteIndex": 0 },
        "declarationKind": "export",
        "functionDef": { "params": [] }
      },
      {
        "name": "myFunc",
        "kind": "function",
        "location": { "filename": "test.ts", "line": 3, "col": 0, "byteIndex": 20 },
        "declarationKind": "export",
        "functionDef": { "params": [] }
      }
    ]));
    assert_eq!(doc.symbols.len(), 1);
    assert_eq!(doc.symbols[0].declarations.len(), 2);
  }

  #[test]
  fn jsdoc_tag_type_to_ts_type_param() {
    let doc = docnodes_v1_to_v2(json!([
      {
        "name": "myFunc",
        "kind": "function",
        "location": { "filename": "test.ts", "line": 1, "col": 0, "byteIndex": 0 },
        "declarationKind": "export",
        "jsDoc": {
          "tags": [
            { "kind": "param", "name": "x", "type": "string", "doc": "a param" }
          ]
        },
        "functionDef": { "params": [] }
      }
    ]));
    let decl = &doc.symbols[0].declarations[0];
    let tag = &decl.js_doc.tags[0];
    match tag {
      js_doc::JsDocTag::Param { ts_type, name, .. } => {
        assert_eq!(&**name, "x");
        let ts = ts_type.as_ref().unwrap();
        assert_eq!(ts.repr, "string");
      }
      other => panic!("expected Param, got {:?}", other),
    }
  }

  #[test]
  fn jsdoc_tag_type_to_ts_type_return() {
    let doc = docnodes_v1_to_v2(json!([
      {
        "name": "myFunc",
        "kind": "function",
        "location": { "filename": "test.ts", "line": 1, "col": 0, "byteIndex": 0 },
        "declarationKind": "export",
        "jsDoc": {
          "tags": [
            { "kind": "return", "type": "number" }
          ]
        },
        "functionDef": { "params": [] }
      }
    ]));
    let tag = &doc.symbols[0].declarations[0].js_doc.tags[0];
    match tag {
      js_doc::JsDocTag::Return { ts_type, .. } => {
        let ts = ts_type.as_ref().unwrap();
        assert_eq!(ts.repr, "number");
      }
      other => panic!("expected Return, got {:?}", other),
    }
  }

  #[test]
  fn jsdoc_tag_type_to_ts_type_enum() {
    let doc = docnodes_v1_to_v2(json!([
      {
        "name": "x",
        "kind": "variable",
        "location": { "filename": "test.ts", "line": 1, "col": 0, "byteIndex": 0 },
        "declarationKind": "export",
        "jsDoc": {
          "tags": [
            { "kind": "enum", "type": "number" }
          ]
        },
        "variableDef": { "kind": "const" }
      }
    ]));
    let tag = &doc.symbols[0].declarations[0].js_doc.tags[0];
    match tag {
      js_doc::JsDocTag::Enum { ts_type, .. } => {
        assert_eq!(ts_type.repr, "number");
      }
      other => panic!("expected Enum, got {:?}", other),
    }
  }

  #[test]
  fn jsdoc_tag_type_to_ts_type_extends() {
    let doc = docnodes_v1_to_v2(json!([
      {
        "name": "x",
        "kind": "variable",
        "location": { "filename": "test.ts", "line": 1, "col": 0, "byteIndex": 0 },
        "declarationKind": "export",
        "jsDoc": {
          "tags": [
            { "kind": "extends", "type": "Foo" }
          ]
        },
        "variableDef": { "kind": "const" }
      }
    ]));
    let tag = &doc.symbols[0].declarations[0].js_doc.tags[0];
    match tag {
      js_doc::JsDocTag::Extends { ts_type, .. } => {
        assert_eq!(ts_type.repr, "Foo");
      }
      other => panic!("expected Extends, got {:?}", other),
    }
  }

  #[test]
  fn jsdoc_tag_type_to_ts_type_this() {
    let doc = docnodes_v1_to_v2(json!([
      {
        "name": "x",
        "kind": "variable",
        "location": { "filename": "test.ts", "line": 1, "col": 0, "byteIndex": 0 },
        "declarationKind": "export",
        "jsDoc": {
          "tags": [
            { "kind": "this", "type": "Window" }
          ]
        },
        "variableDef": { "kind": "const" }
      }
    ]));
    let tag = &doc.symbols[0].declarations[0].js_doc.tags[0];
    match tag {
      js_doc::JsDocTag::This { ts_type, .. } => {
        assert_eq!(ts_type.repr, "Window");
      }
      other => panic!("expected This, got {:?}", other),
    }
  }

  #[test]
  fn jsdoc_tag_type_to_ts_type_throws() {
    let doc = docnodes_v1_to_v2(json!([
      {
        "name": "x",
        "kind": "function",
        "location": { "filename": "test.ts", "line": 1, "col": 0, "byteIndex": 0 },
        "declarationKind": "export",
        "jsDoc": {
          "tags": [
            { "kind": "throws", "type": "Error" }
          ]
        },
        "functionDef": { "params": [] }
      }
    ]));
    let tag = &doc.symbols[0].declarations[0].js_doc.tags[0];
    match tag {
      js_doc::JsDocTag::Throws { ts_type, .. } => {
        let ts = ts_type.as_ref().unwrap();
        assert_eq!(ts.repr, "Error");
      }
      other => panic!("expected Throws, got {:?}", other),
    }
  }

  #[test]
  fn jsdoc_tag_type_to_ts_type_typedef() {
    let doc = docnodes_v1_to_v2(json!([
      {
        "name": "x",
        "kind": "variable",
        "location": { "filename": "test.ts", "line": 1, "col": 0, "byteIndex": 0 },
        "declarationKind": "export",
        "jsDoc": {
          "tags": [
            { "kind": "typedef", "name": "MyType", "type": "object" }
          ]
        },
        "variableDef": { "kind": "const" }
      }
    ]));
    let tag = &doc.symbols[0].declarations[0].js_doc.tags[0];
    match tag {
      js_doc::JsDocTag::TypeDef { ts_type, name, .. } => {
        assert_eq!(&**name, "MyType");
        assert_eq!(ts_type.repr, "object");
      }
      other => panic!("expected TypeDef, got {:?}", other),
    }
  }

  #[test]
  fn jsdoc_tag_type_to_ts_type_typeref() {
    let doc = docnodes_v1_to_v2(json!([
      {
        "name": "x",
        "kind": "variable",
        "location": { "filename": "test.ts", "line": 1, "col": 0, "byteIndex": 0 },
        "declarationKind": "export",
        "jsDoc": {
          "tags": [
            { "kind": "type", "type": "Record<string, unknown>" }
          ]
        },
        "variableDef": { "kind": "const" }
      }
    ]));
    let tag = &doc.symbols[0].declarations[0].js_doc.tags[0];
    match tag {
      js_doc::JsDocTag::TypeRef { ts_type, .. } => {
        assert_eq!(ts_type.repr, "Record<string, unknown>");
      }
      other => panic!("expected TypeRef, got {:?}", other),
    }
  }

  #[test]
  fn jsdoc_tag_type_to_ts_type_property() {
    let doc = docnodes_v1_to_v2(json!([
      {
        "name": "x",
        "kind": "variable",
        "location": { "filename": "test.ts", "line": 1, "col": 0, "byteIndex": 0 },
        "declarationKind": "export",
        "jsDoc": {
          "tags": [
            { "kind": "property", "name": "foo", "type": "string" }
          ]
        },
        "variableDef": { "kind": "const" }
      }
    ]));
    let tag = &doc.symbols[0].declarations[0].js_doc.tags[0];
    match tag {
      js_doc::JsDocTag::Property { ts_type, name, .. } => {
        assert_eq!(&**name, "foo");
        assert_eq!(ts_type.repr, "string");
      }
      other => panic!("expected Property, got {:?}", other),
    }
  }

  #[test]
  fn jsdoc_tag_null_type_becomes_none() {
    let doc = docnodes_v1_to_v2(json!([
      {
        "name": "myFunc",
        "kind": "function",
        "location": { "filename": "test.ts", "line": 1, "col": 0, "byteIndex": 0 },
        "declarationKind": "export",
        "jsDoc": {
          "tags": [
            { "kind": "return", "type": null }
          ]
        },
        "functionDef": { "params": [] }
      }
    ]));
    let tag = &doc.symbols[0].declarations[0].js_doc.tags[0];
    match tag {
      js_doc::JsDocTag::Return { ts_type, .. } => {
        assert!(ts_type.is_none());
      }
      other => panic!("expected Return, got {:?}", other),
    }
  }

  #[test]
  fn module_doc_jsdoc_tags_migrated() {
    let doc = docnodes_v1_to_v2(json!([
      {
        "kind": "moduleDoc",
        "jsDoc": {
          "doc": "Module docs",
          "tags": [
            { "kind": "type", "type": "module" }
          ]
        }
      }
    ]));
    let tag = &doc.module_doc.tags[0];
    match tag {
      js_doc::JsDocTag::TypeRef { ts_type, .. } => {
        assert_eq!(ts_type.repr, "module");
      }
      other => panic!("expected TypeRef, got {:?}", other),
    }
  }

  #[test]
  fn import_jsdoc_tags_migrated() {
    let doc = docnodes_v1_to_v2(json!([
      {
        "kind": "import",
        "name": "Foo",
        "importDef": { "src": "./foo.ts" },
        "jsDoc": {
          "tags": [
            { "kind": "type", "type": "Foo" }
          ]
        }
      }
    ]));
    let tag = &doc.imports[0].js_doc.tags[0];
    match tag {
      js_doc::JsDocTag::TypeRef { ts_type, .. } => {
        assert_eq!(ts_type.repr, "Foo");
      }
      other => panic!("expected TypeRef, got {:?}", other),
    }
  }

  #[test]
  fn migrate_ts_type_defs_nested() {
    // Ensure nested TsTypeDef objects in v1 format are migrated
    let doc = docnodes_v1_to_v2(json!([
      {
        "name": "myFunc",
        "kind": "function",
        "location": { "filename": "test.ts", "line": 1, "col": 0, "byteIndex": 0 },
        "declarationKind": "export",
        "functionDef": {
          "params": [],
          "returnType": {
            "repr": "string[]",
            "kind": "array",
            "array": {
              "repr": "string",
              "kind": "keyword",
              "keyword": "string"
            }
          }
        }
      }
    ]));
    let decl = &doc.symbols[0].declarations[0];
    match &decl.def {
      node::DeclarationDef::Function(f) => {
        let rt = f.return_type.as_ref().unwrap();
        assert_eq!(rt.repr, "string[]");
      }
      other => panic!("expected Function, got {:?}", other),
    }
  }
}
