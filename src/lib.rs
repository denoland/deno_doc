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
pub use node::Location;
pub use node::Symbol;

use node::ImportDef;
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

pub fn docnodes_v1_to_v2(value: serde_json::Value) -> Vec<Symbol> {
  let serde_json::Value::Array(arr) = value else {
    return vec![];
  };

  // v1 format: flat array where each entry has "name", "kind", "location",
  // "declarationKind", "jsDoc", and def fields all at the top level.
  // v2 format: array of Symbol { name, isDefault, declarations: [...] }
  // where each declaration has the remaining fields.
  let mut symbols: indexmap::IndexMap<Box<str>, Symbol> =
    indexmap::IndexMap::new();

  for item in arr {
    let serde_json::Value::Object(mut obj) = item else {
      continue;
    };

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
      "importDef",
      "referenceDef",
    ] {
      if let Some(val) = obj.remove(field) {
        obj.insert("def".to_string(), val);
        break;
      }
    }

    let declaration = serde_json::Value::Object(obj);

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

  symbols.into_values().collect()
}
