// Copyright 2020-2022 the Deno authors. All rights reserved. MIT license.

#![recursion_limit = "256"]
#![deny(clippy::disallowed_methods)]
#![deny(clippy::disallowed_types)]

#[macro_use]
extern crate cfg_if;
#[macro_use]
extern crate lazy_static;
#[cfg(test)]
#[macro_use]
extern crate serde_json;

mod class;
mod colors;
mod decorators;
mod diagnostics;
mod display;
mod r#enum;
mod function;
mod interface;
mod js_doc;
mod node;
mod params;
mod parser;
mod swc_util;
mod ts_type;
mod ts_type_param;
mod type_alias;
mod variable;

pub use node::DocNode;
pub use node::DocNodeKind;

use node::ImportDef;
use node::Location;
use node::ReexportKind;
use params::ParamDef;

cfg_if! {
  if #[cfg(feature = "rust")] {
    mod printer;
    pub use parser::DocError;
    pub use parser::DocParser;
    pub use parser::DocParserOptions;
    pub use printer::DocPrinter;
  }
}

cfg_if! {
  if #[cfg(feature = "html")] {
    pub mod html;
  }
}

cfg_if! {
  if #[cfg(feature = "wasm")] {
    mod js;
    pub use js::doc;
  }
}

#[cfg(test)]
mod tests;

#[cfg(feature = "rust")]
pub fn find_nodes_by_name_recursively(
  doc_nodes: Vec<DocNode>,
  name: String,
) -> Vec<DocNode> {
  let mut parts = name.splitn(2, '.');
  let name = parts.next();
  let leftover = parts.next();
  if name.is_none() {
    return doc_nodes;
  }

  let name = name.unwrap();
  let doc_nodes = find_nodes_by_name(doc_nodes, name.to_string());

  let mut found: Vec<DocNode> = vec![];
  match leftover {
    Some(leftover) => {
      for node in doc_nodes {
        let children = get_children_of_node(node);
        found.extend(find_nodes_by_name_recursively(
          children,
          leftover.to_string(),
        ));
      }
      found
    }
    None => doc_nodes,
  }
}

#[cfg(feature = "rust")]
fn find_nodes_by_name(doc_nodes: Vec<DocNode>, name: String) -> Vec<DocNode> {
  let mut found: Vec<DocNode> = vec![];
  for node in doc_nodes {
    if node.name == name {
      found.push(node);
    }
  }
  found
}

#[cfg(feature = "rust")]
fn get_children_of_node(node: DocNode) -> Vec<DocNode> {
  match node.kind {
    DocNodeKind::Namespace => {
      let namespace_def = node.namespace_def.unwrap();
      namespace_def.elements
    }
    DocNodeKind::Interface => {
      let interface_def = node.interface_def.unwrap();
      let mut doc_nodes: Vec<DocNode> = vec![];
      for method in interface_def.methods {
        doc_nodes.push(method.into());
      }
      for property in interface_def.properties {
        doc_nodes.push(property.into());
      }
      doc_nodes
    }
    DocNodeKind::Class => {
      let class_def = node.class_def.unwrap();
      let mut doc_nodes: Vec<DocNode> = vec![];
      for method in class_def.methods {
        doc_nodes.push(method.into());
      }
      for property in class_def.properties {
        doc_nodes.push(property.into());
      }
      doc_nodes
    }
    _ => vec![],
  }
}
