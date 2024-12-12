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
mod colors;
mod decorators;
mod diagnostics;
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

pub use node::DocNode;
pub use node::DocNodeKind;
pub use node::Location;

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

#[cfg(test)]
mod tests;

#[cfg(feature = "rust")]
pub fn find_nodes_by_name_recursively(
  doc_nodes: Vec<DocNode>,
  name: &str,
) -> Vec<DocNode> {
  let mut parts = name.splitn(2, '.');
  let name = parts.next();
  let leftover = parts.next();
  if name.is_none() {
    return doc_nodes;
  }

  let name = name.unwrap();
  let doc_nodes = find_nodes_by_name(doc_nodes, name);

  let mut found: Vec<DocNode> = vec![];
  match leftover {
    Some(leftover) => {
      for node in doc_nodes {
        let children = get_children_of_node(node);
        found.extend(find_nodes_by_name_recursively(children, leftover));
      }
      found
    }
    None => doc_nodes,
  }
}

#[cfg(feature = "rust")]
fn find_nodes_by_name(doc_nodes: Vec<DocNode>, name: &str) -> Vec<DocNode> {
  let mut found: Vec<DocNode> = vec![];
  for node in doc_nodes {
    if &*node.name == name {
      found.push(node);
    }
  }
  found
}

#[cfg(feature = "rust")]
fn get_children_of_node(node: DocNode) -> Vec<DocNode> {
  use node::DocNodeDef;

  match node.def {
    DocNodeDef::Namespace { namespace_def } => namespace_def
      .elements
      .into_iter()
      .map(std::rc::Rc::unwrap_or_clone)
      .collect(),
    DocNodeDef::Interface { interface_def } => {
      let mut doc_nodes: Vec<DocNode> = vec![];
      for method in interface_def.methods {
        doc_nodes.push(method.into());
      }
      for property in interface_def.properties {
        doc_nodes.push(property.into());
      }
      doc_nodes
    }
    DocNodeDef::Class { class_def } => {
      let mut doc_nodes: Vec<DocNode> = vec![];
      for method in class_def.methods.into_vec().into_iter() {
        doc_nodes.push(method.into());
      }
      for property in class_def.properties.into_vec().into_iter() {
        doc_nodes.push(property.into());
      }
      doc_nodes
    }
    _ => vec![],
  }
}
