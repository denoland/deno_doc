// Copyright 2018-2020 the Deno authors. All rights reserved. MIT license.

#[macro_use]
extern crate lazy_static;
#[cfg(test)]
#[macro_use]
extern crate serde_json;

pub mod class;
mod colors;
mod display;
pub mod r#enum;
pub mod function;
pub mod interface;
pub mod module;
pub mod namespace;
mod node;
pub mod params;
pub mod parser;
pub mod printer;
mod swc_util;
pub mod ts_type;
pub mod ts_type_param;
pub mod type_alias;
pub mod variable;

pub use node::DocNode;
pub use node::DocNodeKind;
pub use node::ImportDef;
pub use node::Location;
pub use node::ModuleDoc;
pub use node::Reexport;
pub use node::ReexportKind;
pub use params::ParamDef;
pub use parser::DocError;
pub use parser::DocParser;
pub use printer::DocPrinter;

#[cfg(test)]
mod tests;

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

fn find_nodes_by_name(doc_nodes: Vec<DocNode>, name: String) -> Vec<DocNode> {
  let mut found: Vec<DocNode> = vec![];
  for node in doc_nodes {
    if node.name == name {
      found.push(node);
    }
  }
  found
}

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
