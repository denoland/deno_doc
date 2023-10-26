use super::util::RenderContext;
use crate::DocNodeKind;
use std::collections::HashMap;

pub fn render_symbol_group(
  mut doc_nodes: Vec<crate::DocNode>,
  name: &str,
  context: &RenderContext,
) -> String {
  doc_nodes.sort_by(|a, b| a.kind.cmp(&b.kind));

  let mut split_nodes = HashMap::<DocNodeKind, Vec<crate::DocNode>>::default();
  let mut is_reexport = false;

  for doc_node in doc_nodes {
    if doc_node.kind == DocNodeKind::Import {
      is_reexport = true;
      continue;
    }

    split_nodes
      .entry(doc_node.kind)
      .or_insert(vec![])
      .push(doc_node);
  }

  // TODO: property drilldown

  let items = split_nodes
    .values()
    .map(|doc_nodes| render_symbol(doc_nodes, name, context))
    .collect::<String>();

  format!(
    r#"<article class="symbol_group" id="symbol_{name}">{items}</article>"#
  )
}

fn render_symbol(
  doc_nodes: &[crate::DocNode],
  name: &str,
  context: &RenderContext,
) -> String {
  let js_doc = doc_nodes
    .iter()
    .find_map(|doc_node| doc_node.js_doc.doc.as_deref());

  let is_function = doc_nodes[0].kind == DocNodeKind::Function;

  // TODO: tags

  format!(
    r#"<div class="symbol">
  <div class="symbol_header">
    <div>
      {}
    </div>
  </div>
  {}
</div>"#,
    doc_block_title(doc_nodes[0].kind, name),
    doc_block(doc_nodes, context, name),
  )
}

fn doc_block_title(kind: DocNodeKind, name: &str) -> String {
  let kind_str = match kind {
    DocNodeKind::Function => "function",
    DocNodeKind::Variable => "variable",
    DocNodeKind::Class => "class",
    DocNodeKind::Enum => "enum",
    DocNodeKind::Interface => "interface",
    DocNodeKind::TypeAlias => "type alias",
    DocNodeKind::Namespace => "namespace",
    _ => unimplemented!(),
  };

  format!(
    r#"<div class="doc_block_title"><div><span class="kind_{kind:?}_text">{kind_str}</span> <span style="font-weight: 700;">{name}</span></div></div>"#
  )
}

fn doc_block(
  doc_nodes: &[crate::DocNode],
  context: &RenderContext,
  name: &str,
) -> String {
  let mut content = String::new();
  let mut functions = vec![];

  for doc_node in doc_nodes {
    match doc_node.kind {
      DocNodeKind::Function => functions.push(doc_node),
      DocNodeKind::Variable => {
        content.push_str(&super::variable::render_variable(doc_node))
      }
      DocNodeKind::Class => {
        content.push_str(&super::class::render_class(doc_node))
      }
      DocNodeKind::Enum => {
        content.push_str(&super::r#enum::render_enum(doc_node))
      }
      DocNodeKind::Interface => {
        content.push_str(&super::interface::render_interface(doc_node))
      }
      DocNodeKind::TypeAlias => {
        content.push_str(&super::type_alias::render_type_alias(doc_node))
      }
      DocNodeKind::Namespace => {
        content.push_str(&super::namespace::render_namespace(
          doc_node,
          &RenderContext {
            additional_css: context.additional_css.clone(),
            namespace: Some(name.to_string()),
          },
        ))
      }
      DocNodeKind::ModuleDoc => {}
      DocNodeKind::Import => {}
    };
  }

  if !functions.is_empty() {
    content.push_str(&super::function::render_function(functions, context));
  }

  format!("<div>{content}</div>")
}
