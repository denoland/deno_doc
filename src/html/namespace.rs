use crate::html::util::*;
use crate::DocNode;
use crate::DocNodeKind;
use indexmap::IndexMap;
use serde::Serialize;
use serde_json::json;
use std::cmp::Ordering;

use super::GenerateCtx;

pub(super) fn render_namespace(
  ctx: &GenerateCtx,
  doc_node: &crate::DocNode,
  render_ctx: &RenderContext,
) -> String {
  let namespace_def = doc_node.namespace_def.as_ref().unwrap();

  let partitions = partition_nodes_by_kind(&namespace_def.elements);

  format!(
    r#"<div class="doc_block_items">{}{}</div>"#,
    super::jsdoc::render_docs(&doc_node.js_doc, true, false, render_ctx),
    doc_node_kind_sections(ctx, &partitions, render_ctx)
  )
}

fn partition_nodes_by_kind_inner(
  doc_nodes: &[DocNode],
  dedup_overloads: bool,
) -> IndexMap<DocNodeKind, Vec<DocNode>> {
  let mut partitions: IndexMap<DocNodeKind, Vec<DocNode>> = IndexMap::default();

  for node in doc_nodes {
    if matches!(node.kind, DocNodeKind::ModuleDoc | DocNodeKind::Import) {
      continue;
    }

    let entry = partitions.entry(node.kind).or_insert(vec![]);

    if !dedup_overloads || !entry.iter().any(|n: &DocNode| n.name == node.name)
    {
      entry.push(node.clone());
    }
  }

  for (_kind, nodes) in partitions.iter_mut() {
    nodes.sort_by_key(|n| n.name.to_string());
  }

  partitions
    .sorted_by(|key1, _value1, key2, _value2| match key1.cmp(key2) {
      Ordering::Greater => Ordering::Greater,
      Ordering::Less => Ordering::Less,
      Ordering::Equal => unreachable!(),
    })
    .collect()
}

pub fn partition_nodes_by_kind_dedup_overloads(
  doc_nodes: &[DocNode],
) -> IndexMap<DocNodeKind, Vec<DocNode>> {
  partition_nodes_by_kind_inner(doc_nodes, true)
}

pub fn partition_nodes_by_kind(
  doc_nodes: &[DocNode],
) -> IndexMap<DocNodeKind, Vec<DocNode>> {
  partition_nodes_by_kind_inner(doc_nodes, true)
}

pub(super) fn doc_node_kind_sections(
  ctx: &GenerateCtx,
  partitions: &IndexMap<DocNodeKind, Vec<DocNode>>,
  render_ctx: &RenderContext,
) -> String {
  let mut content_parts = Vec::with_capacity(partitions.len());

  for (kind, doc_nodes) in partitions {
    let title = match kind {
      DocNodeKind::Function => "Functions",
      DocNodeKind::Variable => "Variables",
      DocNodeKind::Class => "Classes",
      DocNodeKind::Enum => "Enums",
      DocNodeKind::Interface => "Interfaces",
      DocNodeKind::TypeAlias => "Type Aliases",
      DocNodeKind::Namespace => "Namespaces",
      DocNodeKind::ModuleDoc | DocNodeKind::Import => unimplemented!(),
    };

    content_parts.push(symbol_section(ctx, title, doc_nodes, render_ctx))
  }

  content_parts.join("")
}

fn symbol_section(
  ctx: &GenerateCtx,
  title: &str,
  doc_nodes: &[DocNode],
  render_ctx: &RenderContext,
) -> String {
  #[derive(Serialize)]
  struct SectionNode {
    icon: String,
    path: String,
    name: String,
    docs: String,
  }

  let nodes = doc_nodes
    .iter()
    .map(|doc_node| {
      // TODO: linking, tags

      let mut name = doc_node.name.clone();
      let mut path = doc_node.name.clone();

      if let Some(namespace) = render_ctx.get_namespace() {
        name = format!("{namespace}.{}", doc_node.name);
        path = format!(
          "{}/{}",
          namespace
            .rsplit_once('.')
            .map_or(&*namespace, |(_prev, current)| current),
          doc_node.name
        );
      }

      SectionNode {
        // TODO(bartlomieju): make it a template
        icon: doc_node_kind_icon(doc_node.kind),
        path,
        name,
        // TODO(bartlomieju): make it a template
        docs: super::jsdoc::render_docs(
          &doc_node.js_doc,
          false,
          true,
          render_ctx,
        ),
      }
    })
    .collect::<Vec<_>>();

  ctx
    .tt
    .render(
      "doc_node_kind_section.html",
      &json!({
        // TODO(bartlomieju): turn it into a template
        "title": section_title(title),
        "nodes": nodes
      }),
    )
    .unwrap()
}
