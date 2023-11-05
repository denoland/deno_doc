use crate::html::util::*;
use crate::DocNode;
use crate::DocNodeKind;
use indexmap::IndexMap;
use serde::Serialize;
use std::cmp::Ordering;

#[derive(Serialize)]
pub struct NamespaceRenderCtx {
  pub sections: Vec<NamespaceSectionRenderCtx>,
}

#[derive(Serialize)]
pub struct NamespaceSectionRenderCtx {
  pub id: String,
  pub title: String,
  pub nodes: Vec<NamespaceSectionNodeCtx>,
}

#[derive(Serialize)]
pub struct NamespaceSectionNodeCtx {
  pub doc_node_kind_ctx: DocNodeKindCtx,
  pub path: String,
  pub name: String,
  pub docs: String,
}

pub(crate) fn get_namespace_render_ctx(
  ctx: &RenderContext,
  partitions: &IndexMap<DocNodeKind, Vec<DocNode>>,
) -> NamespaceRenderCtx {
  let mut sections = Vec::with_capacity(partitions.len());

  for (kind, doc_nodes) in partitions {
    let ns_section_ctx =
      get_namespace_section_render_ctx(ctx, *kind, doc_nodes);
    sections.push(ns_section_ctx)
  }

  NamespaceRenderCtx { sections }
}

pub(crate) fn render_namespace(
  ctx: &RenderContext,
  doc_node: &crate::DocNode,
) -> String {
  let namespace_def = doc_node.namespace_def.as_ref().unwrap();

  let partitions = partition_nodes_by_kind(&namespace_def.elements);
  let namespace_ctx = get_namespace_render_ctx(ctx, &partitions);

  let content_parts = namespace_ctx
    .sections
    .into_iter()
    .map(|section| ctx.render("namespace_section.html", &section))
    .collect::<Vec<_>>();

  content_parts.join("")
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

fn get_namespace_section_render_ctx(
  ctx: &RenderContext,
  kind: DocNodeKind,
  doc_nodes: &[DocNode],
) -> NamespaceSectionRenderCtx {
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

  let nodes = doc_nodes
    .iter()
    .map(|doc_node| {
      // TODO: linking, tags

      let mut name = doc_node.name.clone();
      let mut path = doc_node.name.clone();

      if let Some(namespace) = ctx.get_namespace() {
        name = format!("{namespace}.{}", doc_node.name);
        path = format!(
          "{}/{}",
          namespace
            .rsplit_once('.')
            .map_or(&*namespace, |(_prev, current)| current),
          doc_node.name
        );
      }

      NamespaceSectionNodeCtx {
        doc_node_kind_ctx: (&doc_node.kind).into(),
        path,
        name,
        // TODO(bartlomieju): make it a template
        docs: crate::html::jsdoc::render_docs_summary(ctx, &doc_node.js_doc),
      }
    })
    .collect::<Vec<_>>();

  NamespaceSectionRenderCtx {
    id: title_to_id(title),
    title: title.to_string(),
    nodes,
  }
}
