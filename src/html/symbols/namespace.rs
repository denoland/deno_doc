use crate::html::render_context::RenderContext;
use crate::html::util::*;
use crate::html::DocNodeWithContext;
use crate::DocNodeKind;
use indexmap::IndexMap;
use serde::Serialize;
use std::collections::HashSet;

pub fn render_namespace(
  ctx: &RenderContext,
  partitions: IndexMap<DocNodeKind, Vec<DocNodeWithContext>>,
) -> Vec<SectionCtx> {
  partitions
    .into_iter()
    .map(|(kind, doc_nodes)| {
      get_namespace_section_render_ctx(ctx, kind, doc_nodes)
    })
    .collect()
}

fn get_namespace_section_render_ctx(
  ctx: &RenderContext,
  kind: DocNodeKind,
  doc_nodes: Vec<DocNodeWithContext>,
) -> SectionCtx {
  let kind_ctx = super::super::util::DocNodeKindCtx::from(kind);

  let mut grouped_nodes = IndexMap::new();

  for node in doc_nodes {
    let entry = grouped_nodes
      .entry(if !node.ns_qualifiers.is_empty() {
        format!(
          "{}.{}",
          node.ns_qualifiers.join("."),
          node.doc_node.get_name()
        )
      } else {
        node.doc_node.get_name().to_string()
      })
      .or_insert(vec![]);
    entry.push(node);
  }

  let nodes = grouped_nodes
    .into_iter()
    .map(|(name, nodes)| NamespaceNodeCtx::new(ctx, name, nodes))
    .collect::<Vec<_>>();

  SectionCtx {
    title: kind_ctx.title_plural,
    content: SectionContentCtx::NamespaceSection(nodes),
  }
}

#[derive(Debug, Serialize, Clone)]
pub struct NamespaceNodeCtx {
  pub tags: HashSet<Tag>,
  pub doc_node_kind_ctx: Vec<DocNodeKindCtx>,
  pub origin_name: Option<String>,
  pub href: String,
  pub name: String,
  pub docs: Option<String>,
  pub deprecated: bool,
}

impl NamespaceNodeCtx {
  fn new(
    ctx: &RenderContext,
    mut name: String,
    nodes: Vec<DocNodeWithContext>,
  ) -> Self {
    // TODO: linking

    let ns_parts = ctx.get_namespace_parts();
    if !ns_parts.is_empty() {
      name = format!("{}.{}", ns_parts.join("."), name);
    }

    let current_resolve = ctx.get_current_resolve();

    let docs = crate::html::jsdoc::jsdoc_body_to_html(
      ctx,
      &nodes[0].doc_node.js_doc,
      true,
    );

    let tags = Tag::from_js_doc(&nodes[0].doc_node.js_doc);

    NamespaceNodeCtx {
      tags,
      doc_node_kind_ctx: nodes
        .iter()
        .map(|node| node.doc_node.kind.into())
        .collect(),
      origin_name: if ctx.ctx.single_file_mode {
        None
      } else {
        nodes[0].origin.as_ref().map(|origin| origin.to_name())
      },
      href: ctx.ctx.href_resolver.resolve_path(
        current_resolve,
        UrlResolveKind::Symbol {
          file: nodes[0]
            .origin
            .as_deref()
            .or_else(|| current_resolve.get_file())
            .unwrap(),
          symbol: &name,
        },
      ),
      name,
      docs,
      deprecated: all_deprecated(
        &nodes.iter().map(|node| node.doc_node).collect::<Vec<_>>(),
      ),
    }
  }
}
