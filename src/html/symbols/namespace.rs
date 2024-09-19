use crate::html::render_context::RenderContext;
use crate::html::util::*;
use crate::html::DocNodeWithContext;
use indexmap::IndexMap;
use indexmap::IndexSet;
use serde::Serialize;

pub fn render_namespace(
  ctx: &RenderContext,
  partitions: impl Iterator<Item = (SectionHeaderCtx, Vec<DocNodeWithContext>)>,
) -> Vec<SectionCtx> {
  partitions
    .map(|(header, doc_nodes)| {
      get_namespace_section_render_ctx(ctx, header, doc_nodes)
    })
    .collect()
}

fn get_namespace_section_render_ctx(
  ctx: &RenderContext,
  header: SectionHeaderCtx,
  doc_nodes: Vec<DocNodeWithContext>,
) -> SectionCtx {
  let mut grouped_nodes = IndexMap::new();

  for node in doc_nodes {
    let entry = grouped_nodes
      .entry(node.get_qualified_name())
      .or_insert(vec![]);
    entry.push(node);
  }

  let nodes = grouped_nodes
    .into_iter()
    .filter_map(|(name, nodes)| {
      if nodes[0].is_internal() {
        None
      } else {
        Some(NamespaceNodeCtx::new(ctx, name, nodes))
      }
    })
    .collect::<Vec<_>>();

  let mut section = SectionCtx::new(
    ctx,
    &header.title,
    SectionContentCtx::NamespaceSection(nodes),
  );
  section.header = header;
  section
}

#[derive(Debug, Serialize, Clone)]
pub struct NamespaceNodeCtx {
  pub id: String,
  pub anchor: AnchorCtx,
  pub tags: IndexSet<Tag>,
  pub doc_node_kind_ctx: IndexSet<DocNodeKindCtx>,
  pub href: String,
  pub name: String,
  pub docs: Option<String>,
  pub deprecated: bool,
}

impl NamespaceNodeCtx {
  fn new(
    ctx: &RenderContext,
    name: String,
    nodes: Vec<DocNodeWithContext>,
  ) -> Self {
    let id = name_to_id("namespace", &name);

    let docs =
      crate::html::jsdoc::jsdoc_body_to_html(ctx, &nodes[0].js_doc, true);

    let tags = Tag::from_js_doc(&nodes[0].js_doc);

    NamespaceNodeCtx {
      id: id.clone(),
      anchor: AnchorCtx { id },
      tags,
      doc_node_kind_ctx: nodes
        .iter()
        .map(|node| node.kind_with_drilldown.into())
        .collect(),
      href: ctx.ctx.resolve_path(
        ctx.get_current_resolve(),
        UrlResolveKind::Symbol {
          file: &nodes[0].origin,
          symbol: &name,
        },
      ),
      name,
      docs,
      deprecated: all_deprecated(&nodes.iter().collect::<Vec<_>>()),
    }
  }
}
