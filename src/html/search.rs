use std::borrow::Cow;
use super::DocNodeKindCtx;
use super::DocNodeWithContext;
use super::GenerateCtx;
use super::RenderContext;
use super::UrlResolveKind;
use crate::html::util::Id;
use crate::html::util::IdBuilder;
use crate::html::util::IdKind;
use crate::js_doc::JsDocTag;
use serde::Serialize;
use serde::Deserialize;
use serde_json::json;

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SlimKindCtx {
  pub char: char,
  pub kind: Cow<'static, str>,
  pub title: Cow<'static, str>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SearchIndexNode {
  pub id: Id,
  pub kind: Vec<SlimKindCtx>,
  pub name: Box<str>,
  pub file: Box<str>,
  pub doc: Box<str>,
  pub url: Box<str>,
  #[serde(skip_serializing_if = "is_empty", default)]
  pub category: Box<str>,
  pub deprecated: bool,
}

fn is_empty(s: &str) -> bool {
  s.is_empty()
}

pub fn doc_nodes_into_search_index_node(
  ctx: &RenderContext,
  doc_nodes: Vec<DocNodeWithContext>,
  name: String,
  parent_id: Option<Id>,
) -> Vec<SearchIndexNode> {
  let kinds = doc_nodes
    .iter()
    .map(|node| {
      let kind = DocNodeKindCtx::from(node.kind);
      SlimKindCtx {
        char: kind.char,
        kind: kind.kind.into(),
        title: kind.title.into(),
      }
    })
    .collect();
  let deprecated =
    super::util::all_deprecated(&doc_nodes.iter().collect::<Vec<_>>());

  let doc = super::jsdoc::strip(
    ctx,
    &doc_nodes[0].js_doc.doc.clone().unwrap_or_default(),
  )
  .into_boxed_str();

  let abs_url = ctx.ctx.resolve_path(
    super::UrlResolveKind::Root,
    super::UrlResolveKind::Symbol {
      file: &doc_nodes[0].origin,
      symbol: &name,
    },
  );

  let category = doc_nodes[0]
    .js_doc
    .tags
    .iter()
    .find_map(|x| {
      if let JsDocTag::Category { doc } = x {
        Some(doc.clone())
      } else {
        None
      }
    })
    .unwrap_or_default();

  let id = parent_id.unwrap_or_else(|| {
    IdBuilder::new(ctx.ctx)
      .kind(IdKind::Namespace)
      .name(&name)
      .build()
  });

  let mut out = vec![SearchIndexNode {
    id: id.clone(),
    kind: kinds,
    name: html_escape::encode_text(&name).into(),
    file: html_escape::encode_double_quoted_attribute(
      &doc_nodes[0].origin.path,
    )
    .into(),
    doc,
    url: abs_url.into_boxed_str(),
    category,
    deprecated,
  }];

  out.extend(
    doc_nodes
      .iter()
      .filter_map(|node| node.get_drilldown_symbols())
      .flatten()
      .flat_map(|drilldown_node| {
        let name = drilldown_node.get_qualified_name();

        doc_nodes_into_search_index_node(
          ctx,
          vec![drilldown_node],
          name,
          Some(id.clone()),
        )
      }),
  );

  out
}

pub fn generate_search_index(ctx: &GenerateCtx) -> serde_json::Value {
  let doc_nodes = ctx
    .doc_nodes
    .values()
    .flatten()
    .map(std::borrow::Cow::Borrowed);
  let partitions =
    super::partition::partition_nodes_by_name(ctx, doc_nodes, true);

  let render_ctx = RenderContext::new(ctx, &[], UrlResolveKind::AllSymbols);

  let mut doc_nodes = partitions
    .into_iter()
    .flat_map(|(name, nodes)| {
      doc_nodes_into_search_index_node(&render_ctx, nodes, name, None)
    })
    .collect::<Vec<_>>();

  doc_nodes.sort_by(|a, b| a.file.cmp(&b.file));

  let search_index = json!({
    "kind": "search",
    "nodes": doc_nodes
  });

  search_index
}

pub(crate) fn get_search_index_file(
  ctx: &GenerateCtx,
) -> Result<String, anyhow::Error> {
  let search_index = generate_search_index(ctx);
  let search_index_str = serde_json::to_string(&search_index)?;

  let index = format!(
    r#"(function () {{
  window.DENO_DOC_SEARCH_INDEX = {};
}})()"#,
    search_index_str
  );
  Ok(index)
}
