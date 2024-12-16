use super::DocNodeKindCtx;
use super::DocNodeWithContext;
use super::GenerateCtx;
use crate::js_doc::JsDocTag;
use serde::Serialize;
use serde_json::json;

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct SlimKindCtx {
  char: char,
  pub kind: &'static str,
  pub title: &'static str,
}

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct SearchIndexNode {
  kind: Vec<SlimKindCtx>,
  name: Box<str>,
  file: Box<str>,
  doc: Box<str>,
  url: Box<str>,
  #[serde(skip_serializing_if = "is_empty", default)]
  category: Box<str>,
  deprecated: bool,
}

fn is_empty(s: &str) -> bool {
  s.is_empty()
}

fn doc_nodes_into_search_index_node(
  ctx: &GenerateCtx,
  doc_nodes: Vec<DocNodeWithContext>,
  name: String,
) -> Vec<SearchIndexNode> {
  let kinds = doc_nodes
    .iter()
    .map(|node| {
      let kind = DocNodeKindCtx::from(node.kind_with_drilldown);
      SlimKindCtx {
        char: kind.char,
        kind: kind.kind,
        title: kind.title,
      }
    })
    .collect();
  let deprecated =
    super::util::all_deprecated(&doc_nodes.iter().collect::<Vec<_>>());

  let doc = doc_nodes[0].js_doc.doc.clone().unwrap_or_default();

  let abs_url = ctx.resolve_path(
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

  let mut out = vec![SearchIndexNode {
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

        doc_nodes_into_search_index_node(ctx, vec![drilldown_node], name)
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

  let mut doc_nodes = partitions
    .into_iter()
    .flat_map(|(name, nodes)| {
      doc_nodes_into_search_index_node(ctx, nodes, name)
    })
    .collect::<Vec<_>>();

  doc_nodes.sort_by(|a, b| a.file.cmp(&b.file));

  let search_index = json!({
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
