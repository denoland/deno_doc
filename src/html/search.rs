use super::DocNodeWithContext;
use super::GenerateCtx;
use crate::js_doc::JsDocTag;
use crate::node::Location;
use crate::DocNodeKind;
use deno_ast::ModuleSpecifier;
use serde::Serialize;
use serde_json::json;

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct SearchIndexNode {
  kind: Vec<DocNodeKind>,
  name: Box<str>,
  file: Box<str>,
  doc: Box<str>,
  location: Location,
  url: Box<str>,
  category: Box<str>,
  declaration_kind: crate::node::DeclarationKind,
  deprecated: bool,
}

fn doc_nodes_into_search_index_node(
  ctx: &GenerateCtx,
  doc_nodes: Vec<DocNodeWithContext>,
  name: String,
) -> SearchIndexNode {
  let kinds = doc_nodes.iter().map(|node| node.kind()).collect();
  let deprecated =
    super::util::all_deprecated(&doc_nodes.iter().collect::<Vec<_>>());

  let mut location = doc_nodes[0].location.clone();
  let location_url = ModuleSpecifier::parse(&location.filename).unwrap();
  location.filename = if let Some(short_path) = ctx
    .doc_nodes
    .keys()
    .find(|short_path| short_path.specifier == location_url)
  {
    html_escape::encode_text(&short_path.display_name()).into()
  } else {
    "".into()
  };

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

  SearchIndexNode {
    kind: kinds,
    name: html_escape::encode_text(&name).into(),
    file: html_escape::encode_double_quoted_attribute(
      &doc_nodes[0].origin.path,
    )
    .into(),
    doc,
    location,
    url: abs_url.into_boxed_str(),
    category,
    declaration_kind: doc_nodes[0].declaration_kind,
    deprecated,
  }
}

pub fn generate_search_index(ctx: &GenerateCtx) -> serde_json::Value {
  let doc_nodes = ctx
    .doc_nodes
    .values()
    .flatten()
    .cloned()
    .collect::<Vec<_>>();
  let partitions = super::partition::partition_nodes_by_name(&doc_nodes, true);

  let mut doc_nodes = partitions
    .into_iter()
    .map(|(name, nodes)| doc_nodes_into_search_index_node(ctx, nodes, name))
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
