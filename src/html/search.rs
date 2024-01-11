use crate::html::short_path_to_name;
use crate::html::DocNodeWithContext;
use crate::html::GenerateCtx;
use crate::node::Location;
use crate::DocNodeKind;
use deno_ast::ModuleSpecifier;
use serde::Serialize;
use serde_json::json;

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct SearchIndexNode {
  kind: DocNodeKind,
  name: String,
  file: String,
  file_name: String,
  location: Location,
  declaration_kind: crate::node::DeclarationKind,
  deprecated: bool,
}

fn doc_node_into_search_index_nodes_inner(
  ctx: &GenerateCtx,
  doc_node: &DocNodeWithContext,
  ns_qualifiers: Vec<String>,
) -> Vec<SearchIndexNode> {
  if matches!(
    doc_node.doc_node.kind,
    DocNodeKind::Import | DocNodeKind::ModuleDoc
  ) {
    return vec![];
  }

  let deprecated = doc_node
    .doc_node
    .js_doc
    .tags
    .iter()
    .any(|tag| matches!(tag, crate::JsDocTag::Deprecated { .. }));

  let name = if ns_qualifiers.is_empty() {
    doc_node.doc_node.name.to_string()
  } else {
    format!("{}.{}", ns_qualifiers.join("."), doc_node.doc_node.name)
  };

  if !matches!(doc_node.doc_node.kind, DocNodeKind::Namespace) {
    let mut location = doc_node.doc_node.location.clone();
    let location_url = ModuleSpecifier::parse(&location.filename).unwrap();
    let location_url_str = ctx.url_to_short_path(&location_url);
    location.filename = location_url_str;

    return vec![SearchIndexNode {
      kind: doc_node.doc_node.kind,
      name,
      file: doc_node.origin.clone().unwrap(),
      file_name: short_path_to_name(&doc_node.origin.clone().unwrap()),
      location,
      declaration_kind: doc_node.doc_node.declaration_kind,
      deprecated,
    }];
  }

  let ns_def = doc_node.doc_node.namespace_def.as_ref().unwrap();
  let mut nodes = Vec::with_capacity(1 + ns_def.elements.len());
  let ns_name = doc_node.doc_node.name.to_string();

  let mut location = doc_node.doc_node.location.clone();
  let location_url = ModuleSpecifier::parse(&location.filename).unwrap();
  let location_url_str = ctx.url_to_short_path(&location_url);
  location.filename = location_url_str;

  nodes.push(SearchIndexNode {
    kind: doc_node.doc_node.kind,
    name,
    file: doc_node.origin.clone().unwrap(),
    file_name: short_path_to_name(&doc_node.origin.clone().unwrap()),
    location,
    declaration_kind: doc_node.doc_node.declaration_kind,
    deprecated,
  });

  for el in &ns_def.elements {
    let mut ns_qualifiers_ = ns_qualifiers.clone();
    ns_qualifiers_.push(ns_name.to_string());

    let mut location = el.location.clone();
    let location_url = ModuleSpecifier::parse(&location.filename).unwrap();
    let location_url_str = ctx.url_to_short_path(&location_url);
    location.filename = location_url_str;

    let name = if ns_qualifiers_.is_empty() {
      el.name.to_string()
    } else {
      format!("{}.{}", ns_qualifiers_.join("."), el.name)
    };

    nodes.push(SearchIndexNode {
      kind: el.kind,
      name,
      file: doc_node.origin.clone().unwrap(),
      file_name: short_path_to_name(&doc_node.origin.clone().unwrap()),
      location,
      declaration_kind: el.declaration_kind,
      deprecated,
    });

    if el.kind == DocNodeKind::Namespace {
      nodes.extend_from_slice(&doc_node_into_search_index_nodes_inner(
        ctx,
        &DocNodeWithContext {
          origin: doc_node.origin.clone(),
          doc_node: el.clone(),
        },
        ns_qualifiers.clone(),
      ));
    }
  }

  nodes
}

/// A single DocNode can produce multiple SearchIndexNode - eg. a namespace
/// node is flattened into a list of its elements.
fn doc_node_into_search_index_nodes(
  ctx: &GenerateCtx,
  doc_node: &DocNodeWithContext,
) -> Vec<SearchIndexNode> {
  doc_node_into_search_index_nodes_inner(ctx, doc_node, vec![])
}

pub fn generate_search_index(
  ctx: &GenerateCtx,
  doc_nodes_by_url: &indexmap::IndexMap<ModuleSpecifier, Vec<crate::DocNode>>,
) -> Result<String, anyhow::Error> {
  // TODO(bartlomieju): remove
  let doc_nodes = doc_nodes_by_url
    .iter()
    .flat_map(|(specifier, nodes)| {
      nodes.iter().map(|node| DocNodeWithContext {
        origin: Some(ctx.url_to_short_path(specifier)),
        doc_node: node.clone(),
      })
    })
    .collect::<Vec<_>>();

  let mut doc_nodes = doc_nodes
    .iter()
    .flat_map(|node| doc_node_into_search_index_nodes(ctx, node))
    .collect::<Vec<_>>();

  doc_nodes.sort_by(|a, b| a.file.cmp(&b.file));
  doc_nodes.dedup_by(|a, b| {
    a.deprecated == b.deprecated
      && a.name == b.name
      && a.kind == b.kind
      && a.location == b.location
      && a.declaration_kind == b.declaration_kind
  });

  let search_index = json!({
    "nodes": doc_nodes
  });

  Ok(serde_json::to_string(&search_index)?)
}

pub(crate) fn get_search_index_file(
  ctx: &GenerateCtx,
  doc_nodes_by_url: &indexmap::IndexMap<ModuleSpecifier, Vec<crate::DocNode>>,
) -> Result<String, anyhow::Error> {
  let search_index_str = generate_search_index(ctx, doc_nodes_by_url)?;

  let index = format!(
    r#"(function () {{
  window.DENO_DOC_SEARCH_INDEX = {};
}})()"#,
    search_index_str
  );
  Ok(index)
}
