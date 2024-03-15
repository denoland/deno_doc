use crate::html::DocNodeWithContext;
use crate::html::GenerateCtx;
use crate::node::Location;
use crate::DocNodeKind;
use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use serde::Serialize;
use serde_json::json;
use std::borrow::Cow;
use std::rc::Rc;

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct SearchIndexNode {
  kind: Vec<DocNodeKind>,
  name: String,
  file: String,
  location: Location,
  declaration_kind: crate::node::DeclarationKind,
  deprecated: bool,
}

/// A single DocNode can produce multiple SearchIndexNode - eg. a namespace
/// node is flattened into a list of its elements.
fn doc_node_into_search_index_nodes(
  ctx: &GenerateCtx,
  name: &str,
  doc_nodes: &[&DocNodeWithContext],
) -> Vec<SearchIndexNode> {
  let kinds = doc_nodes.iter().map(|node| node.inner.kind).collect();

  let deprecated = super::util::all_deprecated(&doc_nodes);

  let name = if doc_nodes
    .first()
    .expect("doc_nodes should not be empty")
    .ns_qualifiers
    .is_empty()
  {
    name.to_string()
  } else {
    format!("{}.{}", doc_nodes[0].ns_qualifiers.join("."), name)
  };

  if !matches!(doc_nodes[0].inner.kind, DocNodeKind::Namespace) {
    let mut location = doc_nodes[0].inner.location.clone();
    let location_url = ModuleSpecifier::parse(&location.filename).unwrap();
    location.filename = if ctx
      .main_entrypoint
      .as_ref()
      .map(|main_entrypoint| main_entrypoint != &location_url)
      .unwrap_or(true)
    {
      ctx.url_to_short_path(&location_url).to_name()
    } else {
      String::new()
    };

    return vec![SearchIndexNode {
      kind: kinds,
      name,
      file: doc_nodes[0]
        .origin
        .as_ref()
        .map(|origin| origin.as_str().to_string())
        .unwrap(),
      location,
      declaration_kind: doc_nodes[0].inner.declaration_kind,
      deprecated,
    }];
  }

  let ns_def = doc_nodes[0].inner.namespace_def.as_ref().unwrap();
  let mut nodes = Vec::with_capacity(1 + ns_def.elements.len());
  let ns_name = doc_nodes[0].inner.get_name().to_string();

  let mut location = doc_nodes[0].inner.location.clone();
  let location_url = ModuleSpecifier::parse(&location.filename).unwrap();
  location.filename = if ctx
    .main_entrypoint
    .as_ref()
    .map(|main_entrypoint| main_entrypoint != &location_url)
    .unwrap_or(true)
  {
    ctx.url_to_short_path(&location_url).to_name()
  } else {
    String::new()
  };

  nodes.push(SearchIndexNode {
    kind: kinds,
    name,
    file: doc_nodes[0]
      .origin
      .as_ref()
      .map(|origin| origin.as_str().to_string())
      .unwrap(),
    location,
    declaration_kind: doc_nodes[0].inner.declaration_kind,
    deprecated,
  });

  let mut grouped_nodes: IndexMap<&str, Vec<DocNodeWithContext>> =
    IndexMap::new();

  for node in &ns_def.elements {
    if matches!(node.kind, DocNodeKind::Import | DocNodeKind::ModuleDoc) {
      continue;
    }

    let entry = grouped_nodes.entry(node.get_name()).or_default();
    if !entry.iter().any(|n| n.kind == node.kind) {
      entry.push(DocNodeWithContext {
        origin: doc_nodes[0].origin.clone(),
        ns_qualifiers: Rc::new(vec![]),
        inner: Cow::Borrowed(node),
      });
    }
  }

  for (el_name, el_nodes) in grouped_nodes {
    let mut ns_qualifiers_ = (*doc_nodes[0].ns_qualifiers).clone();
    ns_qualifiers_.push(ns_name.clone());

    let mut location = el_nodes[0].location.clone();
    let location_url = ModuleSpecifier::parse(&location.filename).unwrap();
    location.filename = if ctx
      .main_entrypoint
      .as_ref()
      .map(|main_entrypoint| main_entrypoint != &location_url)
      .unwrap_or(true)
    {
      ctx.url_to_short_path(&location_url).to_name()
    } else {
      String::new()
    };

    let name = if ns_qualifiers_.is_empty() {
      el_name.to_string()
    } else {
      format!("{}.{}", ns_qualifiers_.join("."), el_name)
    };

    let kinds = el_nodes.iter().map(|node| node.kind).collect();

    nodes.push(SearchIndexNode {
      kind: kinds,
      name,
      file: doc_nodes[0]
        .origin
        .as_ref()
        .map(|origin| origin.as_str().to_string())
        .unwrap(),
      location,
      declaration_kind: el_nodes[0].declaration_kind,
      deprecated,
    });

    if el_nodes[0].kind == DocNodeKind::Namespace {
      nodes.extend_from_slice(&doc_node_into_search_index_nodes(
        ctx,
        el_nodes[0].get_name(),
        &[&el_nodes[0]],
      ));
    }
  }

  nodes
}

pub fn generate_search_index(
  ctx: &GenerateCtx,
  doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<DocNodeWithContext>>,
) -> serde_json::Value {
  let mut grouped_nodes: IndexMap<&str, Vec<&DocNodeWithContext>> =
    IndexMap::new();

  for node in doc_nodes_by_url.values().flatten() {
    if matches!(
      node.inner.kind,
      DocNodeKind::Import | DocNodeKind::ModuleDoc
    ) {
      continue;
    }

    let entry = grouped_nodes.entry(node.inner.get_name()).or_default();
    if !entry.iter().any(|n| n.inner.kind == node.inner.kind) {
      entry.push(node);
    }
  }

  let mut doc_nodes = grouped_nodes
    .iter()
    .flat_map(|(name, node)| doc_node_into_search_index_nodes(ctx, name, node))
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

  search_index
}

pub(crate) fn get_search_index_file(
  ctx: &GenerateCtx,
  doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<DocNodeWithContext>>,
) -> Result<String, anyhow::Error> {
  let search_index = generate_search_index(ctx, doc_nodes_by_url);
  let search_index_str = serde_json::to_string(&search_index)?;

  let index = format!(
    r#"(function () {{
  window.DENO_DOC_SEARCH_INDEX = {};
}})()"#,
    search_index_str
  );
  Ok(index)
}
