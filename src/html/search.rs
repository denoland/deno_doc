use crate::html::DocNodeWithContext;
use crate::html::GenerateCtx;
use crate::node::Location;
use crate::DocNodeKind;
use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use serde::Serialize;
use serde_json::json;

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

fn doc_node_into_search_index_nodes_inner(
  ctx: &GenerateCtx,
  name: &str,
  doc_nodes: &[&DocNodeWithContext],
  ns_qualifiers: Vec<String>,
) -> Vec<SearchIndexNode> {
  let kinds = doc_nodes.iter().map(|node| node.doc_node.kind).collect();

  let deprecated = super::util::all_deprecated(
    &doc_nodes
      .iter()
      .map(|node| node.doc_node)
      .collect::<Vec<_>>(),
  );

  let name = if ns_qualifiers.is_empty() {
    name.to_string()
  } else {
    format!("{}.{}", ns_qualifiers.join("."), name)
  };

  if !matches!(doc_nodes[0].doc_node.kind, DocNodeKind::Namespace) {
    let mut location = doc_nodes[0].doc_node.location.clone();
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
      declaration_kind: doc_nodes[0].doc_node.declaration_kind,
      deprecated,
    }];
  }

  let ns_def = doc_nodes[0].doc_node.namespace_def.as_ref().unwrap();
  let mut nodes = Vec::with_capacity(1 + ns_def.elements.len());
  let ns_name = doc_nodes[0].doc_node.name.to_string();

  let mut location = doc_nodes[0].doc_node.location.clone();
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
    declaration_kind: doc_nodes[0].doc_node.declaration_kind,
    deprecated,
  });

  let mut grouped_nodes: IndexMap<String, Vec<&crate::DocNode>> =
    IndexMap::new();

  for node in &ns_def.elements {
    if matches!(node.kind, DocNodeKind::Import | DocNodeKind::ModuleDoc) {
      continue;
    }

    let entry = grouped_nodes.entry(node.name.clone()).or_insert(vec![]);
    if !entry.iter().any(|n| n.kind == node.kind) {
      entry.push(node);
    }
  }

  for (el_name, el_nodes) in &grouped_nodes {
    let mut ns_qualifiers_ = ns_qualifiers.clone();
    ns_qualifiers_.push(ns_name.to_string());

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
      nodes.extend_from_slice(&doc_node_into_search_index_nodes_inner(
        ctx,
        &el_nodes[0].name,
        &[&DocNodeWithContext {
          origin: doc_nodes[0].origin.clone(),
          doc_node: el_nodes[0],
        }],
        ns_qualifiers_.clone(),
      ));
    }
  }

  nodes
}

/// A single DocNode can produce multiple SearchIndexNode - eg. a namespace
/// node is flattened into a list of its elements.
fn doc_node_into_search_index_nodes(
  ctx: &GenerateCtx,
  name: &str,
  doc_nodes: &[&DocNodeWithContext],
) -> Vec<SearchIndexNode> {
  doc_node_into_search_index_nodes_inner(ctx, name, doc_nodes, vec![])
}

pub fn generate_search_index(
  ctx: &GenerateCtx,
  doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<crate::DocNode>>,
) -> serde_json::Value {
  // TODO(bartlomieju): remove
  let doc_nodes = doc_nodes_by_url
    .iter()
    .flat_map(|(specifier, nodes)| {
      nodes.iter().map(|node| DocNodeWithContext {
        origin: Some(std::borrow::Cow::Owned(ctx.url_to_short_path(specifier))),
        doc_node: node,
      })
    })
    .collect::<Vec<_>>();

  let mut grouped_nodes: IndexMap<String, Vec<&DocNodeWithContext>> =
    IndexMap::new();

  for node in &doc_nodes {
    if matches!(
      node.doc_node.kind,
      DocNodeKind::Import | DocNodeKind::ModuleDoc
    ) {
      continue;
    }

    let entry = grouped_nodes
      .entry(node.doc_node.name.clone())
      .or_insert(vec![]);
    if !entry.iter().any(|n| n.doc_node.kind == node.doc_node.kind) {
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
  doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<crate::DocNode>>,
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
