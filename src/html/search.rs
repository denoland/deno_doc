use crate::html::DocNodeWithContext;
use crate::html::GenerateCtx;
use crate::node::Location;
use crate::DocNodeKind;
use deno_ast::ModuleSpecifier;
use serde::Serialize;
use serde_json::json;

fn join_qualifiers<S>(qualifiers: &[String], s: S) -> Result<S::Ok, S::Error>
where
  S: serde::Serializer,
{
  s.serialize_str(&qualifiers.join("."))
}

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct SearchIndexNode {
  kind: DocNodeKind,
  name: String,
  file: String,
  #[serde(serialize_with = "join_qualifiers")]
  ns_qualifiers: Vec<String>,
  location: Location,
  declaration_kind: crate::node::DeclarationKind,
}

fn doc_node_into_search_index_nodes_inner(
  ctx: &GenerateCtx,
  doc_node: &DocNodeWithContext,
  ns_qualifiers: Vec<String>,
) -> Vec<SearchIndexNode> {
  if !matches!(doc_node.doc_node.kind, DocNodeKind::Namespace) {
    let mut location = doc_node.doc_node.location.clone();
    let location_url = ModuleSpecifier::parse(&location.filename).unwrap();
    let location_url_str = ctx.url_to_short_path(&location_url);
    location.filename = location_url_str;

    return vec![SearchIndexNode {
      kind: doc_node.doc_node.kind,
      name: doc_node.doc_node.name.to_string(),
      file: doc_node.origin.clone().unwrap(),
      ns_qualifiers: ns_qualifiers.clone(),
      location,
      declaration_kind: doc_node.doc_node.declaration_kind,
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
    name: doc_node.doc_node.name.to_string(),
    file: doc_node.origin.clone().unwrap(),
    ns_qualifiers: ns_qualifiers.clone(),
    location,
    declaration_kind: doc_node.doc_node.declaration_kind,
  });

  for el in &ns_def.elements {
    let mut ns_qualifiers_ = ns_qualifiers.clone();
    ns_qualifiers_.push(ns_name.to_string());

    let mut location = el.location.clone();
    let location_url = ModuleSpecifier::parse(&location.filename).unwrap();
    let location_url_str = ctx.url_to_short_path(&location_url);
    location.filename = location_url_str;

    nodes.push(SearchIndexNode {
      kind: el.kind,
      name: el.name.to_string(),
      file: doc_node.origin.clone().unwrap(),
      ns_qualifiers: ns_qualifiers_,
      location,
      declaration_kind: el.declaration_kind,
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

  let doc_nodes = doc_nodes.iter().fold(
    Vec::with_capacity(doc_nodes.len()),
    |mut output, node| {
      output.extend_from_slice(&doc_node_into_search_index_nodes(ctx, node));
      output
    },
  );

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
