use super::DocNodeWithContext;
use super::GenerateCtx;
use crate::node::Location;
use crate::DocNodeKind;
use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use serde::Serialize;
use serde_json::json;
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
  doc_nodes: &[&DocNodeWithContext],
) -> Vec<SearchIndexNode> {
  let kinds = doc_nodes.iter().map(|node| node.kind).collect();

  let deprecated = super::util::all_deprecated(doc_nodes);

  let name =
    html_escape::encode_text(&doc_nodes[0].get_qualified_name()).to_string();

  if doc_nodes[0].kind != DocNodeKind::Namespace {
    let mut location = doc_nodes[0].location.clone();
    let location_url = ModuleSpecifier::parse(&location.filename).unwrap();
    location.filename = if let Some(short_path) = ctx
      .doc_nodes
      .keys()
      .find(|short_path| short_path.specifier == location_url)
    {
      html_escape::encode_text(&short_path.display_name()).into_owned()
    } else {
      String::new()
    };

    return vec![SearchIndexNode {
      kind: kinds,
      name,
      file: html_escape::encode_double_quoted_attribute(
        &doc_nodes[0].origin.path,
      )
      .into_owned(),
      location,
      declaration_kind: doc_nodes[0].declaration_kind,
      deprecated,
    }];
  }

  let ns_def = doc_nodes[0].namespace_def.as_ref().unwrap();
  let mut nodes = Vec::with_capacity(1 + ns_def.elements.len());
  let ns_name = doc_nodes[0].get_name().to_string();

  let mut location = doc_nodes[0].location.clone();
  let location_url = ModuleSpecifier::parse(&location.filename).unwrap();
  location.filename = if let Some(short_path) = ctx
    .doc_nodes
    .keys()
    .find(|short_path| short_path.specifier == location_url)
  {
    html_escape::encode_text(&short_path.display_name()).into_owned()
  } else {
    String::new()
  };

  nodes.push(SearchIndexNode {
    kind: kinds,
    name,
    file: html_escape::encode_double_quoted_attribute(
      &doc_nodes[0].origin.path,
    )
    .into_owned(),
    location,
    declaration_kind: doc_nodes[0].declaration_kind,
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
      entry.push(doc_nodes[0].create_child(node.clone()));
    }
  }

  for (el_name, el_nodes) in grouped_nodes {
    let mut ns_qualifiers_ = (*doc_nodes[0].ns_qualifiers).clone();
    ns_qualifiers_.push(ns_name.clone());

    let mut location = el_nodes[0].location.clone();
    let location_url = ModuleSpecifier::parse(&location.filename).unwrap();
    location.filename = if let Some(short_path) = ctx
      .doc_nodes
      .keys()
      .find(|short_path| short_path.specifier == location_url)
    {
      html_escape::encode_text(&short_path.display_name()).into_owned()
    } else {
      String::new()
    };

    let name = if ns_qualifiers_.is_empty() {
      html_escape::encode_text(el_name).into_owned()
    } else {
      format!(
        "{}.{}",
        ns_qualifiers_.join("."),
        html_escape::encode_text(el_name)
      )
    };

    let kinds = el_nodes.iter().map(|node| node.kind).collect();

    nodes.push(SearchIndexNode {
      kind: kinds,
      name,
      file: html_escape::encode_double_quoted_attribute(
        &doc_nodes[0].origin.path,
      )
      .into_owned(),
      location,
      declaration_kind: el_nodes[0].declaration_kind,
      deprecated,
    });

    if el_nodes[0].kind == DocNodeKind::Namespace {
      nodes.extend(doc_node_into_search_index_nodes(
        ctx,
        &[&DocNodeWithContext {
          origin: doc_nodes[0].origin.clone(),
          ns_qualifiers: Rc::new(ns_qualifiers_),
          drilldown_parent_kind: None,
          kind_with_drilldown: el_nodes[0].kind_with_drilldown,
          inner: el_nodes[0].inner.clone(),
        }],
      ));
    }
  }

  nodes
}

pub fn generate_search_index(ctx: &GenerateCtx) -> serde_json::Value {
  let doc_nodes = ctx.doc_nodes.values().flatten().collect::<Vec<_>>();

  let mut grouped_nodes: IndexMap<&str, Vec<&DocNodeWithContext>> =
    IndexMap::new();

  for node in doc_nodes {
    if matches!(node.kind, DocNodeKind::Import | DocNodeKind::ModuleDoc) {
      continue;
    }

    let entry = grouped_nodes.entry(node.get_name()).or_default();
    if !entry.iter().any(|n| n.kind == node.kind) {
      entry.push(node);
    }
  }

  let mut doc_nodes = grouped_nodes
    .values()
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
