use crate::html::util::*;
use crate::html::DocNodeWithContext;
use crate::DocNode;
use crate::DocNodeKind;
use indexmap::IndexMap;
use serde::Serialize;
use std::cmp::Ordering;

#[derive(Serialize)]
pub struct NamespaceRenderCtx {
  pub sections: Vec<NamespaceSectionRenderCtx>,
}

#[derive(Serialize)]
pub struct NamespaceSectionRenderCtx {
  pub id: String,
  pub title: String,
  pub nodes: Vec<NamespaceSectionNodeCtx>,
}

#[derive(Serialize)]
pub struct NamespaceSectionNodeCtx {
  pub doc_node_kind_ctx: DocNodeKindCtx,
  pub origin: Option<String>,
  pub href: String,
  pub name: String,
  pub docs: String,
}

pub fn get_namespace_render_ctx(
  ctx: &RenderContext,
  partitions: &IndexMap<DocNodeKind, Vec<DocNodeWithContext>>,
) -> NamespaceRenderCtx {
  let mut sections = Vec::with_capacity(partitions.len());

  for (kind, doc_nodes) in partitions {
    let ns_section_ctx = get_namespace_section_render_ctx(ctx, kind, doc_nodes);
    sections.push(ns_section_ctx)
  }

  NamespaceRenderCtx { sections }
}

pub(crate) fn render_namespace(
  ctx: &RenderContext,
  doc_node: &DocNode,
) -> String {
  let namespace_def = doc_node.namespace_def.as_ref().unwrap();

  let partitions = partition_nodes_by_kind(
    &namespace_def
      .elements
      .iter()
      .map(|node| DocNodeWithContext {
        doc_node: node.clone(),
        origin: None,
      })
      .collect::<Vec<_>>(),
    false,
  );
  let namespace_ctx = get_namespace_render_ctx(ctx, &partitions);

  namespace_ctx
    .sections
    .into_iter()
    .map(|section| ctx.render("namespace_section.html", &section))
    .collect::<String>()
}

pub fn partition_nodes_by_kind(
  doc_nodes: &[DocNodeWithContext],
  flatten_namespaces: bool,
) -> IndexMap<DocNodeKind, Vec<DocNodeWithContext>> {
  let mut partitions: IndexMap<DocNodeKind, Vec<DocNodeWithContext>> =
    IndexMap::default();

  for node in doc_nodes {
    if matches!(
      node.doc_node.kind,
      DocNodeKind::ModuleDoc | DocNodeKind::Import
    ) {
      continue;
    }

    if flatten_namespaces && node.doc_node.kind == DocNodeKind::Namespace {
      let namespace_def = node.doc_node.namespace_def.as_ref().unwrap();
      partitions.extend(partition_nodes_by_kind(
        &namespace_def
          .elements
          .iter()
          .map(|element| DocNodeWithContext {
            origin: node.origin.clone(),
            doc_node: element.clone(),
          })
          .collect::<Vec<_>>(),
        true,
      ));
    }

    let entry = partitions.entry(node.doc_node.kind).or_insert(vec![]);

    if !entry.iter().any(|n| n.doc_node.name == node.doc_node.name) {
      entry.push(node.clone());
    }
  }

  for (_kind, nodes) in partitions.iter_mut() {
    nodes.sort_by_key(|n| n.doc_node.name.to_string());
  }

  partitions
    .sorted_by(|key1, _value1, key2, _value2| match key1.cmp(key2) {
      Ordering::Greater => Ordering::Greater,
      Ordering::Less => Ordering::Less,
      Ordering::Equal => unreachable!(),
    })
    .collect()
}

pub fn partition_nodes_by_category(
  doc_nodes: &[DocNodeWithContext],
  flatten_namespaces: bool,
) -> IndexMap<String, Vec<DocNodeWithContext>> {
  let mut partitions: IndexMap<String, Vec<DocNodeWithContext>> =
    IndexMap::default();

  for node in doc_nodes {
    if matches!(
      node.doc_node.kind,
      DocNodeKind::ModuleDoc | DocNodeKind::Import
    ) {
      continue;
    }

    if flatten_namespaces && node.doc_node.kind == DocNodeKind::Namespace {
      let namespace_def = node.doc_node.namespace_def.as_ref().unwrap();
      partitions.extend(partition_nodes_by_category(
        &namespace_def
          .elements
          .iter()
          .map(|element| DocNodeWithContext {
            origin: node.origin.clone(),
            doc_node: element.clone(),
          })
          .collect::<Vec<_>>(),
        true,
      ));
    }

    let category = node
      .doc_node
      .js_doc
      .tags
      .iter()
      .find_map(|tag| {
        if let crate::js_doc::JsDocTag::Category { doc } = tag {
          doc.clone()
        } else {
          None
        }
      })
      .unwrap_or(String::from("Uncategorized"));

    let entry = partitions.entry(category).or_insert(vec![]);

    if !entry.iter().any(|n| n.doc_node.name == node.doc_node.name) {
      entry.push(node.clone());
    }
  }

  for (_kind, nodes) in partitions.iter_mut() {
    nodes.sort_by_key(|n| n.doc_node.name.to_string());
  }

  partitions
    .sorted_by(|key1, _value1, key2, _value2| {
      match (key1.as_str(), key2.as_str()) {
        ("Uncategorized", _) => Ordering::Greater,
        (_, "Uncategorized") => Ordering::Less,
        _ => match key1.cmp(key2) {
          Ordering::Greater => Ordering::Greater,
          Ordering::Less => Ordering::Less,
          Ordering::Equal => unreachable!(),
        },
      }
    })
    .collect()
}

fn get_namespace_section_render_ctx(
  ctx: &RenderContext,
  kind: &DocNodeKind,
  doc_nodes: &[DocNodeWithContext],
) -> NamespaceSectionRenderCtx {
  let kind_ctx = super::super::util::DocNodeKindCtx::from(*kind);

  let nodes = doc_nodes
    .iter()
    .map(|DocNodeWithContext { doc_node, origin }| {
      // TODO: linking, tags

      let mut name = doc_node.name.clone();

      let ns_parts = ctx.get_namespace_parts();
      if !ns_parts.is_empty() {
        name = format!("{}.{}", ns_parts.join("."), doc_node.name);
      }

      NamespaceSectionNodeCtx {
        doc_node_kind_ctx: doc_node.kind.into(),
        origin: origin.clone(),
        href: (ctx.url_resolver)(
          ctx.get_current_file(),
          crate::html::UrlResolveKinds::Symbol {
            target_file: origin
              .as_deref()
              .or_else(|| ctx.get_current_file())
              .unwrap(),
            target_symbol: &name,
          },
        ),
        name,
        // TODO(bartlomieju): make it a template
        docs: crate::html::jsdoc::render_docs_summary(ctx, &doc_node.js_doc),
      }
    })
    .collect::<Vec<_>>();

  NamespaceSectionRenderCtx {
    id: title_to_id(&kind_ctx.kind),
    title: kind_ctx.title_plural.to_string(),
    nodes,
  }
}
