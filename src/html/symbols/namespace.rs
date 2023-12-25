use crate::html::render_context::RenderContext;
use crate::html::short_path_to_name;
use crate::html::util::*;
use crate::html::DocNodeWithContext;
use crate::DocNode;
use crate::DocNodeKind;
use indexmap::IndexMap;
use serde::Serialize;
use std::cmp::Ordering;
use std::collections::HashMap;

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
  pub doc_node_kind_ctx: Vec<DocNodeKindCtx>,
  pub origin: Option<String>,
  pub href: String,
  pub name: String,
  pub docs: Option<String>,
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
    .map(|section| ctx.ctx.hbs.render("namespace_section", &section).unwrap())
    .collect::<String>()
}

pub fn partition_nodes_by_kind(
  doc_nodes: &[DocNodeWithContext],
  flatten_namespaces: bool,
) -> IndexMap<DocNodeKind, Vec<DocNodeWithContext>> {
  fn partition_nodes_by_kind_inner(
    partitions: &mut IndexMap<DocNodeKind, Vec<DocNodeWithContext>>,
    doc_nodes: &[DocNodeWithContext],
    flatten_namespaces: bool,
  ) {
    for node in doc_nodes {
      if matches!(
        node.doc_node.kind,
        DocNodeKind::ModuleDoc | DocNodeKind::Import
      ) {
        continue;
      }

      if flatten_namespaces && node.doc_node.kind == DocNodeKind::Namespace {
        let namespace_def = node.doc_node.namespace_def.as_ref().unwrap();

        partition_nodes_by_kind_inner(
          partitions,
          &namespace_def
            .elements
            .iter()
            .map(|element| DocNodeWithContext {
              origin: node.origin.clone(),
              doc_node: element.clone(),
            })
            .collect::<Vec<_>>(),
          true,
        );
      }

      if let Some((node_kind, nodes)) =
        partitions.iter_mut().find(|(kind, nodes)| {
          nodes.iter().any(|n| {
            n.doc_node.name == node.doc_node.name
              && n.doc_node.kind != node.doc_node.kind
              && kind != &&node.doc_node.kind
          })
        })
      {
        assert_ne!(node_kind, &node.doc_node.kind,);

        nodes.push(node.clone());
      } else {
        let entry = partitions.entry(node.doc_node.kind).or_insert(vec![]);
        if !entry.iter().any(|n| n.doc_node.name == node.doc_node.name) {
          entry.push(node.clone());
        }
      }
    }
  }

  let mut partitions: IndexMap<DocNodeKind, Vec<DocNodeWithContext>> =
    IndexMap::default();

  partition_nodes_by_kind_inner(&mut partitions, doc_nodes, flatten_namespaces);

  for (_kind, nodes) in partitions.iter_mut() {
    nodes.sort_by(|node1, node2| {
      node1
        .doc_node
        .name
        .cmp(&node2.doc_node.name)
        .then_with(|| node1.doc_node.kind.cmp(&node2.doc_node.kind))
    });
  }

  partitions
    .sorted_by(|kind1, _nodes1, kind2, _nodes2| kind1.cmp(kind2))
    .collect()
}

pub fn partition_nodes_by_category(
  doc_nodes: &[DocNodeWithContext],
  flatten_namespaces: bool,
) -> IndexMap<String, Vec<DocNodeWithContext>> {
  fn partition_nodes_by_category_inner(
    partitions: &mut IndexMap<String, Vec<DocNodeWithContext>>,
    doc_nodes: &[DocNodeWithContext],
    flatten_namespaces: bool,
  ) {
    for node in doc_nodes {
      if matches!(
        node.doc_node.kind,
        DocNodeKind::ModuleDoc | DocNodeKind::Import
      ) {
        continue;
      }

      if flatten_namespaces && node.doc_node.kind == DocNodeKind::Namespace {
        let namespace_def = node.doc_node.namespace_def.as_ref().unwrap();
        partition_nodes_by_category_inner(
          partitions,
          &namespace_def
            .elements
            .iter()
            .map(|element| DocNodeWithContext {
              origin: node.origin.clone(),
              doc_node: element.clone(),
            })
            .collect::<Vec<_>>(),
          true,
        )
      }

      let category = node
        .doc_node
        .js_doc
        .tags
        .iter()
        .find_map(|tag| {
          if let crate::js_doc::JsDocTag::Category { doc } = tag {
            doc.as_ref().map(|doc| doc.trim().to_owned())
          } else {
            None
          }
        })
        .unwrap_or(String::from("Uncategorized"));

      let entry = partitions.entry(category).or_insert(vec![]);

      if !entry.iter().any(|n| {
        n.doc_node.name == node.doc_node.name
          && n.doc_node.kind == node.doc_node.kind
      }) {
        entry.push(node.clone());
      }
    }
  }

  let mut partitions: IndexMap<String, Vec<DocNodeWithContext>> =
    IndexMap::default();

  partition_nodes_by_category_inner(
    &mut partitions,
    doc_nodes,
    flatten_namespaces,
  );

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

  let mut grouped_nodes = HashMap::new();

  for node in doc_nodes {
    let entry = grouped_nodes
      .entry(node.doc_node.name.clone())
      .or_insert(vec![]);
    entry.push(node);
  }

  let nodes = grouped_nodes
    .iter()
    .map(|(name, nodes)| {
      // TODO: linking, tags

      let mut name = name.to_owned();

      let ns_parts = ctx.get_namespace_parts();
      if !ns_parts.is_empty() {
        name = format!("{}.{}", ns_parts.join("."), name);
      }

      let current_resolve = ctx.get_current_resolve();

      let docs =
        crate::html::jsdoc::render_docs_summary(ctx, &nodes[0].doc_node.js_doc);

      NamespaceSectionNodeCtx {
        doc_node_kind_ctx: nodes
          .iter()
          .map(|node| node.doc_node.kind.into())
          .collect(),
        origin: if ctx.ctx.single_file_mode {
          None
        } else {
          nodes[0].origin.as_deref().map(short_path_to_name)
        },
        href: (ctx.ctx.url_resolver)(
          current_resolve,
          crate::html::UrlResolveKind::Symbol {
            file: nodes[0]
              .origin
              .as_deref()
              .or_else(|| current_resolve.get_file())
              .unwrap(),
            symbol: &name,
          },
        ),
        name,
        docs,
      }
    })
    .collect::<Vec<_>>();

  NamespaceSectionRenderCtx {
    id: title_to_id(&kind_ctx.kind),
    title: kind_ctx.title_plural.to_string(),
    nodes,
  }
}
