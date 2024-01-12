use crate::html::render_context::RenderContext;
use crate::html::short_path_to_name;
use crate::html::util::*;
use crate::html::DocNodeWithContext;
use crate::js_doc::JsDocTag;
use crate::DocNodeKind;
use indexmap::IndexMap;
use serde::Serialize;
use std::cmp::Ordering;
use std::collections::HashSet;

pub fn render_namespace(
  ctx: &RenderContext,
  partitions: IndexMap<DocNodeKind, Vec<DocNodeWithContext>>,
) -> Vec<SectionCtx> {
  partitions
    .into_iter()
    .map(|(kind, doc_nodes)| {
      get_namespace_section_render_ctx(ctx, kind, doc_nodes)
    })
    .collect()
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
      let node1_is_deprecated = node1
        .doc_node
        .js_doc
        .tags
        .iter()
        .any(|tag| matches!(tag, JsDocTag::Deprecated { .. }));
      let node2_is_deprecated = node2
        .doc_node
        .js_doc
        .tags
        .iter()
        .any(|tag| matches!(tag, JsDocTag::Deprecated { .. }));

      (!node2_is_deprecated)
        .cmp(&!node1_is_deprecated)
        .then_with(|| {
          node1
            .doc_node
            .name
            .cmp(&node2.doc_node.name)
            .then_with(|| node1.doc_node.kind.cmp(&node2.doc_node.kind))
        })
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
  kind: DocNodeKind,
  doc_nodes: Vec<DocNodeWithContext>,
) -> SectionCtx {
  let kind_ctx = super::super::util::DocNodeKindCtx::from(kind);

  let mut grouped_nodes = IndexMap::new();

  for node in doc_nodes {
    let entry = grouped_nodes
      .entry(node.doc_node.name.clone())
      .or_insert(vec![]);
    entry.push(node);
  }

  let nodes = grouped_nodes
    .into_iter()
    .map(|(name, nodes)| NamespaceNodeCtx::new(ctx, name, nodes))
    .collect::<Vec<_>>();

  SectionCtx {
    title: kind_ctx.title_plural,
    content: SectionContentCtx::NamespaceSection(nodes),
  }
}

#[derive(Debug, Serialize, Clone)]
pub struct NamespaceNodeCtx {
  pub tags: HashSet<Tag>,
  pub doc_node_kind_ctx: Vec<DocNodeKindCtx>,
  pub origin: Option<String>,
  pub href: String,
  pub name: String,
  pub docs: Option<String>,
  pub deprecated: bool,
}

impl NamespaceNodeCtx {
  fn new(
    ctx: &RenderContext,
    mut name: String,
    nodes: Vec<DocNodeWithContext>,
  ) -> Self {
    // TODO: linking

    let ns_parts = ctx.get_namespace_parts();
    if !ns_parts.is_empty() {
      name = format!("{}.{}", ns_parts.join("."), name);
    }

    let current_resolve = ctx.get_current_resolve();

    let docs =
      crate::html::jsdoc::render_docs_summary(ctx, &nodes[0].doc_node.js_doc);

    let tags = Tag::from_js_doc(&nodes[0].doc_node.js_doc);

    NamespaceNodeCtx {
      tags,
      doc_node_kind_ctx: nodes
        .iter()
        .map(|node| node.doc_node.kind.into())
        .collect(),
      origin: if ctx.ctx.single_file_mode {
        None
      } else {
        nodes[0].origin.as_deref().map(short_path_to_name)
      },
      href: ctx.ctx.href_resolver.resolve_path(
        current_resolve,
        UrlResolveKind::Symbol {
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
      deprecated: all_deprecated(
        &nodes.iter().map(|node| &node.doc_node).collect::<Vec<_>>(),
      ),
    }
  }
}
