use super::DocNodeWithContext;
use super::GenerateCtx;
use super::ShortPath;
use crate::js_doc::JsDocTag;
use crate::DocNode;
use crate::DocNodeKind;
use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::rc::Rc;

pub fn partition_nodes_by_name(
  doc_nodes: &[DocNode],
) -> IndexMap<String, Vec<DocNode>> {
  let mut partitions = IndexMap::default();

  for node in doc_nodes {
    if matches!(node.kind, DocNodeKind::ModuleDoc | DocNodeKind::Import)
      || node.declaration_kind == crate::node::DeclarationKind::Private
    {
      continue;
    }

    partitions
      .entry(node.get_name().to_string())
      .or_insert(vec![])
      .push(node.clone());
  }

  for val in partitions.values_mut() {
    val.sort_by_key(|n| n.kind);
  }

  partitions.sort_by(|k1, _v1, k2, _v2| k1.cmp(k2));

  partitions
}

pub fn partition_nodes_by_kind<'a>(
  doc_nodes: &[DocNodeWithContext<'a>],
  flatten_namespaces: bool,
) -> IndexMap<DocNodeKind, Vec<DocNodeWithContext<'a>>> {
  fn partition_nodes_by_kind_inner<'b>(
    partitions: &mut IndexMap<DocNodeKind, Vec<DocNodeWithContext<'b>>>,
    doc_nodes: &[DocNodeWithContext<'b>],
    flatten_namespaces: bool,
  ) {
    for node in doc_nodes {
      if matches!(
        node.doc_node.kind,
        DocNodeKind::ModuleDoc | DocNodeKind::Import
      ) || node.doc_node.declaration_kind
        == crate::node::DeclarationKind::Private
      {
        continue;
      }

      if flatten_namespaces && node.doc_node.kind == DocNodeKind::Namespace {
        let namespace_def = node.doc_node.namespace_def.as_ref().unwrap();
        let mut namespace = (*node.ns_qualifiers).clone();
        namespace.push(node.doc_node.get_name().to_string());

        let ns_qualifiers = std::rc::Rc::new(namespace);

        partition_nodes_by_kind_inner(
          partitions,
          &namespace_def
            .elements
            .iter()
            .map(|element| DocNodeWithContext {
              origin: node.origin.clone(),
              ns_qualifiers: ns_qualifiers.clone(),
              doc_node: element,
            })
            .collect::<Vec<_>>(),
          true,
        );
      }

      if let Some((node_kind, nodes)) =
        partitions.iter_mut().find(|(kind, nodes)| {
          nodes.iter().any(|n| {
            n.doc_node.get_name() == node.doc_node.get_name()
              && n.doc_node.kind != node.doc_node.kind
              && kind != &&node.doc_node.kind
          })
        })
      {
        assert_ne!(node_kind, &node.doc_node.kind,);

        nodes.push(node.clone());
      } else {
        let entry = partitions.entry(node.doc_node.kind).or_default();
        if !entry
          .iter()
          .any(|n| n.doc_node.get_name() == node.doc_node.get_name())
        {
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
            .get_name()
            .cmp(node2.doc_node.get_name())
            .then_with(|| node1.doc_node.kind.cmp(&node2.doc_node.kind))
        })
    });
  }

  partitions
    .sorted_by(|kind1, _nodes1, kind2, _nodes2| kind1.cmp(kind2))
    .collect()
}

pub fn partition_nodes_by_category<'a>(
  doc_nodes: &[DocNodeWithContext<'a>],
  flatten_namespaces: bool,
) -> IndexMap<String, Vec<DocNodeWithContext<'a>>> {
  fn partition_nodes_by_category_inner<'b>(
    partitions: &mut IndexMap<String, Vec<DocNodeWithContext<'b>>>,
    doc_nodes: &[DocNodeWithContext<'b>],
    flatten_namespaces: bool,
  ) {
    for node in doc_nodes {
      if matches!(
        node.doc_node.kind,
        DocNodeKind::ModuleDoc | DocNodeKind::Import
      ) || node.doc_node.declaration_kind
        == crate::node::DeclarationKind::Private
      {
        continue;
      }

      if flatten_namespaces && node.doc_node.kind == DocNodeKind::Namespace {
        let namespace_def = node.doc_node.namespace_def.as_ref().unwrap();
        let mut namespace = (*node.ns_qualifiers).clone();
        namespace.push(node.doc_node.get_name().to_string());

        let ns_qualifiers = std::rc::Rc::new(namespace);

        partition_nodes_by_category_inner(
          partitions,
          &namespace_def
            .elements
            .iter()
            .map(|element| DocNodeWithContext {
              origin: node.origin.clone(),
              ns_qualifiers: ns_qualifiers.clone(),
              doc_node: element,
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
          if let JsDocTag::Category { doc } = tag {
            Some(doc.trim().to_owned())
          } else {
            None
          }
        })
        .unwrap_or(String::from("Uncategorized"));

      let entry = partitions.entry(category).or_default();

      if !entry.iter().any(|n| {
        n.doc_node.get_name() == node.doc_node.get_name()
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
    nodes.sort_by_key(|n| n.doc_node.get_name());
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

pub fn get_partitions_for_file<'a>(
  ctx: &GenerateCtx,
  doc_nodes: &'a [DocNode],
  short_path: Cow<'a, ShortPath>,
) -> IndexMap<String, Vec<DocNodeWithContext<'a>>> {
  let doc_nodes_with_context = doc_nodes
    .iter()
    .map(|node| DocNodeWithContext {
      doc_node: node,
      origin: Some(short_path.clone()),
      ns_qualifiers: Rc::new(vec![]),
    })
    .collect::<Vec<_>>();

  let categories = partition_nodes_by_category(
    &doc_nodes_with_context,
    ctx.sidebar_flatten_namespaces,
  );

  if categories.len() == 1 && categories.contains_key("Uncategorized") {
    partition_nodes_by_kind(
      &doc_nodes_with_context,
      ctx.sidebar_flatten_namespaces,
    )
    .into_iter()
    .map(|(kind, nodes)| {
      let doc_node_kind_ctx: super::DocNodeKindCtx = kind.into();
      (doc_node_kind_ctx.title.to_string(), nodes)
    })
    .collect()
  } else {
    categories
  }
}

pub fn get_partitions_for_main_entrypoint<'a>(
  ctx: &GenerateCtx,
  doc_nodes_by_url: &'a IndexMap<ModuleSpecifier, Vec<DocNode>>,
) -> IndexMap<String, Vec<DocNodeWithContext<'a>>> {
  let doc_nodes = ctx
    .main_entrypoint
    .as_ref()
    .and_then(|main_entrypoint| doc_nodes_by_url.get(main_entrypoint));

  if let Some(doc_nodes) = doc_nodes {
    get_partitions_for_file(
      ctx,
      doc_nodes,
      Cow::Owned(ctx.url_to_short_path(ctx.main_entrypoint.as_ref().unwrap())),
    )
  } else {
    Default::default()
  }
}
