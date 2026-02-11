use super::DocNodeWithContext;
use super::GenerateCtx;
use crate::js_doc::JsDocTag;
use crate::node::DocNodeDef;
use indexmap::IndexMap;
use std::borrow::Cow;
use std::cmp::Ordering;

pub type Partitions<T> = IndexMap<T, Vec<DocNodeWithContext>>;

fn create_partitioner<'a, T, F>(
  ctx: &GenerateCtx,
  doc_nodes: impl Iterator<Item = Cow<'a, DocNodeWithContext>> + 'a,
  flatten_namespaces: bool,
  process: &F,
) -> Partitions<T>
where
  F: Fn(&mut IndexMap<T, Vec<DocNodeWithContext>>, &DocNodeWithContext),
{
  fn partitioner_inner<'a, T, F>(
    ctx: &GenerateCtx,
    partitions: &mut Partitions<T>,
    parent_node: Option<&DocNodeWithContext>,
    doc_nodes: Box<dyn Iterator<Item = Cow<'a, DocNodeWithContext>> + 'a>,
    flatten_namespaces: bool,
    process: &F,
  ) where
    F: Fn(&mut IndexMap<T, Vec<DocNodeWithContext>>, &DocNodeWithContext),
  {
    for node in doc_nodes {
      if matches!(node.def, DocNodeDef::ModuleDoc | DocNodeDef::Import { .. }) {
        continue;
      }

      if flatten_namespaces && matches!(node.def, DocNodeDef::Namespace { .. })
      {
        partitioner_inner(
          ctx,
          partitions,
          Some(&node),
          Box::new(
            node
              .namespace_children
              .as_ref()
              .unwrap()
              .iter()
              .map(Cow::Borrowed),
          ),
          true,
          process,
        );
      }

      if let Some(reference) = node.reference_def() {
        partitioner_inner(
          ctx,
          partitions,
          parent_node,
          Box::new(ctx.resolve_reference(parent_node, &reference.target)),
          flatten_namespaces,
          process,
        )
      } else {
        process(partitions, &node);
      }
    }
  }

  let mut partitions = IndexMap::default();

  partitioner_inner(
    ctx,
    &mut partitions,
    None,
    Box::new(doc_nodes),
    flatten_namespaces,
    process,
  );

  partitions
}

pub fn partition_nodes_by_name<'a>(
  ctx: &GenerateCtx,
  doc_nodes: impl Iterator<Item = Cow<'a, DocNodeWithContext>> + 'a,
  flatten_namespaces: bool,
) -> Partitions<String> {
  let mut partitions = create_partitioner(
    ctx,
    doc_nodes,
    flatten_namespaces,
    &|partitions, node| {
      partitions
        .entry(node.get_qualified_name())
        .or_default()
        .push(node.clone());
    },
  );

  for val in partitions.values_mut() {
    val.sort_by_key(|n| n.kind);
  }

  partitions.sort_keys();

  partitions
}

pub fn partition_nodes_by_kind<'a>(
  ctx: &GenerateCtx,
  doc_nodes: impl Iterator<Item = Cow<'a, DocNodeWithContext>> + 'a,
  flatten_namespaces: bool,
) -> Partitions<String> {
  let mut partitions = create_partitioner(
    ctx,
    doc_nodes,
    flatten_namespaces,
    &|partitions, node| {
      let maybe_nodes = partitions.values_mut().find(|nodes| {
        nodes
          .iter()
          .any(|n| n.get_qualified_name() == node.get_qualified_name())
      });

      if let Some(nodes) = maybe_nodes {
        nodes.push(node.clone());
      } else {
        let entry = partitions.entry(node.kind).or_default();
        entry.push(node.clone());
      }
    },
  );

  for (_kind, nodes) in partitions.iter_mut() {
    nodes.sort_by(compare_node);
  }

  partitions
    .sorted_by(|kind1, _nodes1, kind2, _nodes2| kind1.cmp(kind2))
    .map(|(kind, nodes)| {
      (
        super::DocNodeKindCtx::from(kind).title_plural.to_string(),
        nodes,
      )
    })
    .collect()
}

pub fn partition_nodes_by_category<'a>(
  ctx: &GenerateCtx,
  doc_nodes: impl Iterator<Item = Cow<'a, DocNodeWithContext>> + 'a,
  flatten_namespaces: bool,
) -> Partitions<String> {
  let mut partitions = create_partitioner(
    ctx,
    doc_nodes,
    flatten_namespaces,
    &|partitions, node| {
      let category = node
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
        n.get_qualified_name() == node.get_qualified_name()
          && n.kind == node.kind
      }) {
        entry.push(node.clone());
      }
    },
  );

  for (_kind, nodes) in partitions.iter_mut() {
    nodes.sort_by(compare_node);
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

fn compare_node(
  node1: &DocNodeWithContext,
  node2: &DocNodeWithContext,
) -> Ordering {
  let node1_is_deprecated = node1
    .js_doc
    .tags
    .iter()
    .any(|tag| matches!(tag, JsDocTag::Deprecated { .. }));
  let node2_is_deprecated = node2
    .js_doc
    .tags
    .iter()
    .any(|tag| matches!(tag, JsDocTag::Deprecated { .. }));

  (!node2_is_deprecated)
    .cmp(&!node1_is_deprecated)
    .then_with(|| {
      let p1 = node1.js_doc.tags.iter().find_map(|tag| {
        if let JsDocTag::Priority { priority } = tag {
          Some(priority)
        } else {
          None
        }
      });
      let p2 = node2.js_doc.tags.iter().find_map(|tag| {
        if let JsDocTag::Priority { priority } = tag {
          Some(priority)
        } else {
          None
        }
      });

      match (p1, p2) {
        (Some(p1), Some(p2)) => p1.cmp(p2),
        (Some(p1), None) if p1 == &0 => Ordering::Equal,
        (Some(p1), None) if p1.is_negative() => Ordering::Less,
        (Some(_), None) => Ordering::Greater,
        (None, Some(p2)) if p2 == &0 => Ordering::Equal,
        (None, Some(p2)) if p2.is_negative() => Ordering::Greater,
        (None, Some(_)) => Ordering::Less,
        (None, None) => Ordering::Equal,
      }
      .reverse()
    })
    .then_with(|| {
      node1
        .get_qualified_name()
        .to_ascii_lowercase()
        .cmp(&node2.get_qualified_name().to_ascii_lowercase())
    })
    .then_with(|| node1.get_qualified_name().cmp(&node2.get_qualified_name()))
    .then_with(|| node1.kind.cmp(&node2.kind))
}
