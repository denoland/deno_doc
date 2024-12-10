use super::ShortPath;
use super::{DocNodeWithContext, GenerateCtx};
use crate::js_doc::JsDocTag;
use crate::DocNodeKind;
use indexmap::IndexMap;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::rc::Rc;

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
    doc_nodes: Box<dyn Iterator<Item = Cow<'a, DocNodeWithContext>> + 'a>,
    flatten_namespaces: bool,
    process: &F,
  ) where
    F: Fn(&mut IndexMap<T, Vec<DocNodeWithContext>>, &DocNodeWithContext),
  {
    for node in doc_nodes {
      if matches!(node.kind(), DocNodeKind::ModuleDoc | DocNodeKind::Import) {
        continue;
      }

      if flatten_namespaces && node.kind() == DocNodeKind::Namespace {
        partitioner_inner(
          ctx,
          partitions,
          Box::new(node.namespace_children.as_ref().unwrap().iter().flat_map(
            |node| {
              if let Some(reference_def) = node.reference_def() {
                ctx
                  .resolve_reference(&reference_def.target)
                  .map(Cow::Borrowed)
                  .collect()
              } else {
                vec![Cow::Borrowed(node)]
              }
            },
          )),
          true,
          process,
        );
      }

      if node.kind() == DocNodeKind::Reference {
        partitioner_inner(
          ctx,
          partitions,
          Box::new(ctx.resolve_reference(&node.location).map(Cow::Borrowed)),
          false,
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
    val.sort_by_key(|n| n.kind());
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
        let entry = partitions.entry(node.kind_with_drilldown).or_default();
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
          && n.kind() == node.kind()
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

pub fn partition_nodes_by_entrypoint<'a>(
  ctx: &GenerateCtx,
  doc_nodes: impl Iterator<Item = Cow<'a, DocNodeWithContext>> + 'a,
  flatten_namespaces: bool,
) -> Partitions<Rc<ShortPath>> {
  let mut partitions = create_partitioner(
    ctx,
    doc_nodes,
    flatten_namespaces,
    &|partitions, node| {
      let entry = partitions.entry(node.origin.clone()).or_default();

      entry.push(node.clone());
    },
  );

  for (_file, nodes) in partitions.iter_mut() {
    nodes.sort_by(compare_node);
  }

  partitions.sort_keys();

  partitions
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
      node1
        .get_qualified_name()
        .to_ascii_lowercase()
        .cmp(&node2.get_qualified_name().to_ascii_lowercase())
    })
    .then_with(|| node1.get_qualified_name().cmp(&node2.get_qualified_name()))
    .then_with(|| node1.kind().cmp(&node2.kind()))
}
