use super::DocNodeWithContext;
use super::ShortPath;
use crate::js_doc::JsDocTag;
use crate::DocNodeKind;
use indexmap::IndexMap;
use std::cmp::Ordering;
use std::rc::Rc;

pub type Partitions<T> = IndexMap<T, Vec<DocNodeWithContext>>;

fn create_partitioner<T, F>(
  doc_nodes: &[DocNodeWithContext],
  flatten_namespaces: bool,
  process: &F,
) -> Partitions<T>
where
  F: Fn(&mut IndexMap<T, Vec<DocNodeWithContext>>, &DocNodeWithContext),
{
  fn partitioner_inner<T, F>(
    partitions: &mut Partitions<T>,
    doc_nodes: &[DocNodeWithContext],
    flatten_namespaces: bool,
    process: &F,
  ) where
    F: Fn(&mut IndexMap<T, Vec<DocNodeWithContext>>, &DocNodeWithContext),
  {
    for node in doc_nodes {
      if matches!(node.kind, DocNodeKind::ModuleDoc | DocNodeKind::Import) {
        continue;
      }

      if flatten_namespaces && node.kind == DocNodeKind::Namespace {
        let namespace_def = node.namespace_def.as_ref().unwrap();
        let ns_qualifiers = Rc::new(node.sub_qualifier());

        partitioner_inner(
          partitions,
          &namespace_def
            .elements
            .iter()
            .map(|element| {
              node
                .create_namespace_child(element.clone(), ns_qualifiers.clone())
            })
            .collect::<Vec<_>>(),
          true,
          process,
        );
      }

      process(partitions, node);
    }
  }

  let mut partitions = IndexMap::default();

  partitioner_inner(&mut partitions, doc_nodes, flatten_namespaces, process);

  partitions
}

pub fn partition_nodes_by_name(
  doc_nodes: &[DocNodeWithContext],
  flatten_namespaces: bool,
) -> Partitions<String> {
  let mut partitions =
    create_partitioner(doc_nodes, flatten_namespaces, &|partitions, node| {
      partitions
        .entry(node.get_qualified_name())
        .or_default()
        .push(node.clone());
    });

  for val in partitions.values_mut() {
    val.sort_by_key(|n| n.kind);
  }

  partitions.sort_keys();

  partitions
}

pub fn partition_nodes_by_kind(
  doc_nodes: &[DocNodeWithContext],
  flatten_namespaces: bool,
) -> Partitions<String> {
  let mut partitions =
    create_partitioner(doc_nodes, flatten_namespaces, &|partitions, node| {
      if let Some((node_kind, nodes)) =
        partitions.iter_mut().find(|(kind, nodes)| {
          nodes.iter().any(|n| {
            n.get_qualified_name() == node.get_qualified_name()
              && n.kind_with_drilldown != node.kind_with_drilldown
              && kind != &&node.kind_with_drilldown
          })
        })
      {
        assert_ne!(node_kind, &node.kind_with_drilldown);

        nodes.push(node.clone());
      } else {
        let entry = partitions.entry(node.kind_with_drilldown).or_default();
        if !entry
          .iter()
          .any(|n| n.get_qualified_name() == node.get_qualified_name())
        {
          entry.push(node.clone());
        }
      }
    });

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

pub fn partition_nodes_by_category(
  doc_nodes: &[DocNodeWithContext],
  flatten_namespaces: bool,
) -> Partitions<String> {
  let mut partitions =
    create_partitioner(doc_nodes, flatten_namespaces, &|partitions, node| {
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
    });

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

pub fn partition_nodes_by_entrypoint(
  doc_nodes: &[DocNodeWithContext],
  flatten_namespaces: bool,
) -> Partitions<Rc<ShortPath>> {
  let mut partitions =
    create_partitioner(doc_nodes, flatten_namespaces, &|partitions, node| {
      let entry = partitions.entry(node.origin.clone()).or_default();

      if !entry.iter().any(|n| {
        n.get_qualified_name() == node.get_qualified_name()
          && n.kind == node.kind
      }) {
        entry.push(node.clone());
      }
    });

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
    .then_with(|| node1.kind.cmp(&node2.kind))
}
