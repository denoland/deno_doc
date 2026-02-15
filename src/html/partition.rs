use super::DocNodeWithContext;
use super::GenerateCtx;
use crate::js_doc::JsDocTag;
use crate::node::DocNodeDef;
use indexmap::IndexMap;
use std::borrow::Cow;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;

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
        .entry(node.get_qualified_name().to_string())
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
  let name_to_kind = RefCell::new(HashMap::<String, super::DocNodeKind>::new());

  let mut partitions = create_partitioner(
    ctx,
    doc_nodes,
    flatten_namespaces,
    &|partitions, node| {
      let qname = node.get_qualified_name();
      let mut index = name_to_kind.borrow_mut();

      if let Some(&existing_kind) = index.get(qname) {
        partitions
          .get_mut(&existing_kind)
          .unwrap()
          .push(node.clone());
      } else {
        index.insert(qname.to_string(), node.kind);
        partitions.entry(node.kind).or_default().push(node.clone());
      }
    },
  );

  sort_nodes(&mut partitions);

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
  let seen = RefCell::new(std::collections::HashSet::<(
    String,
    super::DocNodeKind,
  )>::new());

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

      let key = (node.get_qualified_name().to_string(), node.kind);
      if seen.borrow_mut().insert(key) {
        let entry = partitions.entry(category).or_default();
        entry.push(node.clone());
      }
    },
  );

  sort_nodes(&mut partitions);

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

fn sort_nodes<T>(partitions: &mut Partitions<T>) {
  for (_key, nodes) in partitions.iter_mut() {
    nodes.sort_by_cached_key(|node| {
      let is_deprecated = node
        .js_doc
        .tags
        .iter()
        .any(|tag| matches!(tag, JsDocTag::Deprecated { .. }));

      let priority = node
        .js_doc
        .tags
        .iter()
        .find_map(|tag| {
          if let JsDocTag::Priority { priority } = tag {
            Some(*priority)
          } else {
            None
          }
        })
        .unwrap_or(0);

      let qname_lower = node.get_qualified_name().to_ascii_lowercase();
      let qname = node.get_qualified_name().to_string();

      (
        is_deprecated,
        std::cmp::Reverse(priority),
        qname_lower,
        qname,
        node.kind,
      )
    });
  }
}
