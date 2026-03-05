use super::DocNodeWithContext;
use super::GenerateCtx;
use crate::DeclarationDef;
use crate::js_doc::JsDocTag;
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
    'outer: for node in doc_nodes {
      for decl in &node.declarations {
        if matches!(decl.def, DeclarationDef::Import(..)) {
          continue 'outer;
        }

        if flatten_namespaces
          && matches!(decl.def, DeclarationDef::Namespace(..))
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

        if let Some(reference) = decl.reference_def() {
          partitioner_inner(
            ctx,
            partitions,
            parent_node,
            Box::new(ctx.resolve_reference(parent_node, &reference.target)),
            flatten_namespaces,
            process,
          );
          // hack until reference nodes are separate from normal symbols
          continue 'outer;
        }
      }

      process(partitions, &node);
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

pub fn partition_nodes_by_kind<'a>(
  ctx: &GenerateCtx,
  doc_nodes: impl Iterator<Item = Cow<'a, DocNodeWithContext>> + 'a,
  flatten_namespaces: bool,
) -> Partitions<String> {
  let name_to_kind =
    RefCell::new(HashMap::<String, crate::node::DocNodeKind>::new());

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
        let kind = node.declarations[0].def.to_kind();
        index.insert(qname.to_string(), kind);
        partitions.entry(kind).or_default().push(node.clone());
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
  let mut partitions = create_partitioner(
    ctx,
    doc_nodes,
    flatten_namespaces,
    &|partitions, node| {
      let category = node
        .declarations
        .iter()
        .flat_map(|decl| decl.js_doc.tags.iter())
        .find_map(|tag| {
          if let JsDocTag::Category { doc } = tag {
            Some(doc.trim().to_owned())
          } else {
            None
          }
        })
        .unwrap_or(String::from("Uncategorized"));

      let entry = partitions.entry(category).or_default();
      entry.push(node.clone());
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
      let mut tags =
        node.declarations.iter().flat_map(|d| d.js_doc.tags.iter());

      let is_deprecated = tags
        .clone()
        .any(|tag| matches!(tag, JsDocTag::Deprecated { .. }));

      let priority = tags
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
      let kind = node.declarations.first().map(|d| d.def.to_kind());

      (
        is_deprecated,
        std::cmp::Reverse(priority),
        qname_lower,
        qname,
        kind,
      )
    });
  }
}

pub fn flatten_namespace<'a>(
  ctx: &GenerateCtx,
  doc_nodes: impl Iterator<Item = Cow<'a, DocNodeWithContext>> + 'a,
) -> Vec<Cow<'a, DocNodeWithContext>> {
  fn partitioner_inner<'a>(
    ctx: &GenerateCtx,
    out: &mut Vec<Cow<'a, DocNodeWithContext>>,
    parent_node: Option<&DocNodeWithContext>,
    doc_nodes: Box<dyn Iterator<Item = Cow<'a, DocNodeWithContext>> + 'a>,
  ) {
    let nodes: Vec<_> = doc_nodes.collect();

    'outer: for node in &nodes {
      for decl in &node.declarations {
        if matches!(decl.def, DeclarationDef::Import(..)) {
          continue 'outer;
        }

        if matches!(decl.def, DeclarationDef::Namespace(..)) {
          let children: Vec<_> =
            node.namespace_children.as_ref().unwrap().clone();
          partitioner_inner(
            ctx,
            out,
            Some(&**node),
            Box::new(children.into_iter().map(Cow::Owned)),
          );
        }

        if let Some(reference) = decl.reference_def() {
          let resolved: Vec<_> = ctx
            .resolve_reference(parent_node, &reference.target)
            .map(|c| c.into_owned())
            .collect();
          partitioner_inner(
            ctx,
            out,
            parent_node,
            Box::new(resolved.into_iter().map(Cow::Owned)),
          );
          // hack until reference nodes are separate from normal symbols
          continue 'outer;
        }
      }

      out.push((*node).clone());
    }
  }

  let mut out = vec![];

  partitioner_inner(ctx, &mut out, None, Box::new(doc_nodes));

  out
}
