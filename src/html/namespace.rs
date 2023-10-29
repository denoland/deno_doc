use crate::html::util::*;
use crate::DocNodeKind;
use indexmap::IndexMap;
use std::fmt::Write;

pub fn render_namespace(
  doc_node: &crate::DocNode,
  ctx: &RenderContext,
) -> String {
  let namespace_def = doc_node.namespace_def.as_ref().unwrap();

  let partitions = partition_nodes_by_kind(&namespace_def.elements);

  format!(
    r#"<div class="doc_block_items">{}{}</div>"#,
    super::jsdoc::render_docs(&doc_node.js_doc, true, false, ctx),
    doc_node_kind_sections(&partitions, ctx)
  )
}

fn partition_nodes_by_kind_inner(
  doc_nodes: &[crate::DocNode],
  dedup_overloads: bool,
) -> IndexMap<DocNodeKind, Vec<crate::DocNode>> {
  let mut partitions = IndexMap::default();

  for node in doc_nodes {
    if node.kind == DocNodeKind::ModuleDoc {
      continue;
    }

    let entry = partitions.entry(node.kind).or_insert(vec![]);

    if !dedup_overloads {
      entry.push(node.clone());
    } else if !entry.iter().any(|n| n.name == node.name) {
      entry.push(node.clone());
    }
  }

  for (_kind, nodes) in partitions.iter_mut() {
    nodes.sort_by_key(|n| n.name.to_string());
  }

  partitions
}

pub fn partition_nodes_by_kind_with_dedup(
  doc_nodes: &[crate::DocNode],
) -> IndexMap<DocNodeKind, Vec<crate::DocNode>> {
  partition_nodes_by_kind_inner(doc_nodes, true)
}

pub fn partition_nodes_by_kind(
  doc_nodes: &[crate::DocNode],
) -> IndexMap<DocNodeKind, Vec<crate::DocNode>> {
  partition_nodes_by_kind_inner(doc_nodes, true)
}

pub fn doc_node_kind_sections(
  partitions: &IndexMap<DocNodeKind, Vec<crate::DocNode>>,
  ctx: &RenderContext,
) -> String {
  let mut content = String::new();

  for (kind, doc_nodes) in partitions {
    let title = match kind {
      DocNodeKind::Function => "Functions",
      DocNodeKind::Variable => "Variables",
      DocNodeKind::Class => "Classes",
      DocNodeKind::Enum => "Enums",
      DocNodeKind::Interface => "Interfaces",
      DocNodeKind::TypeAlias => "Type Aliases",
      DocNodeKind::Namespace => "Namespaces",
      DocNodeKind::ModuleDoc | DocNodeKind::Import => unimplemented!(),
    };

    content.push_str(&symbol_section(title, doc_nodes, ctx))
  }

  content
}

fn symbol_section(
  title: &str,
  doc_nodes: &[crate::DocNode],
  ctx: &RenderContext,
) -> String {
  let content =
    doc_nodes
      .into_iter()
      .fold(String::new(), |mut output, doc_node| {
        // TODO: linking, tags

        let (name, path) = ctx.namespace.as_ref().map_or_else(
          || (doc_node.name.clone(), doc_node.name.clone()),
          |namespace| {
            (
              format!("{namespace}.{}", doc_node.name),
              format!(
                "{}/{}",
                namespace
                  .rsplit_once('.')
                  .map_or(&**namespace, |(_prev, current)| current),
                doc_node.name
              ),
            )
          },
        );

        write!(
          output,
          r#"<tr>
      <td class="symbol_section_symbol">
        <div>
          {}
          <a href="./{path}.html">{name}</a>
        </div>
      </td>
      <td class="symbol_section_doc">
      {}
      </td>
      </tr>"#,
          doc_node_kind_icon(doc_node.kind),
          super::jsdoc::render_docs(&doc_node.js_doc, false, true, ctx),
        )
        .unwrap();
        output
      });

  format!(
    r#"<div>{}<table class="symbol_section">{content}</table></div>"#,
    section_title(title)
  )
}
