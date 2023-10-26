use crate::html::util::*;
use crate::DocNodeKind;
use indexmap::IndexMap;
use std::fmt::Write;

pub fn render_namespace(
  doc_node: &crate::DocNode,
  context: &RenderContext,
) -> String {
  let namespace_def = doc_node.namespace_def.as_ref().unwrap();

  let partitions = partition_nodes_by_kind(&namespace_def.elements);

  format!(
    r#"<div class="doc_block_items">{}{}</div>"#,
    super::jsdoc::render_docs(&doc_node.js_doc, true, false),
    doc_node_kind_sections(partitions, context)
  )
}

pub fn partition_nodes_by_kind(
  doc_nodes: &[crate::DocNode],
) -> IndexMap<DocNodeKind, Vec<crate::DocNode>> {
  let mut partitions = IndexMap::default();

  for node in doc_nodes {
    partitions
      .entry(node.kind)
      .or_insert(vec![])
      .push(node.clone());
  }

  for (_kind, nodes) in partitions.iter_mut() {
    nodes.sort_by_key(|n| n.name.to_string());
  }

  partitions
}

fn doc_node_kind_sections(
  partitions: IndexMap<DocNodeKind, Vec<crate::DocNode>>,
  context: &RenderContext,
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

    content.push_str(&symbol_section(title, doc_nodes, context))
  }

  content
}

fn symbol_section(
  title: &str,
  doc_nodes: Vec<crate::DocNode>,
  context: &RenderContext,
) -> String {
  let content =
    doc_nodes
      .into_iter()
      .fold(String::new(), |mut output, doc_node| {
        // TODO: linking, tags

        let name = context.namespace.as_ref().map_or_else(
          || doc_node.name.clone(),
          |namespace| format!("{namespace}.{}", doc_node.name),
        );

        write!(
          output,
          r#"<tr>
      <td class="symbol_section_symbol">
        <div>
          {}
          <a href="">{name}</a>
        </div>
      </td>
      <td class="symbol_section_doc">
      {}
      </td>
      </tr>"#,
          doc_node_kind_icon(doc_node.kind),
          super::jsdoc::render_docs(&doc_node.js_doc, false, true),
        )
        .unwrap();
        output
      });

  format!(
    r#"<div>{}<table class="symbol_section">{content}</table></div>"#,
    section_title(title)
  )
}
