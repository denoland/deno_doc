use crate::DocNodeKind;
use std::collections::HashMap;

mod class;
mod r#enum;
mod function;
mod interface;
mod namespace;
mod parameters;
mod r#type;
mod type_alias;
mod util;
mod variable;

const HTML_HEAD: &str = r#"
<html>
<head>
<style>
html {
  font-family: sans-serif;
}

thead,
tfoot {
  background-color: #3f87a6;
  color: #fff;
}

tbody {
  background-color: #e4f0f5;
}

caption {
  padding: 10px;
  caption-side: bottom;
}

table {
  border-collapse: collapse;
  border: 2px solid rgb(200, 200, 200);
  letter-spacing: 1px;
  font-family: sans-serif;
  font-size: 0.8rem;
}

td,
th {
  border: 1px solid rgb(190, 190, 190);
  padding: 5px 10px;
}

td {
  text-align: center;
}

a {
  color: inherit;
  text-decoration: inherit;
}


/* doc classes */

.doc_block_items > * + * {
  margin-top: 1.75rem; /* 28px */
}

.section_title {
  font-size: 0.875rem; /* 14px */
  line-height: 1.5rem; /* 24px */
  font-weight: 600;
  color: rgb(156 163 175);
  padding-top: 0.25rem; /* 4px */
  padding-bottom: 0.25rem; /* 4px */
}

.section {
  margin-top: 0.5rem; /* 8px */
}

.section > * + * {
  margin-top: 1.75rem; /* 28px */
}

.doc_item {
  /* TODO: group */
  position: relative;
}

.doc_item:hover .anchor {
  display: block;
}

.doc_entry {
  display: flex;
  justify-content: space-between;
}

.doc_entry_children {
  display: flex;
  align-items: center;
  overflow-wrap: break-word;
  gap: 0.5rem; /* 8px */
}

.anchor {
  /* TODO: group-hover:block */
  float: left;
  line-height: 1;
  display: none;
  color: rgb(75 85 99);
  margin-left: -18px;
  padding-right: 4px;
}

.anchor:hover {
  display: block;
}

</style>
</head>
<body>
"#;
const HTML_TAIL: &str = r#"
</body>
<script>
</script>
</html>"#;

pub fn generate(doc_nodes: &[crate::DocNode]) -> String {
  let mut parts = Vec::with_capacity(1024);
  parts.push(HTML_HEAD.to_string());

  let partitions = partition_nodes_by_kind(doc_nodes);

  for (kind, doc_nodes) in partitions.iter() {
    parts.push(format!(r#"<h1>{:?}</h1>"#, kind));
    parts.push(
      r#"<table><thead><tr><th scope="col">Name</th><th></th></tr></thead><tbody>"#
        .to_string(),
    );
    for doc_node in doc_nodes {
      parts.push(render_doc_node(doc_node));
    }
    parts.push("</tbody></table>".to_string());
  }

  let name_partitions = partition_nodes_by_name(doc_nodes);

  parts.push(r#"<div style="padding: 30px;">"#.to_string());
  for (name, doc_nodes) in name_partitions.iter() {
    parts.push(doc_block_title(name));
    parts.push(doc_block(doc_nodes));
    parts.push("<hr />".to_string());
  }
  parts.push("</div>".to_string());

  parts.push(HTML_TAIL.to_string());

  parts.join("")
}

fn partition_nodes_by_kind(
  doc_nodes: &[crate::DocNode],
) -> HashMap<DocNodeKind, Vec<crate::DocNode>> {
  let mut partitions = HashMap::default();

  for node in doc_nodes {
    partitions
      .entry(node.kind)
      .or_insert(vec![])
      .push(node.clone());
  }

  partitions
}

fn partition_nodes_by_name(
  doc_nodes: &[crate::DocNode],
) -> HashMap<String, Vec<crate::DocNode>> {
  let mut partitions = HashMap::default();

  for node in doc_nodes {
    partitions
      .entry(node.name.clone())
      .or_insert(vec![])
      .push(node.clone());
  }

  partitions
}

fn render_doc_node(doc_node: &crate::DocNode) -> String {
  let tpl = format!(
    r#"<tr>
    <th scope="row">{}</th>
    <td scope="row">{}:{}</td>
  </tr>"#,
    doc_node.name, doc_node.location.filename, doc_node.location.line
  );

  match doc_node.kind {
    DocNodeKind::ModuleDoc => {}
    DocNodeKind::Function => {}
    DocNodeKind::Variable => {}
    DocNodeKind::Class => {}
    DocNodeKind::Enum => {}
    DocNodeKind::Interface => {}
    DocNodeKind::TypeAlias => {}
    DocNodeKind::Namespace => {}
    DocNodeKind::Import => {}
  }

  tpl
}

fn doc_block(doc_nodes: &[crate::DocNode]) -> String {
  let mut content = String::new();
  let mut functions = vec![];

  for doc_node in doc_nodes {
    match doc_node.kind {
      DocNodeKind::ModuleDoc => {}
      DocNodeKind::Function => functions.push(doc_node),
      DocNodeKind::Variable => {
        content.push_str(&variable::render_variable(doc_node))
      }
      DocNodeKind::Class => {}
      DocNodeKind::Enum => content.push_str(&r#enum::render_enum(doc_node)),
      DocNodeKind::Interface => {}
      DocNodeKind::TypeAlias => {
        content.push_str(&type_alias::render_type_alias(doc_node))
      }
      DocNodeKind::Namespace => {}
      DocNodeKind::Import => {}
    };
  }

  if !functions.is_empty() {
    // TODO: functions
  }

  format!("<div>{content}</div>")
}

fn doc_block_title(name: &str) -> String {
  format!(
    r#"<div class="doc_block_title"><div><span class="font-bold">{name}</span></div></div>"#
  )
}
