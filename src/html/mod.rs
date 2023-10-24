use crate::DocNodeKind;
use std::collections::HashMap;

mod class;
mod r#enum;
mod function;
mod interface;
mod namespace;
mod parameters;
mod type_alias;
mod variable;

const HTML_HEAD: &str = r#"
<html>
<head>
<style>
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
