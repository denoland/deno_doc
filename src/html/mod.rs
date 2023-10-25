use crate::DocNodeKind;
use std::collections::HashMap;

mod class;
mod r#enum;
mod function;
mod interface;
mod jsdoc;
mod namespace;
mod parameters;
mod symbol;
mod type_alias;
mod types;
mod util;
mod variable;

pub const STYLESHEET: &str = include_str!("./styles.css");
pub const STYLESHEET_FILENAME: &str = "styles.css";
// TODO(bartlomieju): reference STYLESHEET_FILENAME below
const HTML_HEAD: &str = r#"
<html>
<head>
  <link rel="stylesheet" href="./styles.css">
</head>
<body>
"#;
const HTML_TAIL: &str = r#"
</body>
<script>
</script>
</html>"#;

pub fn generate(doc_nodes: &[crate::DocNode]) -> HashMap<String, String> {
  let mut files = HashMap::new();

  let mut sidepanel = String::with_capacity(1024);
  // FIXME(bartlomieju): functions can have duplicates because of overloads
  let mut partitions = partition_nodes_by_kind(doc_nodes);

  sidepanel.push_str(r#"<div>"#);
  for (kind, doc_nodes) in partitions.iter_mut() {
    sidepanel.push_str(&format!(r#"<h3>{:?}</h3><ul>"#, kind));

    doc_nodes.sort_by_key(|n| n.name.to_string());
    for doc_node in doc_nodes {
      sidepanel.push_str(&format!(
        r##"<li><a href="./{}.html">{}</a></li>"##,
        doc_node.name, doc_node.name
      ));
    }

    sidepanel.push_str(r#"</ul>"#);
  }
  sidepanel.push_str(r#"</div>"#);

  let name_partitions = partition_nodes_by_name(doc_nodes);

  for (name, doc_nodes) in name_partitions.iter() {
    files.insert(name.to_string(), format!(
      r##"{HTML_HEAD}<div style="display: flex;">{sidepanel}<div style="padding: 30px;">{}</div></div>{HTML_TAIL}"##,
      symbol::render_symbol_group(doc_nodes.clone(), name),
    ));
  }

  files.insert("index".to_string(), sidepanel);

  files
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
