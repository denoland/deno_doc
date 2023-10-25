use indexmap::IndexMap;
use std::collections::HashMap;

use crate::DocNodeKind;

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

use jsdoc::render_docs;

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
  let partitions = partition_nodes_by_kind(doc_nodes);

  sidepanel.push_str(r#"<div>"#);
  for (kind, doc_nodes) in partitions.iter() {
    sidepanel.push_str(&format!(r#"<h3>{:?}</h3><ul>"#, kind));

    for doc_node in doc_nodes {
      sidepanel.push_str(&format!(
        r##"<li><a href="./{}.html">{}</a></li>"##,
        doc_node.name, doc_node.name
      ));
    }

    sidepanel.push_str(r#"</ul>"#);
  }
  sidepanel.push_str(r#"</div>"#);

  files.insert("index".to_string(), render_index(&sidepanel, partitions));

  let name_partitions = partition_nodes_by_name(doc_nodes);

  for (name, doc_nodes) in name_partitions.iter() {
    // FIXME(bartlomieju): hardcoded path to index in `<a>`
    files.insert(name.to_string(), format!(
      r##"{HTML_HEAD}<div style="display: flex;">{sidepanel}<div style="padding: 30px;"><a href="/generated_docs/"><- Index</a>{}</div></div>{HTML_TAIL}"##,
      symbol::render_symbol_group(doc_nodes.clone(), name),
    ));
  }

  files
}

fn render_index(
  sidepanel: &str,
  partitions: IndexMap<DocNodeKind, Vec<crate::DocNode>>,
) -> String {
  let mut content = String::with_capacity(32 * 1024);

  content.push_str(HTML_HEAD);
  content.push_str(&format!(
    r#"<div style="display: flex;">{sidepanel}<div style="padding: 30px;"><h1>Index</h1>"#
  ));

  for (kind, doc_nodes) in partitions {
    content.push_str(&format!(r#"<h2>{:?}</h2><ul>"#, kind));

    for doc_node in doc_nodes {
      content.push_str(&format!(
        r##"<li><a href="./{}.html">{}</a>{}</li>"##,
        doc_node.name,
        doc_node.name,
        render_docs(&doc_node.js_doc),
      ));
    }

    content.push_str(r#"</ul>"#);
  }

  content.push_str(&format!(r#"</div></div>{HTML_TAIL}"#));

  content
}

fn partition_nodes_by_kind(
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

fn partition_nodes_by_name(
  doc_nodes: &[crate::DocNode],
) -> IndexMap<String, Vec<crate::DocNode>> {
  let mut partitions = IndexMap::default();

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
