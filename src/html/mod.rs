use crate::DocNodeKind;
use std::collections::HashMap;

mod class;
mod r#enum;
mod function;
mod interface;
mod namespace;
mod parameters;
mod symbol;
mod type_alias;
mod types;
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

.symbol_group > * + * {
  margin-top: 3rem; /* 48px */
}

.symbol > * + * {
  margin-top: 2rem; /* 32px */
}

.symbol_header {
  display: flex;
  justify-content: space-between;
  align-items: flex-start;
}


.doc_block_items > * + * {
  margin-top: 1.75rem; /* 28px */
}

.doc_block_title {
  font-weight: 500;
  background-color: unset !important;
}

.doc_block_title > * + * {
  margin-top: 0.25rem; /* 4px */
}

.doc_block_title > *:first-child {
  font-size: 1.25rem; /* 20px */
  line-height: 1.75rem; /* 28px */
}

.section_title {
  font-size: 0.875rem; /* 14px */
  line-height: 1.5rem; /* 24px */
  font-weight: 500;
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



.kind_Function_text {
  color: #056CF0;
}

.kind_Function_bg {
  background-color: #026BEB1A;
}

.kind_Variable_text {
  color: #7E57C0;
}

.kind_Variable_bg {
  background-color: #7E57C01A;
}

.kind_Class_text {
  color: #20B44B;
}

.kind_Class_bg {
  background-color: #2FA8501A;
}

.kind_Enum_text {
  color: #22ABB0;
}

.kind_Enum_bg {
  background-color: #22ABB01A;
}

.kind_Interface_text {
  color: #D2A064;
}

.kind_Interface_bg {
  background-color: #D4A0681A;
}

.kind_TypeAlias_text {
  color: #A4478C;
}

.kind_TypeAlias_bg {
  background-color: #A4478C1A;
}

.kind_Namespace_text {
  color: #D25646;
}

.kind_Namespace_bg {
  background-color: #D256461A;
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

pub fn generate(doc_nodes: &[crate::DocNode]) -> HashMap<String, String> {
  let mut files = HashMap::new();

  let mut sidepanel = String::with_capacity(1024);
  sidepanel.push_str(r#"<ul>"#);
  for doc_node in doc_nodes {
    sidepanel.push_str(&format!(
      r##"<li><a href="./{}.html">{}</a></li>"##,
      doc_node.name, doc_node.name
    ));
  }

  sidepanel.push_str(r#"</ul>"#);

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
