use super::parameters::render_params;
use super::types::render_type_def;
use super::types::render_type_params;
use super::util::*;
use crate::function::FunctionDef;
use crate::html::jsdoc::render_docs;

pub fn render_function(doc_nodes: Vec<&crate::DocNode>) -> String {
  // TODO: this needs to be handled more gracefully on the frontend
  let mut content = String::with_capacity(16 * 1024);
  for (i, doc_node) in doc_nodes.iter().enumerate() {
    content.push_str(&format!("<h3>Overload {}</h3>", i));
    content.push_str(&render_single_function(doc_node));
  }
  content
}

fn render_single_function(doc_node: &crate::DocNode) -> String {
  let function_def = doc_node.function_def.as_ref().unwrap();
  format!(
    r#"<div class="doc_block_items">
        {}
        {}
        <div>{}</div>
        <div>{}</div>
    </div>"#,
    section("Docs", &render_docs(&doc_node.js_doc)),
    render_type_params(&function_def.type_params),
    render_function_params(function_def),
    render_function_return_type(function_def),
  )
}

fn render_function_params(def: &FunctionDef) -> String {
  let items = render_params(&def.params);
  section("Parameters", &items)
}

fn render_function_return_type(def: &FunctionDef) -> String {
  let Some(return_type) = def.return_type.as_ref() else {
    return "".to_string();
  };

  section("Return type", &render_type_def(return_type))
}
