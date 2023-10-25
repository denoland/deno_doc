use super::parameters::render_params;
use super::types::render_type_def;
use super::util::*;
use crate::function::FunctionDef;
use crate::js_doc::JsDoc;

pub fn render_function(doc_node: &crate::DocNode) -> String {
  if doc_node.name == "setTimeout" {
    eprintln!("render function {:#?}", doc_node);
  }
  let function_def = doc_node.function_def.as_ref().unwrap();

  format!(
    r#"<div class="doc_block_items">
    <pre>{}</pre>
    <div>{}</div>
    <div>{}</div>
</div>"#,
    render_js_doc(&doc_node.js_doc),
    render_function_params(function_def),
    render_function_return_type(function_def),
  )
}

fn render_js_doc(js_doc: &JsDoc) -> String {
  js_doc.doc.clone().unwrap_or_default()
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
