use crate::html::r#type::render_type_def;
use crate::html::util::*;

pub fn render_variable(doc_node: &crate::DocNode) -> String {
  let variable_def = doc_node.variable_def.as_ref().unwrap();

  if variable_def.ts_type.is_none() {
    return String::new();
  }

  let id = name_to_id("variable", &doc_node.name);

  // TODO: examples

  format!(
    r#"<div class="docBlockItems">{}</div>"#,
    section(
      "type",
      &doc_entry(
        &id,
        "",
        &render_type_def(variable_def.ts_type.as_ref().unwrap())
      )
    )
  )
}

// TODO: classes: docBlockItems
