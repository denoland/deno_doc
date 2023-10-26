use super::util::*;
use crate::html::parameters::render_params;

pub fn render_class(doc_node: &crate::DocNode) -> String {
  let class_def = doc_node.class_def.as_ref().unwrap();

  // TODO: examples, class items

  format!(
    r#"<div class="doc_block_items">{}{}{}{}</div>"#,
    super::jsdoc::render_docs(&doc_node.js_doc, true),
    render_constructors(&class_def.constructors, &doc_node.name),
    super::types::render_type_params(&class_def.type_params),
    render_index_signatures(&class_def.index_signatures),
  )
}

fn render_constructors(
  constructors: &[crate::class::ClassConstructorDef],
  name: &str,
) -> String {
  if constructors.is_empty() {
    return String::new();
  }

  let items = constructors
    .iter()
    .enumerate()
    .map(|(i, constructor)| {
      let id = name_to_id("constructor", &i.to_string());

      // TODO: render constructor params
      doc_entry(&id, name, &format!("()"), constructor.js_doc.doc.as_deref())
    })
    .collect::<String>();

  section("Constructors", &items)
}

fn render_index_signatures(
  index_signatures: &[crate::class::ClassIndexSignatureDef],
) -> String {
  if index_signatures.is_empty() {
    return String::new();
  }

  let items = index_signatures
    .iter()
    .enumerate()
    .map(|(i, index_signature)| {
      let id = name_to_id("index_signature", &i.to_string());

      let readonly = index_signature
        .readonly
        .then_some("<span>readonly </span>")
        .unwrap_or_default();

      let ts_type = index_signature
        .ts_type
        .as_ref()
        .map(|ts_type| format!(": {}", super::types::render_type_def(ts_type)))
        .unwrap_or_default();

      format!(
        r#"<div class="doc_item" id="{id}">{}{readonly}[{}]{ts_type}</div>"#,
        anchor(&id),
        render_params(&index_signature.params),
      )
    })
    .collect::<String>();

  section("Index Signatures", &items)
}
