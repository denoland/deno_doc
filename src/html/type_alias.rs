use crate::html::r#type::render_type_def;
use crate::html::util::*;

pub fn render_type_alias(doc_node: &crate::DocNode) -> String {
  let type_alias_def = doc_node.type_alias_def.as_ref().unwrap();

  let id = name_to_id("typeAlias", &doc_node.name);

  // TODO: tags, examples, TypeParamsDoc

  format!(
    r#"<div class="docBlockItems">{}</div>"#,
    section(
      "type",
      &doc_entry(
        &id,
        "definition",
        &format!(": {}", render_type_def(&type_alias_def.ts_type)),
      )
    )
  )
}

// TODO: classes: docBlockItems
