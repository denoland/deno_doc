use crate::html::types::render_type_def;
use crate::html::util::*;

pub fn render_type_alias(
  doc_node: &crate::DocNode,
  ctx: &RenderContext,
) -> String {
  let type_alias_def = doc_node.type_alias_def.as_ref().unwrap();

  let ctx = &RenderContext {
    current_type_params: type_alias_def
      .type_params
      .iter()
      .map(|def| def.name.clone())
      .collect::<std::collections::HashSet<String>>(),
    ..ctx.clone()
  };

  let id = name_to_id("typeAlias", &doc_node.name);

  // TODO: tags, TypeParamsDoc

  format!(
    r#"<div class="doc_block_items">{}{}{}</div>"#,
    super::jsdoc::render_docs(&doc_node.js_doc, true, false),
    super::types::render_type_params(&type_alias_def.type_params, ctx),
    section(
      "Definition",
      &doc_entry(
        &id,
        "",
        &format!(": {}", render_type_def(&type_alias_def.ts_type, ctx)),
        None,
      )
    ),
  )
}
