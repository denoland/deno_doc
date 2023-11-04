use crate::html::types::render_type_def;
use crate::html::util::*;
use crate::html::GenerateCtx;
use serde_json::json;

pub(crate) fn render_type_alias(
  ctx: &GenerateCtx,
  doc_node: &crate::DocNode,
  render_ctx: &RenderContext,
) -> String {
  let type_alias_def = doc_node.type_alias_def.as_ref().unwrap();

  let current_type_params = type_alias_def
    .type_params
    .iter()
    .map(|def| def.name.clone())
    .collect::<std::collections::HashSet<String>>();
  let render_ctx = &render_ctx.with_current_type_params(current_type_params);

  let id = name_to_id("typeAlias", &doc_node.name);

  // TODO: tags, TypeParamsDoc
  [
    crate::html::types::render_type_params(ctx, &type_alias_def.type_params, render_ctx),
    ctx.render(
      "section.html",
      &json!({
        "title": "Definition",
        "content": &render_doc_entry(
          ctx,
          &id,
          "",
          &format!(": {}", render_type_def(ctx, &type_alias_def.ts_type, render_ctx)),
          None,
          render_ctx,
        )
      }),
    ),
  ].join("")
}
