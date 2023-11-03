use super::parameters::render_params;
use super::types::render_type_def;
use super::types::render_type_params;
use super::types::type_params_summary;
use super::util::*;
use super::GenerateCtx;
use crate::function::FunctionDef;
use crate::js_doc::JsDocTag;
use crate::params::ParamPatternDef;
use serde_json::json;

pub(super) fn render_function(
  ctx: &GenerateCtx,
  doc_nodes: Vec<&crate::DocNode>,
  render_ctx: &RenderContext,
) -> String {
  // TODO: this needs to be handled more gracefully on the frontend
  let mut content = String::with_capacity(16 * 1024);
  let mut overload_inputs = String::with_capacity(1024);
  let mut overload_labels = String::with_capacity(1024);

  for (i, doc_node) in doc_nodes.into_iter().enumerate() {
    let function_def = doc_node.function_def.as_ref().unwrap();

    if function_def.has_body && i != 0 {
      continue;
    }

    let overload_id = name_to_id("function", &format!("{}_{i}", doc_node.name));

    {
      let id = name_to_id("function", &doc_node.name);

      {
        render_ctx.add_additional_css(format!(
          r#"
#{overload_id} {{
  display: none;
}}
#{overload_id}:checked ~ *:last-child > :not(#{overload_id}_div) {{
  display: none;
}}
#{overload_id}:checked ~ div:first-of-type > label[for='{overload_id}'] {{
  background-color: #056CF00C;
  border: solid 2px rgb(37 99 235);
  cursor: unset;
}}
#{overload_id}:checked ~ div:first-of-type > label[for='{overload_id}'] > code {{
  margin: -1px;
}}
"#));
      }

      overload_inputs.push_str(&format!(
        r#"<input type="radio" name="{id}" id="{overload_id}" {} />"#,
        (i == 0).then_some("checked").unwrap_or_default()
      ));

      let summary_doc = if !(function_def.has_body && i == 0) {
        format!(
          r#"<div style="width: 100%;">{}</div>"#,
          super::jsdoc::render_docs_summary(ctx, render_ctx, &doc_node.js_doc)
        )
      } else {
        String::new()
      };

      overload_labels.push_str(&format!(
        r#"<label for="{overload_id}" class="function_overload_label">
    <code>
      <span style="font-weight: 700;">{}</span><span style="font-weight: 500;">{}</span>
    </code>
    {summary_doc}
</label>"#,
        doc_node.name,
        render_function_summary(ctx, function_def, render_ctx),
      ));
    }

    content.push_str(&render_single_function(
      ctx,
      doc_node,
      &overload_id,
      render_ctx,
    ));
  }

  format!(
    r#"<div class="doc_block_items">{overload_inputs}<div class="function_overload_selectors">{overload_labels}</div><div>{content}</div></div>"#
  )
}

pub(super) fn render_function_summary(
  ctx: &GenerateCtx,
  function_def: &FunctionDef,
  render_ctx: &RenderContext,
) -> String {
  let return_type = function_def
    .return_type
    .as_ref()
    .map(|ts_type| format!(": {}", render_type_def(ctx, ts_type, render_ctx)))
    .unwrap_or_default();

  format!(
    "{}({}){return_type}",
    type_params_summary(ctx, &function_def.type_params, render_ctx),
    render_params(ctx, &function_def.params, render_ctx)
  )
}

fn render_single_function(
  ctx: &GenerateCtx,
  doc_node: &crate::DocNode,
  overload_id: &str,
  render_ctx: &RenderContext,
) -> String {
  let function_def = doc_node.function_def.as_ref().unwrap();

  let current_type_params = function_def
    .type_params
    .iter()
    .map(|def| def.name.clone())
    .collect::<std::collections::HashSet<String>>();
  let render_ctx = &render_ctx.with_current_type_params(current_type_params);

  // TODO: tags

  let param_docs = doc_node
    .js_doc
    .tags
    .iter()
    .filter_map(|tag| {
      if let JsDocTag::Param { doc, .. } = tag {
        doc.as_deref()
      } else {
        None
      }
    })
    .collect::<Vec<&str>>();

  let params = function_def
    .params
    .iter()
    .enumerate()
    .map(|(i, param)| {
      let (name, str_name) = super::parameters::param_name(param, i);
      let id = name_to_id(overload_id, &format!("parameters_{str_name}"));

      let ts_type = if let ParamPatternDef::Assign { left, .. } = &param.pattern
      {
        left.ts_type.as_ref()
      } else {
        param.ts_type.as_ref()
      };

      let ts_type = ts_type
        .map(|ts_type| {
          format!(": {}", render_type_def(ctx, ts_type, render_ctx))
        })
        .unwrap_or_default();

      // TODO: default_value, tags

      render_doc_entry(
        ctx,
        &id,
        &name,
        &ts_type,
        param_docs.get(i).copied(),
        render_ctx,
      )
    })
    .collect::<String>();

  format!(
    r##"<div class="doc_block_items" id="{overload_id}_div">{}{}{}{}</div>"##,
    super::jsdoc::render_docs_with_examples(ctx, render_ctx, &doc_node.js_doc,),
    render_type_params(ctx, &function_def.type_params, render_ctx),
    ctx.render(
      "section.html",
      &json!({ "title": "Parameters", "content": &params })
    ),
    ctx.render(
      "section.html",
      &json!({
        "title": "Return Type",
        "content": &render_function_return_type(
          ctx,
          function_def,
          &doc_node.js_doc,
          overload_id,
          render_ctx
        )
      })
    )
  )
}

fn render_function_return_type(
  ctx: &GenerateCtx,
  def: &FunctionDef,
  js_doc: &crate::js_doc::JsDoc,
  overload_id: &str,
  render_ctx: &RenderContext,
) -> String {
  let Some(return_type) = def.return_type.as_ref() else {
    return "".to_string();
  };

  let id = name_to_id(overload_id, "return");

  let return_type_doc = js_doc.tags.iter().find_map(|tag| {
    if let JsDocTag::Return { doc, .. } = tag {
      doc.as_deref()
    } else {
      None
    }
  });

  render_doc_entry(
    ctx,
    &id,
    "",
    &render_type_def(ctx, return_type, render_ctx),
    return_type_doc,
    render_ctx,
  )
}
