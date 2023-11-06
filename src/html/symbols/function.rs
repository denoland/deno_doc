use crate::function::FunctionDef;
use crate::html::jsdoc::render_doc_entry;
use crate::html::parameters::render_params;
use crate::html::types::render_type_def;
use crate::html::types::render_type_params;
use crate::html::types::type_params_summary;
use crate::html::util::*;
use crate::js_doc::JsDocTag;
use crate::params::ParamPatternDef;
use serde::Serialize;
use serde_json::json;

fn render_css_for_fn(overload_id: &str) -> String {
  format!(
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
"#
  )
}

#[derive(Serialize)]
struct OverloadRenderCtx {
  function_id: String,
  overload_id: String,
  additional_css: String,
  html_attrs: String,
  name: String,
  summary: String,
  summary_doc: String,
}

#[derive(Serialize)]
struct FunctionRenderCtx {
  overloads_ctx: Vec<OverloadRenderCtx>,
  content: String,
}

pub(crate) fn render_function(
  ctx: &RenderContext,
  doc_nodes: Vec<&crate::DocNode>,
) -> String {
  // TODO: this needs to be handled more gracefully on the frontend
  let mut content = Vec::with_capacity(doc_nodes.len());
  let mut overloads_ctx = Vec::with_capacity(doc_nodes.len());

  for (i, doc_node) in doc_nodes.into_iter().enumerate() {
    let function_def = doc_node.function_def.as_ref().unwrap();

    if function_def.has_body && i != 0 {
      continue;
    }

    let overload_id = name_to_id("function", &format!("{}_{i}", doc_node.name));
    let id = name_to_id("function", &doc_node.name);
    let css = render_css_for_fn(&overload_id);

    let summary_doc = if !(function_def.has_body && i == 0) {
      format!(
        r#"<div style="width: 100%;">{}</div>"#,
        crate::html::jsdoc::render_docs_summary(ctx, &doc_node.js_doc)
      )
    } else {
      String::new()
    };

    let html_attrs = (i == 0)
      .then_some("checked")
      .unwrap_or_default()
      .to_string();

    overloads_ctx.push(OverloadRenderCtx {
      function_id: id.to_string(),
      overload_id: overload_id.to_string(),
      additional_css: css,
      html_attrs,
      name: doc_node.name.to_string(),
      summary: render_function_summary(function_def, ctx),
      summary_doc,
    });

    content.push(render_single_function(ctx, doc_node, &overload_id));
  }

  let function_ctx = FunctionRenderCtx {
    overloads_ctx,
    content: content.join(""),
  };
  ctx.render("function.html", &function_ctx)
}

pub(crate) fn render_function_summary(
  function_def: &FunctionDef,
  render_ctx: &RenderContext,
) -> String {
  let return_type = function_def
    .return_type
    .as_ref()
    .map(|ts_type| format!(": {}", render_type_def(render_ctx, ts_type)))
    .unwrap_or_default();

  format!(
    "{}({}){return_type}",
    type_params_summary(render_ctx, &function_def.type_params),
    render_params(render_ctx, &function_def.params)
  )
}

fn render_single_function(
  ctx: &RenderContext,
  doc_node: &crate::DocNode,
  overload_id: &str,
) -> String {
  let function_def = doc_node.function_def.as_ref().unwrap();

  let current_type_params = function_def
    .type_params
    .iter()
    .map(|def| def.name.clone())
    .collect::<std::collections::HashSet<String>>();
  let ctx = &ctx.with_current_type_params(current_type_params);

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
      let (name, str_name) = crate::html::parameters::param_name(param, i);
      let id = name_to_id(overload_id, &format!("parameters_{str_name}"));

      let ts_type = if let ParamPatternDef::Assign { left, .. } = &param.pattern
      {
        left.ts_type.as_ref()
      } else {
        param.ts_type.as_ref()
      };

      let ts_type = ts_type
        .map(|ts_type| format!(": {}", render_type_def(ctx, ts_type)))
        .unwrap_or_default();

      // TODO: default_value, tags

      render_doc_entry(ctx, &id, &name, &ts_type, param_docs.get(i).copied())
    })
    .collect::<String>();

  format!(
    r##"<div class="doc_block_items" id="{overload_id}_div">{}{}{}{}</div>"##,
    crate::html::jsdoc::render_docs_with_examples(ctx, &doc_node.js_doc,),
    render_type_params(ctx, &function_def.type_params),
    ctx.render(
      "section.html",
      &json!({ "title": "Parameters", "content": &params })
    ),
    ctx.render(
      "section.html",
      &json!({
        "title": "Return Type",
        "content": &render_function_return_type(
          function_def,
          &doc_node.js_doc,
          overload_id,
          ctx
        )
      })
    )
  )
}

fn render_function_return_type(
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
    render_ctx,
    &id,
    "",
    &render_type_def(render_ctx, return_type),
    return_type_doc,
  )
}
