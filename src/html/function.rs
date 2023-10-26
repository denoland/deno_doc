use super::parameters::render_params;
use super::types::render_type_def;
use super::types::render_type_params;
use super::types::type_params_summary;
use super::util::*;
use crate::function::FunctionDef;
use crate::js_doc::JsDocTag;
use crate::params::ParamPatternDef;

pub fn render_function(
  doc_nodes: Vec<&crate::DocNode>,
  ctx: &RenderContext,
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
        ctx.additional_css.borrow_mut().push_str(&format!(
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

      overload_labels.push_str(&format!(
        r#"<label for="{overload_id}" class="function_overload_label">
    <code>
      <span style="font-weight: 700;">{}</span><span style="font-weight: 500;">{}</span>
    </code>
</label>"#,
        doc_node.name, render_function_summary(function_def, ctx),
      ));
    }

    content.push_str(&render_single_function(doc_node, &overload_id, ctx));
  }

  format!(
    r#"<div class="doc_block_items">{overload_inputs}<div class="function_overload_selectors">{overload_labels}</div><div>{content}</div></div>"#
  )
}

fn render_function_summary(
  function_def: &FunctionDef,
  ctx: &RenderContext,
) -> String {
  let return_type = function_def
    .return_type
    .as_ref()
    .map(|ts_type| format!(": {}", render_type_def(ts_type, ctx)))
    .unwrap_or_default();

  format!(
    "{}({}){return_type}",
    type_params_summary(&function_def.type_params, ctx),
    render_params(&function_def.params, ctx)
  )
}

fn render_single_function(
  doc_node: &crate::DocNode,
  overload_id: &str,
  ctx: &RenderContext,
) -> String {
  let function_def = doc_node.function_def.as_ref().unwrap();

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
      let name = super::parameters::param_name(param, i);
      let id = name_to_id(overload_id, &format!("parameters_{name}"));

      let ts_type = if let ParamPatternDef::Assign { left, .. } = &param.pattern
      {
        left.ts_type.as_ref()
      } else {
        param.ts_type.as_ref()
      };

      let ts_type = ts_type
        .map(|ts_type| format!(": {}", render_type_def(ts_type, ctx)))
        .unwrap_or_default();

      // TODO: default_value

      doc_entry(&id, &name, &ts_type, param_docs.get(i).copied())
    })
    .collect::<String>();

  format!(
    r##"<div class="doc_block_items" id="{overload_id}_div">{}{}{}{}</div>"##,
    super::jsdoc::render_docs(&doc_node.js_doc, true, false),
    render_type_params(&function_def.type_params, ctx),
    section("Parameters", &params),
    section(
      "Return Type",
      &render_function_return_type(
        function_def,
        &doc_node.js_doc,
        overload_id,
        ctx
      )
    ),
  )
}

fn render_function_return_type(
  def: &FunctionDef,
  js_doc: &crate::js_doc::JsDoc,
  overload_id: &str,
  ctx: &RenderContext,
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

  doc_entry(&id, "", &render_type_def(return_type, ctx), return_type_doc)
}
