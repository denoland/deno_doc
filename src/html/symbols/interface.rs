use crate::html::parameters::render_params;
use crate::html::types::render_type_params;
use crate::html::util::*;
use crate::html::GenerateCtx;
use serde_json::json;

pub(crate) fn render_interface(
  ctx: &GenerateCtx,
  doc_node: &crate::DocNode,
  render_ctx: &RenderContext,
) -> String {
  let interface_def = doc_node.interface_def.as_ref().unwrap();

  let current_type_params = interface_def
    .type_params
    .iter()
    .map(|def| def.name.clone())
    .collect::<std::collections::HashSet<String>>();
  let render_ctx = &render_ctx.with_current_type_params(current_type_params);

  [
    render_type_params(ctx, &interface_def.type_params, render_ctx),
    render_index_signatures(ctx, &interface_def.index_signatures, render_ctx),
    render_call_signatures(ctx, &interface_def.call_signatures, render_ctx),
    render_properties(ctx, &interface_def.properties, render_ctx),
    render_methods(ctx, &interface_def.methods, render_ctx),
  ]
  .join("")
}

fn render_index_signatures(
  ctx: &GenerateCtx,
  index_signatures: &[crate::interface::InterfaceIndexSignatureDef],
  render_ctx: &RenderContext,
) -> String {
  if index_signatures.is_empty() {
    return String::new();
  }

  let mut items = Vec::with_capacity(index_signatures.len());

  for (i, index_signature) in index_signatures.iter().enumerate() {
    let id = name_to_id("index_signature", &i.to_string());

    let readonly = index_signature
      .readonly
      .then_some("<span>readonly </span>")
      .unwrap_or_default();

    let ts_type = index_signature
      .ts_type
      .as_ref()
      .map(|ts_type| {
        format!(
          ": {}",
          crate::html::types::render_type_def(ctx, ts_type, render_ctx)
        )
      })
      .unwrap_or_default();

    let content = format!(
      r#"<div class="doc_item" id="{id}">{}{readonly}[{}]{ts_type}</div>"#,
      ctx.render("anchor.html", &json!({ "href": &id })),
      render_params(ctx, &index_signature.params, render_ctx),
    );
    items.push(content);
  }

  let content = items.join("");

  ctx.render(
    "section.html",
    &json!({ "title": "Index Signatures", "content": &content }),
  )
}

fn render_call_signatures(
  ctx: &GenerateCtx,
  call_signatures: &[crate::interface::InterfaceCallSignatureDef],
  render_ctx: &RenderContext,
) -> String {
  if call_signatures.is_empty() {
    return String::new();
  }

  let items = call_signatures
    .iter()
    .enumerate()
    .map(|(i, call_signature)| {
      let id = name_to_id("call_signature", &i.to_string());
      // TODO: tags

      let ts_type = call_signature
        .ts_type
        .as_ref()
        .map(|ts_type| {
          format!(
            ": {}",
            crate::html::types::render_type_def(ctx, ts_type, render_ctx)
          )
        })
        .unwrap_or_default();

      render_doc_entry(
        ctx,
        &id,
        "",
        &format!(
          "{}({}){ts_type}",
          crate::html::types::type_params_summary(
            ctx,
            &call_signature.type_params,
            render_ctx
          ),
          render_params(ctx, &call_signature.params, render_ctx),
        ),
        call_signature.js_doc.doc.as_deref(),
        render_ctx,
      )
    })
    .collect::<String>();

  ctx.render(
    "section.html",
    &json!({ "title": "Call Signatures", "content": &items }),
  )
}

fn render_properties(
  ctx: &GenerateCtx,
  properties: &[crate::interface::InterfacePropertyDef],
  render_ctx: &RenderContext,
) -> String {
  if properties.is_empty() {
    return String::new();
  }

  let items = properties
    .iter()
    .map(|property| {
      let id = name_to_id("property", &property.name);
      // TODO: tags

      let default_value = property
        .js_doc
        .tags
        .iter()
        .find_map(|tag| {
          if let crate::js_doc::JsDocTag::Default { value, .. } = tag {
            // TODO: font-normal
            Some(format!(
              r#"<span><span class="font-normal"> = </span>{value}</span>"#
            ))
          } else {
            None
          }
        })
        .unwrap_or_default();

      let ts_type = property
        .ts_type
        .as_ref()
        .map(|ts_type| {
          format!(
            ": {}",
            crate::html::types::render_type_def(ctx, ts_type, render_ctx)
          )
        })
        .unwrap_or_default();

      render_doc_entry(
        ctx,
        &id,
        &if property.computed {
          format!("[{}]", property.name)
        } else {
          property.name.clone()
        },
        &format!("{ts_type}{default_value}"),
        property.js_doc.doc.as_deref(),
        render_ctx,
      )
    })
    .collect::<String>();

  ctx.render(
    "section.html",
    &json!({ "title": "Properties", "content": &items }),
  )
}

fn render_methods(
  ctx: &GenerateCtx,
  methods: &[crate::interface::InterfaceMethodDef],
  render_ctx: &RenderContext,
) -> String {
  if methods.is_empty() {
    return String::new();
  }

  let items = methods
    .iter()
    .enumerate()
    .map(|(i, method)| {
      let id = name_to_id("call_signature", &format!("{}_{i}", method.name));
      // TODO: tags

      let name = if method.name == "new" {
        "<span>new</span>".to_string()
      } else if method.computed {
        format!("[{}]", method.name)
      } else {
        method.name.clone()
      };

      let return_type = method
        .return_type
        .as_ref()
        .map(|ts_type| {
          format!(
            ": {}",
            crate::html::types::render_type_def(ctx, ts_type, render_ctx)
          )
        })
        .unwrap_or_default();

      render_doc_entry(
        ctx,
        &id,
        &name,
        &format!(
          "{}({}){return_type}",
          render_type_params(ctx, &method.type_params, render_ctx),
          render_params(ctx, &method.params, render_ctx)
        ),
        method.js_doc.doc.as_deref(),
        render_ctx,
      )
    })
    .collect::<String>();

  ctx.render(
    "section.html",
    &json!({ "title": "Methods", "content": &items }),
  )
}
