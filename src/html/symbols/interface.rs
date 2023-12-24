use crate::html::jsdoc::render_doc_entry;
use crate::html::parameters::render_params;
use crate::html::types::render_type_params;
use crate::html::util::*;
use serde_json::json;

pub(crate) fn render_interface(
  ctx: &RenderContext,
  doc_node: &crate::DocNode,
) -> String {
  let interface_def = doc_node.interface_def.as_ref().unwrap();

  let current_type_params = interface_def
    .type_params
    .iter()
    .map(|def| def.name.clone())
    .collect::<std::collections::HashSet<String>>();
  let ctx = &ctx.with_current_type_params(current_type_params);

  [
    render_type_params(ctx, &interface_def.type_params),
    render_index_signatures(ctx, &interface_def.index_signatures),
    render_call_signatures(ctx, &interface_def.call_signatures),
    render_properties(ctx, &interface_def.properties),
    render_methods(ctx, &interface_def.methods),
  ]
  .join("")
}

fn render_index_signatures(
  ctx: &RenderContext,
  index_signatures: &[crate::interface::InterfaceIndexSignatureDef],
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
        format!(": {}", crate::html::types::render_type_def(ctx, ts_type))
      })
      .unwrap_or_default();

    let content = format!(
      r#"<div class="doc_item" id="{id}">{}{readonly}[{}]{ts_type}</div>"#,
      ctx.render("anchor", &json!({ "href": &id })),
      render_params(ctx, &index_signature.params),
    );
    items.push(content);
  }

  let content = items.join("");

  ctx.render(
    "section",
    &json!({ "title": "Index Signatures", "content": &content }),
  )
}

fn render_call_signatures(
  ctx: &RenderContext,
  call_signatures: &[crate::interface::InterfaceCallSignatureDef],
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
          format!(": {}", crate::html::types::render_type_def(ctx, ts_type))
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
          ),
          render_params(ctx, &call_signature.params),
        ),
        call_signature.js_doc.doc.as_deref(),
      )
    })
    .collect::<String>();

  ctx.render(
    "section",
    &json!({ "title": "Call Signatures", "content": &items }),
  )
}

fn render_properties(
  ctx: &RenderContext,
  properties: &[crate::interface::InterfacePropertyDef],
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
          format!(": {}", crate::html::types::render_type_def(ctx, ts_type))
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
      )
    })
    .collect::<String>();

  ctx.render(
    "section",
    &json!({ "title": "Properties", "content": &items }),
  )
}

fn render_methods(
  ctx: &RenderContext,
  methods: &[crate::interface::InterfaceMethodDef],
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
          format!(": {}", crate::html::types::render_type_def(ctx, ts_type))
        })
        .unwrap_or_default();

      render_doc_entry(
        ctx,
        &id,
        &name,
        &format!(
          "{}({}){return_type}",
          render_type_params(ctx, &method.type_params),
          render_params(ctx, &method.params)
        ),
        method.js_doc.doc.as_deref(),
      )
    })
    .collect::<String>();

  ctx.render("section", &json!({ "title": "Methods", "content": &items }))
}
