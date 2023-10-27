use super::parameters::render_params;
use super::util::*;
use crate::html::types::render_type_params;
use std::fmt::Write;

pub fn render_interface(
  doc_node: &crate::DocNode,
  ctx: &RenderContext,
) -> String {
  let interface_def = doc_node.interface_def.as_ref().unwrap();

  let ctx = &RenderContext {
    current_type_params: interface_def
      .type_params
      .iter()
      .map(|def| def.name.clone())
      .collect::<std::collections::HashSet<String>>(),
    ..ctx.clone()
  };

  format!(
    r#"<div class="doc_block_items">{}{}{}{}{}{}</div>"#,
    super::jsdoc::render_docs(&doc_node.js_doc, true, false, ctx),
    render_type_params(&interface_def.type_params, ctx),
    render_index_signatures(&interface_def.index_signatures, ctx),
    render_call_signatures(&interface_def.call_signatures, ctx),
    render_properties(&interface_def.properties, ctx),
    render_methods(&interface_def.methods, ctx),
  )
}

fn render_index_signatures(
  index_signatures: &[crate::interface::InterfaceIndexSignatureDef],
  ctx: &RenderContext,
) -> String {
  if index_signatures.is_empty() {
    return String::new();
  }

  let items = index_signatures.iter().enumerate().fold(
    String::new(),
    |mut output, (i, index_signature)| {
      let id = name_to_id("index_signature", &i.to_string());

      let readonly = index_signature
        .readonly
        .then_some("<span>readonly </span>")
        .unwrap_or_default();

      let ts_type = index_signature
        .ts_type
        .as_ref()
        .map(|ts_type| {
          format!(": {}", super::types::render_type_def(ts_type, ctx))
        })
        .unwrap_or_default();

      write!(
        output,
        r#"<div class="doc_item" id="{id}">{}{readonly}[{}]{ts_type}</div>"#,
        anchor(&id),
        render_params(&index_signature.params, ctx),
      )
      .unwrap();
      output
    },
  );

  section("Index Signatures", &items)
}

fn render_call_signatures(
  call_signatures: &[crate::interface::InterfaceCallSignatureDef],
  ctx: &RenderContext,
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
          format!(": {}", super::types::render_type_def(ts_type, ctx))
        })
        .unwrap_or_default();

      doc_entry(
        &id,
        "",
        &format!(
          "{}({}){ts_type}",
          super::types::type_params_summary(&call_signature.type_params, ctx),
          render_params(&call_signature.params, ctx),
        ),
        call_signature.js_doc.doc.as_deref(),
        ctx,
      )
    })
    .collect::<String>();

  section("Call Signatures", &items)
}

fn render_properties(
  properties: &[crate::interface::InterfacePropertyDef],
  ctx: &RenderContext,
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
          format!(": {}", super::types::render_type_def(ts_type, ctx))
        })
        .unwrap_or_default();

      doc_entry(
        &id,
        &if property.computed {
          format!("[{}]", property.name)
        } else {
          property.name.clone()
        },
        &format!("{ts_type}{default_value}"),
        property.js_doc.doc.as_deref(),
        ctx,
      )
    })
    .collect::<String>();

  section("Properties", &items)
}

fn render_methods(
  methods: &[crate::interface::InterfaceMethodDef],
  ctx: &RenderContext,
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
          format!(": {}", super::types::render_type_def(ts_type, ctx))
        })
        .unwrap_or_default();

      doc_entry(
        &id,
        &name,
        &format!(
          "{}({}){return_type}",
          render_type_params(&method.type_params, ctx),
          render_params(&method.params, ctx)
        ),
        method.js_doc.doc.as_deref(),
        ctx,
      )
    })
    .collect::<String>();

  section("Methods", &items)
}
