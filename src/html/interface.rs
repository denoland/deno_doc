use super::parameters::render_params;
use super::util::*;
use crate::html::types::render_type_params;
use crate::js_doc::JsDocTag;

pub fn render_interface(doc_node: &crate::DocNode) -> String {
  let interface_def = doc_node.interface_def.as_ref().unwrap();

  format!(
    r#"<div class="doc_block_items">{}{}{}{}</div>"#,
    render_type_params(&interface_def.type_params),
    render_index_signatures(&interface_def.index_signatures),
    render_properties(&interface_def.properties),
    render_methods(&interface_def.methods),
  )
}

fn render_index_signatures(
  index_signatures: &[crate::interface::InterfaceIndexSignatureDef],
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

fn render_call_signatures(
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
        .map(|ts_type| format!(": {}", super::types::render_type_def(ts_type)))
        .unwrap_or_default();

      doc_entry(
        &id,
        "",
        &format!(
          "{}({}){ts_type}",
          super::types::type_params_summary(&call_signature.type_params),
          render_params(&call_signature.params),
        ),
      )
    })
    .collect::<String>();

  section("Call Signatures", &items)
}

fn render_properties(
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
          if let JsDocTag::Default { value, .. } = tag {
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
        .map(|ts_type| format!(": {}", super::types::render_type_def(ts_type)))
        .unwrap_or_default();

      doc_entry(
        &id,
        &if property.computed {
          format!("[{}]", property.name)
        } else {
          property.name.clone()
        },
        &format!("{ts_type}{default_value}",),
      )
    })
    .collect::<String>();

  section("Properties", &items)
}

fn render_methods(methods: &[crate::interface::InterfaceMethodDef]) -> String {
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
        .map(|ts_type| format!(": {}", super::types::render_type_def(ts_type)))
        .unwrap_or_default();

      doc_entry(
        &id,
        &name,
        &format!(
          "{}({}){return_type}",
          render_type_params(&method.type_params),
          render_params(&method.params)
        ),
      )
    })
    .collect::<String>();

  section("Methods", &items)
}
