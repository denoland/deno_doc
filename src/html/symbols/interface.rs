use crate::html::parameters::render_params;
use crate::html::render_context::RenderContext;
use crate::html::symbols::class::IndexSignatureCtx;
use crate::html::types::render_type_def_colon;
use crate::html::types::render_type_params;
use crate::html::types::type_params_summary;
use crate::html::util::*;

pub(crate) fn render_interface(
  ctx: &RenderContext,
  doc_node: &crate::DocNode,
) -> Vec<SectionCtx> {
  let interface_def = doc_node.interface_def.as_ref().unwrap();

  let current_type_params = interface_def
    .type_params
    .iter()
    .map(|def| def.name.as_str())
    .collect::<std::collections::HashSet<&str>>();
  let ctx = &ctx.with_current_type_params(current_type_params);

  let mut sections = vec![];

  if let Some(type_params) =
    render_type_params(ctx, &interface_def.type_params, &doc_node.location)
  {
    sections.push(type_params);
  }

  if let Some(index_signatures) =
    render_index_signatures(ctx, &interface_def.index_signatures)
  {
    sections.push(index_signatures);
  }

  if let Some(call_signatures) =
    render_call_signatures(ctx, &interface_def.call_signatures)
  {
    sections.push(call_signatures);
  }

  if let Some(properties) = render_properties(ctx, &interface_def.properties) {
    sections.push(properties);
  }

  if let Some(methods) = render_methods(ctx, &interface_def.methods) {
    sections.push(methods);
  }

  sections
}

fn render_index_signatures(
  ctx: &RenderContext,
  index_signatures: &[crate::interface::InterfaceIndexSignatureDef],
) -> Option<SectionCtx> {
  if index_signatures.is_empty() {
    return None;
  }

  let mut items = Vec::with_capacity(index_signatures.len());

  for (i, index_signature) in index_signatures.iter().enumerate() {
    let id = name_to_id("index_signature", &i.to_string());

    let ts_type = index_signature
      .ts_type
      .as_ref()
      .map(|ts_type| render_type_def_colon(ctx, ts_type))
      .unwrap_or_default();

    items.push(IndexSignatureCtx {
      id: id.clone(),
      anchor: AnchorCtx { id },
      readonly: index_signature.readonly,
      params: render_params(ctx, &index_signature.params),
      ts_type,
      source_href: ctx
        .ctx
        .href_resolver
        .resolve_source(&index_signature.location),
    });
  }

  Some(SectionCtx {
    title: "Index Signatures",
    content: SectionContentCtx::IndexSignature(items),
  })
}

fn render_call_signatures(
  ctx: &RenderContext,
  call_signatures: &[crate::interface::InterfaceCallSignatureDef],
) -> Option<SectionCtx> {
  if call_signatures.is_empty() {
    return None;
  }

  let items = call_signatures
    .iter()
    .enumerate()
    .map(|(i, call_signature)| {
      let id = name_to_id("call_signature", &i.to_string());

      let ts_type = call_signature
        .ts_type
        .as_ref()
        .map(|ts_type| render_type_def_colon(ctx, ts_type))
        .unwrap_or_default();

      let tags = Tag::from_js_doc(&call_signature.js_doc);

      DocEntryCtx::new(
        ctx,
        &id,
        "",
        &format!(
          "{}({}){ts_type}",
          type_params_summary(ctx, &call_signature.type_params,),
          render_params(ctx, &call_signature.params),
        ),
        tags,
        call_signature.js_doc.doc.as_deref(),
        &call_signature.location,
      )
    })
    .collect::<Vec<DocEntryCtx>>();

  Some(SectionCtx {
    title: "Call Signatures",
    content: SectionContentCtx::DocEntry(items),
  })
}

fn render_properties(
  ctx: &RenderContext,
  properties: &[crate::interface::InterfacePropertyDef],
) -> Option<SectionCtx> {
  if properties.is_empty() {
    return None;
  }

  let items = properties
    .iter()
    .map(|property| {
      let id = name_to_id("property", &property.name);
      let default_value = property
        .js_doc
        .tags
        .iter()
        .find_map(|tag| {
          if let crate::js_doc::JsDocTag::Default { value, .. } = tag {
            Some(format!(
              r#"<span><span class="font-normal"> = </span>{}</span>"#,
              html_escape::encode_safe(value)
            ))
          } else {
            None
          }
        })
        .unwrap_or_default();

      let ts_type = property
        .ts_type
        .as_ref()
        .map(|ts_type| render_type_def_colon(ctx, ts_type))
        .unwrap_or_default();

      let mut tags = Tag::from_js_doc(&property.js_doc);
      if property.readonly {
        tags.insert(Tag::Readonly);
      }
      if property.optional {
        tags.insert(Tag::Optional);
      }

      DocEntryCtx::new(
        ctx,
        &id,
        &if property.computed {
          format!("[{}]", property.name)
        } else {
          property.name.clone()
        },
        &format!("{ts_type}{default_value}"),
        tags,
        property.js_doc.doc.as_deref(),
        &property.location,
      )
    })
    .collect::<Vec<DocEntryCtx>>();

  Some(SectionCtx {
    title: "Properties",
    content: SectionContentCtx::DocEntry(items),
  })
}

fn render_methods(
  ctx: &RenderContext,
  methods: &[crate::interface::InterfaceMethodDef],
) -> Option<SectionCtx> {
  if methods.is_empty() {
    return None;
  }

  let items = methods
    .iter()
    .enumerate()
    .map(|(i, method)| {
      let id = name_to_id("call_signature", &format!("{}_{i}", method.name));

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
        .map(|ts_type| render_type_def_colon(ctx, ts_type))
        .unwrap_or_default();

      let mut tags = Tag::from_js_doc(&method.js_doc);
      /* TODO: if method.kind {
        tags.push(Tag::ReadOnly);
      }*/
      if method.optional {
        tags.insert(Tag::Optional);
      }

      DocEntryCtx::new(
        ctx,
        &id,
        &name,
        &format!(
          "{}({}){return_type}",
          type_params_summary(ctx, &method.type_params),
          render_params(ctx, &method.params)
        ),
        tags,
        method.js_doc.doc.as_deref(),
        &method.location,
      )
    })
    .collect::<Vec<DocEntryCtx>>();

  Some(SectionCtx {
    title: "Methods",
    content: SectionContentCtx::DocEntry(items),
  })
}
