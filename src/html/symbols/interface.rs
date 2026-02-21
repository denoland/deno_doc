use crate::html::DiffStatus;
use crate::html::DocNodeWithContext;
use crate::html::parameters::render_params;
use crate::html::render_context::RenderContext;
use crate::html::types::render_type_def_colon;
use crate::html::types::type_params_summary;
use crate::html::util::*;

pub(crate) fn render_interface(
  ctx: &RenderContext,
  doc_node: &DocNodeWithContext,
  name: &str,
) -> Vec<SectionCtx> {
  let interface_def = doc_node.interface_def().unwrap();

  let current_type_params = interface_def
    .type_params
    .iter()
    .map(|def| def.name.as_str())
    .collect::<std::collections::HashSet<&str>>();
  let ctx = &ctx.with_current_type_params(current_type_params);

  let interface_diff = ctx.ctx.diff.as_ref().and_then(|diff_index| {
    diff_index
      .get_def_diff(
        &doc_node.origin.specifier,
        doc_node.get_name(),
        doc_node.def.to_kind(),
      )
      .and_then(|d| d.as_interface())
  });

  let mut sections = vec![];

  if let Some(type_params) = crate::html::types::render_type_params(
    ctx,
    &doc_node.js_doc,
    &interface_def.type_params,
    &doc_node.location,
    interface_diff.and_then(|d| d.type_params_change.as_ref()),
  ) {
    sections.push(type_params);
  }

  if let Some(index_signatures) = render_index_signatures(
    ctx,
    &interface_def.index_signatures,
    interface_diff.and_then(|d| d.index_signature_changes.as_ref()),
  ) {
    sections.push(index_signatures);
  }

  if let Some(call_signatures) = render_call_signatures(
    ctx,
    &interface_def.call_signatures,
    interface_diff.and_then(|d| d.call_signature_changes.as_ref()),
  ) {
    sections.push(call_signatures);
  }

  if let Some(properties) = render_properties(
    ctx,
    name,
    &interface_def.properties,
    interface_diff.and_then(|d| d.property_changes.as_ref()),
  ) {
    sections.push(properties);
  }

  if let Some(methods) = render_methods(
    ctx,
    name,
    &interface_def.methods,
    interface_diff.and_then(|d| d.method_changes.as_ref()),
  ) {
    sections.push(methods);
  }

  sections
}

pub(crate) fn render_index_signatures(
  ctx: &RenderContext,
  index_signatures: &[crate::ts_type::IndexSignatureDef],
  index_signatures_diff: Option<&crate::diff::InterfaceIndexSignaturesDiff>,
) -> Option<SectionCtx> {
  let empty_sigs = Vec::new();
  let empty_mod = Vec::new();
  let total = index_signatures.len();

  super::render_index_signatures_with_diff(
    ctx,
    index_signatures,
    index_signatures_diff.map_or(&empty_sigs, |d| &d.removed),
    index_signatures_diff.map_or(&empty_mod, |d| &d.modified),
    |i, _sig| {
      let diff = index_signatures_diff?;
      if !diff.added.is_empty() && i >= total - diff.added.len() {
        Some(DiffStatus::Added)
      } else if !diff.modified.is_empty()
        && i < total.saturating_sub(diff.added.len())
      {
        Some(DiffStatus::Modified)
      } else {
        None
      }
    },
  )
}

pub(crate) fn render_call_signatures(
  ctx: &RenderContext,
  call_signatures: &[crate::ts_type::CallSignatureDef],
  call_signatures_diff: Option<&crate::diff::CallSignaturesDiff>,
) -> Option<SectionCtx> {
  if call_signatures.is_empty()
    && call_signatures_diff.is_none_or(|d| d.removed.is_empty())
  {
    return None;
  }

  let mut items = call_signatures
    .iter()
    .enumerate()
    .map(|(i, call_signature)| {
      let id = IdBuilder::new(ctx)
        .kind(IdKind::CallSignature)
        .index(i)
        .build();

      let ts_type = call_signature
        .ts_type
        .as_ref()
        .map(|ts_type| render_type_def_colon(ctx, ts_type))
        .unwrap_or_default();

      let tags = Tag::from_js_doc(&call_signature.js_doc);
      let old_tags = call_signatures_diff
        .map(|_| super::compute_old_tags(&tags, None, None, None, None));

      let diff_status = if let Some(diff) = call_signatures_diff {
        if !diff.added.is_empty()
          && i >= call_signatures.len() - diff.added.len()
        {
          Some(DiffStatus::Added)
        } else if !diff.modified.is_empty()
          && i < call_signatures.len().saturating_sub(diff.added.len())
        {
          Some(DiffStatus::Modified)
        } else {
          None
        }
      } else {
        None
      };

      let (old_content, js_doc_changed) =
        if matches!(diff_status, Some(DiffStatus::Modified)) {
          let sig_diff = call_signatures_diff.and_then(|d| d.modified.first());
          let old_content = sig_diff
            .and_then(|sd| sd.ts_type_change.as_ref())
            .map(|tc| render_type_def_colon(ctx, &tc.old));
          let js_doc_changed =
            sig_diff.and_then(|sd| sd.js_doc_change.as_ref().map(|_| true));
          (old_content, js_doc_changed)
        } else {
          (None, None)
        };

      DocEntryCtx::new(
        ctx,
        id,
        None,
        None,
        &format!(
          "{}({}){ts_type}",
          type_params_summary(ctx, &call_signature.type_params),
          render_params(ctx, &call_signature.params),
        ),
        tags,
        call_signature.js_doc.doc.as_deref(),
        &call_signature.location,
        diff_status,
        old_content,
        old_tags,
        js_doc_changed,
      )
    })
    .collect::<Vec<DocEntryCtx>>();

  if let Some(diff) = call_signatures_diff {
    for call_signature in &diff.removed {
      let id = IdBuilder::new(ctx)
        .kind(IdKind::CallSignature)
        .index(items.len())
        .build();

      let tags = Tag::from_js_doc(&call_signature.js_doc);

      let ts_type = call_signature
        .ts_type
        .as_ref()
        .map(|ts_type| render_type_def_colon(ctx, ts_type))
        .unwrap_or_default();

      items.push(DocEntryCtx::removed(
        ctx,
        id,
        None,
        None,
        &format!(
          "{}({}){ts_type}",
          type_params_summary(ctx, &call_signature.type_params),
          render_params(ctx, &call_signature.params),
        ),
        tags,
        call_signature.js_doc.doc.as_deref(),
        &call_signature.location,
      ));
    }
  }

  if items.is_empty() {
    return None;
  }

  Some(SectionCtx::new(
    ctx,
    "Call Signatures",
    SectionContentCtx::DocEntry(items),
  ))
}

pub(crate) fn render_properties(
  ctx: &RenderContext,
  interface_name: &str,
  properties: &[crate::ts_type::PropertyDef],
  properties_diff: Option<&crate::diff::InterfacePropertiesDiff>,
) -> Option<SectionCtx> {
  if properties.is_empty()
    && properties_diff.is_none_or(|d| d.removed.is_empty())
  {
    return None;
  }

  let mut items = properties
    .iter()
    .map(|property| {
      let id = IdBuilder::new(ctx)
        .kind(IdKind::Property)
        .name(&property.name)
        .build();
      let default_value = property
        .js_doc
        .tags
        .iter()
        .find_map(|tag| {
          if let crate::js_doc::JsDocTag::Default { value, .. } = tag {
            Some(format!(
              r#"<span><span class="font-normal"> = </span>{}</span>"#,
              html_escape::encode_text(value)
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

      let diff_status = if let Some(diff) = properties_diff {
        if diff.added.iter().any(|p| p.name == property.name) {
          Some(DiffStatus::Added)
        } else if let Some(md) =
          diff.modified.iter().find(|p| p.name == property.name)
        {
          if let Some(name_change) = &md.name_change {
            Some(DiffStatus::Renamed {
              old_name: name_change.old.clone(),
            })
          } else {
            Some(DiffStatus::Modified)
          }
        } else {
          None
        }
      } else {
        None
      };

      let (old_content, old_tags, js_doc_changed) = if matches!(
        diff_status,
        Some(DiffStatus::Modified | DiffStatus::Renamed { .. })
      ) {
        let prop_diff = properties_diff
          .and_then(|pc| pc.modified.iter().find(|p| p.name == property.name));

        let old_content = prop_diff
          .and_then(|pd| pd.type_change.as_ref())
          .map(|tc| render_type_def_colon(ctx, &tc.old));

        let old_tags = prop_diff.map(|diff| {
          super::compute_old_tags(
            &tags,
            None,
            diff.readonly_change.as_ref(),
            None,
            diff.optional_change.as_ref(),
          )
        });

        let js_doc_changed =
          prop_diff.and_then(|pd| pd.js_doc_change.as_ref().map(|_| true));

        (old_content, old_tags, js_doc_changed)
      } else {
        (None, None, None)
      };

      DocEntryCtx::new(
        ctx,
        id,
        Some(if property.computed {
          format!("[{}]", html_escape::encode_text(&property.name))
        } else {
          html_escape::encode_text(&property.name).into_owned()
        }),
        ctx.lookup_symbol_href(&qualify_drilldown_name(
          interface_name,
          &property.name,
          true,
        )),
        &format!("{ts_type}{default_value}"),
        tags,
        property.js_doc.doc.as_deref(),
        &property.location,
        diff_status,
        old_content,
        old_tags,
        js_doc_changed,
      )
    })
    .collect::<Vec<DocEntryCtx>>();

  if let Some(prop_diff) = properties_diff {
    for removed_prop in &prop_diff.removed {
      super::push_removed_property_entry(
        ctx,
        &removed_prop.name,
        removed_prop.ts_type.as_ref(),
        &removed_prop.location,
        &mut items,
      );
    }
  }

  if items.is_empty() {
    None
  } else {
    Some(SectionCtx::new(
      ctx,
      "Properties",
      SectionContentCtx::DocEntry(items),
    ))
  }
}

pub(crate) fn render_methods(
  ctx: &RenderContext,
  interface_name: &str,
  methods: &[crate::ts_type::MethodDef],
  methods_diff: Option<&crate::diff::InterfaceMethodsDiff>,
) -> Option<SectionCtx> {
  if methods.is_empty() && methods_diff.is_none_or(|d| d.removed.is_empty()) {
    return None;
  }

  let mut items = methods
    .iter()
    .enumerate()
    .map(|(i, method)| {
      let id = IdBuilder::new(ctx)
        .kind(IdKind::Method)
        .name(&method.name)
        .index(i)
        .build();

      let name = if method.name == "new" {
        "<span>new</span>".to_string()
      } else if method.computed {
        format!("[{}]", html_escape::encode_text(&method.name))
      } else {
        html_escape::encode_text(&method.name).into_owned()
      };

      let return_type = method
        .return_type
        .as_ref()
        .map(|ts_type| render_type_def_colon(ctx, ts_type))
        .unwrap_or_default();

      let mut tags = Tag::from_js_doc(&method.js_doc);
      if method.optional {
        tags.insert(Tag::Optional);
      }

      let diff_status = if let Some(diff) = methods_diff {
        if diff.added.iter().any(|m| m.name == name) {
          Some(DiffStatus::Added)
        } else if let Some(md) = diff.modified.iter().find(|m| m.name == name) {
          if let Some(name_change) = &md.name_change {
            Some(DiffStatus::Renamed {
              old_name: name_change.old.clone(),
            })
          } else {
            Some(DiffStatus::Modified)
          }
        } else {
          None
        }
      } else {
        None
      };

      let (old_content, old_tags, js_doc_changed) = if matches!(
        diff_status,
        Some(DiffStatus::Modified | DiffStatus::Renamed { .. })
      ) {
        let method_diff = methods_diff
          .and_then(|mc| mc.modified.iter().find(|m| m.name == *method.name));

        let old_content = method_diff.and_then(|md| {
          super::function::render_old_function_summary(
            ctx,
            &method.params,
            &method.return_type,
            md.params_change.as_ref(),
            md.return_type_change.as_ref(),
          )
        });

        let old_tags = method_diff.map(|diff| {
          super::compute_old_tags(
            &tags,
            None,
            None,
            None,
            diff.optional_change.as_ref(),
          )
        });

        let js_doc_changed =
          method_diff.and_then(|md| md.js_doc_change.as_ref().map(|_| true));

        (old_content, old_tags, js_doc_changed)
      } else {
        (None, None, None)
      };

      DocEntryCtx::new(
        ctx,
        id,
        Some(name),
        ctx.lookup_symbol_href(&qualify_drilldown_name(
          interface_name,
          &method.name,
          true,
        )),
        &format!(
          "{}({}){return_type}",
          type_params_summary(ctx, &method.type_params),
          render_params(ctx, &method.params)
        ),
        tags,
        method.js_doc.doc.as_deref(),
        &method.location,
        diff_status,
        old_content,
        old_tags,
        js_doc_changed,
      )
    })
    .collect::<Vec<DocEntryCtx>>();

  if let Some(method_diff) = methods_diff {
    for removed_method in &method_diff.removed {
      let return_type = removed_method
        .return_type
        .as_ref()
        .map(|ts_type| render_type_def_colon(ctx, ts_type))
        .unwrap_or_default();

      super::push_removed_method_entry(
        ctx,
        &removed_method.name,
        &format!(
          "{}({}){return_type}",
          type_params_summary(ctx, &removed_method.type_params),
          render_params(ctx, &removed_method.params)
        ),
        &removed_method.location,
        &mut items,
      );
    }
  }

  if items.is_empty() {
    None
  } else {
    Some(SectionCtx::new(
      ctx,
      "Methods",
      SectionContentCtx::DocEntry(items),
    ))
  }
}
