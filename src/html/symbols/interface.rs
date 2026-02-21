use crate::diff::InterfaceConstructorsDiff;
use crate::diff::InterfaceDiff;
use crate::diff::InterfaceMethodDiff;
use crate::diff::InterfacePropertyDiff;
use crate::html::DiffStatus;
use crate::html::DocNodeWithContext;
use crate::html::parameters::render_params;
use crate::html::render_context::RenderContext;
use crate::html::symbols::class::IndexSignatureCtx;
use crate::html::types::render_type_def_colon;
use crate::html::types::type_params_summary;
use crate::html::util::*;
use indexmap::IndexSet;

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

  let iface_diff = ctx.ctx.diff.as_ref().and_then(|diff_index| {
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
    iface_diff.and_then(|d| d.type_params_change.as_ref()),
  ) {
    sections.push(type_params);
  }

  if let Some(index_signatures) = render_interface_index_signatures(
    ctx,
    &interface_def.index_signatures,
    iface_diff,
  ) {
    sections.push(index_signatures);
  }

  if let Some(constructors) =
    render_interface_constructors(ctx, &interface_def.constructors, iface_diff)
  {
    sections.push(constructors);
  }

  if let Some(call_signatures) = render_interface_call_signatures(
    ctx,
    &interface_def.call_signatures,
    iface_diff,
  ) {
    sections.push(call_signatures);
  }

  if let Some(properties) =
    render_properties(ctx, name, &interface_def.properties, iface_diff)
  {
    sections.push(properties);
  }

  if let Some(methods) =
    render_methods(ctx, name, &interface_def.methods, iface_diff)
  {
    sections.push(methods);
  }

  sections
}

pub(crate) fn render_index_signatures(
  ctx: &RenderContext,
  index_signatures: &[crate::ts_type::IndexSignatureDef],
) -> Option<SectionCtx> {
  if index_signatures.is_empty() {
    return None;
  }

  let mut items = Vec::with_capacity(index_signatures.len());

  for (i, index_signature) in index_signatures.iter().enumerate() {
    let id = IdBuilder::new(ctx)
      .kind(IdKind::IndexSignature)
      .index(i)
      .build();

    let ts_type = index_signature
      .ts_type
      .as_ref()
      .map(|ts_type| render_type_def_colon(ctx, ts_type))
      .unwrap_or_default();

    items.push(IndexSignatureCtx {
      anchor: AnchorCtx::new(id),
      readonly: index_signature.readonly,
      params: render_params(ctx, &index_signature.params),
      ts_type,
      source_href: ctx
        .ctx
        .href_resolver
        .resolve_source(&index_signature.location),
      diff_status: None,
      old_readonly: None,
      old_params: None,
      old_ts_type: None,
    });
  }

  Some(SectionCtx::new(
    ctx,
    "Index Signatures",
    SectionContentCtx::IndexSignature(items),
  ))
}

fn render_interface_index_signatures(
  ctx: &RenderContext,
  index_signatures: &[crate::ts_type::IndexSignatureDef],
  iface_diff: Option<&InterfaceDiff>,
) -> Option<SectionCtx> {
  let idx_diff = iface_diff.and_then(|d| d.index_signature_changes.as_ref());
  let empty_sigs = Vec::new();
  let empty_mod: Vec<crate::diff::InterfaceIndexSignatureDiff> = Vec::new();
  let total = index_signatures.len();

  super::render_index_signatures_with_diff(
    ctx,
    index_signatures,
    idx_diff.map_or(&empty_sigs, |d| &d.removed),
    idx_diff.map_or(&empty_mod, |d| &d.modified),
    |i, _sig| {
      let diff = idx_diff?;
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
) -> Option<SectionCtx> {
  if call_signatures.is_empty() {
    return None;
  }

  let items = call_signatures
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
        None,
        None,
        None,
        None,
      )
    })
    .collect::<Vec<DocEntryCtx>>();

  Some(SectionCtx::new(
    ctx,
    "Call Signatures",
    SectionContentCtx::DocEntry(items),
  ))
}

fn render_interface_call_signatures(
  ctx: &RenderContext,
  call_signatures: &[crate::ts_type::CallSignatureDef],
  iface_diff: Option<&InterfaceDiff>,
) -> Option<SectionCtx> {
  let cs_diff = iface_diff.and_then(|d| d.call_signature_changes.as_ref());

  if call_signatures.is_empty() && cs_diff.is_none_or(|d| d.removed.is_empty())
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

      let diff_status = if let Some(diff) = cs_diff {
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
          let sig_diff = cs_diff.and_then(|d| d.modified.first());
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
        None,
        js_doc_changed,
      )
    })
    .collect::<Vec<DocEntryCtx>>();

  // Inject removed call signatures
  if let Some(diff) = cs_diff {
    for removed_sig in &diff.removed {
      let id = IdBuilder::new(ctx)
        .kind(IdKind::CallSignature)
        .index(items.len())
        .build();

      let ts_type = removed_sig
        .ts_type
        .as_ref()
        .map(|ts_type| render_type_def_colon(ctx, ts_type))
        .unwrap_or_default();

      items.push(DocEntryCtx::new(
        ctx,
        id,
        None,
        None,
        &format!(
          "{}({}){ts_type}",
          type_params_summary(ctx, &removed_sig.type_params),
          render_params(ctx, &removed_sig.params),
        ),
        Default::default(),
        None,
        &removed_sig.location,
        Some(DiffStatus::Removed),
        None,
        None,
        None,
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

fn render_interface_constructors(
  ctx: &RenderContext,
  constructors: &[crate::ts_type::ConstructorDef],
  iface_diff: Option<&InterfaceDiff>,
) -> Option<SectionCtx> {
  let ctor_diff = iface_diff.and_then(|d| d.constructor_changes.as_ref());

  if constructors.is_empty() && ctor_diff.is_none_or(|d| d.removed.is_empty()) {
    return None;
  }

  let mut items = constructors
    .iter()
    .enumerate()
    .map(|(i, constructor)| {
      let id = IdBuilder::new(ctx)
        .kind(IdKind::Constructor)
        .index(i)
        .build();

      let return_type = constructor
        .return_type
        .as_ref()
        .map(|ts_type| render_type_def_colon(ctx, ts_type))
        .unwrap_or_default();

      let tags = Tag::from_js_doc(&constructor.js_doc);

      let diff_status = get_constructor_diff_status(ctor_diff, constructor);

      let (old_content, js_doc_changed) =
        if matches!(diff_status, Some(DiffStatus::Modified)) {
          let cd = ctor_diff.and_then(|cc| cc.modified.first());
          let old_content = cd.and_then(|cd| {
            super::function::render_old_function_summary(
              ctx,
              &constructor.params,
              &constructor.return_type,
              cd.params_change.as_ref(),
              cd.return_type_change.as_ref(),
            )
          });
          let js_doc_changed =
            cd.and_then(|cd| cd.js_doc_change.as_ref().map(|_| true));
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
          "new {}({}){return_type}",
          type_params_summary(ctx, &constructor.type_params),
          render_params(ctx, &constructor.params),
        ),
        tags,
        constructor.js_doc.doc.as_deref(),
        &constructor.location,
        diff_status,
        old_content,
        None,
        js_doc_changed,
      )
    })
    .collect::<Vec<DocEntryCtx>>();

  // Inject removed constructors
  if let Some(diff) = ctor_diff {
    for removed_ctor in &diff.removed {
      let id = IdBuilder::new(ctx)
        .kind(IdKind::Constructor)
        .index(items.len())
        .build();

      let return_type = removed_ctor
        .return_type
        .as_ref()
        .map(|ts_type| render_type_def_colon(ctx, ts_type))
        .unwrap_or_default();

      items.push(DocEntryCtx::new(
        ctx,
        id,
        None,
        None,
        &format!(
          "new {}({}){return_type}",
          type_params_summary(ctx, &removed_ctor.type_params),
          render_params(ctx, &removed_ctor.params),
        ),
        Default::default(),
        None,
        &removed_ctor.location,
        Some(DiffStatus::Removed),
        None,
        None,
        None,
      ));
    }
  }

  if items.is_empty() {
    return None;
  }

  Some(SectionCtx::new(
    ctx,
    "Constructors",
    SectionContentCtx::DocEntry(items),
  ))
}

fn get_constructor_diff_status(
  ctor_diff: Option<&InterfaceConstructorsDiff>,
  constructor: &crate::ts_type::ConstructorDef,
) -> Option<DiffStatus> {
  let changes = ctor_diff?;
  let param_count = constructor.params.len();

  if changes.added.iter().any(|c| c.params.len() == param_count) {
    return Some(DiffStatus::Added);
  }
  if changes
    .modified
    .iter()
    .any(|m| m.param_count == param_count)
  {
    return Some(DiffStatus::Modified);
  }
  None
}

fn get_property_diff_status(
  iface_diff: Option<&InterfaceDiff>,
  name: &str,
) -> Option<DiffStatus> {
  let diff = iface_diff?;
  let prop_diff = diff.property_changes.as_ref()?;

  if prop_diff.added.iter().any(|p| &*p.name == name) {
    return Some(DiffStatus::Added);
  }
  if let Some(pd) = prop_diff.modified.iter().find(|p| p.name == name) {
    if let Some(nc) = &pd.name_change {
      return Some(DiffStatus::Renamed {
        old_name: nc.old.clone(),
      });
    }
    return Some(DiffStatus::Modified);
  }
  None
}

fn get_method_diff_status(
  iface_diff: Option<&InterfaceDiff>,
  name: &str,
) -> Option<DiffStatus> {
  let diff = iface_diff?;
  let method_diff = diff.method_changes.as_ref()?;

  if method_diff.added.iter().any(|m| &*m.name == name) {
    return Some(DiffStatus::Added);
  }
  if let Some(md) = method_diff.modified.iter().find(|m| m.name == name) {
    if let Some(nc) = &md.name_change {
      return Some(DiffStatus::Renamed {
        old_name: nc.old.clone(),
      });
    }
    return Some(DiffStatus::Modified);
  }
  None
}

pub(crate) fn render_properties(
  ctx: &RenderContext,
  interface_name: &str,
  properties: &[crate::ts_type::PropertyDef],
  interface_diff: Option<&InterfaceDiff>,
) -> Option<SectionCtx> {
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

      let diff_status =
        get_property_diff_status(interface_diff, &property.name);

      let (old_content, old_tags, js_doc_changed) = if matches!(
        diff_status,
        Some(DiffStatus::Modified | DiffStatus::Renamed { .. })
      ) {
        let prop_diff = interface_diff
          .and_then(|d| d.property_changes.as_ref())
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
    .collect::<Vec<_>>();

  if let Some(interface_diff) = interface_diff
    && let Some(prop_diff) = &interface_diff.property_changes
  {
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
  interface_diff: Option<&InterfaceDiff>,
) -> Option<SectionCtx> {
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

      let diff_status = get_method_diff_status(interface_diff, &method.name);

      let (old_content, old_tags, js_doc_changed) = if matches!(
        diff_status,
        Some(DiffStatus::Modified | DiffStatus::Renamed { .. })
      ) {
        let method_diff = interface_diff
          .and_then(|d| d.method_changes.as_ref())
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
    .collect::<Vec<_>>();

  if let Some(interface_diff) = interface_diff
    && let Some(method_diff) = &interface_diff.method_changes
  {
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
