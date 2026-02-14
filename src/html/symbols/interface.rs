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

  // Extract InterfaceDiff if available
  let iface_diff = ctx.ctx.diff.as_ref().and_then(|diff_index| {
    let info = diff_index.get_node_diff(
      &doc_node.origin.specifier,
      doc_node.get_name(),
      doc_node.def.to_kind(),
    )?;
    let node_diff = info.diff.as_ref()?;
    if let crate::diff::DocNodeDefDiff::Interface(iface_diff) =
      node_diff.def_changes.as_ref()?
    {
      Some(iface_diff)
    } else {
      None
    }
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

  if let Some(index_signatures) =
    render_interface_index_signatures(ctx, &interface_def.index_signatures, iface_diff)
  {
    sections.push(index_signatures);
  }

  if let Some(constructors) =
    render_interface_constructors(ctx, &interface_def.constructors, iface_diff)
  {
    sections.push(constructors);
  }

  if let Some(call_signatures) =
    render_interface_call_signatures(ctx, &interface_def.call_signatures, iface_diff)
  {
    sections.push(call_signatures);
  }

  let mut properties =
    render_properties_vec(ctx, name, &interface_def.properties, iface_diff);
  if let Some(diff) = iface_diff {
    inject_removed_properties(ctx, diff, &mut properties);
  }

  if !properties.is_empty() {
    sections.push(SectionCtx::new(
      ctx,
      "Properties",
      SectionContentCtx::DocEntry(properties),
    ));
  }

  let mut methods =
    render_methods_vec(ctx, name, &interface_def.methods, iface_diff);
  if let Some(diff) = iface_diff {
    inject_removed_methods(ctx, diff, &mut methods);
  }

  if !methods.is_empty() {
    sections.push(SectionCtx::new(
      ctx,
      "Methods",
      SectionContentCtx::DocEntry(methods),
    ));
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

  if index_signatures.is_empty() && idx_diff.map_or(true, |d| d.removed.is_empty()) {
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

    let diff_status = if let Some(diff) = idx_diff {
      if !diff.added.is_empty() && i >= index_signatures.len() - diff.added.len() {
        Some(DiffStatus::Added)
      } else if diff.modified.iter().any(|_| true) && i < index_signatures.len().saturating_sub(diff.added.len()) {
        // Position-based: modified entries are at matching positions
        Some(DiffStatus::Modified)
      } else {
        None
      }
    } else {
      None
    };

    let (old_readonly, old_ts_type) = if matches!(diff_status, Some(DiffStatus::Modified)) {
      let sig_diff = idx_diff.and_then(|d| d.modified.first());
      let old_readonly = sig_diff
        .and_then(|sd| sd.readonly_change.as_ref())
        .map(|c| c.old);
      let old_ts_type = sig_diff
        .and_then(|sd| sd.type_change.as_ref())
        .map(|tc| format!(": {}", &tc.old.repr));
      (old_readonly, old_ts_type)
    } else {
      (None, None)
    };

    items.push(IndexSignatureCtx {
      anchor: AnchorCtx { id },
      readonly: index_signature.readonly,
      params: render_params(ctx, &index_signature.params),
      ts_type,
      source_href: ctx
        .ctx
        .href_resolver
        .resolve_source(&index_signature.location),
      diff_status,
      old_readonly,
      old_ts_type,
    });
  }

  // Inject removed index signatures
  if let Some(diff) = idx_diff {
    for removed_sig in &diff.removed {
      let id = IdBuilder::new(ctx)
        .kind(IdKind::IndexSignature)
        .index(items.len())
        .build();

      let ts_type = removed_sig
        .ts_type
        .as_ref()
        .map(|ts_type| render_type_def_colon(ctx, ts_type))
        .unwrap_or_default();

      items.push(IndexSignatureCtx {
        anchor: AnchorCtx { id },
        readonly: removed_sig.readonly,
        params: render_params(ctx, &removed_sig.params),
        ts_type,
        source_href: ctx
          .ctx
          .href_resolver
          .resolve_source(&removed_sig.location),
        diff_status: Some(DiffStatus::Removed),
        old_readonly: None,
        old_ts_type: None,
      });
    }
  }

  Some(SectionCtx::new(
    ctx,
    "Index Signatures",
    SectionContentCtx::IndexSignature(items),
  ))
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

  if call_signatures.is_empty() && cs_diff.map_or(true, |d| d.removed.is_empty()) {
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
        if !diff.added.is_empty() && i >= call_signatures.len() - diff.added.len() {
          Some(DiffStatus::Added)
        } else if !diff.modified.is_empty() && i < call_signatures.len().saturating_sub(diff.added.len()) {
          Some(DiffStatus::Modified)
        } else {
          None
        }
      } else {
        None
      };

      let (old_content, js_doc_changed) = if matches!(diff_status, Some(DiffStatus::Modified)) {
        let sig_diff = cs_diff.and_then(|d| d.modified.first());
        let old_content = sig_diff
          .and_then(|sd| sd.ts_type_change.as_ref())
          .map(|tc| format!(": {}", &tc.old.repr));
        let js_doc_changed = sig_diff
          .and_then(|sd| sd.js_doc_change.as_ref().map(|_| true));
        (old_content, js_doc_changed)
      } else {
        (None, None)
      };

      DocEntryCtx::new_with_diff(
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
        None,
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

      items.push(DocEntryCtx::new_with_diff(
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

  if constructors.is_empty() && ctor_diff.map_or(true, |d| d.removed.is_empty()) {
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

      let (old_content, js_doc_changed) = if matches!(diff_status, Some(DiffStatus::Modified)) {
        let cd = ctor_diff.and_then(|cc| cc.modified.iter().find(|_| true));
        let old_content = cd
          .and_then(|cd| cd.return_type_change.as_ref())
          .map(|tc| format!(": {}", &tc.old.repr));
        let js_doc_changed = cd
          .and_then(|cd| cd.js_doc_change.as_ref().map(|_| true));
        (old_content, js_doc_changed)
      } else {
        (None, None)
      };

      DocEntryCtx::new_with_diff(
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
        None,
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

      items.push(DocEntryCtx::new_with_diff(
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
  if changes.modified.iter().any(|_| true) {
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
  if prop_diff.modified.iter().any(|p| &*p.name == name) {
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
  if method_diff.modified.iter().any(|m| &*m.name == name) {
    return Some(DiffStatus::Modified);
  }
  None
}

fn inject_removed_properties(
  ctx: &RenderContext,
  iface_diff: &InterfaceDiff,
  entries: &mut Vec<DocEntryCtx>,
) {
  if let Some(prop_diff) = &iface_diff.property_changes {
    for removed_prop in &prop_diff.removed {
      let id = IdBuilder::new(ctx)
        .kind(IdKind::Property)
        .name(&removed_prop.name)
        .build();

      let ts_type = removed_prop
        .ts_type
        .as_ref()
        .map(|ts_type| render_type_def_colon(ctx, ts_type))
        .unwrap_or_default();

      entries.push(DocEntryCtx::new_with_diff(
        ctx,
        id,
        Some(html_escape::encode_text(&removed_prop.name).into_owned()),
        None,
        &ts_type,
        Default::default(),
        None,
        &removed_prop.location,
        Some(DiffStatus::Removed),
        None,
        None,
        None,
        None,
      ));
    }
  }
}

fn inject_removed_methods(
  ctx: &RenderContext,
  iface_diff: &InterfaceDiff,
  entries: &mut Vec<DocEntryCtx>,
) {
  if let Some(method_diff) = &iface_diff.method_changes {
    for removed_method in &method_diff.removed {
      let id = IdBuilder::new(ctx)
        .kind(IdKind::Method)
        .name(&removed_method.name)
        .index(0)
        .build();

      let return_type = removed_method
        .return_type
        .as_ref()
        .map(|ts_type| render_type_def_colon(ctx, ts_type))
        .unwrap_or_default();

      entries.push(DocEntryCtx::new_with_diff(
        ctx,
        id,
        Some(html_escape::encode_text(&removed_method.name).into_owned()),
        None,
        &format!(
          "{}({}){return_type}",
          type_params_summary(ctx, &removed_method.type_params),
          render_params(ctx, &removed_method.params)
        ),
        Default::default(),
        None,
        &removed_method.location,
        Some(DiffStatus::Removed),
        None,
        None,
        None,
        None,
      ));
    }
  }
}

fn render_properties_vec(
  ctx: &RenderContext,
  interface_name: &str,
  properties: &[crate::ts_type::PropertyDef],
  iface_diff: Option<&InterfaceDiff>,
) -> Vec<DocEntryCtx> {
  properties
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

      let diff_status = get_property_diff_status(iface_diff, &property.name);

      let (old_content, old_tags, js_doc_changed) = if matches!(diff_status, Some(DiffStatus::Modified)) {
        let prop_diff = iface_diff
          .and_then(|d| d.property_changes.as_ref())
          .and_then(|pc| {
            pc.modified.iter().find(|p| &*p.name == &*property.name)
          });

        let old_content = prop_diff
          .and_then(|pd| pd.type_change.as_ref())
          .map(|tc| format!(": {}", &tc.old.repr));

        let old_tags = prop_diff.map(|pd| {
          compute_old_iface_property_tags(&tags, pd)
        });

        let js_doc_changed = prop_diff.and_then(|pd| pd.js_doc_change.as_ref().map(|_| true));

        (old_content, old_tags, js_doc_changed)
      } else {
        (None, None, None)
      };

      DocEntryCtx::new_with_diff(
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
        None,
        old_content,
        old_tags,
        js_doc_changed,
      )
    })
    .collect()
}

fn render_methods_vec(
  ctx: &RenderContext,
  interface_name: &str,
  methods: &[crate::ts_type::MethodDef],
  iface_diff: Option<&InterfaceDiff>,
) -> Vec<DocEntryCtx> {
  methods
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

      let diff_status = get_method_diff_status(iface_diff, &method.name);

      let (old_content, old_tags, js_doc_changed) = if matches!(diff_status, Some(DiffStatus::Modified)) {
        let method_diff = iface_diff
          .and_then(|d| d.method_changes.as_ref())
          .and_then(|mc| mc.modified.iter().find(|m| m.name == *method.name));

        let old_content = method_diff.and_then(|md| {
          let func_diff = crate::diff::FunctionDiff {
            params_change: md.params_change.clone(),
            return_type_change: md.return_type_change.clone(),
            is_async_change: None,
            is_generator_change: None,
            type_params_change: None,
            decorators_change: None,
          };
          super::function::render_old_function_summary(
            &method.params,
            &method.return_type,
            &func_diff,
          )
        });

        let old_tags = method_diff.map(|md| compute_old_iface_method_tags(&tags, md));

        let js_doc_changed = method_diff.and_then(|md| md.js_doc_change.as_ref().map(|_| true));

        (old_content, old_tags, js_doc_changed)
      } else {
        (None, None, None)
      };

      DocEntryCtx::new_with_diff(
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
        None,
        old_content,
        old_tags,
        js_doc_changed,
      )
    })
    .collect()
}

pub(crate) fn render_properties(
  ctx: &RenderContext,
  interface_name: &str,
  properties: &[crate::ts_type::PropertyDef],
) -> Option<SectionCtx> {
  if properties.is_empty() {
    return None;
  }

  let items = render_properties_vec(ctx, interface_name, properties, None);

  Some(SectionCtx::new(
    ctx,
    "Properties",
    SectionContentCtx::DocEntry(items),
  ))
}

pub(crate) fn render_methods(
  ctx: &RenderContext,
  interface_name: &str,
  methods: &[crate::ts_type::MethodDef],
) -> Option<SectionCtx> {
  if methods.is_empty() {
    return None;
  }

  let items = render_methods_vec(ctx, interface_name, methods, None);

  Some(SectionCtx::new(
    ctx,
    "Methods",
    SectionContentCtx::DocEntry(items),
  ))
}

/// Reconstruct old tags for an interface property by reversing modifier diffs.
fn compute_old_iface_property_tags(
  current_tags: &IndexSet<Tag>,
  diff: &InterfacePropertyDiff,
) -> IndexSet<Tag> {
  let mut old_tags = current_tags.clone();

  // Reverse readonly change
  if let Some(change) = &diff.readonly_change {
    if change.new && !change.old {
      old_tags.swap_remove(&Tag::Readonly);
    } else if !change.new && change.old {
      old_tags.insert(Tag::Readonly);
    }
  }

  // Reverse optional change
  if let Some(change) = &diff.optional_change {
    if change.new && !change.old {
      old_tags.swap_remove(&Tag::Optional);
    } else if !change.new && change.old {
      old_tags.insert(Tag::Optional);
    }
  }

  old_tags
}

/// Reconstruct old tags for an interface method by reversing modifier diffs.
fn compute_old_iface_method_tags(
  current_tags: &IndexSet<Tag>,
  diff: &InterfaceMethodDiff,
) -> IndexSet<Tag> {
  let mut old_tags = current_tags.clone();

  // Reverse optional change
  if let Some(change) = &diff.optional_change {
    if change.new && !change.old {
      old_tags.swap_remove(&Tag::Optional);
    } else if !change.new && change.old {
      old_tags.insert(Tag::Optional);
    }
  }

  old_tags
}
