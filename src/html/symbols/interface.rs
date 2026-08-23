use crate::Declaration;
use crate::html::DiffStatus;
use crate::html::DocNodeWithContext;
use crate::html::parameters::render_params;
use crate::html::render_context::RenderContext;
use crate::html::types::render_type_def_colon;
use crate::html::types::type_params_summary;
use crate::html::util::*;
use deno_ast::swc::ast::MethodKind;
use indexmap::IndexMap;

pub(crate) fn render_interface(
  ctx: &RenderContext,
  symbol: &DocNodeWithContext,
  decl: &Declaration,
) -> Vec<SectionCtx> {
  let interface_def = decl.interface_def().unwrap();
  let name = symbol.get_qualified_name();

  let current_type_params = interface_def
    .type_params
    .iter()
    .map(|def| def.name.as_str())
    .collect::<std::collections::HashSet<&str>>();
  let ctx = &ctx.with_current_type_params(current_type_params);

  let interface_diff = ctx.ctx.diff.as_ref().and_then(|diff_index| {
    diff_index
      .get_def_diff(
        &symbol.declared_origin().specifier,
        symbol.get_name(),
        decl.def.to_kind(),
      )
      .and_then(|d| d.as_interface())
  });

  let mut sections = vec![];

  if let Some(type_params) = crate::html::types::render_type_params(
    ctx,
    &decl.js_doc,
    &interface_def.type_params,
    &decl.location,
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

  if let Some(construct_signatures) = render_construct_signatures(
    ctx,
    &interface_def.constructors,
    interface_diff.and_then(|d| d.constructor_changes.as_ref()),
  ) {
    sections.push(construct_signatures);
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
    &interface_def.methods,
    interface_diff.and_then(|d| d.property_changes.as_ref()),
    interface_diff.and_then(|d| d.method_changes.as_ref()),
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
      } else if diff.modified.iter().any(|m| m.index == i) {
        Some(DiffStatus::Modified)
      } else {
        None
      }
    },
  )
}

pub(crate) fn render_construct_signatures(
  ctx: &RenderContext,
  constructors: &[crate::ts_type::ConstructorDef],
  constructor_changes: Option<&crate::diff::InterfaceConstructorsDiff>,
) -> Option<SectionCtx> {
  if constructors.is_empty()
    && constructor_changes.is_none_or(|d| d.removed.is_empty())
  {
    return None;
  }

  let mut items = constructors
    .iter()
    .enumerate()
    .map(|(i, constructor)| {
      let id = IdBuilder::new(ctx)
        .kind(IdKind::ConstructSignature)
        .index(i)
        .build();
      let id_for_params = id.clone();

      let return_type = constructor
        .return_type
        .as_ref()
        .map(|ts_type| render_type_def_colon(ctx, ts_type))
        .unwrap_or_default();

      let tags = Tag::from_js_doc(&constructor.js_doc);

      let ctor_diff = constructor_changes.and_then(|d| {
        d.modified
          .iter()
          .find(|m| m.param_count == constructor.params.len())
      });

      let diff_status = if let Some(diff) = constructor_changes {
        if diff.added.iter().any(|a| a == constructor) {
          Some(DiffStatus::Added)
        } else if ctor_diff.is_some() {
          Some(DiffStatus::Modified)
        } else {
          None
        }
      } else {
        None
      };

      let old_tags = if matches!(
        diff_status,
        Some(DiffStatus::Modified | DiffStatus::Renamed { .. })
      ) {
        Some(super::compute_old_tags(&tags, None, None, None, None))
      } else {
        None
      };

      let old_content = if matches!(diff_status, Some(DiffStatus::Modified)) {
        ctor_diff.and_then(|cd| {
          super::function::render_old_function_summary(
            ctx,
            &constructor.type_params,
            &constructor.params,
            &constructor.return_type,
            cd.type_params_change.as_ref(),
            cd.params_change.as_ref(),
            cd.return_type_change.as_ref(),
          )
        })
      } else {
        None
      };

      let mut entry = DocEntryCtx::new(
        ctx,
        id,
        None,
        None,
        &format!(
          "{}({}){return_type}",
          type_params_summary(ctx, &constructor.type_params),
          render_params(ctx, &constructor.params),
        ),
        tags,
        constructor.js_doc.doc.as_deref(),
        &constructor.location,
        diff_status,
        old_content,
        old_tags,
        ctor_diff.and_then(|cd| cd.js_doc_change.as_ref()),
      );
      entry.name_prefix = Some("new".into());
      entry.examples =
        crate::html::jsdoc::jsdoc_example_ctxs(ctx, &constructor.js_doc);
      crate::html::parameters::attach_signature_docs(
        ctx,
        &mut entry,
        &constructor.params,
        &constructor.js_doc,
        &id_for_params,
        &constructor.location,
      );

      entry
    })
    .collect::<Vec<DocEntryCtx>>();

  if let Some(diff) = constructor_changes {
    for constructor in &diff.removed {
      let id = IdBuilder::new(ctx)
        .kind(IdKind::ConstructSignature)
        .index(items.len())
        .build();

      let tags = Tag::from_js_doc(&constructor.js_doc);

      let return_type = constructor
        .return_type
        .as_ref()
        .map(|ts_type| render_type_def_colon(ctx, ts_type))
        .unwrap_or_default();

      let mut entry = DocEntryCtx::removed(
        ctx,
        id,
        None,
        None,
        &format!(
          "{}({}){return_type}",
          type_params_summary(ctx, &constructor.type_params),
          render_params(ctx, &constructor.params),
        ),
        tags,
        constructor.js_doc.doc.as_deref(),
        &constructor.location,
      );
      entry.name_prefix = Some("new".into());

      items.push(entry);
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
      let id_for_params = id.clone();

      let ts_type = call_signature
        .ts_type
        .as_ref()
        .map(|ts_type| render_type_def_colon(ctx, ts_type))
        .unwrap_or_default();

      let tags = Tag::from_js_doc(&call_signature.js_doc);

      let sig_diff = call_signatures_diff
        .and_then(|d| d.modified.iter().find(|m| m.index == i));

      let diff_status = if let Some(diff) = call_signatures_diff {
        if diff.added.iter().any(|a| a == call_signature) {
          Some(DiffStatus::Added)
        } else if sig_diff.is_some() {
          Some(DiffStatus::Modified)
        } else {
          None
        }
      } else {
        None
      };

      let old_tags = if matches!(
        diff_status,
        Some(DiffStatus::Modified | DiffStatus::Renamed { .. })
      ) {
        Some(super::compute_old_tags(&tags, None, None, None, None))
      } else {
        None
      };

      let old_content = if matches!(diff_status, Some(DiffStatus::Modified)) {
        sig_diff.and_then(|sd| {
          super::function::render_old_function_summary(
            ctx,
            &call_signature.type_params,
            &call_signature.params,
            &call_signature.ts_type,
            sd.type_params_change.as_ref(),
            sd.params_change.as_ref(),
            sd.ts_type_change.as_ref(),
          )
        })
      } else {
        None
      };

      let mut entry = DocEntryCtx::new(
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
        sig_diff.and_then(|sd| sd.js_doc_change.as_ref()),
      );
      entry.examples =
        crate::html::jsdoc::jsdoc_example_ctxs(ctx, &call_signature.js_doc);
      crate::html::parameters::attach_signature_docs(
        ctx,
        &mut entry,
        &call_signature.params,
        &call_signature.js_doc,
        &id_for_params,
        &call_signature.location,
      );

      entry
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
  methods: &[crate::ts_type::MethodDef],
  properties_diff: Option<&crate::diff::InterfacePropertiesDiff>,
  methods_diff: Option<&crate::diff::InterfaceMethodsDiff>,
) -> Option<SectionCtx> {
  // getter/setter pairs render as a single accessor entry in the properties
  // section (like classes), not as methods
  let mut accessors: IndexMap<
    &str,
    (
      Option<&crate::ts_type::MethodDef>,
      Option<&crate::ts_type::MethodDef>,
    ),
  > = IndexMap::new();
  for method in methods {
    match method.kind {
      MethodKind::Getter => {
        accessors.entry(&method.name).or_default().0 = Some(method);
      }
      MethodKind::Setter => {
        accessors.entry(&method.name).or_default().1 = Some(method);
      }
      MethodKind::Method => {}
    }
  }

  let has_removed_accessors = methods_diff.is_some_and(|d| {
    d.removed
      .iter()
      .any(|m| matches!(m.kind, MethodKind::Getter | MethodKind::Setter))
  });

  if properties.is_empty()
    && accessors.is_empty()
    && properties_diff.is_none_or(|d| d.removed.is_empty())
    && !has_removed_accessors
  {
    return None;
  }

  // Interleave properties and accessors in declaration order (an accessor
  // pair sits at its first-declared member), matching how classes mix the two
  // in one section.
  enum Member<'a> {
    Property(&'a crate::ts_type::PropertyDef),
    Accessor(
      Option<&'a crate::ts_type::MethodDef>,
      Option<&'a crate::ts_type::MethodDef>,
    ),
  }

  let mut members = properties
    .iter()
    .map(|property| (&property.location, Member::Property(property)))
    .collect::<Vec<_>>();
  for (_name, (getter, setter)) in &accessors {
    let location = match (getter, setter) {
      (Some(getter), Some(setter)) => {
        if (setter.location.line, setter.location.col)
          < (getter.location.line, getter.location.col)
        {
          &setter.location
        } else {
          &getter.location
        }
      }
      (Some(getter), None) => &getter.location,
      (None, Some(setter)) => &setter.location,
      (None, None) => unreachable!(),
    };
    members.push((location, Member::Accessor(*getter, *setter)));
  }
  members.sort_by_key(|(location, _)| (location.line, location.col));

  let mut items = members
    .into_iter()
    .map(|(_location, member)| match member {
      Member::Property(property) => {
        render_property(ctx, interface_name, property, properties_diff)
      }
      Member::Accessor(getter, setter) => {
        render_accessor(ctx, interface_name, getter, setter, methods_diff)
      }
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

  // Inject removed getters/setters (skipped by render_methods). A name that
  // still has a live accessor is skipped here: its live entry reports the
  // partial removal as a modification instead.
  if let Some(method_diff) = methods_diff {
    let mut removed_accessors: IndexMap<
      &str,
      (
        Option<&crate::ts_type::MethodDef>,
        Option<&crate::ts_type::MethodDef>,
      ),
    > = IndexMap::new();

    for removed_method in &method_diff.removed {
      if accessors.contains_key(&*removed_method.name) {
        continue;
      }
      match removed_method.kind {
        MethodKind::Getter => {
          removed_accessors.entry(&removed_method.name).or_default().0 =
            Some(removed_method);
        }
        MethodKind::Setter => {
          removed_accessors.entry(&removed_method.name).or_default().1 =
            Some(removed_method);
        }
        MethodKind::Method => {}
      }
    }

    for (_name, (getter, setter)) in removed_accessors {
      let getter_or_setter = getter.or(setter).unwrap();
      let name = &getter_or_setter.name;

      let id = IdBuilder::new(ctx)
        .kind(IdKind::Accessor)
        .name(name)
        .build();

      let ts_type = accessor_type(ctx, getter, setter);
      let tags = accessor_tags(getter, setter);

      items.push(DocEntryCtx::removed(
        ctx,
        id,
        Some(html_escape::encode_text(name).into_owned()),
        None,
        &ts_type,
        tags,
        getter_or_setter.js_doc.doc.as_deref(),
        &getter_or_setter.location,
      ));
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

fn render_property(
  ctx: &RenderContext,
  interface_name: &str,
  property: &crate::ts_type::PropertyDef,
  properties_diff: Option<&crate::diff::InterfacePropertiesDiff>,
) -> DocEntryCtx {
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

  let (old_content, old_tags, prop_diff) = if matches!(
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

    (old_content, old_tags, prop_diff)
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
    prop_diff.and_then(|pd| pd.js_doc_change.as_ref()),
  )
}

fn accessor_type(
  ctx: &RenderContext,
  getter: Option<&crate::ts_type::MethodDef>,
  setter: Option<&crate::ts_type::MethodDef>,
) -> String {
  super::render_accessor_type(
    ctx,
    getter.and_then(|getter| getter.return_type.as_ref()),
    setter.and_then(|setter| {
      setter
        .params
        .first()
        .and_then(|param| param.ts_type.as_ref())
    }),
  )
}

fn accessor_tags(
  getter: Option<&crate::ts_type::MethodDef>,
  setter: Option<&crate::ts_type::MethodDef>,
) -> indexmap::IndexSet<Tag> {
  let getter_or_setter = getter.or(setter).unwrap();

  let mut tags = Tag::from_js_doc(&getter_or_setter.js_doc);
  if getter_or_setter.optional {
    tags.insert(Tag::Optional);
  }
  if getter.is_some() && setter.is_none() {
    tags.insert(Tag::Readonly);
  } else if getter.is_none() && setter.is_some() {
    tags.insert(Tag::Writeonly);
  }

  tags
}

fn render_accessor(
  ctx: &RenderContext,
  interface_name: &str,
  getter: Option<&crate::ts_type::MethodDef>,
  setter: Option<&crate::ts_type::MethodDef>,
  methods_diff: Option<&crate::diff::InterfaceMethodsDiff>,
) -> DocEntryCtx {
  let getter_or_setter = getter.or(setter).unwrap();
  let name = &getter_or_setter.name;

  let id = IdBuilder::new(ctx)
    .kind(IdKind::Accessor)
    .name(name)
    .build();

  let ts_type = accessor_type(ctx, getter, setter);
  let tags = accessor_tags(getter, setter);

  // The diff tracks the getter and the setter as separate members (a pair
  // shares its name), so every lookup must match on kind as well.
  let getter_added = getter.is_some()
    && methods_diff.is_some_and(|d| {
      d.added
        .iter()
        .any(|m| m.name == *name && m.kind == MethodKind::Getter)
    });
  let setter_added = setter.is_some()
    && methods_diff.is_some_and(|d| {
      d.added
        .iter()
        .any(|m| m.name == *name && m.kind == MethodKind::Setter)
    });
  let removed_getter = methods_diff.and_then(|d| {
    d.removed
      .iter()
      .find(|m| m.name == *name && m.kind == MethodKind::Getter)
  });
  let removed_setter = methods_diff.and_then(|d| {
    d.removed
      .iter()
      .find(|m| m.name == *name && m.kind == MethodKind::Setter)
  });
  let getter_diff = getter.and(methods_diff).and_then(|mc| {
    mc.modified
      .iter()
      .find(|m| m.name == *name && m.kind == MethodKind::Getter)
  });
  let setter_diff = setter.and(methods_diff).and_then(|mc| {
    mc.modified
      .iter()
      .find(|m| m.name == *name && m.kind == MethodKind::Setter)
  });

  let members_removed = removed_getter.is_some() || removed_setter.is_some();
  let members_modified = getter_diff.is_some() || setter_diff.is_some();
  // Only a pair with no pre-existing member is truly new; a setter joining an
  // existing getter (or vice versa) modifies the accessor.
  let fully_added = !members_removed
    && match (getter, setter) {
      (Some(_), Some(_)) => getter_added && setter_added,
      (Some(_), None) => getter_added,
      (None, Some(_)) => setter_added,
      (None, None) => unreachable!(),
    };

  let name_change = getter_diff
    .and_then(|d| d.name_change.as_ref())
    .or_else(|| setter_diff.and_then(|d| d.name_change.as_ref()));

  let diff_status = if fully_added {
    Some(DiffStatus::Added)
  } else if let Some(name_change) = name_change {
    Some(DiffStatus::Renamed {
      old_name: name_change.old.clone(),
    })
  } else if getter_added || setter_added || members_removed || members_modified
  {
    Some(DiffStatus::Modified)
  } else {
    None
  };

  let (old_content, old_tags) = if matches!(
    diff_status,
    Some(DiffStatus::Modified | DiffStatus::Renamed { .. })
  ) {
    // Reconstruct each member's previous type: a removed member contributes
    // its own type, an added member contributes nothing, and a surviving
    // member contributes its old type (or its current one when unchanged).
    let old_getter_type = if let Some(removed_getter) = removed_getter {
      removed_getter.return_type.as_ref()
    } else if getter_added {
      None
    } else {
      getter.and_then(|g| {
        getter_diff
          .and_then(|d| d.return_type_change.as_ref())
          .map(|tc| &tc.old)
          .or(g.return_type.as_ref())
      })
    };
    let old_setter_type = if let Some(removed_setter) = removed_setter {
      removed_setter
        .params
        .first()
        .and_then(|param| param.ts_type.as_ref())
    } else if setter_added {
      None
    } else {
      setter.and_then(|s| {
        setter_diff
          .and_then(|d| d.params_change.as_ref())
          .and_then(|pc| pc.modified.iter().find(|pd| pd.index == 0))
          .and_then(|pd| pd.type_change.as_ref())
          .map(|tc| &tc.old)
          .or_else(|| s.params.first().and_then(|param| param.ts_type.as_ref()))
      })
    };

    let types_changed = getter_added
      || setter_added
      || members_removed
      || getter_diff.is_some_and(|d| d.return_type_change.is_some())
      || setter_diff.is_some_and(|d| d.params_change.is_some());

    let old_content = types_changed
      .then(|| {
        super::render_accessor_type(ctx, old_getter_type, old_setter_type)
      })
      .filter(|old| *old != ts_type);

    let optional_change = getter_diff
      .and_then(|d| d.optional_change.as_ref())
      .or_else(|| setter_diff.and_then(|d| d.optional_change.as_ref()));
    let mut old_tags =
      super::compute_old_tags(&tags, None, None, None, optional_change);
    let old_has_getter =
      removed_getter.is_some() || (getter.is_some() && !getter_added);
    let old_has_setter =
      removed_setter.is_some() || (setter.is_some() && !setter_added);
    old_tags.swap_remove(&Tag::Readonly);
    old_tags.swap_remove(&Tag::Writeonly);
    if old_has_getter && !old_has_setter {
      old_tags.insert(Tag::Readonly);
    } else if !old_has_getter && old_has_setter {
      old_tags.insert(Tag::Writeonly);
    }

    (old_content, Some(old_tags))
  } else {
    (None, None)
  };

  let js_doc_change = getter_diff
    .and_then(|md| md.js_doc_change.as_ref())
    .or_else(|| setter_diff.and_then(|md| md.js_doc_change.as_ref()));

  DocEntryCtx::new(
    ctx,
    id,
    Some(if getter_or_setter.computed {
      format!("[{}]", html_escape::encode_text(name))
    } else {
      html_escape::encode_text(name).into_owned()
    }),
    ctx.lookup_symbol_href(&qualify_drilldown_name(interface_name, name, true)),
    &ts_type,
    tags,
    getter_or_setter.js_doc.doc.as_deref(),
    &getter_or_setter.location,
    diff_status,
    old_content,
    old_tags,
    js_doc_change,
  )
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

  // ids index overloads within a name (like class methods), not positions in
  // the method list, so links built elsewhere can rely on `_0` for the first
  // overload of a method
  let mut method_indexes = std::collections::HashMap::<&str, usize>::new();
  let mut items = methods
    .iter()
    // getters/setters render as accessors in the properties section
    .filter(|method| method.kind == MethodKind::Method)
    .map(|method| {
      let index = method_indexes.entry(method.name.as_ref()).or_default();
      let i = *index;
      *index += 1;

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
        if diff
          .added
          .iter()
          .any(|m| m.name == method.name && m.kind == MethodKind::Method)
        {
          Some(DiffStatus::Added)
        } else if let Some(md) = diff
          .modified
          .iter()
          .find(|m| m.name == method.name && m.kind == MethodKind::Method)
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

      let (old_content, old_tags, method_diff) = if matches!(
        diff_status,
        Some(DiffStatus::Modified | DiffStatus::Renamed { .. })
      ) {
        let method_diff = methods_diff.and_then(|mc| {
          mc.modified
            .iter()
            .find(|m| m.name == *method.name && m.kind == MethodKind::Method)
        });

        let old_content = method_diff.and_then(|md| {
          super::function::render_old_function_summary(
            ctx,
            &method.type_params,
            &method.params,
            &method.return_type,
            md.type_params_change.as_ref(),
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

        (old_content, old_tags, method_diff)
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
        method_diff.and_then(|md| md.js_doc_change.as_ref()),
      )
    })
    .collect::<Vec<DocEntryCtx>>();

  if let Some(method_diff) = methods_diff {
    for removed_method in &method_diff.removed {
      // Skip getters/setters (they go in the properties section)
      if removed_method.kind != MethodKind::Method {
        continue;
      }

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
