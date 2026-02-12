use crate::class::ClassMethodDef;
use crate::class::ClassPropertyDef;
use crate::diff::ClassDiff;
use crate::diff::MethodDiff;
use crate::diff::PropertyDiff;
use crate::html::DiffStatus;
use crate::html::DocNodeWithContext;
use crate::html::parameters::render_params;
use crate::html::render_context::RenderContext;
use crate::html::types::render_type_def_colon;
use crate::html::util::*;
use crate::js_doc::JsDocTag;
use deno_ast::swc::ast::Accessibility;
use deno_ast::swc::ast::MethodKind;
use indexmap::IndexSet;
use serde::Deserialize;
use serde::Serialize;
use std::collections::BTreeMap;
use std::collections::HashSet;

pub(crate) fn render_class(
  ctx: &RenderContext,
  doc_node: &DocNodeWithContext,
  name: &str,
) -> Vec<SectionCtx> {
  let class_def = doc_node.class_def().unwrap();

  let current_type_params = class_def
    .type_params
    .iter()
    .map(|def| def.name.as_str())
    .collect::<HashSet<&str>>();

  let ctx = &ctx.with_current_type_params(current_type_params);

  // Extract ClassDiff if available
  let class_diff = ctx.ctx.diff.as_ref().and_then(|diff_index| {
    let info = diff_index.get_node_diff(
      &doc_node.origin.specifier,
      doc_node.get_name(),
      doc_node.def.to_kind(),
    )?;
    let node_diff = info.diff.as_ref()?;
    if let crate::diff::DocNodeDefDiff::Class(class_diff) =
      node_diff.def_changes.as_ref()?
    {
      Some(class_diff)
    } else {
      None
    }
  });

  let class_items = partition_properties_and_classes(
    class_def.properties.clone(),
    class_def.methods.clone(),
  );

  let mut sections = vec![];

  if let Some(constructors) =
    render_constructors(ctx, &class_def.constructors, doc_node.get_name(), class_diff)
  {
    sections.push(constructors);
  }

  if let Some(type_params) = crate::html::types::render_type_params(
    ctx,
    &doc_node.js_doc,
    &class_def.type_params,
    &doc_node.location,
    class_diff.and_then(|d| d.type_params_change.as_ref()),
  ) {
    sections.push(type_params);
  }

  if let Some(index_signatures) =
    render_class_index_signatures(ctx, &class_def.index_signatures, class_diff)
  {
    sections.push(index_signatures);
  }

  // Collect removed static properties from diff
  let mut static_properties = render_class_properties(
    ctx,
    name,
    class_items.static_properties,
    class_diff,
  );
  if let Some(diff) = class_diff {
    inject_removed_properties(ctx, name, diff, true, &mut static_properties);
  }

  if !static_properties.is_empty() {
    sections.push(SectionCtx::new(
      ctx,
      "Static Properties",
      SectionContentCtx::DocEntry(static_properties),
    ));
  }

  let mut static_methods = render_class_methods(
    ctx,
    name,
    class_items.static_methods,
    class_diff,
  );
  if let Some(diff) = class_diff {
    inject_removed_methods(ctx, name, diff, true, &mut static_methods);
  }

  if !static_methods.is_empty() {
    sections.push(SectionCtx::new(
      ctx,
      "Static Methods",
      SectionContentCtx::DocEntry(static_methods),
    ));
  }

  let mut properties = render_class_properties(
    ctx,
    name,
    class_items.properties,
    class_diff,
  );
  if let Some(diff) = class_diff {
    inject_removed_properties(ctx, name, diff, false, &mut properties);
  }

  if !properties.is_empty() {
    sections.push(SectionCtx::new(
      ctx,
      "Properties",
      SectionContentCtx::DocEntry(properties),
    ));
  }

  let mut methods =
    render_class_methods(ctx, name, class_items.methods, class_diff);
  if let Some(diff) = class_diff {
    inject_removed_methods(ctx, name, diff, false, &mut methods);
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

fn render_constructors(
  ctx: &RenderContext,
  constructors: &[crate::class::ClassConstructorDef],
  name: &str,
  class_diff: Option<&ClassDiff>,
) -> Option<SectionCtx> {
  if constructors.is_empty() && class_diff.and_then(|d| d.constructor_changes.as_ref()).map_or(true, |c| c.removed.is_empty()) {
    return None;
  }

  let constructor_changes = class_diff.and_then(|d| d.constructor_changes.as_ref());

  let mut items = constructors
    .iter()
    .filter(|constructor| {
      !constructor.js_doc.tags.contains(&JsDocTag::Private)
        && !constructor.js_doc.tags.contains(&JsDocTag::Internal)
        && !matches!(constructor.accessibility, Some(Accessibility::Private))
    })
    .enumerate()
    .map(|(i, constructor)| {
      let id = IdBuilder::new(ctx.ctx)
        .kind(IdKind::Constructor)
        .index(i)
        .build();

      let params = constructor
        .params
        .iter()
        .map(|param| param.param.clone())
        .collect::<Vec<_>>();

      let params = render_params(ctx, &params);

      let diff_status = get_constructor_diff_status(constructor_changes, constructor);

      DocEntryCtx::new_with_diff(
        ctx,
        id,
        Some(html_escape::encode_text(&name).into_owned()),
        None,
        &format!("({params})"),
        [Tag::New].into(),
        constructor.js_doc.doc.as_deref(),
        &constructor.location,
        diff_status,
        None,
        None,
      )
    })
    .collect::<Vec<DocEntryCtx>>();

  // Inject removed constructors
  if let Some(ctor_changes) = constructor_changes {
    for removed_ctor in &ctor_changes.removed {
      let id = IdBuilder::new(ctx.ctx)
        .kind(IdKind::Constructor)
        .index(items.len())
        .build();

      let params = removed_ctor
        .params
        .iter()
        .map(|param| param.param.clone())
        .collect::<Vec<_>>();

      let params = render_params(ctx, &params);

      items.push(DocEntryCtx::new_with_diff(
        ctx,
        id,
        Some(html_escape::encode_text(&name).into_owned()),
        None,
        &format!("({params})"),
        [Tag::New].into(),
        None,
        &removed_ctor.location,
        Some(DiffStatus::Removed),
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
  constructor_changes: Option<&crate::diff::ConstructorsDiff>,
  constructor: &crate::class::ClassConstructorDef,
) -> Option<DiffStatus> {
  let changes = constructor_changes?;

  let param_count = constructor.params.len();

  if changes.added.iter().any(|c| c.params.len() == param_count) {
    return Some(DiffStatus::Added);
  }
  if changes.modified.iter().any(|_m| true) {
    // Constructors are matched by param count, and modified means the one with
    // matching param count was changed
    return Some(DiffStatus::Modified);
  }
  None
}

fn render_class_index_signatures(
  ctx: &RenderContext,
  index_signatures: &[crate::ts_type::IndexSignatureDef],
  class_diff: Option<&ClassDiff>,
) -> Option<SectionCtx> {
  let idx_diff = class_diff.and_then(|d| d.index_signature_changes.as_ref());

  if index_signatures.is_empty() && idx_diff.map_or(true, |d| d.removed.is_empty()) {
    return None;
  }

  let mut items = Vec::with_capacity(index_signatures.len());

  for (i, index_signature) in index_signatures.iter().enumerate() {
    let id = IdBuilder::new(ctx.ctx)
      .kind(IdKind::IndexSignature)
      .index(i)
      .build();

    let ts_type = index_signature
      .ts_type
      .as_ref()
      .map(|ts_type| crate::html::types::render_type_def_colon(ctx, ts_type))
      .unwrap_or_default();

    let diff_status = if let Some(diff) = idx_diff {
      if diff.added.iter().any(|s| s.params.len() == index_signature.params.len() && s.readonly == index_signature.readonly) {
        Some(DiffStatus::Added)
      } else if !diff.modified.is_empty() && i < index_signatures.len() {
        Some(DiffStatus::Modified)
      } else {
        None
      }
    } else {
      None
    };

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
      diff_status,
    });
  }

  // Inject removed index signatures
  if let Some(diff) = idx_diff {
    for removed_sig in &diff.removed {
      let id = IdBuilder::new(ctx.ctx)
        .kind(IdKind::IndexSignature)
        .index(items.len())
        .build();

      let ts_type = removed_sig
        .ts_type
        .as_ref()
        .map(|ts_type| crate::html::types::render_type_def_colon(ctx, ts_type))
        .unwrap_or_default();

      items.push(IndexSignatureCtx {
        id: id.clone(),
        anchor: AnchorCtx { id },
        readonly: removed_sig.readonly,
        params: render_params(ctx, &removed_sig.params),
        ts_type,
        source_href: ctx
          .ctx
          .href_resolver
          .resolve_source(&removed_sig.location),
        diff_status: Some(DiffStatus::Removed),
      });
    }
  }

  Some(SectionCtx::new(
    ctx,
    "Index Signatures",
    SectionContentCtx::IndexSignature(items),
  ))
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct IndexSignatureCtx {
  pub id: Id,
  pub anchor: AnchorCtx,
  pub readonly: bool,
  pub params: String,
  pub ts_type: String,
  pub source_href: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub diff_status: Option<crate::html::DiffStatus>,
}

enum PropertyOrMethod {
  Property(ClassPropertyDef),
  Method(ClassMethodDef),
}

impl PropertyOrMethod {
  fn method(&self) -> Option<&ClassMethodDef> {
    if let PropertyOrMethod::Method(method) = self {
      Some(method)
    } else {
      None
    }
  }

  fn name(&self) -> &str {
    match self {
      PropertyOrMethod::Property(prop) => &prop.name,
      PropertyOrMethod::Method(meth) => &meth.name,
    }
  }
  fn accessibility(&self) -> Option<Accessibility> {
    match self {
      PropertyOrMethod::Property(prop) => prop.accessibility,
      PropertyOrMethod::Method(meth) => meth.accessibility,
    }
  }
}

fn property_or_method_cmp(
  a: &PropertyOrMethod,
  b: &PropertyOrMethod,
) -> std::cmp::Ordering {
  // 1. public < protected < private
  let rank_accessibility = |item: &PropertyOrMethod| -> u8 {
    match item.accessibility() {
      Some(Accessibility::Public) | None => 0,
      Some(Accessibility::Protected) => 1,
      Some(Accessibility::Private) => 2,
    }
  };

  // 2. property/getter/setter < method
  let rank_category = |item: &PropertyOrMethod| -> u8 {
    match item {
      PropertyOrMethod::Property(_)
      | PropertyOrMethod::Method(ClassMethodDef {
        kind: MethodKind::Getter | MethodKind::Setter,
        ..
      }) => 0,
      _ => 1,
    }
  };

  // 3. names (alphabetically)

  // 4. getter < setter
  let rank_kind = |item: &PropertyOrMethod| -> u8 {
    match item {
      PropertyOrMethod::Method(ClassMethodDef {
        kind: MethodKind::Getter,
        ..
      }) => 0,
      PropertyOrMethod::Method(ClassMethodDef {
        kind: MethodKind::Setter,
        ..
      }) => 1,
      _ => 2,
    }
  };

  rank_accessibility(a)
    .cmp(&rank_accessibility(b))
    .then_with(|| rank_category(a).cmp(&rank_category(b)))
    .then_with(|| a.name().cmp(b.name()))
    .then_with(|| rank_kind(a).cmp(&rank_kind(b)))
}

struct ClassItems {
  properties: Vec<PropertyOrMethod>,
  static_properties: Vec<PropertyOrMethod>,
  methods: BTreeMap<Box<str>, Vec<ClassMethodDef>>,
  static_methods: BTreeMap<Box<str>, Vec<ClassMethodDef>>,
}

fn partition_properties_and_classes(
  properties: Box<[ClassPropertyDef]>,
  methods: Box<[ClassMethodDef]>,
) -> ClassItems {
  let mut out_properties = vec![];
  let mut out_static_properties = vec![];
  let mut out_methods = BTreeMap::new();
  let mut out_static_methods = BTreeMap::new();

  for property in properties.into_vec().into_iter() {
    if property.is_static {
      out_static_properties.push(PropertyOrMethod::Property(property));
    } else {
      out_properties.push(PropertyOrMethod::Property(property));
    }
  }

  for method in methods.into_vec().into_iter() {
    if matches!(method.kind, MethodKind::Getter | MethodKind::Setter) {
      if method.is_static {
        out_static_properties.push(PropertyOrMethod::Method(method));
      } else {
        out_properties.push(PropertyOrMethod::Method(method));
      }
    } else if method.is_static {
      let entry = out_static_methods
        .entry(method.name.clone())
        .or_insert(vec![]);
      entry.push(method);
    } else {
      let entry = out_methods.entry(method.name.clone()).or_insert(vec![]);
      entry.push(method);
    }
  }

  out_properties.sort_by(property_or_method_cmp);
  out_static_properties.sort_by(property_or_method_cmp);

  ClassItems {
    properties: out_properties,
    static_properties: out_static_properties,
    methods: out_methods,
    static_methods: out_static_methods,
  }
}

fn render_class_accessor(
  ctx: &RenderContext,
  class_name: &str,
  getter: Option<&ClassMethodDef>,
  setter: Option<&ClassMethodDef>,
  class_diff: Option<&ClassDiff>,
) -> DocEntryCtx {
  let getter_or_setter = getter.or(setter).unwrap();

  let name = &getter_or_setter.name;
  let id = IdBuilder::new(ctx.ctx)
    .kind(IdKind::Accessor)
    .name(name)
    .build();
  let ts_type = getter
    .and_then(|getter| getter.function_def.return_type.as_ref())
    .or_else(|| {
      setter.and_then(|setter| {
        setter
          .function_def
          .params
          .first()
          .and_then(|param| param.ts_type.as_ref())
      })
    })
    .map_or_else(String::new, |ts_type| render_type_def_colon(ctx, ts_type));
  let js_doc = getter_or_setter.js_doc.doc.as_deref();

  let mut tags = Tag::from_js_doc(&getter_or_setter.js_doc);
  if let Some(tag) = Tag::from_accessibility(getter_or_setter.accessibility) {
    tags.insert(tag);
  }
  if getter_or_setter.is_abstract {
    tags.insert(Tag::Abstract);
  }
  if getter.is_some() && setter.is_none() {
    tags.insert(Tag::Readonly);
  } else if getter.is_none() && setter.is_some() {
    tags.insert(Tag::Writeonly);
  }

  let diff_status = get_method_diff_status(
    class_diff,
    name,
    getter_or_setter.is_static,
    getter_or_setter.kind,
  );

  DocEntryCtx::new_with_diff(
    ctx,
    id,
    Some(html_escape::encode_text(&name).into_owned()),
    ctx.lookup_symbol_href(&qualify_drilldown_name(
      class_name,
      name,
      getter_or_setter.is_static,
    )),
    &ts_type,
    tags,
    js_doc,
    &getter_or_setter.location,
    diff_status,
    None,
    None,
  )
}

fn render_class_method(
  ctx: &RenderContext,
  class_name: &str,
  method: &ClassMethodDef,
  i: usize,
  class_diff: Option<&ClassDiff>,
) -> Option<DocEntryCtx> {
  if method.function_def.has_body && i != 0 {
    return None;
  }

  let id = IdBuilder::new(ctx.ctx)
    .kind(IdKind::Method)
    .name(&method.name)
    .index(i)
    .build();

  let mut tags = Tag::from_js_doc(&method.js_doc);
  if let Some(tag) = Tag::from_accessibility(method.accessibility) {
    tags.insert(tag);
  }
  if method.is_abstract {
    tags.insert(Tag::Abstract);
  }
  if method.optional {
    tags.insert(Tag::Optional);
  }

  let diff_status =
    get_method_diff_status(class_diff, &method.name, method.is_static, method.kind);

  let (old_content, old_tags) = if matches!(diff_status, Some(DiffStatus::Modified)) {
    let method_diff = class_diff
      .and_then(|d| d.method_changes.as_ref())
      .and_then(|mc| mc.modified.iter().find(|m| &*m.name == &*method.name));

    let old_content = method_diff
      .and_then(|md| md.function_diff.as_ref())
      .and_then(|fd| fd.return_type_change.as_ref())
      .map(|tc| format!(": {}", &tc.old.repr));

    let old_tags = method_diff.map(|md| compute_old_method_tags(&tags, md));

    (old_content, old_tags)
  } else {
    (None, None)
  };

  Some(DocEntryCtx::new_with_diff(
    ctx,
    id,
    Some(html_escape::encode_text(&method.name).into_owned()),
    ctx.lookup_symbol_href(&qualify_drilldown_name(
      class_name,
      &method.name,
      method.is_static,
    )),
    &super::function::render_function_summary(&method.function_def, ctx),
    tags,
    method.js_doc.doc.as_deref(),
    &method.location,
    diff_status,
    old_content,
    old_tags,
  ))
}

fn render_class_property(
  ctx: &RenderContext,
  class_name: &str,
  property: &ClassPropertyDef,
  class_diff: Option<&ClassDiff>,
) -> DocEntryCtx {
  let id = IdBuilder::new(ctx.ctx)
    .kind(IdKind::Property)
    .name(&property.name)
    .build();

  let mut tags = Tag::from_js_doc(&property.js_doc);
  if let Some(tag) = Tag::from_accessibility(property.accessibility) {
    tags.insert(tag);
  }
  if property.is_abstract {
    tags.insert(Tag::Abstract);
  }
  if property.readonly {
    tags.insert(Tag::Readonly);
  }
  if property.optional {
    tags.insert(Tag::Optional);
  }

  let ts_type = property
    .ts_type
    .as_ref()
    .map(|ts_type| render_type_def_colon(ctx, ts_type))
    .unwrap_or_default();

  let diff_status = get_property_diff_status(
    class_diff,
    &property.name,
    property.is_static,
  );

  // For modified properties, render the old type and old tags
  let (old_content, old_tags) = if matches!(diff_status, Some(DiffStatus::Modified)) {
    let prop_diff = class_diff
      .and_then(|d| d.property_changes.as_ref())
      .and_then(|pc| pc.modified.iter().find(|p| &*p.name == &*property.name));

    let old_content = prop_diff
      .and_then(|pd| pd.type_change.as_ref())
      .map(|tc| format!(": {}", &tc.old.repr));

    let old_tags = prop_diff.map(|pd| {
      compute_old_property_tags(&tags, pd)
    });

    (old_content, old_tags)
  } else {
    (None, None)
  };

  DocEntryCtx::new_with_diff(
    ctx,
    id,
    Some(html_escape::encode_text(&property.name).into_owned()),
    ctx.lookup_symbol_href(&qualify_drilldown_name(
      class_name,
      &property.name,
      property.is_static,
    )),
    &ts_type,
    tags,
    property.js_doc.doc.as_deref(),
    &property.location,
    diff_status,
    old_content,
    old_tags,
  )
}

fn get_property_diff_status(
  class_diff: Option<&ClassDiff>,
  name: &str,
  is_static: bool,
) -> Option<DiffStatus> {
  let diff = class_diff?;
  let prop_diff = diff.property_changes.as_ref()?;

  if prop_diff.added.iter().any(|p| &*p.name == name && p.is_static == is_static) {
    return Some(DiffStatus::Added);
  }
  if prop_diff.modified.iter().any(|p| &*p.name == name) {
    return Some(DiffStatus::Modified);
  }
  None
}

fn get_method_diff_status(
  class_diff: Option<&ClassDiff>,
  name: &str,
  is_static: bool,
  kind: MethodKind,
) -> Option<DiffStatus> {
  let diff = class_diff?;
  let method_diff = diff.method_changes.as_ref()?;

  if method_diff.added.iter().any(|m| &*m.name == name && m.is_static == is_static && m.kind == kind) {
    return Some(DiffStatus::Added);
  }
  if method_diff.modified.iter().any(|m| &*m.name == name) {
    return Some(DiffStatus::Modified);
  }
  None
}

fn inject_removed_properties(
  ctx: &RenderContext,
  _class_name: &str,
  class_diff: &ClassDiff,
  is_static: bool,
  entries: &mut Vec<DocEntryCtx>,
) {
  if let Some(prop_diff) = &class_diff.property_changes {
    for removed_prop in &prop_diff.removed {
      if removed_prop.is_static != is_static {
        continue;
      }
      let id = IdBuilder::new(ctx.ctx)
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
      ));
    }
  }
}

fn inject_removed_methods(
  ctx: &RenderContext,
  _class_name: &str,
  class_diff: &ClassDiff,
  is_static: bool,
  entries: &mut Vec<DocEntryCtx>,
) {
  if let Some(method_diff) = &class_diff.method_changes {
    for removed_method in &method_diff.removed {
      if removed_method.is_static != is_static {
        continue;
      }
      // Skip getters/setters (they go in properties section)
      if matches!(
        removed_method.kind,
        MethodKind::Getter | MethodKind::Setter
      ) {
        continue;
      }
      let id = IdBuilder::new(ctx.ctx)
        .kind(IdKind::Method)
        .name(&removed_method.name)
        .index(0)
        .build();

      entries.push(DocEntryCtx::new_with_diff(
        ctx,
        id,
        Some(html_escape::encode_text(&removed_method.name).into_owned()),
        None,
        &super::function::render_function_summary(
          &removed_method.function_def,
          ctx,
        ),
        Default::default(),
        None,
        &removed_method.location,
        Some(DiffStatus::Removed),
        None,
        None,
      ));
    }
  }
}

fn render_class_properties(
  ctx: &RenderContext,
  class_name: &str,
  properties: Vec<PropertyOrMethod>,
  class_diff: Option<&ClassDiff>,
) -> Vec<DocEntryCtx> {
  let mut properties = properties.into_iter().peekable();
  let mut out = vec![];

  while let Some(property) = properties.next() {
    let content = match property {
      PropertyOrMethod::Property(property) => {
        render_class_property(ctx, class_name, &property, class_diff)
      }
      PropertyOrMethod::Method(method) => {
        let (getter, setter) = if method.kind == MethodKind::Getter {
          let next_is_setter = properties
            .peek()
            .and_then(|property| property.method())
            .is_some_and(|method| method.kind == MethodKind::Setter);

          if next_is_setter {
            let res = if let PropertyOrMethod::Method(method) =
              properties.next().unwrap()
            {
              method
            } else {
              unreachable!()
            };

            (Some(&method), Some(res))
          } else {
            (Some(&method), None)
          }
        } else {
          (None, Some(method))
        };

        render_class_accessor(ctx, class_name, getter, setter.as_ref(), class_diff)
      }
    };

    out.push(content)
  }

  out
}

fn render_class_methods(
  ctx: &RenderContext,
  class_name: &str,
  methods: BTreeMap<Box<str>, Vec<ClassMethodDef>>,
  class_diff: Option<&ClassDiff>,
) -> Vec<DocEntryCtx> {
  methods
    .values()
    .flat_map(|methods| {
      methods.iter().enumerate().filter_map(|(i, method)| {
        render_class_method(ctx, class_name, method, i, class_diff)
      })
    })
    .collect()
}

/// Reconstruct old tags for a class property by reversing modifier diffs.
fn compute_old_property_tags(
  current_tags: &IndexSet<Tag>,
  diff: &PropertyDiff,
) -> IndexSet<Tag> {
  let mut old_tags = current_tags.clone();

  // Reverse accessibility change
  if let Some(change) = &diff.accessibility_change {
    // Remove new accessibility tag, add old one
    if let Some(new_tag) = Tag::from_accessibility(change.new) {
      old_tags.swap_remove(&new_tag);
    }
    if let Some(old_tag) = Tag::from_accessibility(change.old) {
      old_tags.insert(old_tag);
    }
  }

  // Reverse readonly change
  if let Some(change) = &diff.readonly_change {
    if change.new && !change.old {
      old_tags.swap_remove(&Tag::Readonly);
    } else if !change.new && change.old {
      old_tags.insert(Tag::Readonly);
    }
  }

  // Reverse abstract change
  if let Some(change) = &diff.is_abstract_change {
    if change.new && !change.old {
      old_tags.swap_remove(&Tag::Abstract);
    } else if !change.new && change.old {
      old_tags.insert(Tag::Abstract);
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

/// Reconstruct old tags for a class method by reversing modifier diffs.
fn compute_old_method_tags(
  current_tags: &IndexSet<Tag>,
  diff: &MethodDiff,
) -> IndexSet<Tag> {
  let mut old_tags = current_tags.clone();

  // Reverse accessibility change
  if let Some(change) = &diff.accessibility_change {
    if let Some(new_tag) = Tag::from_accessibility(change.new) {
      old_tags.swap_remove(&new_tag);
    }
    if let Some(old_tag) = Tag::from_accessibility(change.old) {
      old_tags.insert(old_tag);
    }
  }

  // Reverse abstract change
  if let Some(change) = &diff.is_abstract_change {
    if change.new && !change.old {
      old_tags.swap_remove(&Tag::Abstract);
    } else if !change.new && change.old {
      old_tags.insert(Tag::Abstract);
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
