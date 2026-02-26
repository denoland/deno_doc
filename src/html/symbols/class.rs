use crate::class::ClassMethodDef;
use crate::class::ClassPropertyDef;
use crate::diff::ConstructorDiff;
use crate::diff::ConstructorsDiff;
use crate::diff::IndexSignaturesDiff;
use crate::diff::MethodsDiff;
use crate::diff::PropertiesDiff;
use crate::html::DiffStatus;
use crate::html::DocNodeWithContext;
use crate::html::parameters::render_params;
use crate::html::render_context::RenderContext;
use crate::html::types::render_type_def_colon;
use crate::html::util::*;
use crate::js_doc::JsDocTag;
use deno_ast::swc::ast::Accessibility;
use deno_ast::swc::ast::MethodKind;
use indexmap::IndexMap;
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

  let class_diff = ctx.ctx.diff.as_ref().and_then(|diff_index| {
    diff_index
      .get_def_diff(
        &doc_node.origin.specifier,
        doc_node.get_name(),
        doc_node.def.to_kind(),
      )
      .and_then(|d| d.as_class())
  });

  let class_items = partition_class_items(
    class_def.properties.clone(),
    class_def.methods.clone(),
    class_diff.and_then(|d| d.property_changes.as_ref()),
    class_diff.and_then(|d| d.method_changes.as_ref()),
  );

  let mut sections = vec![];

  if let Some(constructors) = render_constructors(
    ctx,
    &class_def.constructors,
    doc_node.get_name(),
    class_diff.and_then(|d| d.constructor_changes.as_ref()),
  ) {
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

  if let Some(index_signatures) = render_class_index_signatures(
    ctx,
    &class_def.index_signatures,
    class_diff.and_then(|d| d.index_signature_changes.as_ref()),
  ) {
    sections.push(index_signatures);
  }

  let static_properties = render_class_properties(
    ctx,
    name,
    class_items.static_properties,
    class_items.static_property_changes.as_ref(),
    class_items.static_method_changes.as_ref(),
  );

  if !static_properties.is_empty() {
    sections.push(SectionCtx::new(
      ctx,
      "Static Properties",
      SectionContentCtx::DocEntry(static_properties),
    ));
  }

  let static_methods = render_class_methods(
    ctx,
    name,
    class_items.static_methods,
    class_items.static_method_changes.as_ref(),
  );

  if !static_methods.is_empty() {
    sections.push(SectionCtx::new(
      ctx,
      "Static Methods",
      SectionContentCtx::DocEntry(static_methods),
    ));
  }

  let properties = render_class_properties(
    ctx,
    name,
    class_items.properties,
    class_items.property_changes.as_ref(),
    class_items.method_changes.as_ref(),
  );

  if !properties.is_empty() {
    sections.push(SectionCtx::new(
      ctx,
      "Properties",
      SectionContentCtx::DocEntry(properties),
    ));
  }

  let methods = render_class_methods(
    ctx,
    name,
    class_items.methods,
    class_items.method_changes.as_ref(),
  );

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
  constructor_changes: Option<&ConstructorsDiff>,
) -> Option<SectionCtx> {
  if constructors.is_empty()
    && constructor_changes.is_none_or(|c| c.removed.is_empty())
  {
    return None;
  }

  let mut items = constructors
    .iter()
    .filter(|constructor| {
      !constructor.js_doc.tags.contains(&JsDocTag::Private)
        && !constructor.js_doc.tags.contains(&JsDocTag::Internal)
        && !matches!(constructor.accessibility, Some(Accessibility::Private))
    })
    .enumerate()
    .map(|(i, constructor)| {
      let id = IdBuilder::new(ctx)
        .kind(IdKind::Constructor)
        .index(i)
        .build();

      let params = constructor
        .params
        .iter()
        .map(|param| param.param.clone())
        .collect::<Vec<_>>();

      let params = render_params(ctx, &params);

      let diff_status =
        get_constructor_diff_status(constructor_changes, constructor);

      let (old_content, js_doc_changed) =
        if matches!(diff_status, Some(DiffStatus::Modified)) {
          let ctor_diff =
            constructor_changes.and_then(|cc| cc.modified.first());

          let old_content = ctor_diff.and_then(|cd| {
            render_old_class_constructor_summary(ctx, &constructor.params, cd)
          });
          let js_doc_changed =
            ctor_diff.and_then(|cd| cd.js_doc_change.as_ref().map(|_| true));
          (old_content, js_doc_changed)
        } else {
          (None, None)
        };

      let mut entry = DocEntryCtx::new(
        ctx,
        id,
        Some(html_escape::encode_text(&name).into_owned()),
        None,
        &format!("({params})"),
        Default::default(),
        constructor.js_doc.doc.as_deref(),
        &constructor.location,
        diff_status,
        old_content,
        None,
        js_doc_changed,
      );
      entry.name_prefix = Some("new".into());

      entry
    })
    .collect::<Vec<DocEntryCtx>>();

  // Inject removed constructors
  if let Some(ctor_changes) = constructor_changes {
    for removed_ctor in &ctor_changes.removed {
      let id = IdBuilder::new(ctx)
        .kind(IdKind::Constructor)
        .index(items.len())
        .build();

      let params = removed_ctor
        .params
        .iter()
        .map(|param| param.param.clone())
        .collect::<Vec<_>>();

      let params = render_params(ctx, &params);

      let mut doc_entry = DocEntryCtx::removed(
        ctx,
        id,
        Some(html_escape::encode_text(&name).into_owned()),
        None,
        &format!("({params})"),
        Default::default(),
        None,
        &removed_ctor.location,
      );
      doc_entry.name_prefix = Some("new".into());

      items.push(doc_entry);
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
  if changes
    .modified
    .iter()
    .any(|m| m.param_count == param_count)
  {
    return Some(DiffStatus::Modified);
  }
  None
}

/// Reconstruct the old constructor params from the new params and the diff,
/// then render through the standard `render_params` path.
fn render_old_class_constructor_summary(
  ctx: &RenderContext,
  params: &[crate::class::ClassConstructorParamDef],
  ctor_diff: &ConstructorDiff,
) -> Option<String> {
  let params_change = ctor_diff.params_change.as_ref()?;

  let added_count = params_change.added.len();
  let shared_count = params.len() - added_count;

  let mut old_params = Vec::new();

  for (i, new_param) in params.iter().enumerate().take(shared_count) {
    let modified = params_change.modified.iter().find(|p| p.index == i);

    if let Some(param_diff) = modified {
      if let Some(pc) = &param_diff.param_change {
        let old_param_diff = pc.modified.first();

        let mut param = old_param_diff
          .and_then(|pd| pd.pattern_change.as_ref())
          .map(|pc| pc.old.clone())
          .unwrap_or_else(|| new_param.param.clone());

        if let Some(tc) =
          old_param_diff.and_then(|pd| pd.type_change.as_ref())
        {
          param.ts_type = Some(tc.old.clone());
        }

        old_params.push(param);
      } else {
        old_params.push(new_param.param.clone());
      }
    } else {
      old_params.push(new_param.param.clone());
    }
  }

  old_params.extend(params_change.removed.iter().map(|r| r.param.clone()));

  Some(format!(
    "({})",
    render_params(ctx, &old_params)
  ))
}

fn render_class_index_signatures(
  ctx: &RenderContext,
  index_signatures: &[crate::ts_type::IndexSignatureDef],
  idx_diff: Option<&IndexSignaturesDiff>,
) -> Option<SectionCtx> {
  let empty_sigs = Vec::new();
  let empty_mod = Vec::new();

  super::render_index_signatures_with_diff(
    ctx,
    index_signatures,
    idx_diff.map_or(&empty_sigs, |d| &d.removed),
    idx_diff.map_or(&empty_mod, |d| &d.modified),
    |_i, sig| {
      let diff = idx_diff?;
      if diff.added.iter().any(|s| {
        s.params.len() == sig.params.len() && s.readonly == sig.readonly
      }) {
        Some(DiffStatus::Added)
      } else if !diff.modified.is_empty() {
        Some(DiffStatus::Modified)
      } else {
        None
      }
    },
  )
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct IndexSignatureCtx {
  pub anchor: AnchorCtx,
  pub readonly: bool,
  pub params: String,
  pub ts_type: String,
  pub source_href: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub diff_status: Option<crate::html::DiffStatus>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub old_readonly: Option<bool>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub old_params: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub old_ts_type: Option<String>,
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
  property_changes: Option<PropertiesDiff>,
  static_property_changes: Option<PropertiesDiff>,
  method_changes: Option<MethodsDiff>,
  static_method_changes: Option<MethodsDiff>,
}

fn partition_class_items(
  properties: Box<[ClassPropertyDef]>,
  methods: Box<[ClassMethodDef]>,
  property_diff: Option<&PropertiesDiff>,
  method_diff: Option<&MethodsDiff>,
) -> ClassItems {
  let mut out_properties = vec![];
  let mut out_static_properties = vec![];
  let mut out_methods = BTreeMap::new();
  let mut out_static_methods = BTreeMap::new();

  for property in properties
    .into_vec()
    .into_iter()
    .filter(|p| !matches!(p.accessibility, Some(Accessibility::Private)))
  {
    if property.is_static {
      out_static_properties.push(PropertyOrMethod::Property(property));
    } else {
      out_properties.push(PropertyOrMethod::Property(property));
    }
  }

  for method in methods
    .into_vec()
    .into_iter()
    .filter(|p| !matches!(p.accessibility, Some(Accessibility::Private)))
  {
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

  let (property_changes, static_property_changes) =
    partition_properties_diff(property_diff);
  let (method_changes, static_method_changes) =
    partition_methods_diff(method_diff);

  ClassItems {
    properties: out_properties,
    static_properties: out_static_properties,
    methods: out_methods,
    static_methods: out_static_methods,
    property_changes,
    static_property_changes,
    method_changes,
    static_method_changes,
  }
}

fn partition_properties_diff(
  diff: Option<&PropertiesDiff>,
) -> (Option<PropertiesDiff>, Option<PropertiesDiff>) {
  let Some(diff) = diff else {
    return (None, None);
  };

  let (static_added, instance_added): (Vec<_>, Vec<_>) =
    diff.added.iter().cloned().partition(|p| p.is_static);
  let (static_removed, instance_removed): (Vec<_>, Vec<_>) =
    diff.removed.iter().cloned().partition(|p| p.is_static);

  let instance = PropertiesDiff {
    added: instance_added,
    removed: instance_removed,
    modified: diff.modified.clone(),
  };
  let r#static = PropertiesDiff {
    added: static_added,
    removed: static_removed,
    modified: diff.modified.clone(),
  };

  (Some(instance), Some(r#static))
}

fn partition_methods_diff(
  diff: Option<&MethodsDiff>,
) -> (Option<MethodsDiff>, Option<MethodsDiff>) {
  let Some(diff) = diff else {
    return (None, None);
  };

  let (static_added, instance_added): (Vec<_>, Vec<_>) =
    diff.added.iter().cloned().partition(|m| m.is_static);
  let (static_removed, instance_removed): (Vec<_>, Vec<_>) =
    diff.removed.iter().cloned().partition(|m| m.is_static);

  let instance = MethodsDiff {
    added: instance_added,
    removed: instance_removed,
    modified: diff.modified.clone(),
  };
  let r#static = MethodsDiff {
    added: static_added,
    removed: static_removed,
    modified: diff.modified.clone(),
  };

  (Some(instance), Some(r#static))
}

fn render_class_accessor(
  ctx: &RenderContext,
  class_name: &str,
  getter: Option<&ClassMethodDef>,
  setter: Option<&ClassMethodDef>,
  method_changes: Option<&MethodsDiff>,
) -> DocEntryCtx {
  let getter_or_setter = getter.or(setter).unwrap();

  let name = &getter_or_setter.name;
  let id = IdBuilder::new(ctx)
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

  let diff_status =
    get_method_diff_status(method_changes, name, getter_or_setter.kind);

  DocEntryCtx::new(
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
    None,
  )
}

fn render_class_method(
  ctx: &RenderContext,
  class_name: &str,
  method: &ClassMethodDef,
  i: usize,
  method_changes: Option<&MethodsDiff>,
) -> Option<DocEntryCtx> {
  if method.function_def.has_body && i != 0 {
    return None;
  }

  let id = IdBuilder::new(ctx)
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
    get_method_diff_status(method_changes, &method.name, method.kind);

  let (old_content, old_tags, js_doc_changed) = if matches!(
    diff_status,
    Some(DiffStatus::Modified | DiffStatus::Renamed { .. })
  ) {
    let method_diff = method_changes
      .and_then(|mc| mc.modified.iter().find(|m| m.name == method.name));

    let old_content = method_diff
      .and_then(|md| md.function_diff.as_ref())
      .and_then(|fd| {
        super::function::render_old_function_summary(
          ctx,
          &method.function_def.type_params,
          &method.function_def.params,
          &method.function_def.return_type,
          fd.type_params_change.as_ref(),
          fd.params_change.as_ref(),
          fd.return_type_change.as_ref(),
        )
      });

    let old_tags = method_diff.map(|diff| {
      super::compute_old_tags(
        &tags,
        diff.accessibility_change.as_ref(),
        None,
        diff.is_abstract_change.as_ref(),
        diff.optional_change.as_ref(),
      )
    });

    let js_doc_changed =
      method_diff.and_then(|md| md.js_doc_change.as_ref().map(|_| true));

    (old_content, old_tags, js_doc_changed)
  } else {
    (None, None, None)
  };

  Some(DocEntryCtx::new(
    ctx,
    id,
    Some(html_escape::encode_text(&method.name).into_owned()),
    ctx.lookup_symbol_href(&qualify_drilldown_name(
      class_name,
      &method.name,
      method.is_static,
    )),
    &super::function::render_function_summary(
      ctx,
      &method.function_def.type_params,
      &method.function_def.params,
      &method.function_def.return_type,
    ),
    tags,
    method.js_doc.doc.as_deref(),
    &method.location,
    diff_status,
    old_content,
    old_tags,
    js_doc_changed,
  ))
}

fn render_class_property(
  ctx: &RenderContext,
  class_name: &str,
  property: &ClassPropertyDef,
  property_changes: Option<&PropertiesDiff>,
) -> DocEntryCtx {
  let id = IdBuilder::new(ctx)
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

  let diff_status = get_property_diff_status(property_changes, &property.name);

  // For modified/renamed properties, render the old type and old tags
  let (old_content, old_tags, js_doc_changed) = if matches!(
    diff_status,
    Some(DiffStatus::Modified | DiffStatus::Renamed { .. })
  ) {
    let prop_diff = property_changes
      .and_then(|pc| pc.modified.iter().find(|p| p.name == property.name));

    let old_content = prop_diff
      .and_then(|pd| pd.type_change.as_ref())
      .map(|tc| render_type_def_colon(ctx, &tc.old));

    let old_tags = prop_diff.map(|diff| {
      super::compute_old_tags(
        &tags,
        diff.accessibility_change.as_ref(),
        diff.readonly_change.as_ref(),
        diff.is_abstract_change.as_ref(),
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
    js_doc_changed,
  )
}

fn get_property_diff_status(
  property_changes: Option<&PropertiesDiff>,
  name: &str,
) -> Option<DiffStatus> {
  let prop_diff = property_changes?;

  if prop_diff.added.iter().any(|p| &*p.name == name) {
    return Some(DiffStatus::Added);
  }
  if let Some(pd) = prop_diff.modified.iter().find(|p| &*p.name == name) {
    if let Some(nc) = &pd.name_change {
      return Some(DiffStatus::Renamed {
        old_name: nc.old.to_string(),
      });
    }
    return Some(DiffStatus::Modified);
  }
  None
}

fn get_method_diff_status(
  method_changes: Option<&MethodsDiff>,
  name: &str,
  kind: MethodKind,
) -> Option<DiffStatus> {
  let method_diff = method_changes?;

  if method_diff
    .added
    .iter()
    .any(|m| &*m.name == name && m.kind == kind)
  {
    return Some(DiffStatus::Added);
  }
  if let Some(md) = method_diff.modified.iter().find(|m| &*m.name == name) {
    if let Some(nc) = &md.name_change {
      return Some(DiffStatus::Renamed {
        old_name: nc.old.to_string(),
      });
    }
    return Some(DiffStatus::Modified);
  }
  None
}

fn render_class_properties(
  ctx: &RenderContext,
  class_name: &str,
  properties: Vec<PropertyOrMethod>,
  property_changes: Option<&PropertiesDiff>,
  method_changes: Option<&MethodsDiff>,
) -> Vec<DocEntryCtx> {
  let mut properties = properties.into_iter().peekable();
  let mut out = vec![];

  while let Some(property) = properties.next() {
    let content = match property {
      PropertyOrMethod::Property(property) => {
        render_class_property(ctx, class_name, &property, property_changes)
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

        render_class_accessor(
          ctx,
          class_name,
          getter,
          setter.as_ref(),
          method_changes,
        )
      }
    };

    out.push(content)
  }

  // Inject removed properties
  if let Some(prop_diff) = property_changes {
    for removed_prop in &prop_diff.removed {
      super::push_removed_property_entry(
        ctx,
        &removed_prop.name,
        removed_prop.ts_type.as_ref(),
        &removed_prop.location,
        &mut out,
      );
    }
  }

  // Inject removed getters/setters (skipped by inject_removed_methods)
  if let Some(method_diff) = method_changes {
    let mut removed_accessors: IndexMap<
      &str,
      (Option<&ClassMethodDef>, Option<&ClassMethodDef>),
    > = IndexMap::new();

    for removed_method in &method_diff.removed {
      match removed_method.kind {
        MethodKind::Getter => {
          removed_accessors.entry(&removed_method.name).or_default().0 =
            Some(removed_method);
        }
        MethodKind::Setter => {
          removed_accessors.entry(&removed_method.name).or_default().1 =
            Some(removed_method);
        }
        _ => {}
      }
    }

    for (_name, (getter, setter)) in removed_accessors {
      let getter_or_setter = getter.or(setter).unwrap();
      let name = &getter_or_setter.name;

      let id = IdBuilder::new(ctx)
        .kind(IdKind::Accessor)
        .name(name)
        .build();

      let ts_type = getter
        .and_then(|g| g.function_def.return_type.as_ref())
        .or_else(|| {
          setter.and_then(|s| {
            s.function_def
              .params
              .first()
              .and_then(|param| param.ts_type.as_ref())
          })
        })
        .map_or_else(String::new, |ts_type| {
          render_type_def_colon(ctx, ts_type)
        });

      let mut tags = Tag::from_js_doc(&getter_or_setter.js_doc);
      if let Some(tag) = Tag::from_accessibility(getter_or_setter.accessibility)
      {
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

      out.push(DocEntryCtx::removed(
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

  out
}

fn render_class_methods(
  ctx: &RenderContext,
  class_name: &str,
  methods: BTreeMap<Box<str>, Vec<ClassMethodDef>>,
  method_changes: Option<&MethodsDiff>,
) -> Vec<DocEntryCtx> {
  let mut out: Vec<DocEntryCtx> = methods
    .values()
    .flat_map(|methods| {
      methods.iter().enumerate().filter_map(|(i, method)| {
        render_class_method(ctx, class_name, method, i, method_changes)
      })
    })
    .collect();

  if let Some(method_diff) = method_changes {
    for removed_method in &method_diff.removed {
      // Skip getters/setters (they go in properties section)
      if matches!(removed_method.kind, MethodKind::Getter | MethodKind::Setter)
      {
        continue;
      }
      super::push_removed_method_entry(
        ctx,
        &removed_method.name,
        &super::function::render_function_summary(
          ctx,
          &removed_method.function_def.type_params,
          &removed_method.function_def.params,
          &removed_method.function_def.return_type,
        ),
        &removed_method.location,
        &mut out,
      );
    }
  }

  out
}
