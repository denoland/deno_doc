use crate::class::ClassMethodDef;
use crate::class::ClassPropertyDef;
use crate::html::DocNodeWithContext;
use crate::html::parameters::render_params;
use crate::html::render_context::RenderContext;
use crate::html::types::render_type_def_colon;
use crate::html::util::*;
use crate::js_doc::JsDocTag;
use deno_ast::swc::ast::Accessibility;
use deno_ast::swc::ast::MethodKind;
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

  let class_items = partition_properties_and_classes(
    class_def.properties.clone(),
    class_def.methods.clone(),
  );

  let mut sections = vec![];

  if let Some(constructors) =
    render_constructors(ctx, &class_def.constructors, doc_node.get_name())
  {
    sections.push(constructors);
  }

  if let Some(type_params) = crate::html::types::render_type_params(
    ctx,
    &doc_node.js_doc,
    &class_def.type_params,
    &doc_node.location,
  ) {
    sections.push(type_params);
  }

  if let Some(index_signatures) =
    super::interface::render_index_signatures(ctx, &class_def.index_signatures)
  {
    sections.push(index_signatures);
  }

  if !class_items.static_properties.is_empty() {
    sections.push(SectionCtx::new(
      ctx,
      "Static Properties",
      SectionContentCtx::DocEntry(render_class_properties(
        ctx,
        name,
        class_items.static_properties,
      )),
    ));
  }

  if !class_items.static_methods.is_empty() {
    sections.push(SectionCtx::new(
      ctx,
      "Static Methods",
      SectionContentCtx::DocEntry(render_class_methods(
        ctx,
        name,
        class_items.static_methods,
      )),
    ));
  }

  if !class_items.properties.is_empty() {
    sections.push(SectionCtx::new(
      ctx,
      "Properties",
      SectionContentCtx::DocEntry(render_class_properties(
        ctx,
        name,
        class_items.properties,
      )),
    ));
  }

  if !class_items.methods.is_empty() {
    sections.push(SectionCtx::new(
      ctx,
      "Methods",
      SectionContentCtx::DocEntry(render_class_methods(
        ctx,
        name,
        class_items.methods,
      )),
    ));
  }

  sections
}

fn render_constructors(
  ctx: &RenderContext,
  constructors: &[crate::class::ClassConstructorDef],
  name: &str,
) -> Option<SectionCtx> {
  if constructors.is_empty() {
    return None;
  }

  let items = constructors
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

      DocEntryCtx::new(
        ctx,
        id,
        Some(html_escape::encode_text(&name).into_owned()),
        None,
        &format!("({params})"),
        [Tag::New].into(),
        constructor.js_doc.doc.as_deref(),
        &constructor.location,
      )
    })
    .collect::<Vec<DocEntryCtx>>();

  Some(SectionCtx::new(
    ctx,
    "Constructors",
    SectionContentCtx::DocEntry(items),
  ))
}

#[derive(Debug, Serialize, Clone)]
pub struct IndexSignatureCtx {
  pub id: Id,
  pub anchor: AnchorCtx,
  pub readonly: bool,
  pub params: String,
  pub ts_type: String,
  pub source_href: Option<String>,
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

  fn name(&self) -> &Box<str> {
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
  let name_cmp = a.name().cmp(b.name());

  let accessibility_cmp = match (a.accessibility(), b.accessibility()) {
    (Some(a_acc), Some(b_acc)) => match (a_acc, b_acc) {
      (Accessibility::Public, Accessibility::Public) => {
        std::cmp::Ordering::Equal
      }
      (Accessibility::Private, Accessibility::Private) => {
        std::cmp::Ordering::Equal
      }
      (Accessibility::Protected, Accessibility::Protected) => {
        std::cmp::Ordering::Equal
      }
      (Accessibility::Private, _) => std::cmp::Ordering::Greater,
      (_, Accessibility::Private) => std::cmp::Ordering::Less,
      (Accessibility::Protected, _) => std::cmp::Ordering::Greater,
      (_, Accessibility::Protected) => std::cmp::Ordering::Less,
    },
    _ => std::cmp::Ordering::Equal,
  };

  let accessor_cmp = {
    let a_accessor = a.method().map(|method| &method.kind);
    let b_accessor = b.method().map(|method| &method.kind);

    match (a_accessor, b_accessor) {
      (Some(k1), Some(k2)) => match (k1, k2) {
        (MethodKind::Getter, MethodKind::Getter) => std::cmp::Ordering::Equal,
        (MethodKind::Setter, MethodKind::Setter) => std::cmp::Ordering::Equal,
        (MethodKind::Getter, MethodKind::Setter) => std::cmp::Ordering::Greater,
        (MethodKind::Setter, MethodKind::Getter) => std::cmp::Ordering::Less,
        _ => unreachable!(),
      },
      _ => std::cmp::Ordering::Equal,
    }
  };

  accessibility_cmp.then(accessor_cmp).then(name_cmp)
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
  )
}

fn render_class_method(
  ctx: &RenderContext,
  class_name: &str,
  method: &ClassMethodDef,
  i: usize,
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

  Some(DocEntryCtx::new(
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
  ))
}

fn render_class_property(
  ctx: &RenderContext,
  class_name: &str,
  property: &ClassPropertyDef,
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
  )
}

fn render_class_properties(
  ctx: &RenderContext,
  class_name: &str,
  properties: Vec<PropertyOrMethod>,
) -> Vec<DocEntryCtx> {
  let mut properties = properties.into_iter().peekable();
  let mut out = vec![];

  while let Some(property) = properties.next() {
    let content = match property {
      PropertyOrMethod::Property(property) => {
        render_class_property(ctx, class_name, &property)
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

        render_class_accessor(ctx, class_name, getter, setter.as_ref())
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
) -> Vec<DocEntryCtx> {
  methods
    .values()
    .flat_map(|methods| {
      methods.iter().enumerate().filter_map(|(i, method)| {
        render_class_method(ctx, class_name, method, i)
      })
    })
    .collect()
}
