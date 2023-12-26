use crate::class::ClassMethodDef;
use crate::class::ClassPropertyDef;
use crate::html::parameters::render_params;
use crate::html::render_context::RenderContext;
use crate::html::types::render_type_def;
use crate::html::util::*;
use deno_ast::swc::ast::Accessibility;
use deno_ast::swc::ast::MethodKind;
use serde::Serialize;
use std::collections::BTreeMap;

pub(crate) fn render_class(
  ctx: &RenderContext,
  doc_node: &crate::DocNode,
) -> Vec<SectionCtx> {
  let class_def = doc_node.class_def.as_ref().unwrap();

  let current_type_params = class_def
    .type_params
    .iter()
    .map(|def| def.name.clone())
    .collect::<std::collections::HashSet<String>>();

  let ctx = &ctx.with_current_type_params(current_type_params);

  let class_items = partition_properties_and_classes(
    class_def.properties.clone(),
    class_def.methods.clone(),
  );

  let mut sections = vec![];

  if let Some(constructors) =
    render_constructors(ctx, &class_def.constructors, &doc_node.name)
  {
    sections.push(constructors);
  }

  if let Some(type_params) =
    crate::html::types::render_type_params(ctx, &class_def.type_params)
  {
    sections.push(type_params);
  }

  if let Some(index_signatures) =
    render_index_signatures(ctx, &class_def.index_signatures)
  {
    sections.push(index_signatures);
  }

  if !class_items.properties.is_empty() {
    sections.push(SectionCtx {
      title: "Properties",
      content: SectionContentCtx::DocEntry(render_class_properties(
        ctx,
        class_items.properties,
      )),
    });
  }

  if !class_items.methods.is_empty() {
    sections.push(SectionCtx {
      title: "Methods",
      content: SectionContentCtx::DocEntry(render_class_methods(
        ctx,
        class_items.methods,
      )),
    });
  }

  if !class_items.static_properties.is_empty() {
    sections.push(SectionCtx {
      title: "Static Properties",
      content: SectionContentCtx::DocEntry(render_class_properties(
        ctx,
        class_items.static_properties,
      )),
    });
  }

  if !class_items.static_methods.is_empty() {
    sections.push(SectionCtx {
      title: "Static Methods",
      content: SectionContentCtx::DocEntry(render_class_methods(
        ctx,
        class_items.static_methods,
      )),
    })
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
    .enumerate()
    .map(|(i, constructor)| {
      let id = name_to_id("constructor", &i.to_string());

      // TODO: tags, render constructor params
      DocEntryCtx::new(ctx, &id, name, "()", constructor.js_doc.doc.as_deref())
    })
    .collect::<Vec<DocEntryCtx>>();

  Some(SectionCtx {
    title: "Constructors",
    content: SectionContentCtx::DocEntry(items),
  })
}

#[derive(Debug, Serialize, Clone)]
pub struct IndexSignatureCtx {
  pub id: String,
  pub anchor: AnchorCtx,
  pub readonly: bool,
  pub params: String,
  pub ts_type: String,
}

fn render_index_signatures(
  ctx: &RenderContext,
  index_signatures: &[crate::class::ClassIndexSignatureDef],
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
      .map(|ts_type| format!(": {}", render_type_def(ctx, ts_type)))
      .unwrap_or_default();

    items.push(IndexSignatureCtx {
      id: id.clone(),
      anchor: AnchorCtx { id },
      readonly: index_signature.readonly,
      params: render_params(ctx, &index_signature.params),
      ts_type,
    });
  }

  Some(SectionCtx {
    title: "Index Signatures",
    content: SectionContentCtx::IndexSignature(items),
  })
}

enum PropertyOrMethod {
  Property(ClassPropertyDef),
  Method(ClassMethodDef),
}

impl PropertyOrMethod {
  fn prop(&self) -> Option<&ClassPropertyDef> {
    if let PropertyOrMethod::Property(ref prop) = self {
      Some(prop)
    } else {
      None
    }
  }

  fn method(&self) -> Option<&ClassMethodDef> {
    if let PropertyOrMethod::Method(ref method) = self {
      Some(method)
    } else {
      None
    }
  }
}

fn property_or_method_cmp(
  a: &PropertyOrMethod,
  b: &PropertyOrMethod,
) -> std::cmp::Ordering {
  let name_cmp = {
    let a_name = a
      .prop()
      .map(|prop| &prop.name)
      .unwrap_or_else(|| &a.method().unwrap().name);
    let b_name = b
      .prop()
      .map(|prop| &prop.name)
      .unwrap_or_else(|| &b.method().unwrap().name);

    a_name.cmp(b_name)
  };

  let accessibility_cmp = {
    let a_accessibility = a
      .prop()
      .map(|prop| &prop.accessibility)
      .unwrap_or_else(|| &a.method().unwrap().accessibility);
    let b_accessibility = b
      .prop()
      .map(|prop| &prop.accessibility)
      .unwrap_or_else(|| &b.method().unwrap().accessibility);

    a_accessibility
      .and_then(|a_accessibility| {
        b_accessibility.map(|b_accessibility| {
          match (a_accessibility, b_accessibility) {
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
          }
        })
      })
      .unwrap_or(std::cmp::Ordering::Equal)
  };

  let accessor_cmp = {
    let a_accessor = a.method().map(|method| &method.kind);
    let b_accessor = b.method().map(|method| &method.kind);

    a_accessor
      .and_then(|a_accessor| {
        b_accessor.map(|b_accessor| match (a_accessor, b_accessor) {
          (MethodKind::Getter, MethodKind::Getter) => std::cmp::Ordering::Equal,
          (MethodKind::Setter, MethodKind::Setter) => std::cmp::Ordering::Equal,
          (MethodKind::Getter, MethodKind::Setter) => {
            std::cmp::Ordering::Greater
          }
          (MethodKind::Setter, MethodKind::Getter) => std::cmp::Ordering::Less,
          _ => unreachable!(),
        })
      })
      .unwrap_or(std::cmp::Ordering::Equal)
  };

  accessibility_cmp.then(accessor_cmp).then(name_cmp)
}

struct ClassItems {
  properties: Vec<PropertyOrMethod>,
  static_properties: Vec<PropertyOrMethod>,
  methods: BTreeMap<String, Vec<ClassMethodDef>>,
  static_methods: BTreeMap<String, Vec<ClassMethodDef>>,
}

fn partition_properties_and_classes(
  properties: Vec<ClassPropertyDef>,
  methods: Vec<ClassMethodDef>,
) -> ClassItems {
  let mut out_properties = vec![];
  let mut out_static_properties = vec![];
  let mut out_methods = BTreeMap::new();
  let mut out_static_methods = BTreeMap::new();

  for property in properties {
    if property.is_static {
      out_static_properties.push(PropertyOrMethod::Property(property));
    } else {
      out_properties.push(PropertyOrMethod::Property(property));
    }
  }

  for method in methods {
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
  getter: Option<&ClassMethodDef>,
  setter: Option<&ClassMethodDef>,
) -> DocEntryCtx {
  let name = &getter.or(setter).unwrap().name;
  let id = name_to_id("accessor", name);
  let ts_type = getter
    .and_then(|getter| getter.function_def.return_type.as_ref())
    .or_else(|| {
      setter.and_then(|setter| {
        setter
          .function_def
          .params
          .get(0)
          .and_then(|param| param.ts_type.as_ref())
      })
    })
    .map_or_else(String::new, |ts_type| {
      format!(
        r#"<span>: <span class="font-medium">{}</span></span>"#,
        render_type_def(ctx, ts_type)
      )
    });
  let js_doc = getter.or(setter).unwrap().js_doc.doc.as_deref();

  // TODO: tags

  DocEntryCtx::new(ctx, &id, name, &ts_type, js_doc)
}

fn render_class_method(
  ctx: &RenderContext,
  method: &ClassMethodDef,
  i: usize,
) -> Option<DocEntryCtx> {
  if method.function_def.has_body && i != 0 {
    return None;
  }

  let id = name_to_id("method", &format!("{}_{i}", method.name));

  // TODO: tags

  Some(DocEntryCtx::new(
    ctx,
    &id,
    &method.name,
    &super::function::render_function_summary(&method.function_def, ctx),
    method.js_doc.doc.as_deref(),
  ))
}

fn render_class_property(
  ctx: &RenderContext,
  property: &ClassPropertyDef,
) -> DocEntryCtx {
  let id = name_to_id("property", &property.name);

  // TODO: tags

  let ts_type = property
    .ts_type
    .as_ref()
    .map(|ts_type| format!(": {}", render_type_def(ctx, ts_type)))
    .unwrap_or_default();

  DocEntryCtx::new(
    ctx,
    &id,
    &property.name,
    &ts_type,
    property.js_doc.doc.as_deref(),
  )
}

fn render_class_properties(
  ctx: &RenderContext,
  properties: Vec<PropertyOrMethod>,
) -> Vec<DocEntryCtx> {
  let mut properties = properties.into_iter().peekable();
  let mut out = vec![];

  while let Some(property) = properties.next() {
    let content = match property {
      PropertyOrMethod::Property(property) => {
        render_class_property(ctx, &property)
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

        render_class_accessor(ctx, getter, setter.as_ref())
      }
    };

    out.push(content)
  }

  out
}

fn render_class_methods(
  ctx: &RenderContext,
  methods: BTreeMap<String, Vec<ClassMethodDef>>,
) -> Vec<DocEntryCtx> {
  methods
    .values()
    .flat_map(|methods| {
      methods
        .iter()
        .enumerate()
        .filter_map(|(i, method)| render_class_method(ctx, method, i))
    })
    .collect()
}
