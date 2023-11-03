use super::parameters::render_params;
use super::types::render_type_def;
use super::util::*;
use super::GenerateCtx;
use crate::class::ClassMethodDef;
use crate::class::ClassPropertyDef;
use deno_ast::swc::ast::Accessibility;
use deno_ast::swc::ast::MethodKind;
use serde_json::json;
use std::collections::BTreeMap;
use std::fmt::Write;

pub(super) fn render_class(
  ctx: &GenerateCtx,
  doc_node: &crate::DocNode,
  render_ctx: &RenderContext,
) -> String {
  let class_def = doc_node.class_def.as_ref().unwrap();

  let current_type_params = class_def
    .type_params
    .iter()
    .map(|def| def.name.clone())
    .collect::<std::collections::HashSet<String>>();

  let render_ctx = &render_ctx.with_current_type_params(current_type_params);

  let class_items = partition_properties_and_classes(
    class_def.properties.clone(),
    class_def.methods.clone(),
  );

  let properties = if class_items.properties.is_empty() {
    String::new()
  } else {
    ctx.render(
      "section.html",
      &json!({
        "title": "Properties",
        "content": render_class_properties(ctx, class_items.properties, render_ctx)
      }),
    )
  };

  let methods = if class_items.methods.is_empty() {
    String::new()
  } else {
    ctx.render(
      "section.html",
      &json!({
        "title": "Methods",
        "content": render_class_methods(ctx, class_items.methods, render_ctx),
      }),
    )
  };

  let static_properties = if class_items.static_properties.is_empty() {
    String::new()
  } else {
    ctx.render(
      "section.html",
      &json!({
        "title": "Static Properties",
        "content": render_class_properties(ctx, class_items.static_properties, render_ctx),
      })
    )
  };

  let static_methods = if class_items.static_methods.is_empty() {
    String::new()
  } else {
    ctx.render(
      "section.html",
      &json!({
        "title": "Static Methods",
        "content": render_class_methods(ctx, class_items.static_methods, render_ctx),
      }),
    )
  };

  format!(
    r#"<div class="doc_block_items">{}{}{}{}{}{}{}{}</div>"#,
    super::jsdoc::render_docs(ctx, &doc_node.js_doc, true, false, render_ctx),
    render_constructors(
      ctx,
      &class_def.constructors,
      &doc_node.name,
      render_ctx
    ),
    super::types::render_type_params(ctx, &class_def.type_params, render_ctx),
    render_index_signatures(ctx, &class_def.index_signatures, render_ctx),
    properties,
    methods,
    static_properties,
    static_methods,
  )
}

fn render_constructors(
  ctx: &GenerateCtx,
  constructors: &[crate::class::ClassConstructorDef],
  name: &str,
  render_ctx: &RenderContext,
) -> String {
  if constructors.is_empty() {
    return String::new();
  }

  let items = constructors
    .iter()
    .enumerate()
    .map(|(i, constructor)| {
      let id = name_to_id("constructor", &i.to_string());

      // TODO: tags, render constructor params
      render_doc_entry(
        ctx,
        &id,
        name,
        "()",
        constructor.js_doc.doc.as_deref(),
        render_ctx,
      )
    })
    .collect::<String>();

  ctx.render(
    "section.html",
    &json!({ "title": "Constructors", "content": &items }),
  )
}

fn render_index_signatures(
  ctx: &GenerateCtx,
  index_signatures: &[crate::class::ClassIndexSignatureDef],
  render_ctx: &RenderContext,
) -> String {
  if index_signatures.is_empty() {
    return String::new();
  }

  let items = index_signatures.iter().enumerate().fold(
    String::new(),
    |mut output, (i, index_signature)| {
      let id = name_to_id("index_signature", &i.to_string());

      let readonly = index_signature
        .readonly
        .then_some("<span>readonly </span>")
        .unwrap_or_default();

      let ts_type = index_signature
        .ts_type
        .as_ref()
        .map(|ts_type| {
          format!(": {}", render_type_def(ctx, ts_type, render_ctx))
        })
        .unwrap_or_default();

      write!(
        output,
        r#"<div class="doc_item" id="{id}">{}{readonly}[{}]{ts_type}</div>"#,
        ctx.render("anchor.html", &json!({ "href": &id })),
        render_params(ctx, &index_signature.params, render_ctx),
      )
      .unwrap();
      output
    },
  );

  ctx.render(
    "section.html",
    &json!({ "title": "Index Signatures", "content": &items }),
  )
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
  ctx: &GenerateCtx,
  getter: Option<&ClassMethodDef>,
  setter: Option<&ClassMethodDef>,
  render_ctx: &RenderContext,
) -> String {
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
        render_type_def(ctx, ts_type, render_ctx)
      )
    });
  let js_doc = getter.or(setter).unwrap().js_doc.doc.as_deref();

  // TODO: tags

  render_doc_entry(ctx, &id, name, &ts_type, js_doc, render_ctx)
}

fn render_class_method(
  ctx: &GenerateCtx,
  method: &ClassMethodDef,
  i: usize,
  render_ctx: &RenderContext,
) -> String {
  if method.function_def.has_body && i != 0 {
    return String::new();
  }

  let id = name_to_id("method", &format!("{}_{i}", method.name));

  // TODO: tags

  render_doc_entry(
    ctx,
    &id,
    &method.name,
    &super::function::render_function_summary(
      ctx,
      &method.function_def,
      render_ctx,
    ),
    method.js_doc.doc.as_deref(),
    render_ctx,
  )
}

fn render_class_property(
  ctx: &GenerateCtx,
  property: &ClassPropertyDef,
  render_ctx: &RenderContext,
) -> String {
  let id = name_to_id("property", &property.name);

  // TODO: tags

  let ts_type = property
    .ts_type
    .as_ref()
    .map(|ts_type| format!(": {}", render_type_def(ctx, ts_type, render_ctx)))
    .unwrap_or_default();

  render_doc_entry(
    ctx,
    &id,
    &property.name,
    &ts_type,
    property.js_doc.doc.as_deref(),
    render_ctx,
  )
}

fn render_class_properties(
  ctx: &GenerateCtx,
  properties: Vec<PropertyOrMethod>,
  render_ctx: &RenderContext,
) -> String {
  let mut properties = properties.into_iter().peekable();
  let mut out = String::new();

  while let Some(property) = properties.next() {
    let content = match property {
      PropertyOrMethod::Property(property) => {
        render_class_property(ctx, &property, render_ctx)
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

        render_class_accessor(ctx, getter, setter.as_ref(), render_ctx)
      }
    };

    out.push_str(&content)
  }

  out
}

fn render_class_methods(
  ctx: &GenerateCtx,
  methods: BTreeMap<String, Vec<ClassMethodDef>>,
  render_ctx: &RenderContext,
) -> String {
  methods
    .values()
    .map(|methods| {
      methods
        .iter()
        .enumerate()
        .map(|(i, method)| render_class_method(ctx, method, i, render_ctx))
        .collect::<String>()
    })
    .collect()
}
