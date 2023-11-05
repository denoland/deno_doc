use crate::html::jsdoc::render_doc_entry;
use crate::html::parameters::render_params;
use crate::html::util::*;
use crate::ts_type::LiteralDefKind;
use crate::ts_type::TsTypeDefKind;
use crate::ts_type_param::TsTypeParamDef;
use deno_ast::swc::ast::MethodKind;
use deno_ast::swc::ast::TruePlusMinus;
use serde_json::json;

pub(crate) fn render_type_def(
  ctx: &RenderContext,
  def: &crate::ts_type::TsTypeDef,
) -> String {
  if let Some(kind) = &def.kind {
    match kind {
      TsTypeDefKind::Keyword => {
        format!("<span>{}</span>", def.keyword.as_ref().unwrap())
      }
      TsTypeDefKind::Literal => {
        let lit = def.literal.as_ref().unwrap();

        match lit.kind {
          LiteralDefKind::Number
          | LiteralDefKind::BigInt
          | LiteralDefKind::Boolean => {
            format!("<span>{}</span>", def.repr)
          }
          LiteralDefKind::String => {
            format!("<span>{:?}</span>", def.repr)
          }
          LiteralDefKind::Template => {
            // TODO(@kitsonk) do this properly and escape properly
            format!("<span>`{}`</span>", def.repr)
          }
        }
      }
      TsTypeDefKind::TypeRef => {
        let type_ref = def.type_ref.as_ref().unwrap();

        let href = if ctx.contains_type_param(&type_ref.type_name) {
          Some(format!(
            "#{}",
            name_to_id("type_param", &type_ref.type_name)
          ))
        } else {
          ctx.lookup_symbol_href(&type_ref.type_name)
        };

        let name = if let Some(href) = href {
          format!(
            r#"<a href="{href}" class="link">{}</a>"#,
            type_ref.type_name
          )
        } else {
          format!(r#"<span>{}</span>"#, type_ref.type_name)
        };

        format!(
          "{}{}",
          name,
          type_ref
            .type_params
            .as_ref()
            .map(|type_params| type_arguments(ctx, type_params))
            .unwrap_or_default()
        )
      }
      TsTypeDefKind::Union => {
        type_def_join(ctx, def.union.as_ref().unwrap(), "|")
      }
      TsTypeDefKind::Intersection => {
        type_def_join(ctx, def.intersection.as_ref().unwrap(), "&")
      }
      TsTypeDefKind::Array => {
        format!("{}[]", render_type_def(ctx, def.array.as_ref().unwrap()))
      }
      TsTypeDefKind::Tuple => type_def_tuple(ctx, def.tuple.as_ref().unwrap()),
      TsTypeDefKind::TypeOperator => {
        let operator = def.type_operator.as_ref().unwrap();
        format!(
          "<span>{}</span> {}",
          operator.operator,
          render_type_def(ctx, &operator.ts_type)
        )
      }
      TsTypeDefKind::Parenthesized => {
        format!(
          "({})",
          render_type_def(ctx, def.parenthesized.as_ref().unwrap())
        )
      }
      TsTypeDefKind::Rest => {
        format!("...{}", render_type_def(ctx, def.rest.as_ref().unwrap()))
      }
      TsTypeDefKind::Optional => {
        render_type_def(ctx, def.optional.as_ref().unwrap())
      }
      TsTypeDefKind::TypeQuery => def.type_query.clone().unwrap(),
      TsTypeDefKind::This => "<span>this</span>".to_string(),
      TsTypeDefKind::FnOrConstructor => {
        let fn_or_constructor = def.fn_or_constructor.as_ref().unwrap();

        let new = fn_or_constructor
          .constructor
          .then_some("<span>new </span>")
          .unwrap_or_default();

        format!(
          "{new}{}({}) =&gt; {}",
          type_params_summary(ctx, &fn_or_constructor.type_params),
          render_params(ctx, &fn_or_constructor.params),
          render_type_def(ctx, &fn_or_constructor.ts_type),
        )
      }
      TsTypeDefKind::Conditional => {
        let conditional = def.conditional_type.as_ref().unwrap();

        format!(
          "{} <span>extends</span> {} ? {} : {}",
          render_type_def(ctx, &conditional.check_type),
          render_type_def(ctx, &conditional.extends_type),
          render_type_def(ctx, &conditional.true_type),
          render_type_def(ctx, &conditional.false_type),
        )
      }
      TsTypeDefKind::Infer => format!(
        "<span>infer {}</span>",
        type_param_summary(
          ctx,
          &def.infer.as_ref().unwrap().type_param,
          "extends",
        )
      ),
      TsTypeDefKind::IndexedAccess => {
        let indexed_access = def.indexed_access.as_ref().unwrap();

        format!(
          "{}[{}]",
          render_type_def(ctx, &indexed_access.obj_type),
          render_type_def(ctx, &indexed_access.index_type)
        )
      }
      TsTypeDefKind::Mapped => {
        let mapped = def.mapped_type.as_ref().unwrap();

        let readonly = if let Some(readonly) = mapped.readonly {
          let char = match readonly {
            TruePlusMinus::True => "",
            TruePlusMinus::Plus => "+",
            TruePlusMinus::Minus => "-",
          };

          format!("<span>{char}readonly </span>")
        } else {
          String::new()
        };

        let name_type = mapped
          .name_type
          .as_ref()
          .map(|name_type| {
            format!(
              "<span> in keyof </span>{}",
              render_type_def(ctx, name_type)
            )
          })
          .unwrap_or_default();

        let optional = if let Some(optional) = mapped.optional {
          match optional {
            TruePlusMinus::True => "?",
            TruePlusMinus::Plus => "+?",
            TruePlusMinus::Minus => "-?",
          }
        } else {
          ""
        };

        let ts_type = mapped
          .ts_type
          .as_ref()
          .map(|ts_type| {
            format!("<span>: {}</span>", render_type_def(ctx, ts_type))
          })
          .unwrap_or_default();

        format!(
          "{readonly}[{}{name_type}]{optional}{ts_type}",
          type_param_summary(ctx, &mapped.type_param, "in")
        )
      }
      TsTypeDefKind::TypeLiteral => {
        let type_literal = def.type_literal.as_ref().unwrap();

        let mut index_signatures =
          Vec::with_capacity(type_literal.index_signatures.len());

        for index_signature in type_literal.index_signatures.iter() {
          let readonly = index_signature
            .readonly
            .then_some("<span>readonly </span>")
            .unwrap_or_default();

          let ts_type = index_signature
            .ts_type
            .as_ref()
            .map(|ts_type| format!(": {}", render_type_def(ctx, ts_type)))
            .unwrap_or_default();

          let item = format!(
            "{readonly}[{}]{ts_type}; ",
            render_params(ctx, &index_signature.params)
          );
          index_signatures.push(item);
        }

        let index_signatures = index_signatures.join("");

        let mut call_signatures =
          Vec::with_capacity(type_literal.call_signatures.len());

        for call_signature in type_literal.call_signatures.iter() {
          let ts_type = call_signature
            .ts_type
            .as_ref()
            .map(|ts_type| format!(": {}", render_type_def(ctx, ts_type)))
            .unwrap_or_default();

          let item = format!(
            "{}({}){ts_type}; ",
            type_params_summary(ctx, &call_signature.type_params),
            render_params(ctx, &call_signature.params)
          );
          call_signatures.push(item);
        }
        let call_signatures = call_signatures.join("");

        let mut properties = Vec::with_capacity(type_literal.properties.len());

        for property in type_literal.properties.iter() {
          let readonly = property
            .readonly
            .then_some("<span>readonly </span>")
            .unwrap_or_default();

          let name = if property.computed {
            format!("[{}]", property.name)
          } else {
            property.name.clone()
          };

          let optional = property.optional.then_some("?").unwrap_or_default();

          let ts_type = property
            .ts_type
            .as_ref()
            .map(|ts_type| format!(": {}", render_type_def(ctx, ts_type)))
            .unwrap_or_default();

          let item = format!("{readonly}{name}{optional}{ts_type}; ");
          properties.push(item);
        }
        let properties = properties.join("");

        let mut methods = Vec::with_capacity(type_literal.methods.len());

        for method in type_literal.methods.iter() {
          let kind = match method.kind {
            MethodKind::Method => "",
            MethodKind::Getter => "<span>get </span>",
            MethodKind::Setter => "<span>set </span>",
          };

          let name = if method.name == "new" {
            "<span>new </span>".to_string()
          } else if method.computed {
            format!("[{}]", method.name)
          } else {
            method.name.clone()
          };

          let optional = method.optional.then_some("?").unwrap_or_default();

          let return_type = method
            .return_type
            .as_ref()
            .map(|ts_type| format!(": {}", render_type_def(ctx, ts_type)))
            .unwrap_or_default();

          let item = format!(
            "{kind}{name}{optional}{}({}){return_type}; ",
            type_params_summary(ctx, &method.type_params),
            render_params(ctx, &method.params)
          );

          methods.push(item);
        }
        let methods = methods.join("");

        format!(
          "{{ {index_signatures}{call_signatures}{properties}{methods} }}"
        )
      }
      TsTypeDefKind::TypePredicate => {
        let type_predicate = def.type_predicate.as_ref().unwrap();

        let asserts = type_predicate
          .asserts
          .then_some("<span>asserts </span>")
          .unwrap_or_default();
        let param_type =
          if let crate::ts_type::ThisOrIdent::Identifier { name } =
            &type_predicate.param
          {
            name.clone()
          } else {
            "<span>this</span>".to_string()
          };

        let r#type = type_predicate
          .r#type
          .as_ref()
          .map(|def| format!(" is {}", render_type_def(ctx, def)))
          .unwrap_or_default();

        format!("{asserts}{param_type}{}", r#type)
      }
      TsTypeDefKind::ImportType => {
        let import_type = def.import_type.as_ref().unwrap();

        let qualifier = import_type
          .qualifier
          .as_ref()
          .map(|qualifier| format!("<span>.{qualifier}</span>"))
          .unwrap_or_default();

        let type_arguments = import_type
          .type_params
          .as_ref()
          .map(|type_params| type_arguments(ctx, type_params))
          .unwrap_or_default();

        format!(
          r#"<span>import</span>("{}"){qualifier}{type_arguments}"#,
          import_type.specifier,
        )
      }
    }
  } else {
    html_escape::encode_safe(&def.repr).to_string()
  }
}

fn type_def_join(
  ctx: &RenderContext,
  union: &[crate::ts_type::TsTypeDef],
  join: &str,
) -> String {
  if union.len() <= 3 {
    let items = union
      .iter()
      .map(|element| render_type_def(ctx, element))
      .collect::<Vec<String>>()
      .join(&format!("<span> {join} </span>"));

    format!("<span>{items}</span>")
  } else {
    let mut items = Vec::with_capacity(union.len());

    for element in union {
      items.push(format!(
        "<div><span> {join} </span>{}</div>",
        render_type_def(ctx, element)
      ));
    }

    let content = items.join("");

    format!(r#"<div class="indent">{content}</div>"#)
  }
}

fn type_def_tuple(
  ctx: &RenderContext,
  union: &[crate::ts_type::TsTypeDef],
) -> String {
  if union.len() <= 3 {
    let items = union
      .iter()
      .map(|element| render_type_def(ctx, element))
      .collect::<Vec<String>>()
      .join(", ");

    format!("<span>[{items}]</span>")
  } else {
    let mut items = Vec::with_capacity(union.len());

    for element in union {
      items.push(format!("<div>{}</div>, ", render_type_def(ctx, element)));
    }

    let content = items.join("");
    format!(r#"<div class="indent">[{content}]</div>"#)
  }
}

pub(crate) fn type_params_summary(
  ctx: &RenderContext,
  type_params: &[TsTypeParamDef],
) -> String {
  if type_params.is_empty() {
    String::new()
  } else {
    let items = type_params
      .iter()
      .map(|type_param| type_param_summary(ctx, type_param, "extends"))
      .collect::<Vec<String>>()
      .join("<span>, </span>");

    format!("<span>&lt;{items}&gt;</span>")
  }
}

fn type_param_summary(
  ctx: &RenderContext,
  type_param: &TsTypeParamDef,
  constraint_kind: &str,
) -> String {
  let constraint = type_param
    .constraint
    .as_ref()
    .map(|constraint| {
      format!(
        r#"<span><span> {constraint_kind} </span>{}</span>"#,
        render_type_def(ctx, constraint)
      )
    })
    .unwrap_or_default();

  let default = type_param
    .default
    .as_ref()
    .map(|default| {
      format!(
        r#"<span><span> = </span>{}</span>"#,
        render_type_def(ctx, default)
      )
    })
    .unwrap_or_default();

  format!(
    "<span><span>{}</span>{constraint}{default}</span>",
    type_param.name,
  )
}

pub(crate) fn type_arguments(
  ctx: &RenderContext,
  defs: &[crate::ts_type::TsTypeDef],
) -> String {
  if defs.is_empty() {
    String::new()
  } else {
    let items = defs
      .iter()
      .map(|def| render_type_def(ctx, def))
      .collect::<Vec<String>>()
      .join("<span>, </span>");

    format!("&lt;{items}&gt;")
  }
}

pub(crate) fn render_type_params(
  ctx: &RenderContext,
  type_params: &[TsTypeParamDef],
) -> String {
  if type_params.is_empty() {
    return String::new();
  }

  let items = type_params
    .iter()
    .map(|type_param| {
      let id = name_to_id("type_param", &type_param.name);

      let constraint = type_param
        .constraint
        .as_ref()
        .map(|constraint| {
          format!(
            r#"<span><span> extends </span>{}</span>"#,
            render_type_def(ctx, constraint)
          )
        })
        .unwrap_or_default();

      let default = type_param
        .default
        .as_ref()
        .map(|default| {
          format!(
            r#"<span><span> = </span>{}</span>"#,
            render_type_def(ctx, default)
          )
        })
        .unwrap_or_default();

      render_doc_entry(
        ctx,
        &id,
        &type_param.name,
        &format!("{constraint}{default}"),
        None,
      )
    })
    .collect::<String>();

  ctx.render(
    "section.html",
    &json!({
      "title": "Type Parameters",
      "content": &items
    }),
  )
}
