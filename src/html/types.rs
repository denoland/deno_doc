use super::parameters::render_params;
use super::util::*;
use super::GenerateCtx;
use crate::ts_type::LiteralDefKind;
use crate::ts_type::TsTypeDefKind;
use crate::ts_type_param::TsTypeParamDef;
use deno_ast::swc::ast::MethodKind;
use deno_ast::swc::ast::TruePlusMinus;
use serde_json::json;
use std::fmt::Write;

pub(super) fn render_type_def(
  ctx: &GenerateCtx,
  def: &crate::ts_type::TsTypeDef,
  render_ctx: &RenderContext,
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

        let href = if render_ctx.contains_type_param(&type_ref.type_name) {
          Some(format!(
            "#{}",
            name_to_id("type_param", &type_ref.type_name)
          ))
        } else {
          render_ctx.lookup_symbol_href(&type_ref.type_name)
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
            .map(|type_params| type_arguments(ctx, type_params, render_ctx))
            .unwrap_or_default()
        )
      }
      TsTypeDefKind::Union => {
        type_def_join(ctx, def.union.as_ref().unwrap(), "|", render_ctx)
      }
      TsTypeDefKind::Intersection => {
        type_def_join(ctx, def.intersection.as_ref().unwrap(), "&", render_ctx)
      }
      TsTypeDefKind::Array => {
        format!(
          "{}[]",
          render_type_def(ctx, def.array.as_ref().unwrap(), render_ctx)
        )
      }
      TsTypeDefKind::Tuple => {
        type_def_tuple(ctx, def.tuple.as_ref().unwrap(), render_ctx)
      }
      TsTypeDefKind::TypeOperator => {
        let operator = def.type_operator.as_ref().unwrap();
        format!(
          "<span>{}</span> {}",
          operator.operator,
          render_type_def(ctx, &operator.ts_type, render_ctx)
        )
      }
      TsTypeDefKind::Parenthesized => {
        format!(
          "({})",
          render_type_def(ctx, def.parenthesized.as_ref().unwrap(), render_ctx)
        )
      }
      TsTypeDefKind::Rest => {
        format!(
          "...{}",
          render_type_def(ctx, def.rest.as_ref().unwrap(), render_ctx)
        )
      }
      TsTypeDefKind::Optional => {
        render_type_def(ctx, def.optional.as_ref().unwrap(), render_ctx)
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
          type_params_summary(ctx, &fn_or_constructor.type_params, render_ctx),
          render_params(ctx, &fn_or_constructor.params, render_ctx),
          render_type_def(ctx, &fn_or_constructor.ts_type, render_ctx),
        )
      }
      TsTypeDefKind::Conditional => {
        let conditional = def.conditional_type.as_ref().unwrap();

        format!(
          "{} <span>extends</span> {} ? {} : {}",
          render_type_def(ctx, &conditional.check_type, render_ctx),
          render_type_def(ctx, &conditional.extends_type, render_ctx),
          render_type_def(ctx, &conditional.true_type, render_ctx),
          render_type_def(ctx, &conditional.false_type, render_ctx),
        )
      }
      TsTypeDefKind::Infer => format!(
        "<span>infer {}</span>",
        type_param_summary(
          ctx,
          &def.infer.as_ref().unwrap().type_param,
          "extends",
          render_ctx
        )
      ),
      TsTypeDefKind::IndexedAccess => {
        let indexed_access = def.indexed_access.as_ref().unwrap();

        format!(
          "{}[{}]",
          render_type_def(ctx, &indexed_access.obj_type, render_ctx),
          render_type_def(ctx, &indexed_access.index_type, render_ctx)
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
              render_type_def(ctx, name_type, render_ctx)
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
            format!(
              "<span>: {}</span>",
              render_type_def(ctx, ts_type, render_ctx)
            )
          })
          .unwrap_or_default();

        format!(
          "{readonly}[{}{name_type}]{optional}{ts_type}",
          type_param_summary(ctx, &mapped.type_param, "in", render_ctx)
        )
      }
      TsTypeDefKind::TypeLiteral => {
        let type_literal = def.type_literal.as_ref().unwrap();

        let index_signatures = type_literal.index_signatures.iter().fold(
          String::new(),
          |mut output, index_signature| {
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
              "{readonly}[{}]{ts_type}; ",
              render_params(ctx, &index_signature.params, render_ctx)
            )
            .unwrap();
            output
          },
        );

        let call_signatures = type_literal.call_signatures.iter().fold(
          String::new(),
          |mut output, call_signature| {
            let ts_type = call_signature
              .ts_type
              .as_ref()
              .map(|ts_type| {
                format!(": {}", render_type_def(ctx, ts_type, render_ctx))
              })
              .unwrap_or_default();

            write!(
              output,
              "{}({}){ts_type}; ",
              type_params_summary(ctx, &call_signature.type_params, render_ctx),
              render_params(ctx, &call_signature.params, render_ctx)
            )
            .unwrap();
            output
          },
        );

        let properties = type_literal.properties.iter().fold(
          String::new(),
          |mut output, property| {
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
              .map(|ts_type| {
                format!(": {}", render_type_def(ctx, ts_type, render_ctx))
              })
              .unwrap_or_default();

            write!(output, "{readonly}{name}{optional}{ts_type}; ").unwrap();
            output
          },
        );

        let methods = type_literal.methods.iter().fold(
          String::new(),
          |mut output, method| {
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
              .map(|ts_type| {
                format!(": {}", render_type_def(ctx, ts_type, render_ctx))
              })
              .unwrap_or_default();

            write!(
              output,
              "{kind}{name}{optional}{}({}){return_type}; ",
              type_params_summary(ctx, &method.type_params, render_ctx),
              render_params(ctx, &method.params, render_ctx)
            )
            .unwrap();
            output
          },
        );

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
          .map(|def| format!(" is {}", render_type_def(ctx, def, render_ctx)))
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
          .map(|type_params| type_arguments(ctx, type_params, render_ctx))
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
  ctx: &GenerateCtx,
  union: &[crate::ts_type::TsTypeDef],
  join: &str,
  render_ctx: &RenderContext,
) -> String {
  if union.len() <= 3 {
    let items = union
      .iter()
      .map(|element| render_type_def(ctx, element, render_ctx))
      .collect::<Vec<String>>()
      .join(&format!("<span> {join} </span>"));

    format!("<span>{items}</span>")
  } else {
    let items = union.iter().fold(String::new(), |mut output, element| {
      write!(
        output,
        "<div><span> {join} </span>{}</div>",
        render_type_def(ctx, element, render_ctx)
      )
      .unwrap();
      output
    });

    format!(r#"<div class="indent">{items}</div>"#)
  }
}

fn type_def_tuple(
  ctx: &GenerateCtx,
  union: &[crate::ts_type::TsTypeDef],
  render_ctx: &RenderContext,
) -> String {
  if union.len() <= 3 {
    let items = union
      .iter()
      .map(|element| render_type_def(ctx, element, render_ctx))
      .collect::<Vec<String>>()
      .join(", ");

    format!("<span>[{items}]</span>")
  } else {
    let items = union.iter().fold(String::new(), |mut output, element| {
      let _ = write!(
        output,
        "<div>{}</div>, ",
        render_type_def(ctx, element, render_ctx)
      );
      output
    });

    format!(r#"<div class="indent">[{items}]</div>"#)
  }
}

pub(super) fn type_params_summary(
  ctx: &GenerateCtx,
  type_params: &[TsTypeParamDef],
  render_ctx: &RenderContext,
) -> String {
  if type_params.is_empty() {
    String::new()
  } else {
    let items = type_params
      .iter()
      .map(|type_param| {
        type_param_summary(ctx, type_param, "extends", render_ctx)
      })
      .collect::<Vec<String>>()
      .join("<span>, </span>");

    format!("<span>&lt;{items}&gt;</span>")
  }
}

fn type_param_summary(
  ctx: &GenerateCtx,
  type_param: &TsTypeParamDef,
  constraint_kind: &str,
  render_ctx: &RenderContext,
) -> String {
  let constraint = type_param
    .constraint
    .as_ref()
    .map(|constraint| {
      format!(
        r#"<span><span> {constraint_kind} </span>{}</span>"#,
        render_type_def(ctx, constraint, render_ctx)
      )
    })
    .unwrap_or_default();

  let default = type_param
    .default
    .as_ref()
    .map(|default| {
      format!(
        r#"<span><span> = </span>{}</span>"#,
        render_type_def(ctx, default, render_ctx)
      )
    })
    .unwrap_or_default();

  format!(
    "<span><span>{}</span>{constraint}{default}</span>",
    type_param.name,
  )
}

pub(super) fn type_arguments(
  ctx: &GenerateCtx,
  defs: &[crate::ts_type::TsTypeDef],
  render_ctx: &RenderContext,
) -> String {
  if defs.is_empty() {
    String::new()
  } else {
    let items = defs
      .iter()
      .map(|def| render_type_def(ctx, def, render_ctx))
      .collect::<Vec<String>>()
      .join("<span>, </span>");

    format!("&lt;{items}&gt;")
  }
}

pub(super) fn render_type_params(
  ctx: &GenerateCtx,
  type_params: &[TsTypeParamDef],
  render_ctx: &RenderContext,
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
            render_type_def(ctx, constraint, render_ctx)
          )
        })
        .unwrap_or_default();

      let default = type_param
        .default
        .as_ref()
        .map(|default| {
          format!(
            r#"<span><span> = </span>{}</span>"#,
            render_type_def(ctx, default, render_ctx)
          )
        })
        .unwrap_or_default();

      render_doc_entry(
        ctx,
        &id,
        &type_param.name,
        &format!("{constraint}{default}"),
        None,
        render_ctx,
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
