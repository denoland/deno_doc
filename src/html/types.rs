use super::parameters::render_params;
use super::util::*;
use crate::ts_type::LiteralDefKind;
use crate::ts_type::TsTypeDefKind;
use crate::ts_type_param::TsTypeParamDef;
use deno_ast::swc::ast::MethodKind;
use deno_ast::swc::ast::TruePlusMinus;
use std::fmt::Write;

pub fn render_type_def(
  def: &crate::ts_type::TsTypeDef,
  ctx: &RenderContext,
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

        // TODO: implement TypeRef links

        let name = if let Some(href) = None::<String> {
          format!(r#"<a href={href} class="link">{}</a>"#, type_ref.type_name)
        } else {
          format!(r#"<span>{}</span>"#, type_ref.type_name)
        };

        format!(
          "{}{}",
          name,
          type_ref
            .type_params
            .as_ref()
            .map(|type_params| type_arguments(type_params, ctx))
            .unwrap_or_default()
        );

        type_ref.type_name.clone()
      }
      TsTypeDefKind::Union => {
        type_def_join(def.union.as_ref().unwrap(), "|", ctx)
      }
      TsTypeDefKind::Intersection => {
        type_def_join(def.intersection.as_ref().unwrap(), "&", ctx)
      }
      TsTypeDefKind::Array => {
        format!("{}[]", render_type_def(def.array.as_ref().unwrap(), ctx))
      }
      TsTypeDefKind::Tuple => type_def_tuple(def.tuple.as_ref().unwrap(), ctx),
      TsTypeDefKind::TypeOperator => {
        let operator = def.type_operator.as_ref().unwrap();
        format!(
          "<span>{}</span> {}",
          operator.operator,
          render_type_def(&operator.ts_type, ctx)
        )
      }
      TsTypeDefKind::Parenthesized => {
        format!(
          "({})",
          render_type_def(def.parenthesized.as_ref().unwrap(), ctx)
        )
      }
      TsTypeDefKind::Rest => {
        format!("...{}", render_type_def(def.rest.as_ref().unwrap(), ctx))
      }
      TsTypeDefKind::Optional => {
        render_type_def(def.optional.as_ref().unwrap(), ctx)
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
          type_params_summary(&fn_or_constructor.type_params, ctx),
          render_params(&fn_or_constructor.params, ctx),
          render_type_def(&fn_or_constructor.ts_type, ctx),
        )
      }
      TsTypeDefKind::Conditional => {
        let conditional = def.conditional_type.as_ref().unwrap();

        format!(
          "{} <span>extends</span> {} ? {} : {}",
          render_type_def(&conditional.check_type, ctx),
          render_type_def(&conditional.extends_type, ctx),
          render_type_def(&conditional.true_type, ctx),
          render_type_def(&conditional.false_type, ctx),
        )
      }
      TsTypeDefKind::Infer => format!(
        "<span>infer {}</span>",
        type_param_summary(
          &def.infer.as_ref().unwrap().type_param,
          "extends",
          ctx
        )
      ),
      TsTypeDefKind::IndexedAccess => {
        let indexed_access = def.indexed_access.as_ref().unwrap();

        format!(
          "{}[{}]",
          render_type_def(&indexed_access.obj_type, ctx),
          render_type_def(&indexed_access.index_type, ctx)
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
              render_type_def(name_type, ctx)
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
            format!("<span>: {}</span>", render_type_def(ts_type, ctx))
          })
          .unwrap_or_default();

        format!(
          "{readonly}[{}{name_type}]{optional}{ts_type}",
          type_param_summary(&mapped.type_param, "in", ctx)
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
              .map(|ts_type| format!(": {}", render_type_def(ts_type, ctx)))
              .unwrap_or_default();

            write!(
              output,
              "{readonly}[{}]{ts_type}; ",
              render_params(&index_signature.params, ctx)
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
              .map(|ts_type| format!(": {}", render_type_def(ts_type, ctx)))
              .unwrap_or_default();

            write!(
              output,
              "{}({}){ts_type}; ",
              type_params_summary(&call_signature.type_params, ctx),
              render_params(&call_signature.params, ctx)
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
              .map(|ts_type| format!(": {}", render_type_def(ts_type, ctx)))
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
              .map(|ts_type| format!(": {}", render_type_def(ts_type, ctx)))
              .unwrap_or_default();

            write!(
              output,
              "{kind}{name}{optional}{}({}){return_type}; ",
              type_params_summary(&method.type_params, ctx),
              render_params(&method.params, ctx)
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
          .map(|def| format!(" is {}", render_type_def(def, ctx)))
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
          .map(|type_params| type_arguments(type_params, ctx))
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
  union: &[crate::ts_type::TsTypeDef],
  join: &str,
  ctx: &RenderContext,
) -> String {
  if union.len() <= 3 {
    let items = union
      .iter()
      .map(|element| render_type_def(element, ctx))
      .collect::<Vec<String>>()
      .join(&format!("<span> {join} </span>"));

    format!("<span>{items}</span>")
  } else {
    let items = union.iter().fold(String::new(), |mut output, element| {
      write!(
        output,
        "<div><span> {join} </span>{}</div>",
        render_type_def(element, ctx)
      )
      .unwrap();
      output
    });

    format!(r#"<div class="indent">{items}</div>"#)
  }
}

fn type_def_tuple(
  union: &[crate::ts_type::TsTypeDef],
  ctx: &RenderContext,
) -> String {
  if union.len() <= 3 {
    let items = union
      .iter()
      .map(|element| render_type_def(element, ctx))
      .collect::<Vec<String>>()
      .join(", ");

    format!("<span>[{items}]</span>")
  } else {
    let items = union.iter().fold(String::new(), |mut output, element| {
      let _ = write!(output, "<div>{}</div>, ", render_type_def(element, ctx));
      output
    });

    format!(r#"<div class="indent">[{items}]</div>"#)
  }
}

pub fn type_params_summary(
  type_params: &[TsTypeParamDef],
  ctx: &RenderContext,
) -> String {
  if type_params.is_empty() {
    String::new()
  } else {
    let items = type_params
      .iter()
      .map(|type_param| type_param_summary(type_param, "extends", ctx))
      .collect::<Vec<String>>()
      .join("<span>, </span>");

    format!("<span>&lt;{items}&gt;</span>")
  }
}

fn type_param_summary(
  type_param: &TsTypeParamDef,
  constraint_kind: &str,
  ctx: &RenderContext,
) -> String {
  let constraint = type_param
    .constraint
    .as_ref()
    .map(|constraint| {
      format!(
        r#"<span><span> {constraint_kind} </span>{}</span>"#,
        render_type_def(constraint, ctx)
      )
    })
    .unwrap_or_default();

  let default = type_param
    .default
    .as_ref()
    .map(|default| {
      format!(
        r#"<span><span> = </span>{}</span>"#,
        render_type_def(default, ctx)
      )
    })
    .unwrap_or_default();

  format!(
    "<span><span>{}</span>{constraint}{default}</span>",
    type_param.name,
  )
}

fn type_arguments(
  defs: &[crate::ts_type::TsTypeDef],
  ctx: &RenderContext,
) -> String {
  if defs.is_empty() {
    String::new()
  } else {
    let items = defs
      .iter()
      .map(|def| render_type_def(def, ctx))
      .collect::<Vec<String>>()
      .join(", ");

    format!("&lt;{items}&gt;")
  }
}

pub fn render_type_params(
  type_params: &[TsTypeParamDef],
  ctx: &RenderContext,
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
            render_type_def(constraint, ctx)
          )
        })
        .unwrap_or_default();

      let default = type_param
        .default
        .as_ref()
        .map(|default| {
          format!(
            r#"<span><span> = </span>{}</span>"#,
            render_type_def(default, ctx)
          )
        })
        .unwrap_or_default();

      doc_entry(
        &id,
        &type_param.name,
        &format!("{constraint}{default}"),
        None,
      )
    })
    .collect::<String>();

  section("Type Parameters", &items)
}
