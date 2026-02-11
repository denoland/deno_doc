use super::parameters::render_params;
use super::render_context::RenderContext;
use super::util::*;

use crate::js_doc::JsDoc;
use crate::js_doc::JsDocTag;
use crate::ts_type::LiteralDefKind;
use crate::ts_type::TsTypeDefKind;
use crate::ts_type_param::TsTypeParamDef;
use deno_ast::swc::ast::MethodKind;
use deno_ast::swc::ast::TruePlusMinus;

const MAX_INLINE_LEN: usize = 60;

// not a fan of this, but its the easiest approach
fn strip_indent_wrapper(html: &str) -> &str {
  html
    .strip_prefix(r#"<div class="ml-indent">"#)
    .and_then(|s| s.strip_suffix("</div>"))
    .unwrap_or(html)
}

pub(crate) fn with_trailing_comma(html: &str) -> String {
  if let Some(pos) = html.rfind("</div>") {
    let after = &html[pos + 6..];
    let only_closing_tags = {
      let mut s = after;
      while let Some(rest) = s.strip_prefix("</") {
        if let Some(end) = rest.find('>') {
          s = &rest[end + 1..];
        } else {
          break;
        }
      }
      s.is_empty()
    };
    if only_closing_tags {
      let mut result = String::with_capacity(html.len() + 1);
      result.push_str(&html[..pos]);
      result.push(',');
      result.push_str(&html[pos..]);
      return result;
    }
  }
  format!("{html},")
}

fn html_text_len(html: &str) -> usize {
  let mut len = 0;
  let mut in_tag = false;
  let mut chars = html.chars().peekable();
  while let Some(ch) = chars.next() {
    if ch == '<' {
      in_tag = true;
    } else if ch == '>' {
      in_tag = false;
    } else if !in_tag {
      if ch == '&' {
        // HTML entity - count as 1 character
        for c in chars.by_ref() {
          if c == ';' {
            break;
          }
        }
        len += 1;
      } else {
        len += 1;
      }
    }
  }
  len
}

pub(crate) fn render_type_def_colon(
  ctx: &RenderContext,
  def: &crate::ts_type::TsTypeDef,
) -> String {
  format!("<span>: {}</span>", render_type_def(ctx, def))
}

pub(crate) fn render_type_def(
  ctx: &RenderContext,
  def: &crate::ts_type::TsTypeDef,
) -> String {
  let Some(kind) = &def.kind else {
    return html_escape::encode_text(&def.repr).to_string();
  };

  match kind {
    TsTypeDefKind::Keyword => {
      let keyword = def.keyword.as_ref().unwrap();

      if !ctx.disable_links
        && let Some(href) = ctx
          .ctx
          .href_resolver
          .resolve_global_symbol(&[keyword.to_owned()])
      {
        format!(
          r#"<a href="{}" class="link td-kw">{keyword}</a>"#,
          html_escape::encode_double_quoted_attribute(&href),
        )
      } else {
        format!(r#"<span class="td-kw">{keyword}</span>"#)
      }
    }
    TsTypeDefKind::Literal => {
      let lit = def.literal.as_ref().unwrap();

      match lit.kind {
        LiteralDefKind::Number
        | LiteralDefKind::BigInt
        | LiteralDefKind::Boolean => {
          format!(
            r#"<span class="td-lit">{}</span>"#,
            html_escape::encode_text(&def.repr)
          )
        }
        LiteralDefKind::String => {
          format!(
            r#"<span class="td-str">{:?}</span>"#,
            html_escape::encode_text(&def.repr)
          )
        }
        LiteralDefKind::Template => {
          if let Some(types) = &lit.ts_types {
            let mut out = String::new();

            for ts_type in types {
              out.push_str(&if ts_type
                .literal
                .as_ref()
                .is_some_and(|literal| literal.string.is_some())
              {
                html_escape::encode_text(&ts_type.repr).into_owned()
              } else {
                format!("${{{}}}", render_type_def(ctx, ts_type))
              });
            }

            format!(r#"<span class="td-str">`{out}`</span>"#)
          } else {
            format!(
              r#"<span class="td-str">`{}`</span>"#,
              html_escape::encode_text(&def.repr)
            )
          }
        }
      }
    }
    TsTypeDefKind::TypeRef => {
      let type_ref = def.type_ref.as_ref().unwrap();

      let href = if ctx.disable_links {
        None
      } else if ctx.contains_type_param(&type_ref.type_name) {
        Some(format!(
          "#{}",
          IdBuilder::new(ctx.ctx)
            .kind(IdKind::TypeParam)
            .name(&type_ref.type_name)
            .build()
        ))
      } else {
        ctx.lookup_symbol_href(&type_ref.type_name)
      };

      let name = if let Some(href) = href {
        format!(
          r#"<a href="{}" class="link td-ref">{}</a>"#,
          html_escape::encode_double_quoted_attribute(&href),
          html_escape::encode_text(&type_ref.type_name)
        )
      } else {
        format!(
          r#"<span class="td-ref">{}</span>"#,
          html_escape::encode_text(&type_ref.type_name)
        )
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
      type_def_join(ctx, def.union.as_ref().unwrap(), '|')
    }
    TsTypeDefKind::Intersection => {
      type_def_join(ctx, def.intersection.as_ref().unwrap(), '&')
    }
    TsTypeDefKind::Array => {
      format!("{}[]", render_type_def(ctx, def.array.as_ref().unwrap()))
    }
    TsTypeDefKind::Tuple => type_def_tuple(ctx, def.tuple.as_ref().unwrap()),
    TsTypeDefKind::TypeOperator => {
      let operator = def.type_operator.as_ref().unwrap();
      format!(
        r#"<span class="td-kw">{}</span> {}"#,
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
    TsTypeDefKind::TypeQuery => {
      let query = def.type_query.as_ref().unwrap();

      if !ctx.disable_links
        && let Some(href) = ctx.lookup_symbol_href(query)
      {
        format!(
          r#"<a href="{}" class="link td-ref">{}</a>"#,
          html_escape::encode_double_quoted_attribute(&href),
          html_escape::encode_text(query),
        )
      } else {
        format!(
          r#"<span class="td-ref">{}</span>"#,
          html_escape::encode_text(query)
        )
      }
    }
    TsTypeDefKind::This => r#"<span class="td-kw">this</span>"#.to_string(),
    TsTypeDefKind::FnOrConstructor => {
      let fn_or_constructor = def.fn_or_constructor.as_ref().unwrap();

      let new = if fn_or_constructor.constructor {
        r#"<span class="td-kw">new </span>"#
      } else {
        Default::default()
      };

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
        r#"{} <span class="td-kw">extends</span> {} <span class="td-op">?</span> {} <span class="td-op">:</span> {}"#,
        render_type_def(ctx, &conditional.check_type),
        render_type_def(ctx, &conditional.extends_type),
        render_type_def(ctx, &conditional.true_type),
        render_type_def(ctx, &conditional.false_type),
      )
    }
    TsTypeDefKind::Infer => format!(
      r#"<span class="td-kw">infer </span>{}"#,
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

        format!(r#"<span class="td-kw">{char}readonly </span>"#)
      } else {
        String::new()
      };

      let name_type = mapped
        .name_type
        .as_ref()
        .map(|name_type| {
          format!(
            r#"<span class="td-kw"> in keyof </span>{}"#,
            render_type_def(ctx, name_type)
          )
        })
        .unwrap_or_default();

      let optional = if let Some(optional) = mapped.optional {
        let optional = match optional {
          TruePlusMinus::True => "?",
          TruePlusMinus::Plus => "+?",
          TruePlusMinus::Minus => "-?",
        };
        format!(r#"<span class="td-op">{optional}</span>"#)
      } else {
        String::new()
      };

      let ts_type = mapped
        .ts_type
        .as_ref()
        .map(|ts_type| render_type_def_colon(ctx, ts_type))
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
        let readonly = if index_signature.readonly {
          r#"<span class="td-kw">readonly </span>"#
        } else {
          Default::default()
        };

        let ts_type = index_signature
          .ts_type
          .as_ref()
          .map(|ts_type| render_type_def_colon(ctx, ts_type))
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
          .map(|ts_type| render_type_def_colon(ctx, ts_type))
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
        let readonly = if property.readonly {
          r#"<span class="td-kw">readonly </span>"#
        } else {
          Default::default()
        };

        let name = if property.computed {
          format!("[{}]", html_escape::encode_text(&property.name))
        } else {
          html_escape::encode_text(&property.name).to_string()
        };

        let optional = if property.optional {
          r#"<span class="td-op">?</span>"#
        } else {
          Default::default()
        };

        let ts_type = property
          .ts_type
          .as_ref()
          .map(|ts_type| render_type_def_colon(ctx, ts_type))
          .unwrap_or_default();

        let item = format!("{readonly}{name}{optional}{ts_type}; ");
        properties.push(item);
      }
      let properties = properties.join("");

      let mut methods = Vec::with_capacity(type_literal.methods.len());

      for method in type_literal.methods.iter() {
        let kind = match method.kind {
          MethodKind::Method => "",
          MethodKind::Getter => r#"<span class="td-kw">get </span>"#,
          MethodKind::Setter => r#"<span class="td-kw">set </span>"#,
        };

        let name = if method.name == "new" {
          r#"<span class="td-kw">new </span>"#.to_string()
        } else if method.computed {
          format!("[{}]", method.name)
        } else {
          method.name.clone()
        };

        let optional = if method.optional {
          r#"<span class="td-op">?</span>"#
        } else {
          Default::default()
        };

        let return_type = method
          .return_type
          .as_ref()
          .map(|ts_type| render_type_def_colon(ctx, ts_type))
          .unwrap_or_default();

        let item = format!(
          "{kind}{name}{optional}{}({}){return_type}; ",
          type_params_summary(ctx, &method.type_params),
          render_params(ctx, &method.params)
        );

        methods.push(item);
      }
      let methods = methods.join("");

      format!("{{ {index_signatures}{call_signatures}{properties}{methods} }}")
    }
    TsTypeDefKind::TypePredicate => {
      let type_predicate = def.type_predicate.as_ref().unwrap();

      let asserts = if type_predicate.asserts {
        r#"<span class="td-kw">asserts </span>"#
      } else {
        Default::default()
      };
      let param_type = if let crate::ts_type::ThisOrIdent::Identifier { name } =
        &type_predicate.param
      {
        html_escape::encode_text(name).to_string()
      } else {
        r#"<span class="td-kw">this</span>"#.to_string()
      };

      let r#type = type_predicate
        .r#type
        .as_ref()
        .map(|def| {
          format!(
            r#" <span class="td-kw">is</span> {}"#,
            render_type_def(ctx, def)
          )
        })
        .unwrap_or_default();

      format!("{asserts}{param_type}{}", r#type)
    }
    TsTypeDefKind::ImportType => {
      let import_type = def.import_type.as_ref().unwrap();

      let qualifier = import_type
        .qualifier
        .as_ref()
        .map(|qualifier| {
          format!("<span>.{}</span>", html_escape::encode_text(qualifier))
        })
        .unwrap_or_default();

      let type_arguments = import_type
        .type_params
        .as_ref()
        .map(|type_params| type_arguments(ctx, type_params))
        .unwrap_or_default();

      format!(
        r#"<span class="td-kw">import</span>(<span class="td-str">"{}"</span>){qualifier}{type_arguments}"#,
        html_escape::encode_text(&import_type.specifier),
      )
    }
  }
}

fn type_def_join(
  ctx: &RenderContext,
  union: &[crate::ts_type::TsTypeDef],
  join: char,
) -> String {
  let rendered: Vec<String> =
    union.iter().map(|element| render_type_def(ctx, element)).collect();

  let total_len = rendered.iter().map(|s| html_text_len(s)).sum::<usize>()
    + rendered.len().saturating_sub(1) * 3; // join char + 2 for spaces around

  if total_len <= MAX_INLINE_LEN {
    let items =
      rendered.join(&format!(r#"<span class="td-op"> {join} </span>"#));
    format!("<span>{items}</span>")
  } else {
    let mut items = Vec::with_capacity(rendered.len());

    for (i, rendered_item) in rendered.iter().enumerate() {
      items.push(format!(
        r#"<span>{}{}</span>"#,
        if i != 0 {
          format!(r#"<span class="td-op"> {join} </span>"#)
        } else {
          String::new()
        },
        rendered_item
      ));
    }

    let content = items.join("<br />");

    format!(r#"<div class="ml-indent">{content}</div>"#)
  }
}

fn type_def_tuple(
  ctx: &RenderContext,
  tuple_items: &[crate::ts_type::TsTypeDef],
) -> String {
  let rendered: Vec<String> = tuple_items
    .iter()
    .map(|element| render_type_def(ctx, element))
    .collect();

  let total_len = rendered.iter().map(|s| html_text_len(s)).sum::<usize>()
    + rendered.len().saturating_sub(1) * 2 // for brackets
    + 2; // separator ", "

  if total_len <= MAX_INLINE_LEN {
    let items = rendered.join(", ");
    format!("<span>[{items}]</span>")
  } else {
    let last = rendered.len() - 1;
    let mut items = Vec::with_capacity(rendered.len());

    for (i, rendered_item) in rendered.iter().enumerate() {
      let stripped = strip_indent_wrapper(rendered_item);
      let content = if i < last {
        with_trailing_comma(stripped)
      } else {
        stripped.to_string()
      };
      items.push(format!(r#"<div><span>{content}</span></div>"#));
    }

    let content = items.join("");
    format!(
      r#"<span>[</span><div class="ml-indent">{content}</div><span>]</span>"#
    )
  }
}

pub(crate) fn type_params_summary(
  ctx: &RenderContext,
  type_params: &[TsTypeParamDef],
) -> String {
  if type_params.is_empty() {
    return String::new();
  }

  let rendered: Vec<String> = type_params
    .iter()
    .map(|tp| type_param_summary(ctx, tp, "extends"))
    .collect();

  let total_len = rendered.iter().map(|s| html_text_len(s)).sum::<usize>()
    + rendered.len().saturating_sub(1) * 2 // angled brackets
    + 2; // separator ", "

  if total_len <= MAX_INLINE_LEN {
    let items = rendered.join("<span>, </span>");
    format!("<span>&lt;{items}&gt;</span>")
  } else {
    let last = rendered.len() - 1;
    let mut items = Vec::with_capacity(rendered.len());

    for (i, rendered_item) in rendered.iter().enumerate() {
      let content = if i < last {
        with_trailing_comma(rendered_item)
      } else {
        rendered_item.clone()
      };
      items.push(format!("<div>{content}</div>"));
    }

    let content = items.join("");

    format!(r#"<span>&lt;<div class="ml-indent">{content}</div>&gt;</span>"#)
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
        r#"<span><span class="td-kw"> {constraint_kind} </span>{}</span>"#,
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
    r#"<span><span class="td-tp">{}</span>{constraint}{default}</span>"#,
    type_param.name,
  )
}

pub(crate) fn type_arguments(
  ctx: &RenderContext,
  defs: &[crate::ts_type::TsTypeDef],
) -> String {
  if defs.is_empty() {
    return String::new();
  }

  let rendered: Vec<String> =
    defs.iter().map(|def| render_type_def(ctx, def)).collect();

  let total_len = rendered.iter().map(|s| html_text_len(s)).sum::<usize>()
    + rendered.len().saturating_sub(1) * 2 // angled brackets
    + 2; // separator ", "

  if total_len <= MAX_INLINE_LEN {
    let items = rendered.join("<span>, </span>");
    format!("&lt;{items}&gt;")
  } else {
    let last = rendered.len() - 1;
    let mut items = Vec::with_capacity(rendered.len());

    for (i, rendered_item) in rendered.iter().enumerate() {
      let stripped = strip_indent_wrapper(rendered_item);
      let content = if i < last {
        with_trailing_comma(stripped)
      } else {
        stripped.to_string()
      };
      items.push(format!("<div>{content}</div>"));
    }

    let content = items.join("");

    format!(r#"&lt;<div class="ml-indent">{content}</div>&gt;"#)
  }
}

pub(crate) fn render_type_params(
  ctx: &RenderContext,
  js_doc: &JsDoc,
  type_params: &[TsTypeParamDef],
  location: &crate::Location,
) -> Option<SectionCtx> {
  if type_params.is_empty() {
    return None;
  }

  let mut items = Vec::with_capacity(type_params.len());

  let type_param_docs = js_doc
    .tags
    .iter()
    .filter_map(|tag| {
      if let JsDocTag::Template { name, doc } = tag {
        doc.as_ref().map(|doc| (&**name, &**doc))
      } else {
        None
      }
    })
    .collect::<std::collections::HashMap<&str, &str>>();

  for type_param in type_params.iter() {
    let id = IdBuilder::new(ctx.ctx)
      .kind(IdKind::TypeParam)
      .name(&type_param.name)
      .build();

    let constraint = type_param
      .constraint
      .as_ref()
      .map(|constraint| {
        format!(
          r#"<span><span class="td-kw"> extends </span>{}</span>"#,
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

    let content = DocEntryCtx::new(
      ctx,
      id,
      Some(html_escape::encode_text(&type_param.name).into_owned()),
      None,
      &format!("{constraint}{default}"),
      Default::default(),
      type_param_docs.get(type_param.name.as_str()).cloned(),
      location,
    );

    items.push(content);
  }

  Some(SectionCtx::new(
    ctx,
    "Type Parameters",
    SectionContentCtx::DocEntry(items),
  ))
}
