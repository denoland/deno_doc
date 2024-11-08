use super::DocNodeWithContext;
use super::FileMode;
use super::RenderContext;
use super::UrlResolveKind;
use crate::js_doc::JsDocTag;
use crate::DocNodeKind;
use indexmap::IndexMap;
use regex::Regex;
use serde::Serialize;
use std::borrow::Cow;

lazy_static! {
  static ref IDENTIFIER_RE: Regex = Regex::new(r"[^a-zA-Z$_]").unwrap();
}

fn render_css_for_usage(name: &str) -> String {
  format!(
    r#"
#{name}:checked ~ *:last-child > :not(#{name}_content) {{
  display: none;
}}
#{name}:checked ~ nav #{name}_active_dropdown {{
  display: flex;
}}
#{name}:checked ~ nav label[for='{name}'] {{
  cursor: unset;
  background-color: #EBF6FF;
}}
"#
  )
}

fn usage_to_md(
  ctx: &RenderContext,
  doc_nodes: &[DocNodeWithContext],
  url: &str,
  custom_file_identifier: Option<&str>,
) -> String {
  let usage =
    if let UrlResolveKind::Symbol { symbol, .. } = ctx.get_current_resolve() {
      let mut parts = symbol.split('.');

      let top_node = doc_nodes[0].get_topmost_ancestor();

      let is_default = top_node.is_default.is_some_and(|is_default| is_default)
        || &*top_node.name == "default";

      let import_symbol: Box<str> = if is_default {
        if top_node.is_default.is_some_and(|is_default| is_default) {
          let default_name = top_node.get_name();
          if default_name == "default" {
            get_identifier_for_file(ctx, custom_file_identifier).into()
          } else {
            default_name.into()
          }
        } else {
          "module".into()
        }
      } else {
        parts.clone().next().unwrap().into()
      };

      let usage_symbol = if doc_nodes.iter().all(|node| node.parent.is_some()) {
        None
      } else {
        let last = parts.next_back();
        if let Some(usage_symbol) = last {
          if usage_symbol == symbol {
            None
          } else {
            Some((
              usage_symbol,
              // if it is namespaces within namespaces, we simply re-join them together
              // instead of trying to figure out some sort of nested restructuring
              if is_default {
                import_symbol.clone()
              } else {
                let capacity = symbol.len() - usage_symbol.len() - 1;
                let mut joined = String::with_capacity(capacity);
                for part in parts {
                  if !joined.is_empty() {
                    joined.push('.');
                  }
                  joined.push_str(part);
                }
                joined.into_boxed_str()
              },
            ))
          }
        } else {
          None
        }
      };

      let is_type = doc_nodes.iter().all(|doc_node| {
        matches!(
          doc_node
            .parent
            .as_ref()
            .map_or_else(|| doc_node.kind(), |parent| parent.kind()),
          DocNodeKind::TypeAlias | DocNodeKind::Interface
        )
      });

      let mut usage_statement = if is_default {
        format!(
          r#"import {}{} from "{url}";"#,
          if is_type { "type " } else { "" },
          html_escape::encode_text(&import_symbol),
        )
      } else {
        format!(
          r#"import {{ {}{} }} from "{url}";"#,
          if is_type { "type " } else { "" },
          html_escape::encode_text(&import_symbol),
        )
      };

      if let Some((usage_symbol, local_var)) = usage_symbol {
        usage_statement.push_str(&format!(
          "\n{} {{ {} }} = {local_var};",
          if is_type { "type" } else { "const" },
          html_escape::encode_text(usage_symbol),
        ));
      }

      usage_statement
    } else {
      let module_import_symbol =
        get_identifier_for_file(ctx, custom_file_identifier);

      format!(r#"import * as {module_import_symbol} from "{url}";"#)
    };

  format!("```typescript\n{usage}\n```")
}

fn get_identifier_for_file(
  ctx: &RenderContext,
  custom_file_identifier: Option<&str>,
) -> String {
  let maybe_identifier =
    if let Some(file) = ctx.get_current_resolve().get_file() {
      ctx
        .ctx
        .doc_nodes
        .get(file)
        .and_then(|nodes| {
          nodes
            .iter()
            .find(|node| node.kind() == DocNodeKind::ModuleDoc)
        })
        .and_then(|node| {
          node.js_doc.tags.iter().find_map(|tag| {
            if let JsDocTag::Module { name } = tag {
              name.as_ref().map(|name| name.to_string())
            } else {
              None
            }
          })
        })
    } else if let Some(context_name) = custom_file_identifier {
      Some(context_name.to_string())
    } else {
      ctx.ctx.package_name.clone()
    };

  maybe_identifier.as_ref().map_or_else(
    || "mod".to_string(),
    |identifier| IDENTIFIER_RE.replace_all(identifier, "_").to_string(),
  )
}

#[cfg(not(feature = "rust"))]
pub type UsageToMd<'a> = &'a js_sys::Function;
#[cfg(feature = "rust")]
pub type UsageToMd<'a> =
  &'a dyn Fn(&[DocNodeWithContext], &str, Option<&str>) -> String;

#[derive(Clone, Debug, Serialize)]
struct UsageCtx {
  name: String,
  content: String,
  icon: Option<Cow<'static, str>>,
  additional_css: String,
}

#[derive(Clone, Debug, Serialize)]
pub struct UsagesCtx {
  usages: Vec<UsageCtx>,
  composed: bool,
}

impl UsagesCtx {
  pub const TEMPLATE: &'static str = "usages";

  pub fn new(
    ctx: &RenderContext,
    doc_nodes: &[DocNodeWithContext],
  ) -> Option<Self> {
    let is_single_mode = ctx.ctx.usage_composer.is_single_mode();

    if is_single_mode && ctx.ctx.file_mode == FileMode::SingleDts {
      return None;
    }

    let usage_ctx = ctx.clone();

    #[cfg(not(target_arch = "wasm32"))]
    let usage_to_md_closure =
      move |nodes: &[DocNodeWithContext],
            url: &str,
            custom_file_identifier: Option<&str>| {
        usage_to_md(&usage_ctx, nodes, url, custom_file_identifier)
      };

    #[cfg(target_arch = "wasm32")]
    let usage_to_md_closure =
      move |nodes: Vec<DocNodeWithContext>,
            url: String,
            custom_file_identifier: Option<String>| {
        usage_to_md(&usage_ctx, &nodes, &url, custom_file_identifier.as_deref())
      };

    #[cfg(target_arch = "wasm32")]
    let usage_to_md_closure =
      wasm_bindgen::prelude::Closure::wrap(Box::new(usage_to_md_closure)
        as Box<
          dyn Fn(Vec<DocNodeWithContext>, String, Option<String>) -> String,
        >);
    #[cfg(target_arch = "wasm32")]
    let usage_to_md_closure = &wasm_bindgen::JsCast::unchecked_ref::<
      js_sys::Function,
    >(usage_to_md_closure.as_ref());

    let usages = ctx.ctx.usage_composer.compose(
      doc_nodes,
      ctx.get_current_resolve(),
      &usage_to_md_closure,
    );

    if usages.is_empty() {
      None
    } else {
      let usages = usages
        .into_iter()
        .map(|(entry, content)| UsageCtx {
          additional_css: if is_single_mode {
            String::new()
          } else {
            render_css_for_usage(&entry.name)
          },
          name: entry.name,
          icon: entry.icon,
          content: crate::html::jsdoc::render_markdown(ctx, &content, true),
        })
        .collect::<Vec<_>>();

      Some(UsagesCtx {
        usages,
        composed: !is_single_mode,
      })
    }
  }
}

#[derive(Eq, PartialEq, Hash, serde::Deserialize)]
pub struct UsageComposerEntry {
  pub name: String,
  pub icon: Option<Cow<'static, str>>,
}

pub trait UsageComposer {
  fn is_single_mode(&self) -> bool;

  fn compose(
    &self,
    doc_nodes: &[DocNodeWithContext],
    current_resolve: UrlResolveKind,
    usage_to_md: UsageToMd,
  ) -> IndexMap<UsageComposerEntry, String>;
}
