use super::RenderContext;
use super::UrlResolveKind;
use super::{DocNodeWithContext, FileMode};
use crate::DocNodeKind;
use serde::Serialize;

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

pub fn usage_to_md(
  ctx: &RenderContext,
  doc_nodes: &[DocNodeWithContext],
  url: &str,
) -> String {
  let usage =
    if let UrlResolveKind::Symbol { symbol, .. } = ctx.get_current_resolve() {
      let mut parts = symbol.split('.').collect::<Vec<&str>>();

      let top_node = doc_nodes[0].get_topmost_ancestor();

      let is_default = top_node.is_default.is_some_and(|is_default| is_default)
        || top_node.name == "default";

      let import_symbol = if is_default {
        if top_node.is_default.is_some_and(|is_default| is_default) {
          top_node.name.clone()
        } else {
          "module".to_string()
        }
      } else {
        parts[0].to_string()
      };

      let usage_symbol = if doc_nodes
        .iter()
        .all(|node| node.drilldown_parent_kind.is_some())
      {
        None
      } else if parts.len() > 1 {
        parts.pop().map(|usage_symbol| {
          (
            usage_symbol,
            // if it is namespaces within namespaces, we simply re-join them together
            // instead of trying to figure out some sort of nested restructuring
            if is_default {
              import_symbol.clone()
            } else {
              parts.join(".")
            },
          )
        })
      } else {
        None
      };

      let is_type = doc_nodes.iter().all(|doc_node| {
        matches!(
          doc_node.drilldown_parent_kind.unwrap_or(doc_node.kind),
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
      // when the imported symbol is a namespace import, we try to guess at an
      // intelligent camelized name for the import based on the package name.
      let import_symbol = "mod";

      format!(r#"import * as {import_symbol} from "{url}";"#)
    };

  format!("```typescript\n{usage}\n```")
}

#[derive(Clone, Debug, Serialize)]
struct UsageCtx {
  name: String,
  content: String,
  icon: Option<std::borrow::Cow<'static, str>>,
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
    if ctx.ctx.usage_composer.is_none()
      && ctx.ctx.file_mode == FileMode::SingleDts
    {
      return None;
    }

    let url = ctx
      .ctx
      .href_resolver
      .resolve_usage(ctx.get_current_resolve())?;

    if let Some(usage_composer) = &ctx.ctx.usage_composer {
      let usages = usage_composer(ctx, doc_nodes, url);

      let usages = usages
        .into_iter()
        .map(|(entry, content)| UsageCtx {
          additional_css: render_css_for_usage(&entry.name),
          name: entry.name,
          icon: entry.icon,
          content: crate::html::jsdoc::render_markdown(ctx, &content),
        })
        .collect::<Vec<_>>();

      if usages.is_empty() {
        None
      } else {
        Some(UsagesCtx {
          usages,
          composed: true,
        })
      }
    } else {
      let import_statement = usage_to_md(ctx, doc_nodes, &url);
      let rendered_import_statement =
        crate::html::jsdoc::render_markdown(ctx, &import_statement);

      Some(UsagesCtx {
        usages: vec![UsageCtx {
          name: "".to_string(),
          content: rendered_import_statement,
          icon: None,
          additional_css: "".to_string(),
        }],
        composed: false,
      })
    }
  }
}
