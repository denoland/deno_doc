use super::DocNodeWithContext;
use super::RenderContext;
use super::UrlResolveKind;
use crate::DocNodeKind;
use serde::Serialize;

fn render_css_for_usage(name: &str) -> String {
  format!(
    r#"
#{name}:checked ~ *:last-child > :not(#{name}_content) {{
  display: none;
}}
#{name}:checked ~ nav:first-of-type > label[for='{name}'] {{
  border-bottom-width: 2px;
  cursor: unset;
  border-color: rgb(17 24 39);
}}
#{name}:not:checked ~ nav:first-of-type > label[for='{name}'] {{
  border-color: rgb(209 213 219);
}}
"#
  )
}

pub fn usage_to_md(
  ctx: &RenderContext,
  doc_nodes: &[DocNodeWithContext],
  url: &str,
) -> String {
  let usage = if let UrlResolveKind::Symbol { symbol, file } =
    ctx.get_current_resolve()
  {
    let mut parts = symbol.split('.').collect::<Vec<&str>>();

    let is_default = doc_nodes[0].name == "default";

    let import_symbol = if is_default && doc_nodes[0].get_name() == "default" {
      file
        .to_name()
        .replace("-", "_")
        .replace(|c: char| c.is_ascii_alphanumeric(), "")
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
          parts.join("."),
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
pub struct UsagesCtx {
  show_tabs: bool,
  usages: Vec<UsageCtx>,
}

#[derive(Clone, Debug, Serialize)]
pub struct UsageCtx {
  name: String,
  content: String,
  additional_css: String,
}

impl UsagesCtx {
  pub fn new(
    ctx: &RenderContext,
    doc_nodes: &[DocNodeWithContext],
  ) -> Option<Self> {
    let url = ctx.ctx.href_resolver.resolve_usage(
      ctx.get_current_specifier()?,
      ctx.get_current_resolve().get_file(),
    )?;

    if let Some(usage_composer) = &ctx.ctx.usage_composer {
      let usages = usage_composer(ctx, doc_nodes, url);

      let usages = usages
        .into_iter()
        .map(|(name, content)| UsageCtx {
          additional_css: render_css_for_usage(&name),
          name,
          content: crate::html::jsdoc::render_markdown(ctx, &content),
        })
        .collect::<Vec<_>>();

      if usages.is_empty() {
        None
      } else {
        Some(UsagesCtx {
          show_tabs: true,
          usages,
        })
      }
    } else {
      let import_statement = usage_to_md(ctx, doc_nodes, &url);
      let rendered_import_statement =
        crate::html::jsdoc::render_markdown(ctx, &import_statement);

      Some(UsagesCtx {
        show_tabs: false,
        usages: vec![UsageCtx {
          name: "".to_string(),
          content: rendered_import_statement,
          additional_css: "".to_string(),
        }],
      })
    }
  }
}
