use super::RenderContext;
use super::UrlResolveKind;
use crate::DocNode;
use crate::DocNodeKind;
use serde::Serialize;

fn render_css_for_usage(name: &str) -> String {
  format!(
    r#"
#{name}:checked ~ *:last-child > :not(#{name}_content) {{
  display: none;
}}
#{name}:checked ~ nav:first-of-type > label[for='{name}'] > div {{
  border-bottom-width: 2px;
  cursor: unset;
  border-color: rgb(0 0 0);
  padding-bottom: 0.375rem !important; /* 6px */
}}
"#
  )
}

pub fn usage_to_md(
  ctx: &RenderContext,
  doc_nodes: &[DocNode],
  url: String,
) -> String {
  let usage =
    if let UrlResolveKind::Symbol { symbol, .. } = ctx.get_current_resolve() {
      let mut parts = symbol.split('.').collect::<Vec<&str>>();

      let import_symbol = parts[0];

      let usage_symbol = if parts.len() > 1 {
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
          doc_node.kind,
          DocNodeKind::TypeAlias | DocNodeKind::Interface
        )
      });

      let mut usage_statement = format!(
        r#"import {{ {}{import_symbol} }} from "{url}";"#,
        if is_type { "type " } else { "" }
      );

      if let Some((usage_symbol, local_var)) = usage_symbol {
        usage_statement
          .push_str(&format!("\nconst {{ {usage_symbol} }} = {local_var};"));
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
pub struct UsageCtx {
  name: String,
  content: String,
  additional_css: String,
}

impl UsageCtx {
  pub fn new(ctx: &RenderContext, doc_nodes: &[DocNode]) -> Option<Vec<Self>> {
    let url = ctx.ctx.href_resolver.resolve_usage(
      ctx.get_current_specifier().unwrap(),
      ctx.get_current_resolve().get_file().unwrap(),
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
        .collect::<Vec<UsageCtx>>();

      if usages.is_empty() {
        None
      } else {
        Some(usages)
      }
    } else {
      let import_statement = usage_to_md(ctx, doc_nodes, url);
      let rendered_import_statement =
        crate::html::jsdoc::render_markdown(ctx, &import_statement);

      Some(vec![UsageCtx {
        name: "".to_string(),
        content: rendered_import_statement,
        additional_css: "".to_string(),
      }])
    }
  }
}
