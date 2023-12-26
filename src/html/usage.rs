use super::RenderContext;
use super::UrlResolveKind;
use crate::DocNode;
use crate::DocNodeKind;
use serde::Serialize;

fn parse_usage(ctx: &RenderContext, doc_nodes: &[DocNode]) -> String {
  let url = (ctx.ctx.usage_resolver)(
    ctx.get_current_specifier().unwrap(),
    ctx.get_current_resolve().get_file().unwrap(),
  );

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
        .push_str(&format!("const {{ {usage_symbol} }} = {local_var};"));
    }

    usage_statement
  } else {
    // when the imported symbol is a namespace import, we try to guess at an
    // intelligent camelized name for the import based on the package name.
    let import_symbol = "mod";

    format!(r#"import * as {import_symbol} from "{url}";"#)
  }
}

#[derive(Clone, Debug, Serialize)]
pub struct UsageCtx {
  import_statement: String,
  raw_import_statement: String,
}

impl UsageCtx {
  pub fn new(ctx: &RenderContext, doc_nodes: &[DocNode]) -> Self {
    let import_statement = parse_usage(ctx, doc_nodes);
    let rendered_import_statement = crate::html::jsdoc::render_markdown(
      &format!("```typescript\n{import_statement}\n```"),
      ctx,
    );
    UsageCtx {
      import_statement: rendered_import_statement,
      raw_import_statement: format!("{import_statement:?}"),
    }
  }
}
