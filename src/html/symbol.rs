use super::util::RenderContext;
use crate::html::symbols;
use crate::html::types::render_type_def;
use crate::html::GenerateCtx;
use crate::DocNode;
use crate::DocNodeKind;
use serde::Serialize;
use serde_json::json;
use std::collections::HashMap;

#[derive(Debug, Serialize, Clone)]
pub struct SymbolGroupCtx {
  name: String,
  symbols: Vec<SymbolCtx>,
}

#[derive(Debug, Serialize, Clone)]
pub struct SymbolCtx {
  kind: super::util::DocNodeKindCtx,
  subtitle: Option<String>,
  body: String,
}

pub(crate) fn get_symbol_group_ctx(
  ctx: &GenerateCtx,
  doc_nodes: &[DocNode],
  name: &str,
  render_ctx: &RenderContext,
) -> SymbolGroupCtx {
  let mut split_nodes = HashMap::<DocNodeKind, Vec<DocNode>>::default();
  // TODO(bartlomieju): I'm not sure what this meant to do
  // let mut is_reexport = false;

  for doc_node in doc_nodes {
    if doc_node.kind == DocNodeKind::Import {
      // TODO(bartlomieju): I'm not sure what this meant to do
      // is_reexport = true;
      continue;
    }

    split_nodes
      .entry(doc_node.kind)
      .or_insert(vec![])
      .push(doc_node.clone());
  }

  // TODO: property drilldown

  let symbols = split_nodes
    .values()
    .map(|doc_nodes| SymbolCtx {
      kind: (&doc_nodes[0].kind).into(),
      subtitle: doc_block_subtitle(ctx, &doc_nodes[0], render_ctx),
      body: doc_block(ctx, doc_nodes, name, render_ctx),
    })
    .collect();

  SymbolGroupCtx {
    name: name.to_string(),
    symbols,
  }
}

fn doc_block_subtitle(
  ctx: &GenerateCtx,
  doc_node: &DocNode,
  render_ctx: &RenderContext,
) -> Option<String> {
  if matches!(
    doc_node.kind,
    DocNodeKind::Function
      | DocNodeKind::Variable
      | DocNodeKind::Enum
      | DocNodeKind::TypeAlias
      | DocNodeKind::Namespace
  ) {
    return None;
  }

  if matches!(doc_node.kind, DocNodeKind::Class) {
    let class_def = doc_node.class_def.as_ref().unwrap();

    let current_type_params = class_def
      .type_params
      .iter()
      .map(|def| def.name.clone())
      .collect::<std::collections::HashSet<String>>();

    let render_ctx = &render_ctx.with_current_type_params(current_type_params);

    let mut class_implements = None;
    let mut class_extends = None;

    if !class_def.implements.is_empty() {
      let impls = class_def
        .implements
        .iter()
        .map(|extend| render_type_def(ctx, extend, render_ctx))
        .collect::<Vec<String>>();

      class_implements = Some(impls);
    }

    if let Some(extends) = class_def.extends.as_ref() {
      let symbol = if let Some(href) = render_ctx.lookup_symbol_href(extends) {
        format!(r#"<a href="{href}" class="link">{extends}</a>"#)
      } else {
        format!("<span>{extends}</span>")
      };

      class_extends = Some(json!({
        "symbol": symbol,
        "type_args": crate::html::types::type_arguments(ctx, &class_def.super_type_params, render_ctx)
      }));
    }

    return Some(ctx.render(
      "doc_block_subtitle.html",
      &json!({
        "class": {
          "implements": class_implements,
          "extends": class_extends,
        },
        "interface": null,
      }),
    ));
  }

  if matches!(doc_node.kind, DocNodeKind::Interface) {
    let interface_def = doc_node.interface_def.as_ref().unwrap();

    if interface_def.extends.is_empty() {
      return None;
    }

    let current_type_params = interface_def
      .type_params
      .iter()
      .map(|def| def.name.clone())
      .collect::<std::collections::HashSet<String>>();
    let render_ctx = &render_ctx.with_current_type_params(current_type_params);

    let extends = interface_def
      .extends
      .iter()
      .map(|extend| render_type_def(ctx, extend, render_ctx))
      .collect::<Vec<String>>();

    return Some(ctx.render(
      "doc_block_subtitle.html",
      &json!({
        "class": null,
        "interface": {
          "extends": extends
        }
      }),
    ));
  }

  unreachable!()
}

fn doc_block(
  ctx: &GenerateCtx,
  doc_nodes: &[DocNode],
  name: &str,
  render_ctx: &RenderContext,
) -> String {
  let mut content_parts = Vec::with_capacity(doc_nodes.len());
  let mut functions = vec![];

  fn doc_block_item(docs: String, content: String) -> String {
    format!(r#"<div class="doc_block_items">{docs}{content}</div>"#)
  }

  for doc_node in doc_nodes {
    match doc_node.kind {
      DocNodeKind::Function => functions.push(doc_node),
      DocNodeKind::Variable => {
        let docs = crate::html::jsdoc::render_docs_with_examples(
          ctx,
          render_ctx,
          &doc_node.js_doc,
        );
        let el = symbols::variable::render_variable(ctx, doc_node, render_ctx);
        let content = doc_block_item(docs, el);
        content_parts.push(content)
      }
      DocNodeKind::Class => {
        let docs = crate::html::jsdoc::render_docs_with_examples(
          ctx,
          render_ctx,
          &doc_node.js_doc,
        );
        let el = symbols::class::render_class(ctx, doc_node, render_ctx);
        let content = doc_block_item(docs, el);
        content_parts.push(content);
      }
      DocNodeKind::Enum => {
        let docs = crate::html::jsdoc::render_docs_with_examples(
          ctx,
          render_ctx,
          &doc_node.js_doc,
        );
        let el = symbols::r#enum::render_enum(ctx, doc_node, render_ctx);
        let content = doc_block_item(docs, el);
        content_parts.push(content);
      }
      DocNodeKind::Interface => {
        let docs = crate::html::jsdoc::render_docs_with_examples(
          ctx,
          render_ctx,
          &doc_node.js_doc,
        );
        let el =
          symbols::interface::render_interface(ctx, doc_node, render_ctx);
        let content = doc_block_item(docs, el);
        content_parts.push(content);
      }
      DocNodeKind::TypeAlias => {
        let docs = crate::html::jsdoc::render_docs_with_examples(
          ctx,
          render_ctx,
          &doc_node.js_doc,
        );
        let el =
          symbols::type_alias::render_type_alias(ctx, doc_node, render_ctx);
        let content = doc_block_item(docs, el);
        content_parts.push(content);
      }
      DocNodeKind::Namespace => {
        let docs = crate::html::jsdoc::render_docs_with_examples(
          ctx,
          render_ctx,
          &doc_node.js_doc,
        );
        let el = symbols::namespace::render_namespace(
          ctx,
          doc_node,
          &render_ctx.with_namespace(name.to_string()),
        );
        let content = doc_block_item(docs, el);
        content_parts.push(content);
      }
      DocNodeKind::ModuleDoc => {}
      DocNodeKind::Import => {}
    };
  }

  if !functions.is_empty() {
    let content =
      symbols::function::render_function(ctx, functions, render_ctx);
    content_parts.push(content);
  }

  content_parts.join("")
}
