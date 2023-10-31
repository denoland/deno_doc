use super::types::render_type_def;
use super::util::RenderContext;
use crate::DocNode;
use crate::DocNodeKind;
use serde::Serialize;
use std::collections::HashMap;
use std::fmt::Write;

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

pub fn render_symbol_group(
  doc_nodes: &[DocNode],
  name: &str,
  ctx: &RenderContext,
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
      subtitle: doc_block_subtitle(&doc_nodes[0], ctx),
      body: doc_block(doc_nodes, name, ctx),
    })
    .collect();

  SymbolGroupCtx {
    name: name.to_string(),
    symbols,
  }
}

fn doc_block_subtitle(
  doc_node: &DocNode,
  ctx: &RenderContext,
) -> Option<String> {
  let subtitle = match doc_node.kind {
    DocNodeKind::Function => None,
    DocNodeKind::Variable => None,
    DocNodeKind::Class => {
      let class_def = doc_node.class_def.as_ref().unwrap();
      let mut subtitle = String::new();

      let ctx = &RenderContext {
        current_type_params: class_def
          .type_params
          .iter()
          .map(|def| def.name.clone())
          .collect::<std::collections::HashSet<String>>(),
        ..ctx.clone()
      };

      if !class_def.implements.is_empty() {
        let implements = class_def
          .implements
          .iter()
          .map(|extend| render_type_def(extend, ctx))
          .collect::<Vec<String>>()
          .join("<span>, </span>");

        write!(
          subtitle,
          r#"<div><span class="doc_block_subtitle_text"> implements </span>{implements}</div>"#
        ).unwrap();
      }

      if let Some(extends) = class_def.extends.as_ref() {
        let extends = ctx.lookup_symbol_href(extends).map_or_else(
          || format!("<span>{extends}</span>"),
          |href| format!(r#"<a href="{href}" class="link">{extends}</a>"#),
        );

        write!(
          subtitle,
          r#"<div><span class="doc_block_subtitle_text"> extends </span>{extends}<span>{}</span></div>"#,
          super::types::type_arguments(&class_def.super_type_params, ctx),
        ).unwrap();
      }

      Some(subtitle)
    }
    DocNodeKind::Enum => None,
    DocNodeKind::Interface => {
      let interface_def = doc_node.interface_def.as_ref().unwrap();

      let subtitle = if !interface_def.extends.is_empty() {
        let ctx = &RenderContext {
          current_type_params: interface_def
            .type_params
            .iter()
            .map(|def| def.name.clone())
            .collect::<std::collections::HashSet<String>>(),
          ..ctx.clone()
        };

        let extends = interface_def
          .extends
          .iter()
          .map(|extend| render_type_def(extend, ctx))
          .collect::<Vec<String>>()
          .join("<span>, </span>");

        Some(format!(
          r#"<div><span class="doc_block_subtitle_text"> extends </span>{extends}</div>"#
        ))
      } else {
        None
      };

      subtitle
    }
    DocNodeKind::TypeAlias => None,
    DocNodeKind::Namespace => None,
    _ => unimplemented!(),
  };

  subtitle.map(|subtitle| {
    format!(r#"<div class="doc_block_subtitle">{subtitle}</div>"#)
  })
}

fn doc_block(doc_nodes: &[DocNode], name: &str, ctx: &RenderContext) -> String {
  let mut content = String::new();
  let mut functions = vec![];

  for doc_node in doc_nodes {
    match doc_node.kind {
      DocNodeKind::Function => functions.push(doc_node),
      DocNodeKind::Variable => {
        content.push_str(&super::variable::render_variable(doc_node, ctx))
      }
      DocNodeKind::Class => {
        content.push_str(&super::class::render_class(doc_node, ctx))
      }
      DocNodeKind::Enum => {
        content.push_str(&super::r#enum::render_enum(doc_node, ctx))
      }
      DocNodeKind::Interface => {
        content.push_str(&super::interface::render_interface(doc_node, ctx))
      }
      DocNodeKind::TypeAlias => {
        content.push_str(&super::type_alias::render_type_alias(doc_node, ctx))
      }
      DocNodeKind::Namespace => {
        content.push_str(&super::namespace::render_namespace(
          doc_node,
          &RenderContext {
            namespace: Some(name.to_string()),
            ..ctx.clone()
          },
        ))
      }
      DocNodeKind::ModuleDoc => {}
      DocNodeKind::Import => {}
    };
  }

  if !functions.is_empty() {
    content.push_str(&super::function::render_function(functions, ctx));
  }

  content
}
