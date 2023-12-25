use crate::html::types::render_type_def;
use crate::html::usage::UsageCtx;
use crate::html::util::SectionCtx;
use crate::html::RenderContext;
use crate::DocNode;
use crate::DocNodeKind;
use serde::Serialize;
use std::collections::HashMap;

pub mod class;
mod r#enum;
mod function;
mod interface;
pub mod namespace;
mod type_alias;
mod variable;

#[derive(Debug, Serialize, Clone)]
struct SymbolCtx {
  kind: super::util::DocNodeKindCtx,
  subtitle: Option<DocBlockSubtitleCtx>,
  content: Vec<SymbolInnerCtx>,
}

#[derive(Debug, Serialize, Clone)]
pub struct SymbolGroupCtx {
  pub name: String,
  symbols: Vec<SymbolCtx>,
  usage: UsageCtx,
}

impl SymbolGroupCtx {
  pub fn new(ctx: &RenderContext, doc_nodes: &[DocNode], name: &str) -> Self {
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
        kind: doc_nodes[0].kind.into(),
        subtitle: DocBlockSubtitleCtx::new(ctx, &doc_nodes[0]),
        content: SymbolInnerCtx::new(ctx, doc_nodes, name),
      })
      .collect();

    SymbolGroupCtx {
      name: name.to_string(),
      symbols,
      usage: UsageCtx::new(ctx, doc_nodes),
    }
  }
}

#[derive(Debug, Serialize, Clone)]
struct DocBlockClassSubtitleExtendsCtx {
  href: Option<String>,
  symbol: String,
  type_args: String,
}

#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "snake_case")]
#[serde(tag = "kind", content = "value")]
enum DocBlockSubtitleCtx {
  Class {
    implements: Option<Vec<String>>,
    extends: Option<DocBlockClassSubtitleExtendsCtx>,
  },
  Interface {
    extends: Vec<String>,
  },
}

impl DocBlockSubtitleCtx {
  fn new(ctx: &RenderContext, doc_node: &DocNode) -> Option<Self> {
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

    if doc_node.kind == DocNodeKind::Class {
      let class_def = doc_node.class_def.as_ref().unwrap();

      let current_type_params = class_def
        .type_params
        .iter()
        .map(|def| def.name.clone())
        .collect::<std::collections::HashSet<String>>();

      let ctx = &ctx.with_current_type_params(current_type_params);

      let mut class_implements = None;
      let mut class_extends = None;

      if !class_def.implements.is_empty() {
        let impls = class_def
          .implements
          .iter()
          .map(|extend| render_type_def(ctx, extend))
          .collect::<Vec<String>>();

        class_implements = Some(impls);
      }

      if let Some(extends) = class_def.extends.as_ref() {
        class_extends = Some(DocBlockClassSubtitleExtendsCtx {
          href: ctx.lookup_symbol_href(extends),
          symbol: extends.to_owned(),
          type_args: super::types::type_arguments(
            ctx,
            &class_def.super_type_params,
          ),
        });
      }

      return Some(DocBlockSubtitleCtx::Class {
        implements: class_implements,
        extends: class_extends,
      });
    }

    if doc_node.kind == DocNodeKind::Interface {
      let interface_def = doc_node.interface_def.as_ref().unwrap();

      if interface_def.extends.is_empty() {
        return None;
      }

      let current_type_params = interface_def
        .type_params
        .iter()
        .map(|def| def.name.clone())
        .collect::<std::collections::HashSet<String>>();
      let ctx = &ctx.with_current_type_params(current_type_params);

      let extends = interface_def
        .extends
        .iter()
        .map(|extend| render_type_def(ctx, extend))
        .collect::<Vec<String>>();

      return Some(DocBlockSubtitleCtx::Interface { extends });
    }

    unreachable!()
  }
}

#[derive(Debug, Serialize, Clone)]
struct SymbolContentCtx {
  pub id: String,
  pub docs: Option<String>,
  pub sections: Vec<SectionCtx>,
}

#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "snake_case", tag = "kind", content = "value")]
enum SymbolInnerCtx {
  Function(function::FunctionCtx),
  Namespace(String),
  Other(SymbolContentCtx),
}

impl SymbolInnerCtx {
  fn new(ctx: &RenderContext, doc_nodes: &[DocNode], name: &str) -> Vec<Self> {
    let mut content_parts = Vec::with_capacity(doc_nodes.len());
    let mut functions = vec![];

    for doc_node in doc_nodes {
      match doc_node.kind {
        DocNodeKind::Function => functions.push(doc_node),

        DocNodeKind::Variable
        | DocNodeKind::Class
        | DocNodeKind::Enum
        | DocNodeKind::Interface
        | DocNodeKind::TypeAlias => {
          let get_section_kinds = match doc_node.kind {
            DocNodeKind::Variable => variable::render_variable,
            DocNodeKind::Class => class::render_class,
            DocNodeKind::Enum => r#enum::render_enum,
            DocNodeKind::Interface => interface::render_interface,
            DocNodeKind::TypeAlias => type_alias::render_type_alias,
            _ => unreachable!(),
          };
          let mut sections = get_section_kinds(ctx, doc_node);

          let (docs, examples) =
            super::jsdoc::render_docs_with_examples(ctx, &doc_node.js_doc);

          if let Some(examples) = examples {
            sections.insert(0, examples);
          }

          content_parts.push(SymbolInnerCtx::Other(SymbolContentCtx {
            id: String::new(),
            sections,
            docs,
          }));
        }

        DocNodeKind::Namespace => {
          let (docs, _examples) =
            super::jsdoc::render_docs_with_examples(ctx, &doc_node.js_doc);
          let ns_parts =
            name.split('.').map(String::from).collect::<Vec<String>>();
          let el = namespace::render_namespace(
            &ctx.with_namespace(ns_parts),
            doc_node,
          );
          let content = format!(
            r#"<div class="space-y-7">{}{el}</div>"#,
            docs.unwrap_or_default()
          );
          content_parts.push(SymbolInnerCtx::Namespace(content));
        }
        DocNodeKind::ModuleDoc => {}
        DocNodeKind::Import => {}
      };
    }

    if !functions.is_empty() {
      content_parts.push(SymbolInnerCtx::Function(function::render_function(
        ctx, functions,
      )));
    }

    content_parts
  }
}
