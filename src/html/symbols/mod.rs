use crate::html::types::render_type_def;
use crate::html::usage::UsagesCtx;
use crate::html::util::SectionCtx;
use crate::html::util::Tag;
use crate::html::DocNodeKindWithDrilldown;
use crate::html::DocNodeWithContext;
use crate::html::RenderContext;
use crate::js_doc::JsDocTag;
use crate::DocNodeKind;
use indexmap::IndexMap;
use serde::Serialize;
use std::collections::HashSet;

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
  tags: HashSet<Tag>,
  subtitle: Option<DocBlockSubtitleCtx>,
  content: Vec<SymbolInnerCtx>,
  deprecated: Option<String>,
  source_href: Option<String>,
}

#[derive(Debug, Serialize, Clone)]
pub struct SymbolGroupCtx {
  pub name: String,
  symbols: Vec<SymbolCtx>,
  usages: Option<UsagesCtx>,
}

impl SymbolGroupCtx {
  pub fn new(
    ctx: &RenderContext,
    doc_nodes: &[DocNodeWithContext],
    name: &str,
  ) -> Self {
    let mut split_nodes =
      IndexMap::<DocNodeKindWithDrilldown, Vec<DocNodeWithContext>>::default();

    for doc_node in doc_nodes {
      if doc_node.kind == DocNodeKind::Import {
        continue;
      }

      split_nodes
        .entry(doc_node.kind_with_drilldown)
        .or_insert(vec![])
        .push(doc_node.clone());
    }

    split_nodes.sort_keys();

    let symbols = split_nodes
      .values()
      .map(|doc_nodes| {
        let all_deprecated =
          super::util::all_deprecated(&doc_nodes.iter().collect::<Vec<_>>());

        let mut tags = HashSet::new();

        let permissions = doc_nodes
          .iter()
          .flat_map(|node| {
            node
              .js_doc
              .tags
              .iter()
              .filter_map(|tag| {
                if let JsDocTag::Tags { tags } = tag {
                  Some(tags.iter().filter_map(|tag| {
                    if tag.starts_with("allow-") {
                      Some(tag.to_owned())
                    } else {
                      None
                    }
                  }))
                } else {
                  None
                }
              })
              .flatten()
          })
          .collect::<Vec<_>>();

        if !permissions.is_empty() {
          tags.insert(Tag::Permissions(permissions));
        }

        let deprecated = if all_deprecated
          && !(doc_nodes[0].kind == DocNodeKind::Function
            && doc_nodes.len() == 1)
        {
          doc_nodes[0].js_doc.tags.iter().find_map(|tag| {
            if let JsDocTag::Deprecated { doc } = tag {
              Some(
                doc
                  .as_ref()
                  .map(|doc| {
                    crate::html::jsdoc::render_markdown_summary(ctx, doc)
                  })
                  .unwrap_or_default(),
              )
            } else {
              None
            }
          })
        } else {
          None
        };

        SymbolCtx {
          tags,
          kind: doc_nodes[0].kind_with_drilldown.into(),
          subtitle: DocBlockSubtitleCtx::new(ctx, &doc_nodes[0]),
          content: SymbolInnerCtx::new(ctx, doc_nodes, name),
          source_href: ctx
            .ctx
            .href_resolver
            .resolve_source(&doc_nodes[0].location),
          deprecated,
        }
      })
      .collect::<Vec<_>>();

    SymbolGroupCtx {
      name: name.to_string(),
      symbols,
      usages: UsagesCtx::new(ctx, doc_nodes),
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
  fn new(ctx: &RenderContext, doc_node: &DocNodeWithContext) -> Option<Self> {
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
        .map(|def| def.name.as_str())
        .collect::<HashSet<&str>>();

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
          symbol: html_escape::encode_text(extends).into_owned(),
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
        .map(|def| def.name.as_str())
        .collect::<HashSet<&str>>();
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

#[derive(Debug, Serialize, Clone, Default)]
pub struct SymbolContentCtx {
  pub id: String,
  pub docs: Option<String>,
  pub sections: Vec<SectionCtx>,
}

#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "snake_case", tag = "kind", content = "value")]
pub enum SymbolInnerCtx {
  Function(function::FunctionCtx),
  Other(SymbolContentCtx),
}

impl SymbolInnerCtx {
  fn new(
    ctx: &RenderContext,
    doc_nodes: &[DocNodeWithContext],
    name: &str,
  ) -> Vec<Self> {
    let mut content_parts = Vec::with_capacity(doc_nodes.len());
    let mut functions = vec![];

    for doc_node in doc_nodes {
      let mut sections = match doc_node.kind {
        DocNodeKind::Function => {
          functions.push(doc_node);
          continue;
        }

        DocNodeKind::Variable => variable::render_variable(ctx, doc_node),
        DocNodeKind::Class => class::render_class(ctx, doc_node),
        DocNodeKind::Enum => r#enum::render_enum(ctx, doc_node),
        DocNodeKind::Interface => interface::render_interface(ctx, doc_node),
        DocNodeKind::TypeAlias => type_alias::render_type_alias(ctx, doc_node),

        DocNodeKind::Namespace => {
          let namespace_def = doc_node.namespace_def.as_ref().unwrap();
          let namespace_nodes = namespace_def
            .elements
            .iter()
            .map(|element| doc_node.create_child(element.clone()))
            .collect::<Vec<_>>();

          let partitions =
            super::partition::partition_nodes_by_kind(&namespace_nodes, false);

          let ns_parts = name.split('.').collect::<Vec<&str>>();

          namespace::render_namespace(&ctx.with_namespace(ns_parts), partitions)
        }
        DocNodeKind::ModuleDoc | DocNodeKind::Import => unreachable!(),
      };

      let docs =
        crate::html::jsdoc::jsdoc_body_to_html(ctx, &doc_node.js_doc, false);
      let examples = crate::html::jsdoc::jsdoc_examples(ctx, &doc_node.js_doc);

      if let Some(examples) = examples {
        sections.insert(0, examples);
      }

      content_parts.push(SymbolInnerCtx::Other(SymbolContentCtx {
        id: String::new(),
        sections,
        docs,
      }));
    }

    if !functions.is_empty() {
      content_parts.push(SymbolInnerCtx::Function(function::render_function(
        ctx, functions,
      )));
    }

    content_parts
  }
}
