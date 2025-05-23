use crate::html::types::render_type_def;
use crate::html::usage::UsagesCtx;
use crate::html::util::AnchorCtx;
use crate::html::util::SectionContentCtx;
use crate::html::util::SectionCtx;
use crate::html::util::Tag;
use crate::html::DocNodeKind;
use crate::html::DocNodeWithContext;
use crate::html::RenderContext;
use crate::js_doc::JsDocTag;
use crate::node::DocNodeDef;
use indexmap::IndexMap;
use indexmap::IndexSet;
use serde::Serialize;
use std::borrow::Cow;
use std::collections::HashSet;

pub mod class;
pub mod r#enum;
pub mod function;
pub mod interface;
pub mod namespace;
pub mod type_alias;
pub mod variable;

#[derive(Debug, Serialize, Clone)]
struct SymbolCtx {
  kind: super::util::DocNodeKindCtx,
  usage: Option<UsagesCtx>,
  tags: IndexSet<Tag>,
  subtitle: Option<DocBlockSubtitleCtx>,
  content: Vec<SymbolInnerCtx>,
  deprecated: Option<String>,
  source_href: Option<String>,
}

#[derive(Debug, Serialize, Clone)]
pub struct SymbolGroupCtx {
  pub name: String,
  symbols: Vec<SymbolCtx>,
}

impl SymbolGroupCtx {
  pub const TEMPLATE: &'static str = "symbol_group";

  pub fn new(
    ctx: &RenderContext,
    doc_nodes: &[DocNodeWithContext],
    name: &str,
  ) -> Self {
    let mut split_nodes =
      IndexMap::<DocNodeKind, Vec<DocNodeWithContext>>::default();

    for doc_node in doc_nodes {
      if matches!(doc_node.def, DocNodeDef::Import { .. }) {
        continue;
      }

      split_nodes
        .entry(doc_node.kind)
        .or_insert(vec![])
        .push(doc_node.clone());
    }

    split_nodes.sort_keys();

    let symbols = split_nodes
      .values()
      .map(|doc_nodes| {
        let all_deprecated =
          super::util::all_deprecated(&doc_nodes.iter().collect::<Vec<_>>());

        let mut tags = indexmap::IndexSet::new();

        if doc_nodes.iter().any(|node| {
          node
            .js_doc
            .tags
            .iter()
            .any(|tag| matches!(tag, JsDocTag::Experimental))
        }) {
          tags.insert(Tag::Unstable);
        }

        let mut permissions = doc_nodes
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
          .collect::<indexmap::IndexSet<_>>();

        if !permissions.is_empty() {
          permissions.sort();
          tags.insert(Tag::Permissions(permissions.into_iter().collect()));
        }

        if doc_nodes[0].is_internal(ctx.ctx) {
          tags.insert(Tag::Private);
        }

        let deprecated = if all_deprecated
          && !(matches!(doc_nodes[0].def, DocNodeDef::Function { .. })
            && doc_nodes.len() == 1)
        {
          doc_nodes[0].js_doc.tags.iter().find_map(|tag| {
            if let JsDocTag::Deprecated { doc } = tag {
              Some(
                doc
                  .as_ref()
                  .map(|doc| {
                    crate::html::jsdoc::render_markdown(ctx, doc, true)
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

        let usage = matches!(
          ctx.ctx.file_mode,
          super::FileMode::SingleDts | super::FileMode::Dts
        )
        .then(|| UsagesCtx::new(ctx, doc_nodes))
        .flatten();

        SymbolCtx {
          tags,
          kind: doc_nodes[0].kind.into(),
          subtitle: DocBlockSubtitleCtx::new(ctx, &doc_nodes[0]),
          content: SymbolInnerCtx::new(ctx, doc_nodes, name),
          source_href: ctx
            .ctx
            .href_resolver
            .resolve_source(&doc_nodes[0].location),
          deprecated,
          usage,
        }
      })
      .collect::<Vec<_>>();

    SymbolGroupCtx {
      name: name.to_string(),
      symbols,
    }
  }
}

#[derive(Debug, Serialize, Clone)]
pub struct DocBlockClassSubtitleExtendsCtx {
  href: Option<String>,
  symbol: String,
  type_args: String,
}

#[derive(Debug, Serialize, Clone)]
#[serde(rename_all = "snake_case")]
#[serde(tag = "kind", content = "value")]
pub enum DocBlockSubtitleCtx {
  Class {
    implements: Option<Vec<String>>,
    extends: Option<DocBlockClassSubtitleExtendsCtx>,
  },
  Interface {
    extends: Vec<String>,
  },
}

impl DocBlockSubtitleCtx {
  pub const TEMPLATE_CLASS: &'static str = "doc_block_subtitle_class";
  pub const TEMPLATE_INTERFACE: &'static str = "doc_block_subtitle_interface";

  fn new(ctx: &RenderContext, doc_node: &DocNodeWithContext) -> Option<Self> {
    match &doc_node.def {
      DocNodeDef::Class { class_def } => {
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

        Some(DocBlockSubtitleCtx::Class {
          implements: class_implements,
          extends: class_extends,
        })
      }
      DocNodeDef::Interface { interface_def } => {
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

        Some(DocBlockSubtitleCtx::Interface { extends })
      }
      _ => None,
    }
  }
}

#[derive(Debug, Serialize, Clone, Default)]
pub struct SymbolContentCtx {
  pub id: crate::html::util::Id,
  pub docs: Option<String>,
  pub sections: Vec<SectionCtx>,
}

impl SymbolContentCtx {
  pub const TEMPLATE: &'static str = "symbol_content";
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
      let mut sections = vec![];
      let docs =
        crate::html::jsdoc::jsdoc_body_to_html(ctx, &doc_node.js_doc, false);

      if !matches!(doc_node.def, DocNodeDef::Function { .. }) {
        if let Some(examples) =
          crate::html::jsdoc::jsdoc_examples(ctx, &doc_node.js_doc)
        {
          sections.push(examples);
        }
      }

      sections.extend(match doc_node.def {
        DocNodeDef::Function { .. } => {
          functions.push(doc_node);
          continue;
        }

        DocNodeDef::Variable { .. } => {
          variable::render_variable(ctx, doc_node, name)
        }
        DocNodeDef::Class { .. } => class::render_class(ctx, doc_node, name),
        DocNodeDef::Enum { .. } => r#enum::render_enum(ctx, doc_node),
        DocNodeDef::Interface { .. } => {
          interface::render_interface(ctx, doc_node, name)
        }
        DocNodeDef::TypeAlias { .. } => {
          type_alias::render_type_alias(ctx, doc_node, name)
        }

        DocNodeDef::Namespace { .. } => {
          let namespace_nodes = doc_node.namespace_children.as_ref().unwrap();
          let ns_qualifiers = namespace_nodes
            .first()
            .map(|node| node.ns_qualifiers.clone())
            .unwrap_or_else(|| doc_node.sub_qualifier().into());

          let partitions = super::partition::partition_nodes_by_kind(
            ctx.ctx,
            namespace_nodes.iter().flat_map(|node| {
              if let Some(reference_def) = node.reference_def() {
                Box::new(
                  ctx
                    .ctx
                    .resolve_reference(Some(doc_node), &reference_def.target),
                )
                  as Box<dyn Iterator<Item = Cow<DocNodeWithContext>>>
              } else {
                Box::new(std::iter::once(Cow::Borrowed(node))) as _
              }
            }),
            false,
          );

          let ctx = ctx.with_namespace(ns_qualifiers);

          namespace::render_namespace(partitions.into_iter().map(
            |(title, nodes)| {
              (
                ctx.clone(),
                Some(crate::html::util::SectionHeaderCtx {
                  title: title.clone(),
                  anchor: AnchorCtx {
                    id: crate::html::util::Id::new(title),
                  },
                  href: None,
                  doc: None,
                }),
                nodes,
              )
            },
          ))
        }
        DocNodeDef::ModuleDoc { .. }
        | DocNodeDef::Import { .. }
        | DocNodeDef::Reference { .. } => unreachable!(),
      });

      let references = doc_node
        .js_doc
        .tags
        .iter()
        .filter_map(|tag| {
          if let JsDocTag::See { doc } = tag {
            Some(generate_see(ctx, doc))
          } else {
            None
          }
        })
        .collect::<Vec<_>>();

      if !references.is_empty() {
        sections.push(SectionCtx::new(
          ctx,
          "See",
          SectionContentCtx::See(references),
        ));
      }

      content_parts.push(SymbolInnerCtx::Other(SymbolContentCtx {
        id: crate::html::util::Id::empty(),
        sections,
        docs,
      }));
    }

    if !functions.is_empty() {
      content_parts.push(SymbolInnerCtx::Function(function::FunctionCtx::new(
        ctx, functions,
      )));
    }

    content_parts
  }
}

fn generate_see(ctx: &RenderContext, doc: &str) -> String {
  let doc = if let Some(href) = ctx.lookup_symbol_href(doc) {
    format!("[{doc}]({href})")
  } else {
    doc.to_string()
  };

  crate::html::jsdoc::render_markdown(ctx, &doc, true)
}
