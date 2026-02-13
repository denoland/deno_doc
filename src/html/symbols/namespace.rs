use crate::html::DiffStatus;
use crate::html::DocNodeKind;
use crate::html::DocNodeWithContext;
use crate::html::MethodKind;
use crate::html::parameters::render_params;
use crate::html::render_context::RenderContext;
use crate::html::symbols::function::render_function_summary;
use crate::html::types::{render_type_def, type_params_summary};
use crate::html::util::*;
use indexmap::IndexMap;
use indexmap::IndexSet;
use serde::Deserialize;
use serde::Serialize;
use std::cmp::Ordering;

pub fn render_namespace<'a>(
  partitions: impl Iterator<
    Item = (
      RenderContext<'a>,
      Option<SectionHeaderCtx>,
      Vec<DocNodeWithContext>,
    ),
  >,
) -> Vec<SectionCtx> {
  partitions
    .map(|(ctx, header, doc_nodes)| {
      get_namespace_section_render_ctx(&ctx, header, doc_nodes)
    })
    .collect()
}

fn get_namespace_section_render_ctx(
  ctx: &RenderContext,
  header: Option<SectionHeaderCtx>,
  doc_nodes: Vec<DocNodeWithContext>,
) -> SectionCtx {
  let mut grouped_nodes = IndexMap::new();

  for node in doc_nodes {
    let entry = grouped_nodes
      .entry(node.get_qualified_name().to_string())
      .or_insert(vec![]);
    entry.push(node);
  }

  let nodes = grouped_nodes
    .into_iter()
    .filter_map(|(name, nodes)| {
      if nodes[0].is_internal(ctx.ctx) {
        None
      } else {
        Some(NamespaceNodeCtx::new(ctx, name, nodes))
      }
    })
    .collect::<Vec<_>>();

  let mut section = SectionCtx::new(
    ctx,
    header
      .as_ref()
      .map(|header| header.title.as_str())
      .unwrap_or_default(),
    SectionContentCtx::NamespaceSection(nodes),
  );
  section.header = header;
  section
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct NamespaceNodeSubItemCtx {
  title: String,
  docs: Option<String>,
  ty: Option<TypeSummaryCtx>,
  href: String,
}

impl std::hash::Hash for NamespaceNodeSubItemCtx {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.title.hash(state)
  }
}

impl PartialOrd for NamespaceNodeSubItemCtx {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for NamespaceNodeSubItemCtx {
  fn cmp(&self, other: &Self) -> Ordering {
    self.title.cmp(&other.title)
  }
}

impl PartialEq for NamespaceNodeSubItemCtx {
  fn eq(&self, other: &Self) -> bool {
    self.title.eq(&other.title)
  }
}

impl Eq for NamespaceNodeSubItemCtx {}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct NamespaceNodeCtx {
  pub anchor: AnchorCtx,
  pub tags: IndexSet<Tag>,
  pub doc_node_kind_ctx: IndexSet<DocNodeKindCtx>,
  pub href: String,
  pub name: String,
  pub ty: Option<TypeSummaryCtx>,
  pub docs: Option<String>,
  pub deprecated: bool,
  pub subitems: IndexSet<NamespaceNodeSubItemCtx>,
  #[serde(skip_serializing_if = "Option::is_none")]
  pub diff_status: Option<DiffStatus>,
}

impl NamespaceNodeCtx {
  fn new(
    ctx: &RenderContext,
    name: String,
    nodes: Vec<DocNodeWithContext>,
  ) -> Self {
    let id = IdBuilder::new(ctx)
      .kind(IdKind::Namespace)
      .name(&name)
      .build();

    let docs =
      crate::html::jsdoc::jsdoc_body_to_html(ctx, &nodes[0].js_doc, true);

    let tags = Tag::from_js_doc(&nodes[0].js_doc);

    let mut subitems = IndexSet::new();

    let href = ctx.ctx.resolve_path(
      ctx.get_current_resolve(),
      UrlResolveKind::Symbol {
        file: &nodes[0].origin,
        symbol: &name,
      },
    );

    for node in &nodes {
      if let Some(drilldown_symbols) = node.get_drilldown_symbols() {
        subitems.extend(
          drilldown_symbols
            .filter(|symbol| {
              !symbol.drilldown_name.as_ref().unwrap().starts_with('[')
            })
            .map(|symbol| {
              let target_id = match symbol.kind {
                DocNodeKind::Property => IdBuilder::new(ctx)
                  .kind(IdKind::Property)
                  .name(&symbol.drilldown_name.as_ref().unwrap().to_lowercase())
                  .build_unregistered(),
                DocNodeKind::Method(kind) => {
                  if matches!(kind, MethodKind::Getter | MethodKind::Setter) {
                    IdBuilder::new(ctx)
                      .kind(IdKind::Accessor)
                      .name(
                        &symbol.drilldown_name.as_ref().unwrap().to_lowercase(),
                      )
                      .build_unregistered()
                  } else {
                    IdBuilder::new(ctx)
                      .kind(IdKind::Method)
                      .name(
                        &symbol.drilldown_name.as_ref().unwrap().to_lowercase(),
                      )
                      .index(0)
                      .build_unregistered()
                  }
                }
                _ => unreachable!(),
              };

              let docs = crate::html::jsdoc::jsdoc_body_to_html(
                ctx,
                &symbol.js_doc,
                true,
              );

              NamespaceNodeSubItemCtx {
                title: symbol.drilldown_name.as_ref().unwrap().to_string(),
                docs,
                ty: summary_for_nodes(ctx, &[symbol]),
                href: format!("{href}#{}", target_id.as_str()),
              }
            }),
        );
      }
    }

    subitems.sort();

    let diff_status = nodes[0].diff_status.clone();

    NamespaceNodeCtx {
      anchor: AnchorCtx::new(id),
      tags,
      doc_node_kind_ctx: nodes.iter().map(|node| node.kind.into()).collect(),
      href,
      name,
      ty: summary_for_nodes(ctx, &nodes),
      docs,
      deprecated: all_deprecated(&nodes.iter().collect::<Vec<_>>()),
      subitems,
      diff_status,
    }
  }
}

fn summary_for_nodes(
  ctx: &RenderContext,
  nodes: &[DocNodeWithContext],
) -> Option<TypeSummaryCtx> {
  match nodes[0].kind {
    DocNodeKind::Method(_) | DocNodeKind::Function => {
      let overloads = nodes
        .iter()
        .filter(|node| {
          matches!(node.kind, DocNodeKind::Method(_) | DocNodeKind::Function)
        })
        .map(|node| node.function_def().unwrap())
        .collect::<Vec<_>>();

      let (def, info) = if overloads.len() > 1 {
        (
          overloads
            .iter()
            .find(|overload| overload.has_body)
            .cloned()
            .unwrap_or(overloads[0]),
          Some(format!("{} overloads", overloads.len() - 1)),
        )
      } else {
        (overloads[0], None)
      };

      Some(TypeSummaryCtx {
        ty: render_function_summary(def, &ctx.with_disable_links(true)),
        info,
      })
    }
    DocNodeKind::Class => {
      let def = nodes[0].class_def().unwrap();
      if !def.constructors.is_empty() {
        let ctx = ctx.with_disable_links(true);

        let (constructor, info) = if def.constructors.len() > 1 {
          (
            def
              .constructors
              .iter()
              .find(|overload| overload.has_body)
              .unwrap_or(&def.constructors[0]),
            Some(format!("{} constructors", def.constructors.len() - 1)),
          )
        } else {
          (&def.constructors[0], None)
        };

        let params = constructor
          .params
          .iter()
          .map(|param| param.param.clone())
          .collect::<Vec<_>>();

        Some(TypeSummaryCtx {
          ty: format!(
            "{}({})",
            type_params_summary(&ctx, &def.type_params),
            render_params(&ctx, &params)
          ),
          info,
        })
      } else {
        None
      }
    }
    DocNodeKind::Enum => None,
    DocNodeKind::Interface => None,
    DocNodeKind::TypeAlias => {
      let ctx = ctx.with_disable_links(true);
      let def = nodes[0].type_alias_def().unwrap();

      Some(TypeSummaryCtx {
        ty: format!(
          "{} = {}",
          crate::html::types::type_params_summary(&ctx, &def.type_params),
          render_type_def(&ctx, &def.ts_type)
        ),
        info: None,
      })
    }
    DocNodeKind::Property | DocNodeKind::Variable => nodes[0]
      .variable_def()
      .unwrap()
      .ts_type
      .as_ref()
      .map(|ts_type| TypeSummaryCtx {
        ty: format!(
          ": {}",
          render_type_def(&ctx.with_disable_links(true), ts_type)
        ),
        info: None,
      }),
    DocNodeKind::Reference
    | DocNodeKind::Namespace
    | DocNodeKind::Import
    | DocNodeKind::ModuleDoc => None,
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct TypeSummaryCtx {
  pub ty: String,
  pub info: Option<String>,
}
