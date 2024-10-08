use crate::html::render_context::RenderContext;
use crate::html::util::*;
use crate::html::DocNodeWithContext;
use crate::node::DocNodeDef;
use deno_ast::swc::ast::MethodKind;
use indexmap::IndexMap;
use indexmap::IndexSet;
use serde::Serialize;
use std::cmp::Ordering;

pub fn render_namespace(
  ctx: &RenderContext,
  partitions: impl Iterator<Item = (SectionHeaderCtx, Vec<DocNodeWithContext>)>,
) -> Vec<SectionCtx> {
  partitions
    .map(|(header, doc_nodes)| {
      get_namespace_section_render_ctx(ctx, header, doc_nodes)
    })
    .collect()
}

fn get_namespace_section_render_ctx(
  ctx: &RenderContext,
  header: SectionHeaderCtx,
  doc_nodes: Vec<DocNodeWithContext>,
) -> SectionCtx {
  let mut grouped_nodes = IndexMap::new();

  for node in doc_nodes {
    let entry = grouped_nodes
      .entry(node.get_qualified_name())
      .or_insert(vec![]);
    entry.push(node);
  }

  let nodes = grouped_nodes
    .into_iter()
    .filter_map(|(name, nodes)| {
      if nodes[0].is_internal() {
        None
      } else {
        Some(NamespaceNodeCtx::new(ctx, name, nodes))
      }
    })
    .collect::<Vec<_>>();

  let mut section = SectionCtx::new(
    ctx,
    &header.title,
    SectionContentCtx::NamespaceSection(nodes),
  );
  section.header = header;
  section
}

#[derive(Debug, Serialize, Clone)]
pub struct NamespaceNodeSubItemCtx {
  title: String,
  href: String,
}

impl std::hash::Hash for NamespaceNodeSubItemCtx {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.title.hash(state)
  }
}

impl PartialOrd for NamespaceNodeSubItemCtx {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.title.cmp(&other.title))
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

#[derive(Debug, Serialize, Clone)]
pub struct NamespaceNodeCtx {
  pub id: String,
  pub anchor: AnchorCtx,
  pub tags: IndexSet<Tag>,
  pub doc_node_kind_ctx: IndexSet<DocNodeKindCtx>,
  pub href: String,
  pub name: String,
  pub docs: Option<String>,
  pub deprecated: bool,
  pub subitems: IndexSet<NamespaceNodeSubItemCtx>,
}

impl NamespaceNodeCtx {
  fn new(
    ctx: &RenderContext,
    name: String,
    nodes: Vec<DocNodeWithContext>,
  ) -> Self {
    let id = name_to_id("namespace", &name);

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
      match &node.def {
        DocNodeDef::Class { class_def } => {
          subitems.extend(
            class_def
              .methods
              .iter()
              .filter(|method| !method.name.starts_with('['))
              .map(|method| NamespaceNodeSubItemCtx {
                title: method.name.to_string(),
                href: format!(
                  "{href}#{}",
                  if matches!(
                    method.kind,
                    MethodKind::Getter | MethodKind::Setter
                  ) {
                    name_to_id("accessor", &method.name.to_lowercase())
                  } else {
                    name_to_id(
                      "method",
                      &format!("{}_0", method.name.to_lowercase()),
                    )
                  }
                ),
              }),
          );
          subitems.extend(
            class_def
              .properties
              .iter()
              .filter(|property| !property.name.starts_with('['))
              .map(|property| NamespaceNodeSubItemCtx {
                title: property.name.to_string(),
                href: format!(
                  "{href}#{}",
                  name_to_id("property", &property.name.to_lowercase())
                ),
              }),
          );
        }
        DocNodeDef::Interface { interface_def } => {
          subitems.extend(
            interface_def
              .methods
              .iter()
              .filter(|method| !method.name.starts_with('['))
              .map(|method| NamespaceNodeSubItemCtx {
                title: method.name.to_string(),
                href: format!(
                  "{href}#{}",
                  if matches!(
                    method.kind,
                    MethodKind::Getter | MethodKind::Setter
                  ) {
                    name_to_id("accessor", &method.name.to_lowercase())
                  } else {
                    name_to_id(
                      "method",
                      &format!("{}_0", method.name.to_lowercase()),
                    )
                  }
                ),
              }),
          );
          subitems.extend(
            interface_def
              .properties
              .iter()
              .filter(|property| !property.name.starts_with('['))
              .map(|property| NamespaceNodeSubItemCtx {
                title: property.name.to_string(),
                href: format!(
                  "{href}#{}",
                  name_to_id("property", &property.name.to_lowercase())
                ),
              }),
          );
        }
        DocNodeDef::Variable { variable_def } => {
          if let Some(type_literal) = variable_def
            .ts_type
            .as_ref()
            .and_then(|ts_type| ts_type.type_literal.as_ref())
          {
            subitems.extend(
              type_literal
                .methods
                .iter()
                .filter(|method| !method.name.starts_with('['))
                .map(|method| NamespaceNodeSubItemCtx {
                  title: method.name.to_string(),
                  href: format!(
                    "{href}#{}",
                    if matches!(
                      method.kind,
                      MethodKind::Getter | MethodKind::Setter
                    ) {
                      name_to_id("accessor", &method.name.to_lowercase())
                    } else {
                      name_to_id(
                        "method",
                        &format!("{}_0", method.name.to_lowercase()),
                      )
                    }
                  ),
                }),
            );

            subitems.extend(
              type_literal
                .properties
                .iter()
                .filter(|property| !property.name.starts_with('['))
                .map(|property| NamespaceNodeSubItemCtx {
                  title: property.name.to_string(),
                  href: format!(
                    "{href}#{}",
                    name_to_id("property", &property.name.to_lowercase())
                  ),
                }),
            );
          }
        }
        DocNodeDef::TypeAlias { type_alias_def } => {
          if let Some(type_literal) =
            type_alias_def.ts_type.type_literal.as_ref()
          {
            subitems.extend(
              type_literal
                .methods
                .iter()
                .filter(|method| !method.name.starts_with('['))
                .map(|method| NamespaceNodeSubItemCtx {
                  title: method.name.to_string(),
                  href: format!(
                    "{href}#{}",
                    if matches!(
                      method.kind,
                      MethodKind::Getter | MethodKind::Setter
                    ) {
                      name_to_id("accessor", &method.name.to_lowercase())
                    } else {
                      name_to_id(
                        "method",
                        &format!("{}_0", method.name.to_lowercase()),
                      )
                    }
                  ),
                }),
            );

            subitems.extend(
              type_literal
                .properties
                .iter()
                .filter(|property| !property.name.starts_with('['))
                .map(|property| NamespaceNodeSubItemCtx {
                  title: property.name.to_string(),
                  href: format!(
                    "{href}#{}",
                    name_to_id("property", &property.name.to_lowercase())
                  ),
                }),
            );
          }
        }
        DocNodeDef::Function { .. } => {}
        DocNodeDef::Enum { .. } => {}
        DocNodeDef::Namespace { .. } => {}
        DocNodeDef::Import { .. } => {}
        DocNodeDef::ModuleDoc => {}
      };
    }

    subitems.sort();

    NamespaceNodeCtx {
      id: id.clone(),
      anchor: AnchorCtx { id },
      tags,
      doc_node_kind_ctx: nodes
        .iter()
        .map(|node| node.kind_with_drilldown.into())
        .collect(),
      href,
      name,
      docs,
      deprecated: all_deprecated(&nodes.iter().collect::<Vec<_>>()),
      subitems,
    }
  }
}
