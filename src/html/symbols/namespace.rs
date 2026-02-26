use crate::diff::DocNodeDefDiff;
use crate::html::DiffStatus;
use crate::html::DocNodeKind;
use crate::html::DocNodeWithContext;
use crate::html::MethodKind;
use crate::html::parameters::render_params;
use crate::html::render_context::RenderContext;
use crate::html::symbols::function::render_function_summary;
use crate::html::types::render_type_def;
use crate::html::types::render_type_def_colon;
use crate::html::types::type_params_summary;
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
  #[serde(skip_serializing_if = "Option::is_none")]
  pub diff_status: Option<DiffStatus>,
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
  pub tags: Vec<TagCtx>,
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
      // Look up parent node diff for subitem diff status
      let node_def_diff = ctx.ctx.diff.as_ref().and_then(|diff_index| {
        let info = diff_index.get_node_diff(
          &node.origin.specifier,
          node.get_name(),
          node.def.to_kind(),
        )?;
        info.diff.as_ref()?.def_changes.as_ref()
      });

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

              let drilldown_name = symbol.drilldown_name.as_ref().unwrap();

              // If the parent node is removed, subitems inherit that
              // status so they survive diff_only filtering. The marker
              // is stripped later when the entire symbol is removed.
              let diff_status =
                if matches!(node.diff_status, Some(DiffStatus::Removed)) {
                  Some(DiffStatus::Removed)
                } else {
                  node_def_diff.and_then(|diff| {
                    get_subitem_diff_status(diff, drilldown_name, &symbol.kind)
                  })
                };

              // If there's a doc change, render with inline diff annotations
              let docs = node_def_diff
                .and_then(|diff| {
                  render_subitem_docs_with_diff(
                    ctx,
                    diff,
                    drilldown_name,
                    &symbol.kind,
                    &symbol.js_doc,
                  )
                })
                .or_else(|| {
                  crate::html::jsdoc::jsdoc_body_to_html(
                    ctx,
                    &symbol.js_doc,
                    true,
                  )
                });

              NamespaceNodeSubItemCtx {
                title: drilldown_name.to_string(),
                docs,
                ty: summary_for_nodes(ctx, &[symbol]),
                href: format!("{href}#{}", target_id.as_str()),
                diff_status,
              }
            }),
        );
      }

      // Inject removed sub-items from the diff
      if let Some(node_def_diff) = node_def_diff {
        inject_removed_subitems(ctx, node_def_diff, &href, &mut subitems);
      }
    }

    subitems.sort();

    let diff_status =
      super::compute_combined_diff_status(&nodes).and_then(|status| {
        // If the symbol is Modified and the only changes are in subitems
        // (methods/properties), don't mark the symbol itself as changed.
        // Skip this check if any node is Added/Removed (kind change scenario).
        if matches!(status, DiffStatus::Modified)
          && !nodes.iter().any(|n| {
            matches!(
              n.diff_status,
              Some(DiffStatus::Added) | Some(DiffStatus::Removed)
            )
          })
        {
          let only_subitem_changes = nodes.iter().all(|node| {
            ctx
              .ctx
              .diff
              .as_ref()
              .and_then(|diff_index| {
                let info = diff_index.get_node_diff(
                  &node.origin.specifier,
                  node.get_name(),
                  node.def.to_kind(),
                )?;
                let diff = info.diff.as_ref()?;

                // If there are non-def changes, it's not subitem-only
                if diff.name_change.is_some()
                  || diff.js_doc_changes.is_some()
                  || diff.declaration_kind_change.is_some()
                {
                  return Some(false);
                }

                Some(match diff.def_changes.as_ref()? {
                  DocNodeDefDiff::Class(class_diff) => {
                    class_diff.is_abstract_change.is_none()
                      && class_diff.extends_change.is_none()
                      && class_diff.implements_change.is_none()
                      && class_diff.type_params_change.is_none()
                      && class_diff.super_type_params_change.is_none()
                      && class_diff.constructor_changes.is_none()
                      && class_diff.index_signature_changes.is_none()
                      && class_diff.decorators_change.is_none()
                  }
                  DocNodeDefDiff::Interface(iface_diff) => {
                    iface_diff.extends_change.is_none()
                      && iface_diff.type_params_change.is_none()
                      && iface_diff.constructor_changes.is_none()
                      && iface_diff.call_signature_changes.is_none()
                      && iface_diff.index_signature_changes.is_none()
                  }
                  _ => false,
                })
              })
              .unwrap_or(true)
          });
          if only_subitem_changes {
            return None;
          }
        }
        Some(status)
      });

    let old_tags = if matches!(diff_status, Some(DiffStatus::Modified)) {
      let mut old_tags = tags.clone();
      if let Some(tags_diff) = ctx.ctx.diff.as_ref().and_then(|diff_index| {
        let info = diff_index.get_node_diff(
          &nodes[0].origin.specifier,
          nodes[0].get_name(),
          nodes[0].def.to_kind(),
        )?;
        info
          .diff
          .as_ref()?
          .js_doc_changes
          .as_ref()?
          .tags_change
          .as_ref()
      }) {
        for added in &tags_diff.added {
          if matches!(added, crate::js_doc::JsDocTag::Deprecated { .. }) {
            old_tags.swap_remove(&Tag::Deprecated);
          }
        }
        for removed in &tags_diff.removed {
          if matches!(removed, crate::js_doc::JsDocTag::Deprecated { .. }) {
            old_tags.insert(Tag::Deprecated);
          }
        }
      }
      Some(old_tags)
    } else {
      None
    };

    // When the entire symbol is removed, strip the inherited removed
    // markers from subitems — the parent's status is sufficient.
    if matches!(diff_status, Some(DiffStatus::Removed)) {
      subitems = subitems
        .into_iter()
        .map(|mut item| {
          item.diff_status = None;
          item
        })
        .collect();
    }

    NamespaceNodeCtx {
      anchor: AnchorCtx::new(id),
      tags: compute_tag_ctx(tags, old_tags),
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
  // Use only non-removed nodes for the summary so we always show the current
  // (new) state — both for normal modifications and kind changes.
  let current = nodes
    .iter()
    .filter(|n| !matches!(n.diff_status, Some(DiffStatus::Removed)))
    .cloned()
    .collect::<Vec<_>>();
  let nodes = if current.is_empty() { nodes } else { &current };

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
        ty: render_function_summary(
          &ctx.with_disable_links(true),
          &def.type_params,
          &def.params,
          &def.return_type,
        ),
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

fn get_subitem_diff_status(
  def_diff: &DocNodeDefDiff,
  name: &str,
  kind: &DocNodeKind,
) -> Option<DiffStatus> {
  match def_diff {
    DocNodeDefDiff::Class(class_diff) => match kind {
      DocNodeKind::Method(_) => {
        let mc = class_diff.method_changes.as_ref()?;
        if mc.added.iter().any(|m| &*m.name == name) {
          return Some(DiffStatus::Added);
        }
        if mc.removed.iter().any(|m| &*m.name == name) {
          return Some(DiffStatus::Removed);
        }
        if let Some(md) = mc.modified.iter().find(|m| &*m.name == name) {
          if let Some(nc) = &md.name_change {
            return Some(DiffStatus::Renamed {
              old_name: nc.old.to_string(),
            });
          }
          return Some(DiffStatus::Modified);
        }
        None
      }
      DocNodeKind::Property => {
        let pc = class_diff.property_changes.as_ref()?;
        if pc.added.iter().any(|p| &*p.name == name) {
          return Some(DiffStatus::Added);
        }
        if pc.removed.iter().any(|p| &*p.name == name) {
          return Some(DiffStatus::Removed);
        }
        if let Some(pd) = pc.modified.iter().find(|p| &*p.name == name) {
          if let Some(nc) = &pd.name_change {
            return Some(DiffStatus::Renamed {
              old_name: nc.old.to_string(),
            });
          }
          return Some(DiffStatus::Modified);
        }
        None
      }
      _ => None,
    },
    DocNodeDefDiff::Interface(iface_diff) => match kind {
      DocNodeKind::Method(_) => {
        let mc = iface_diff.method_changes.as_ref()?;
        if mc.added.iter().any(|m| m.name == name) {
          return Some(DiffStatus::Added);
        }
        if mc.removed.iter().any(|m| m.name == name) {
          return Some(DiffStatus::Removed);
        }
        if let Some(md) = mc.modified.iter().find(|m| m.name == name) {
          if let Some(nc) = &md.name_change {
            return Some(DiffStatus::Renamed {
              old_name: nc.old.clone(),
            });
          }
          return Some(DiffStatus::Modified);
        }
        None
      }
      DocNodeKind::Property => {
        let pc = iface_diff.property_changes.as_ref()?;
        if pc.added.iter().any(|p| p.name == name) {
          return Some(DiffStatus::Added);
        }
        if pc.removed.iter().any(|p| p.name == name) {
          return Some(DiffStatus::Removed);
        }
        if let Some(pd) = pc.modified.iter().find(|p| p.name == name) {
          if let Some(nc) = &pd.name_change {
            return Some(DiffStatus::Renamed {
              old_name: nc.old.clone(),
            });
          }
          return Some(DiffStatus::Modified);
        }
        None
      }
      _ => None,
    },
    _ => None,
  }
}

/// If the subitem has a doc text change, render docs with inline diff
/// annotations. Returns `None` if there's no doc change (caller falls back
/// to normal rendering).
fn render_subitem_docs_with_diff(
  ctx: &RenderContext,
  def_diff: &DocNodeDefDiff,
  name: &str,
  kind: &DocNodeKind,
  js_doc: &crate::js_doc::JsDoc,
) -> Option<String> {
  let js_doc_diff = match def_diff {
    DocNodeDefDiff::Class(class_diff) => match kind {
      DocNodeKind::Method(_) => {
        let mc = class_diff.method_changes.as_ref()?;
        mc.modified
          .iter()
          .find(|m| &*m.name == name)?
          .js_doc_change
          .as_ref()
      }
      DocNodeKind::Property => {
        let pc = class_diff.property_changes.as_ref()?;
        pc.modified
          .iter()
          .find(|p| &*p.name == name)?
          .js_doc_change
          .as_ref()
      }
      _ => None,
    },
    DocNodeDefDiff::Interface(iface_diff) => match kind {
      DocNodeKind::Method(_) => {
        let mc = iface_diff.method_changes.as_ref()?;
        mc.modified
          .iter()
          .find(|m| m.name == name)?
          .js_doc_change
          .as_ref()
      }
      DocNodeKind::Property => {
        let pc = iface_diff.property_changes.as_ref()?;
        pc.modified
          .iter()
          .find(|p| p.name == name)?
          .js_doc_change
          .as_ref()
      }
      _ => None,
    },
    _ => None,
  }?;

  let doc_change = js_doc_diff.doc_change.as_ref()?;
  let old_doc = doc_change.old.as_deref().unwrap_or_default();
  let new_doc = js_doc.doc.as_deref().unwrap_or_default();
  crate::html::jsdoc::render_docs_with_diff(ctx, old_doc, new_doc)
}

/// Inject removed methods and properties from the diff as sub-items.
/// These don't appear in `get_drilldown_symbols` since they only exist in the
/// old version.
fn inject_removed_subitems(
  ctx: &RenderContext,
  def_diff: &DocNodeDefDiff,
  href: &str,
  subitems: &mut IndexSet<NamespaceNodeSubItemCtx>,
) {
  let no_links_ctx = ctx.with_disable_links(true);

  match def_diff {
    DocNodeDefDiff::Class(class_diff) => {
      if let Some(mc) = &class_diff.method_changes {
        for method in &mc.removed {
          if matches!(
            method.accessibility,
            Some(deno_ast::swc::ast::Accessibility::Private)
          ) {
            continue;
          }
          let target_id = if matches!(
            method.kind,
            deno_ast::swc::ast::MethodKind::Getter
              | deno_ast::swc::ast::MethodKind::Setter
          ) {
            IdBuilder::new(ctx)
              .kind(IdKind::Accessor)
              .name(&method.name.to_lowercase())
              .build_unregistered()
          } else {
            IdBuilder::new(ctx)
              .kind(IdKind::Method)
              .name(&method.name.to_lowercase())
              .index(0)
              .build_unregistered()
          };

          subitems.insert(NamespaceNodeSubItemCtx {
            title: method.name.to_string(),
            docs: crate::html::jsdoc::jsdoc_body_to_html(
              ctx,
              &method.js_doc,
              true,
            ),
            ty: Some(TypeSummaryCtx {
              ty: render_function_summary(
                &no_links_ctx,
                &method.function_def.type_params,
                &method.function_def.params,
                &method.function_def.return_type,
              ),
              info: None,
            }),
            href: format!("{href}#{}", target_id.as_str()),
            diff_status: Some(DiffStatus::Removed),
          });
        }
      }
      if let Some(pc) = &class_diff.property_changes {
        for prop in &pc.removed {
          if matches!(
            prop.accessibility,
            Some(deno_ast::swc::ast::Accessibility::Private)
          ) {
            continue;
          }
          let target_id = IdBuilder::new(ctx)
            .kind(IdKind::Property)
            .name(&prop.name.to_lowercase())
            .build_unregistered();

          let ty = prop.ts_type.as_ref().map(|ts_type| TypeSummaryCtx {
            ty: render_type_def_colon(&no_links_ctx, ts_type),
            info: None,
          });

          subitems.insert(NamespaceNodeSubItemCtx {
            title: prop.name.to_string(),
            docs: crate::html::jsdoc::jsdoc_body_to_html(
              ctx,
              &prop.js_doc,
              true,
            ),
            ty,
            href: format!("{href}#{}", target_id.as_str()),
            diff_status: Some(DiffStatus::Removed),
          });
        }
      }
    }
    DocNodeDefDiff::Interface(iface_diff) => {
      if let Some(mc) = &iface_diff.method_changes {
        for method in &mc.removed {
          let target_id = if matches!(
            method.kind,
            deno_ast::swc::ast::MethodKind::Getter
              | deno_ast::swc::ast::MethodKind::Setter
          ) {
            IdBuilder::new(ctx)
              .kind(IdKind::Accessor)
              .name(&method.name.to_lowercase())
              .build_unregistered()
          } else {
            IdBuilder::new(ctx)
              .kind(IdKind::Method)
              .name(&method.name.to_lowercase())
              .index(0)
              .build_unregistered()
          };

          let return_type = method
            .return_type
            .as_ref()
            .map(|ts_type| render_type_def_colon(&no_links_ctx, ts_type))
            .unwrap_or_default();

          subitems.insert(NamespaceNodeSubItemCtx {
            title: method.name.to_string(),
            docs: crate::html::jsdoc::jsdoc_body_to_html(
              ctx,
              &method.js_doc,
              true,
            ),
            ty: Some(TypeSummaryCtx {
              ty: format!(
                "{}({}){return_type}",
                type_params_summary(&no_links_ctx, &method.type_params),
                render_params(&no_links_ctx, &method.params),
              ),
              info: None,
            }),
            href: format!("{href}#{}", target_id.as_str()),
            diff_status: Some(DiffStatus::Removed),
          });
        }
      }
      if let Some(pc) = &iface_diff.property_changes {
        for prop in &pc.removed {
          let target_id = IdBuilder::new(ctx)
            .kind(IdKind::Property)
            .name(&prop.name.to_lowercase())
            .build_unregistered();

          let ty = prop.ts_type.as_ref().map(|ts_type| TypeSummaryCtx {
            ty: render_type_def_colon(&no_links_ctx, ts_type),
            info: None,
          });

          subitems.insert(NamespaceNodeSubItemCtx {
            title: prop.name.to_string(),
            docs: crate::html::jsdoc::jsdoc_body_to_html(
              ctx,
              &prop.js_doc,
              true,
            ),
            ty,
            href: format!("{href}#{}", target_id.as_str()),
            diff_status: Some(DiffStatus::Removed),
          });
        }
      }
    }
    _ => {}
  }
}
