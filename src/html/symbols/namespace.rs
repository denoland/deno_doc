use crate::diff::DeclarationDefDiff;
use crate::html::DiffStatus;
use crate::html::DocNodeWithContext;
use crate::html::DrilldownKind;
use crate::html::parameters::render_params;
use crate::html::render_context::RenderContext;
use crate::html::symbols::function::render_function_summary;
use crate::html::types::render_type_def;
use crate::html::types::render_type_def_colon;
use crate::html::types::type_params_summary;
use crate::html::util::*;
use crate::node::DeclarationDef;
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
  symbols: Vec<DocNodeWithContext>,
) -> SectionCtx {
  let nodes = symbols
    .into_iter()
    .filter_map(|symbol| {
      if symbol.is_internal(ctx.ctx) {
        None
      } else {
        Some(NamespaceNodeCtx::new(ctx, symbol))
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
  fn new(ctx: &RenderContext, symbol: DocNodeWithContext) -> Self {
    let name = symbol.get_qualified_name();
    let id = IdBuilder::new(ctx)
      .kind(IdKind::Namespace)
      .name(name)
      .build();

    let docs = crate::html::jsdoc::jsdoc_body_to_html(
      ctx,
      &symbol.declarations[0].js_doc,
      true,
    );

    let tags = Tag::from_js_doc(&symbol.declarations[0].js_doc);

    let mut subitems = IndexSet::new();

    let href = ctx.ctx.resolve_path(
      ctx.get_current_resolve(),
      UrlResolveKind::Symbol {
        file: &symbol.origin,
        symbol: name,
      },
    );

    let node_def_diffs: Vec<&DeclarationDefDiff> = ctx
      .ctx
      .diff
      .as_ref()
      .map(|diff_index| {
        symbol
          .declarations
          .iter()
          .filter_map(|decl| {
            diff_index.get_def_diff(
              &symbol.origin.specifier,
              symbol.get_name(),
              decl.def.to_kind(),
            )
          })
          .collect()
      })
      .unwrap_or_default();

    if let Some(drilldown_symbols) = symbol.get_drilldown_symbols() {
      subitems.extend(
        drilldown_symbols
          .filter(|symbol| {
            !symbol.drilldown_name.as_ref().unwrap().starts_with('[')
          })
          .map(|drilldown| {
            let drilldown_name = drilldown.drilldown_name.as_ref().unwrap();
            let drilldown_kind = drilldown.drilldown_kind.unwrap();

            let target_id = match drilldown_kind {
              DrilldownKind::Property => IdBuilder::new(ctx)
                .kind(IdKind::Property)
                .name(&drilldown_name.to_lowercase())
                .build_unregistered(),
              DrilldownKind::Method(kind) => {
                if matches!(
                  kind,
                  crate::html::MethodKind::Getter
                    | crate::html::MethodKind::Setter
                ) {
                  IdBuilder::new(ctx)
                    .kind(IdKind::Accessor)
                    .name(&drilldown_name.to_lowercase())
                    .build_unregistered()
                } else {
                  IdBuilder::new(ctx)
                    .kind(IdKind::Method)
                    .name(&drilldown_name.to_lowercase())
                    .index(0)
                    .build_unregistered()
                }
              }
            };

            let diff_status =
              if matches!(symbol.diff_status, Some(DiffStatus::Removed)) {
                Some(DiffStatus::Removed)
              } else {
                node_def_diffs.iter().find_map(|diff| {
                  get_subitem_diff_status(diff, drilldown_name, &drilldown_kind)
                })
              };

            let docs = crate::html::jsdoc::jsdoc_body_to_html(
              ctx,
              &drilldown.declarations[0].js_doc,
              true,
            );

            NamespaceNodeSubItemCtx {
              title: drilldown_name.to_string(),
              docs,
              ty: summary_for_symbol(ctx, &drilldown),
              href: format!("{href}#{}", target_id.as_str()),
              diff_status,
            }
          }),
      );
    }

    // Inject removed sub-items from the diff
    for def_diff in &node_def_diffs {
      inject_removed_subitems(ctx, def_diff, &href, &mut subitems);
    }

    subitems.sort();

    let diff_status = symbol.diff_status.clone().and_then(|status| {
      if matches!(status, DiffStatus::Modified) {
        // Check if any declarations were added or removed
        let has_added_or_removed_decls = ctx
          .ctx
          .diff
          .as_ref()
          .and_then(|d| {
            d.get_symbol_diff(&symbol.origin.specifier, symbol.get_name())
          })
          .and_then(|info| info.diff.as_ref())
          .and_then(|sd| sd.declarations.as_ref())
          .is_some_and(|dd| !dd.added.is_empty() || !dd.removed.is_empty());

        if !has_added_or_removed_decls {
          let only_subitem_changes = symbol.declarations.iter().all(|decl| {
            ctx
              .ctx
              .diff
              .as_ref()
              .and_then(|diff_index| {
                let decl_diff = diff_index.get_declaration_diff(
                  &symbol.origin.specifier,
                  symbol.get_name(),
                  decl.def.to_kind(),
                )?;

                // If there are non-def changes, it's not subitem-only
                if decl_diff.js_doc_changes.is_some()
                  || decl_diff.declaration_kind_change.is_some()
                {
                  return Some(false);
                }

                Some(match decl_diff.def_changes.as_ref()? {
                  DeclarationDefDiff::Class(class_diff) => {
                    class_diff.is_abstract_change.is_none()
                      && class_diff.extends_change.is_none()
                      && class_diff.implements_change.is_none()
                      && class_diff.type_params_change.is_none()
                      && class_diff.super_type_params_change.is_none()
                      && class_diff.constructor_changes.is_none()
                      && class_diff.index_signature_changes.is_none()
                      && class_diff.decorators_change.is_none()
                  }
                  DeclarationDefDiff::Interface(iface_diff) => {
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
      }
      Some(status)
    });

    let old_tags = if matches!(diff_status, Some(DiffStatus::Modified)) {
      let mut old_tags = tags.clone();
      if let Some(tags_diff) = ctx.ctx.diff.as_ref().and_then(|diff_index| {
        symbol.declarations.iter().find_map(|decl| {
          diff_index
            .get_declaration_diff(
              &symbol.origin.specifier,
              symbol.get_name(),
              decl.def.to_kind(),
            )?
            .js_doc_changes
            .as_ref()?
            .tags_change
            .as_ref()
        })
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
      doc_node_kind_ctx: symbol.get_kind_ctxs(),
      href,
      name: name.to_string(),
      ty: summary_for_symbol(ctx, &symbol),
      docs,
      deprecated: all_deprecated(
        &symbol.declarations.iter().collect::<Vec<_>>(),
      ),
      subitems,
      diff_status,
    }
  }
}

fn summary_for_symbol(
  ctx: &RenderContext,
  symbol: &DocNodeWithContext,
) -> Option<TypeSummaryCtx> {
  match &symbol.declarations[0].def {
    DeclarationDef::Function(..) => {
      let overloads: Vec<_> = symbol
        .declarations
        .iter()
        .filter_map(|d| d.function_def())
        .collect();

      let (def, info) = if overloads.len() > 1 {
        (
          overloads
            .iter()
            .find(|overload| overload.has_body)
            .unwrap_or(&overloads[0]),
          Some(format!("{} overloads", overloads.len() - 1)),
        )
      } else {
        (&overloads[0], None)
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
    DeclarationDef::Class(class_def) => {
      if !class_def.constructors.is_empty() {
        let ctx = ctx.with_disable_links(true);

        let (constructor, info) = if class_def.constructors.len() > 1 {
          (
            class_def
              .constructors
              .iter()
              .find(|overload| overload.has_body)
              .unwrap_or(&class_def.constructors[0]),
            Some(format!("{} constructors", class_def.constructors.len() - 1)),
          )
        } else {
          (&class_def.constructors[0], None)
        };

        let params = constructor
          .params
          .iter()
          .map(|param| param.param.clone())
          .collect::<Vec<_>>();

        Some(TypeSummaryCtx {
          ty: format!(
            "{}({})",
            type_params_summary(&ctx, &class_def.type_params),
            render_params(&ctx, &params)
          ),
          info,
        })
      } else {
        None
      }
    }
    DeclarationDef::Enum(..) => None,
    DeclarationDef::Interface(..) => None,
    DeclarationDef::TypeAlias(type_alias_def) => {
      let ctx = ctx.with_disable_links(true);

      Some(TypeSummaryCtx {
        ty: format!(
          "{} = {}",
          type_params_summary(&ctx, &type_alias_def.type_params),
          render_type_def(&ctx, &type_alias_def.ts_type)
        ),
        info: None,
      })
    }
    DeclarationDef::Variable(variable_def) => {
      variable_def.ts_type.as_ref().map(|ts_type| TypeSummaryCtx {
        ty: format!(
          ": {}",
          render_type_def(&ctx.with_disable_links(true), ts_type)
        ),
        info: None,
      })
    }
    DeclarationDef::Reference(..)
    | DeclarationDef::Namespace(..)
    | DeclarationDef::Import(..) => None,
  }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct TypeSummaryCtx {
  pub ty: String,
  pub info: Option<String>,
}

fn get_subitem_diff_status(
  def_diff: &DeclarationDefDiff,
  name: &str,
  kind: &DrilldownKind,
) -> Option<DiffStatus> {
  match def_diff {
    DeclarationDefDiff::Class(class_diff) => match kind {
      DrilldownKind::Method(_) => {
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
      DrilldownKind::Property => {
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
    },
    DeclarationDefDiff::Interface(iface_diff) => match kind {
      DrilldownKind::Method(_) => {
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
      DrilldownKind::Property => {
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
    },
    _ => None,
  }
}

fn inject_removed_subitems(
  ctx: &RenderContext,
  def_diff: &DeclarationDefDiff,
  href: &str,
  subitems: &mut IndexSet<NamespaceNodeSubItemCtx>,
) {
  let no_links_ctx = ctx.with_disable_links(true);

  match def_diff {
    DeclarationDefDiff::Class(class_diff) => {
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
    DeclarationDefDiff::Interface(iface_diff) => {
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
