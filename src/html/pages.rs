use super::sidepanels;
use super::sidepanels::SidepanelCtx;
use super::symbols::SymbolContentCtx;
use super::util::BreadcrumbsCtx;
use super::util::SectionHeaderCtx;
use super::DocNodeWithContext;
use super::FileMode;
use super::GenerateCtx;
use super::RenderContext;
use super::ShortPath;
use super::SymbolGroupCtx;
use super::UrlResolveKind;
use std::rc::Rc;

use super::FUSE_FILENAME;
use super::PAGE_STYLESHEET_FILENAME;
use super::SCRIPT_FILENAME;
use super::SEARCH_FILENAME;
use super::SEARCH_INDEX_FILENAME;
use super::STYLESHEET_FILENAME;

use crate::DocNode;
use crate::DocNodeKind;
use indexmap::IndexMap;
use serde::Serialize;

#[derive(Debug, Serialize, Clone)]
pub struct HtmlHeadCtx {
  title: String,
  current_file: String,
  stylesheet_url: String,
  page_stylesheet_url: String,
  url_search_index: String,
  script_js: String,
  fuse_js: String,
  url_search: String,
}

impl HtmlHeadCtx {
  pub const TEMPLATE: &'static str = "pages/html_head";

  pub fn new(
    root: &str,
    page: &str,
    package_name: Option<&String>,
    current_file: Option<&ShortPath>,
  ) -> Self {
    Self {
      title: format!(
        "{page} - {}documentation",
        package_name
          .map(|package_name| format!("{package_name} "))
          .unwrap_or_default()
      ),
      current_file: current_file
        .map(|current_file| &*current_file.path)
        .unwrap_or_default()
        .to_string(),
      stylesheet_url: format!("{root}{STYLESHEET_FILENAME}"),
      page_stylesheet_url: format!("{root}{PAGE_STYLESHEET_FILENAME}"),
      url_search_index: format!("{root}{SEARCH_INDEX_FILENAME}"),
      script_js: format!("{root}{SCRIPT_FILENAME}"),
      fuse_js: format!("{root}{FUSE_FILENAME}"),
      url_search: format!("{root}{SEARCH_FILENAME}"),
    }
  }
}

#[derive(Debug, Serialize, Clone)]
pub struct IndexCtx {
  pub html_head_ctx: HtmlHeadCtx,
  pub sidepanel_ctx: sidepanels::IndexSidepanelCtx,
  pub module_doc: Option<super::jsdoc::ModuleDocCtx>,
  pub all_symbols: Option<SymbolContentCtx>,
  pub breadcrumbs_ctx: BreadcrumbsCtx,
}

impl IndexCtx {
  pub const TEMPLATE: &'static str = "pages/index";

  /// short_path is None in the case this is a root index page but there is no main entrypoint
  pub fn new(
    ctx: &GenerateCtx,
    short_path: Option<Rc<ShortPath>>,
    partitions: super::partition::Partitions<String>,
  ) -> Self {
    // will be default on index page with no main entrypoint
    let default = vec![];
    let doc_nodes = short_path
      .as_ref()
      .and_then(|short_path| ctx.doc_nodes.get(short_path))
      .unwrap_or(&default);

    let render_ctx = RenderContext::new(
      ctx,
      doc_nodes,
      short_path
        .as_deref()
        .map_or(UrlResolveKind::Root, ShortPath::as_resolve_kind),
    );

    let module_doc = short_path.as_ref().map(|short_path| {
      super::jsdoc::ModuleDocCtx::new(&render_ctx, short_path)
    });

    let root = ctx.href_resolver.resolve_path(
      short_path
        .as_deref()
        .map_or(UrlResolveKind::Root, ShortPath::as_resolve_kind),
      UrlResolveKind::Root,
    );

    let html_head_ctx =
      HtmlHeadCtx::new(&root, "Index", ctx.package_name.as_ref(), None);

    let all_symbols = if ctx.file_mode == FileMode::SingleDts {
      let sections = super::namespace::render_namespace(
        &render_ctx,
        partitions
          .clone()
          .into_iter()
          .map(|(title, nodes)| {
            (
              SectionHeaderCtx {
                title,
                href: None,
                doc: None,
              },
              nodes,
            )
          })
          .collect(),
      );

      Some(SymbolContentCtx {
        id: String::new(),
        sections,
        docs: None,
      })
    } else {
      None
    };

    let sidepanel_ctx =
      sidepanels::IndexSidepanelCtx::new(ctx, short_path.clone(), partitions);

    IndexCtx {
      html_head_ctx,
      sidepanel_ctx,
      module_doc,
      all_symbols,
      breadcrumbs_ctx: render_ctx.get_breadcrumbs(),
    }
  }
}

#[derive(Serialize)]
pub struct AllSymbolsCtx {
  pub html_head_ctx: HtmlHeadCtx,
  pub content: SymbolContentCtx,
  pub breadcrumbs_ctx: BreadcrumbsCtx,
}

impl AllSymbolsCtx {
  pub const TEMPLATE: &'static str = "pages/all_symbols";

  pub fn new(
    ctx: &GenerateCtx,
    partitions: super::partition::Partitions<Rc<ShortPath>>,
  ) -> Self {
    // TODO(@crowlKats): handle doc_nodes in all symbols page for each symbol
    let render_ctx = RenderContext::new(ctx, &[], UrlResolveKind::AllSymbols);

    let sections = super::namespace::render_namespace(
      &render_ctx,
      partitions
        .into_iter()
        .map(|(path, nodes)| {
          (
            SectionHeaderCtx::new_for_namespace(&render_ctx, &path),
            nodes,
          )
        })
        .collect(),
    );

    let html_head_ctx =
      HtmlHeadCtx::new("./", "All Symbols", ctx.package_name.as_ref(), None);

    AllSymbolsCtx {
      html_head_ctx,
      content: SymbolContentCtx {
        id: String::new(),
        sections,
        docs: None,
      },
      breadcrumbs_ctx: render_ctx.get_breadcrumbs(),
    }
  }
}

pub enum SymbolPage {
  Symbol {
    breadcrumbs_ctx: BreadcrumbsCtx,
    sidepanel_ctx: SidepanelCtx,
    symbol_group_ctx: SymbolGroupCtx,
  },
  Redirect {
    current_symbol: String,
    href: String,
  },
}

pub fn generate_symbol_pages_for_module(
  ctx: &GenerateCtx,
  short_path: &ShortPath,
  sidepanel_partitions: &super::partition::Partitions<String>,
  module_doc_nodes: &[DocNodeWithContext],
) -> Vec<SymbolPage> {
  let mut name_partitions =
    super::partition::partition_nodes_by_name(module_doc_nodes, true);

  let mut drilldown_partitions = IndexMap::new();
  for doc_nodes in name_partitions.values() {
    let has_class =
      doc_nodes.iter().any(|node| node.kind == DocNodeKind::Class);
    for doc_node in doc_nodes {
      match doc_node.kind {
        DocNodeKind::Class => {
          let class = doc_node.class_def.as_ref().unwrap();

          let method_nodes = class
            .methods
            .iter()
            .map(|method| {
              doc_node.create_child_method(
                DocNode::function(
                  method.name.clone(),
                  method.location.clone(),
                  doc_node.declaration_kind,
                  method.js_doc.clone(),
                  method.function_def.clone(),
                ),
                method.is_static,
              )
            })
            .collect::<Vec<_>>();

          drilldown_partitions.extend(
            super::partition::partition_nodes_by_name(&method_nodes, false),
          );

          let property_nodes = class
            .properties
            .iter()
            .map(|property| {
              doc_node.create_child_property(
                DocNode::from(property.clone()),
                property.is_static,
              )
            })
            .collect::<Vec<_>>();

          drilldown_partitions.extend(
            super::partition::partition_nodes_by_name(&property_nodes, false),
          );
        }
        DocNodeKind::Interface => {
          let interface = doc_node.interface_def.as_ref().unwrap();
          let method_nodes = interface
            .methods
            .iter()
            .filter_map(|method| {
              if has_class && method.name == "prototype" {
                None
              } else {
                Some(
                  doc_node
                    .create_child_method(DocNode::from(method.clone()), true),
                )
              }
            })
            .collect::<Vec<_>>();

          drilldown_partitions.extend(
            super::partition::partition_nodes_by_name(&method_nodes, false),
          );

          let property_nodes =
            interface
              .properties
              .iter()
              .filter_map(|property| {
                if has_class && property.name == "prototype" {
                  None
                } else {
                  Some(doc_node.create_child_property(
                    DocNode::from(property.clone()),
                    true,
                  ))
                }
              })
              .collect::<Vec<_>>();

          drilldown_partitions.extend(
            super::partition::partition_nodes_by_name(&property_nodes, false),
          );
        }
        DocNodeKind::TypeAlias => {
          let type_alias = doc_node.type_alias_def.as_ref().unwrap();

          if let Some(ts_type_literal) =
            type_alias.ts_type.type_literal.as_ref()
          {
            let method_nodes = ts_type_literal
              .methods
              .iter()
              .filter_map(|method| {
                if has_class && method.name == "prototype" {
                  None
                } else {
                  Some(
                    doc_node
                      .create_child_method(DocNode::from(method.clone()), true),
                  )
                }
              })
              .collect::<Vec<_>>();

            drilldown_partitions.extend(
              super::partition::partition_nodes_by_name(&method_nodes, false),
            );

            let property_nodes = ts_type_literal
              .properties
              .iter()
              .filter_map(|property| {
                if has_class && property.name == "prototype" {
                  None
                } else {
                  Some(doc_node.create_child_property(
                    DocNode::from(property.clone()),
                    true,
                  ))
                }
              })
              .collect::<Vec<_>>();

            drilldown_partitions.extend(
              super::partition::partition_nodes_by_name(&property_nodes, false),
            );
          }
        }
        DocNodeKind::Variable => {
          let variable = doc_node.variable_def.as_ref().unwrap();

          if let Some(ts_type_literal) = variable
            .ts_type
            .as_ref()
            .and_then(|ts_type| ts_type.type_literal.as_ref())
          {
            let method_nodes = ts_type_literal
              .methods
              .iter()
              .map(|method| {
                doc_node
                  .create_child_method(DocNode::from(method.clone()), true)
              })
              .collect::<Vec<_>>();

            drilldown_partitions.extend(
              super::partition::partition_nodes_by_name(&method_nodes, false),
            );

            let property_nodes = ts_type_literal
              .properties
              .iter()
              .map(|property| {
                doc_node
                  .create_child_property(DocNode::from(property.clone()), true)
              })
              .collect::<Vec<_>>();

            drilldown_partitions.extend(
              super::partition::partition_nodes_by_name(&property_nodes, false),
            );
          }
        }
        _ => {}
      }
    }
  }
  name_partitions.extend(drilldown_partitions);

  let mut generated_pages = Vec::with_capacity(name_partitions.values().len());

  let render_ctx =
    RenderContext::new(ctx, module_doc_nodes, UrlResolveKind::File(short_path));

  for (name, doc_nodes) in name_partitions {
    let sidepanel_ctx =
      SidepanelCtx::new(ctx, sidepanel_partitions, short_path, &name);

    let (breadcrumbs_ctx, symbol_group_ctx) =
      render_symbol_page(&render_ctx, short_path, &name, &doc_nodes);

    generated_pages.push(SymbolPage::Symbol {
      breadcrumbs_ctx,
      sidepanel_ctx,
      symbol_group_ctx,
    });

    if doc_nodes
      .iter()
      .any(|doc_node| doc_node.kind == DocNodeKind::Class)
    {
      let prototype_name = format!("{name}.prototype");
      generated_pages.push(SymbolPage::Redirect {
        href: ctx.href_resolver.resolve_path(
          UrlResolveKind::Symbol {
            file: short_path,
            symbol: &prototype_name,
          },
          UrlResolveKind::Symbol {
            file: short_path,
            symbol: &name,
          },
        ),
        current_symbol: prototype_name,
      });
    }
  }

  generated_pages
}

#[derive(Debug, Serialize)]
pub struct SymbolPageCtx {
  pub html_head_ctx: HtmlHeadCtx,
  pub sidepanel_ctx: SidepanelCtx,
  pub symbol_group_ctx: SymbolGroupCtx,
  pub breadcrumbs_ctx: BreadcrumbsCtx,
}

impl SymbolPageCtx {
  pub const TEMPLATE: &'static str = "pages/symbol";
}

pub fn render_symbol_page(
  render_ctx: &RenderContext,
  short_path: &ShortPath,
  namespaced_name: &str,
  doc_nodes: &[DocNodeWithContext],
) -> (BreadcrumbsCtx, SymbolGroupCtx) {
  let mut render_ctx =
    render_ctx.with_current_resolve(UrlResolveKind::Symbol {
      file: short_path,
      symbol: namespaced_name,
    });
  if !doc_nodes[0].ns_qualifiers.is_empty() {
    render_ctx = render_ctx.with_namespace(doc_nodes[0].ns_qualifiers.clone());
  }

  // NOTE: `doc_nodes` should be sorted at this point.
  let symbol_group_ctx =
    SymbolGroupCtx::new(&render_ctx, doc_nodes, namespaced_name);

  (render_ctx.get_breadcrumbs(), symbol_group_ctx)
}
