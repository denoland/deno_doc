use super::symbols::SymbolContentCtx;
use super::util;
use super::util::AnchorCtx;
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

use crate::js_doc::JsDocTag;
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

#[derive(Debug, Serialize)]
pub struct IndexCtx {
  pub html_head_ctx: HtmlHeadCtx,
  pub module_doc: Option<super::jsdoc::ModuleDocCtx>,
  pub overview: Option<SymbolContentCtx>,
  pub breadcrumbs_ctx: BreadcrumbsCtx,
  pub toc_ctx: util::ToCCtx,
}

impl IndexCtx {
  pub const TEMPLATE: &'static str = "pages/index";

  /// short_path is None in the case this is a root index page but there is no main entrypoint
  pub fn new(
    ctx: &GenerateCtx,
    short_path: Option<Rc<ShortPath>>,
    partitions: super::partition::Partitions<String>,
    is_categories: bool,
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

    let root = ctx
      .href_resolver
      .resolve_path(render_ctx.get_current_resolve(), UrlResolveKind::Root);

    let html_head_ctx =
      HtmlHeadCtx::new(&root, "Index", ctx.package_name.as_ref(), None);

    let overview = match ctx.file_mode {
      FileMode::Dts if short_path.is_none() => {
        let entrypoints = ctx
          .doc_nodes
          .iter()
          .map(|(short_path, nodes)| {
            let docs = nodes
              .iter()
              .find(|node| node.kind == DocNodeKind::ModuleDoc)
              .and_then(|node| {
                crate::html::jsdoc::jsdoc_body_to_html(
                  &render_ctx,
                  &node.js_doc,
                  true,
                )
              });

            crate::html::namespace::NamespaceNodeCtx {
              tags: Default::default(),
              doc_node_kind_ctx: vec![],
              href: ctx.href_resolver.resolve_path(
                UrlResolveKind::Root,
                short_path.as_resolve_kind(),
              ),
              name: short_path.display_name(),
              docs,
              deprecated: false,
            }
          })
          .collect::<Vec<_>>();

        // render list of all entrypoints
        let section = util::SectionCtx {
          header: SectionHeaderCtx {
            title: "".to_string(),
            anchor: AnchorCtx { id: "".to_string() },
            href: None,
            doc: None,
          },
          content: util::SectionContentCtx::NamespaceSection(entrypoints),
        };

        Some(SymbolContentCtx {
          id: String::new(),
          sections: vec![section],
          docs: None,
        })
      }
      FileMode::SingleDts => {
        // render "all symbols" page on index page
        let sections = super::namespace::render_namespace(
          &render_ctx,
          partitions
            .into_iter()
            .map(|(title, nodes)| {
              let anchor = render_ctx.toc.add_entry(1, title.clone());

              (
                SectionHeaderCtx {
                  href: if is_categories {
                    Some(render_ctx.ctx.href_resolver.resolve_path(
                      render_ctx.get_current_resolve(),
                      UrlResolveKind::Category(&title),
                    ))
                  } else {
                    None
                  },
                  title,
                  anchor: AnchorCtx { id: anchor },
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
      }
      _ => None,
    };

    let breadcrumbs_ctx = render_ctx.get_breadcrumbs();

    let toc_ctx =
      util::ToCCtx::new(render_ctx, ctx.file_mode != FileMode::SingleDts, &[]);

    IndexCtx {
      html_head_ctx,
      module_doc,
      overview,
      breadcrumbs_ctx,
      toc_ctx,
    }
  }

  pub fn new_category(
    ctx: &GenerateCtx,
    name: &str,
    partitions: super::partition::Partitions<String>,
    all_doc_nodes: &[DocNodeWithContext],
  ) -> Self {
    let render_ctx =
      RenderContext::new(ctx, all_doc_nodes, UrlResolveKind::Category(name));

    let sections = super::namespace::render_namespace(
      &render_ctx,
      partitions
        .into_iter()
        .map(|(title, nodes)| {
          let anchor = render_ctx.toc.add_entry(1, title.clone());

          (
            SectionHeaderCtx {
              title,
              anchor: AnchorCtx { id: anchor },
              href: None,
              doc: None,
            },
            nodes,
          )
        })
        .collect(),
    );

    let root = ctx
      .href_resolver
      .resolve_path(UrlResolveKind::Category(name), UrlResolveKind::Root);

    let html_head_ctx =
      HtmlHeadCtx::new(&root, name, ctx.package_name.as_ref(), None);

    let breadcrumbs_ctx = render_ctx.get_breadcrumbs();

    let toc_ctx =
      util::ToCCtx::new(render_ctx, ctx.file_mode != FileMode::SingleDts, &[]);

    IndexCtx {
      html_head_ctx,
      module_doc: None,
      overview: Some(SymbolContentCtx {
        id: String::new(),
        sections,
        docs: None,
      }),
      breadcrumbs_ctx,
      toc_ctx,
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
    symbol_group_ctx: SymbolGroupCtx,
    toc_ctx: util::ToCCtx,
  },
  Redirect {
    current_symbol: String,
    href: String,
  },
}

pub fn generate_symbol_pages_for_module(
  ctx: &GenerateCtx,
  short_path: &ShortPath,
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
    let (breadcrumbs_ctx, symbol_group_ctx, toc_ctx) =
      render_symbol_page(&render_ctx, short_path, &name, &doc_nodes);

    generated_pages.push(SymbolPage::Symbol {
      breadcrumbs_ctx,
      symbol_group_ctx,
      toc_ctx,
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
            category: None,
          },
          UrlResolveKind::Symbol {
            file: short_path,
            symbol: &name,
            category: None,
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
  pub symbol_group_ctx: SymbolGroupCtx,
  pub breadcrumbs_ctx: BreadcrumbsCtx,
  pub toc_ctx: util::ToCCtx,
}

impl SymbolPageCtx {
  pub const TEMPLATE: &'static str = "pages/symbol";
}

pub fn render_symbol_page(
  render_ctx: &RenderContext,
  short_path: &ShortPath,
  namespaced_name: &str,
  doc_nodes: &[DocNodeWithContext],
) -> (BreadcrumbsCtx, SymbolGroupCtx, util::ToCCtx) {
  let mut render_ctx =
    render_ctx.with_current_resolve(UrlResolveKind::Symbol {
      file: short_path,
      symbol: namespaced_name,
      category: if render_ctx.ctx.file_mode == FileMode::SingleDts {
        doc_nodes[0].js_doc.tags.iter().find_map(|tag| {
          if let JsDocTag::Category { doc } = tag {
            Some(doc.as_ref())
          } else {
            None
          }
        })
      } else {
        None
      },
    });
  if !doc_nodes[0].ns_qualifiers.is_empty() {
    render_ctx = render_ctx.with_namespace(doc_nodes[0].ns_qualifiers.clone());
  }

  // NOTE: `doc_nodes` should be sorted at this point.
  let symbol_group_ctx =
    SymbolGroupCtx::new(&render_ctx, doc_nodes, namespaced_name);

  (
    render_ctx.get_breadcrumbs(),
    symbol_group_ctx,
    util::ToCCtx::new(render_ctx, false, doc_nodes),
  )
}
