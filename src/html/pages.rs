use super::sidepanels;
use super::sidepanels::SidepanelCtx;
use super::symbols::SymbolContentCtx;
use super::util::qualify_drilldown_name;
use super::util::BreadcrumbsCtx;
use super::util::SectionHeaderCtx;
use super::DocNodeKindWithDrilldown;
use super::DocNodeWithContext;
use super::FileMode;
use super::GenerateCtx;
use super::RenderContext;
use super::ShortPath;
use super::SymbolGroupCtx;
use super::UrlResolveKind;
use std::sync::Arc;

use super::FUSE_FILENAME;
use super::PAGE_STYLESHEET_FILENAME;
use super::SCRIPT_FILENAME;
use super::SEARCH_FILENAME;
use super::SEARCH_INDEX_FILENAME;
use super::STYLESHEET_FILENAME;

use crate::function::FunctionDef;
use crate::html::partition::Partition;
use crate::variable::VariableDef;
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
    short_path: Option<std::rc::Rc<ShortPath>>,
    partitions: Partition,
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
              crate::html::util::SectionHeaderCtx {
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
    partitions: crate::html::partition::EntrypointPartition,
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
  partitions_for_nodes: &Partition,
  doc_nodes: &[DocNodeWithContext],
) -> Vec<SymbolPage> {
  let mut name_partitions =
    super::partition::partition_nodes_by_name(doc_nodes);

  let mut drilldown_partitions = IndexMap::new();
  for (name, doc_nodes) in &name_partitions {
    if doc_nodes[0].kind == DocNodeKind::Class {
      let class = doc_nodes[0].class_def.as_ref().unwrap();
      let method_nodes = class
        .methods
        .iter()
        .map(|method| {
          let mut new_node =
            doc_nodes[0].create_child(Arc::new(DocNode::function(
              qualify_drilldown_name(name, &method.name, method.is_static),
              method.location.clone(),
              doc_nodes[0].declaration_kind,
              method.js_doc.clone(),
              method.function_def.clone(),
            )));
          new_node.drilldown_parent_kind = Some(DocNodeKind::Class);
          new_node.kind_with_drilldown = DocNodeKindWithDrilldown::Method;
          new_node
        })
        .collect::<Vec<_>>();

      drilldown_partitions
        .extend(super::partition::partition_nodes_by_name(&method_nodes));

      let property_nodes = class
        .properties
        .iter()
        .map(|property| {
          let mut new_node =
            doc_nodes[0].create_child(Arc::new(DocNode::variable(
              qualify_drilldown_name(name, &property.name, property.is_static),
              property.location.clone(),
              doc_nodes[0].declaration_kind,
              property.js_doc.clone(),
              VariableDef {
                ts_type: property.ts_type.clone(),
                kind: deno_ast::swc::ast::VarDeclKind::Const,
              },
            )));
          new_node.drilldown_parent_kind = Some(DocNodeKind::Class);
          new_node.kind_with_drilldown = DocNodeKindWithDrilldown::Property;
          new_node
        })
        .collect::<Vec<_>>();

      drilldown_partitions
        .extend(super::partition::partition_nodes_by_name(&property_nodes));
    } else if doc_nodes[0].kind == DocNodeKind::Interface {
      let interface = doc_nodes[0].interface_def.as_ref().unwrap();
      let method_nodes = interface
        .methods
        .iter()
        .map(|method| {
          let mut new_node =
            doc_nodes[0].create_child(Arc::new(DocNode::function(
              qualify_drilldown_name(name, &method.name, true),
              method.location.clone(),
              doc_nodes[0].declaration_kind,
              method.js_doc.clone(),
              FunctionDef {
                def_name: None,
                params: method.params.clone(),
                return_type: method.return_type.clone(),
                has_body: false,
                is_async: false,
                is_generator: false,
                type_params: method.type_params.clone(),
                decorators: vec![],
              },
            )));
          new_node.drilldown_parent_kind = Some(DocNodeKind::Interface);
          new_node.kind_with_drilldown = DocNodeKindWithDrilldown::Method;
          new_node
        })
        .collect::<Vec<_>>();

      drilldown_partitions
        .extend(super::partition::partition_nodes_by_name(&method_nodes));

      let property_nodes = interface
        .properties
        .iter()
        .map(|property| {
          let mut new_node =
            doc_nodes[0].create_child(Arc::new(DocNode::variable(
              qualify_drilldown_name(name, &property.name, true),
              property.location.clone(),
              doc_nodes[0].declaration_kind,
              property.js_doc.clone(),
              VariableDef {
                ts_type: property.ts_type.clone(),
                kind: deno_ast::swc::ast::VarDeclKind::Const,
              },
            )));
          new_node.drilldown_parent_kind = Some(DocNodeKind::Interface);
          new_node.kind_with_drilldown = DocNodeKindWithDrilldown::Property;
          new_node
        })
        .collect::<Vec<_>>();

      drilldown_partitions
        .extend(super::partition::partition_nodes_by_name(&property_nodes));
    }
  }
  name_partitions.extend(drilldown_partitions);

  generate_symbol_pages_inner(
    ctx,
    doc_nodes,
    partitions_for_nodes,
    &name_partitions,
    short_path,
    vec![],
  )
}

fn generate_symbol_pages_inner(
  ctx: &GenerateCtx,
  doc_nodes_for_module: &[DocNodeWithContext],
  partitions_for_nodes: &Partition,
  name_partitions: &Partition,
  short_path: &ShortPath,
  namespace_paths: Vec<&str>,
) -> Vec<SymbolPage> {
  let mut generated_pages =
    Vec::with_capacity(name_partitions.values().len() * 2);

  for (name, doc_nodes) in name_partitions {
    let namespaced_name = if namespace_paths.is_empty() {
      name.to_owned()
    } else {
      format!("{}.{name}", namespace_paths.join("."))
    };

    let sidepanel_ctx = SidepanelCtx::new(
      ctx,
      partitions_for_nodes,
      short_path,
      &namespaced_name,
    );

    let (breadcrumbs_ctx, symbol_group_ctx) = render_symbol_page(
      ctx,
      doc_nodes_for_module,
      short_path,
      &namespace_paths,
      &namespaced_name,
      doc_nodes,
    );

    generated_pages.push(SymbolPage::Symbol {
      breadcrumbs_ctx,
      sidepanel_ctx,
      symbol_group_ctx,
    });

    if let Some(_doc_node) = doc_nodes
      .iter()
      .find(|doc_node| doc_node.kind == DocNodeKind::Class)
    {
      let prototype_name = format!("{namespaced_name}.prototype");
      generated_pages.push(SymbolPage::Redirect {
        href: ctx.href_resolver.resolve_path(
          UrlResolveKind::Symbol {
            file: short_path,
            symbol: &prototype_name,
          },
          UrlResolveKind::Symbol {
            file: short_path,
            symbol: &namespaced_name,
          },
        ),
        current_symbol: prototype_name,
      });
    }

    if let Some(doc_node) = doc_nodes
      .iter()
      .find(|doc_node| doc_node.kind == DocNodeKind::Namespace)
    {
      let namespace = doc_node.namespace_def.as_ref().unwrap();

      let namespace_name_partitions = super::partition::partition_nodes_by_name(
        &namespace
          .elements
          .iter()
          .map(|element| doc_node.create_child(element.clone()))
          .collect::<Vec<_>>(),
      );

      let namespace_paths = {
        let mut ns_paths = namespace_paths.clone();
        ns_paths.push(name);
        ns_paths
      };

      let generated = generate_symbol_pages_inner(
        ctx,
        doc_nodes_for_module,
        partitions_for_nodes,
        &namespace_name_partitions,
        short_path,
        namespace_paths,
      );
      generated_pages.extend(generated);
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
  ctx: &GenerateCtx,
  doc_nodes_for_module: &[DocNodeWithContext],
  short_path: &ShortPath,
  namespace_paths: &[&str],
  namespaced_name: &str,
  doc_nodes: &[DocNodeWithContext],
) -> (BreadcrumbsCtx, SymbolGroupCtx) {
  let mut render_ctx = RenderContext::new(
    ctx,
    doc_nodes_for_module,
    UrlResolveKind::Symbol {
      file: short_path,
      symbol: namespaced_name,
    },
  );
  if !namespace_paths.is_empty() {
    render_ctx = render_ctx.with_namespace(namespace_paths.to_vec())
  }

  // NOTE: `doc_nodes` should be sorted at this point.
  let symbol_group_ctx =
    SymbolGroupCtx::new(&render_ctx, doc_nodes, namespaced_name);

  (render_ctx.get_breadcrumbs(), symbol_group_ctx)
}
