use super::partition_nodes_by_name;
use super::sidepanels;
use super::DocNodeWithContext;
use super::GenerateCtx;
use super::RenderContext;
use super::SymbolGroupCtx;
use super::UrlResolveKind;

use super::FUSE_FILENAME;
use super::PAGE_STYLESHEET_FILENAME;
use super::SEARCH_FILENAME;
use super::SEARCH_INDEX_FILENAME;
use super::STYLESHEET_FILENAME;

use crate::html::sidepanels::SidepanelCtx;
use crate::html::util::BreadcrumbsCtx;
use crate::DocNode;
use crate::DocNodeKind;
use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use serde::Serialize;

#[derive(Debug, Serialize, Clone)]
pub struct HtmlHeadCtx {
  title: String,
  current_file: String,
  stylesheet_url: String,
  page_stylesheet_url: String,
  url_search_index: String,
  fuse_js: String,
  url_search: String,
}

impl HtmlHeadCtx {
  pub fn new(
    root: &str,
    page: &str,
    package_name: Option<&String>,
    current_file: Option<String>,
  ) -> Self {
    Self {
      title: format!(
        "{page} - {}documentation",
        package_name
          .map(|package_name| format!("{package_name} "))
          .unwrap_or_default()
      ),
      current_file: current_file.unwrap_or_default(),
      stylesheet_url: format!("{root}{STYLESHEET_FILENAME}"),
      page_stylesheet_url: format!("{root}{PAGE_STYLESHEET_FILENAME}"),
      url_search_index: format!("{root}{SEARCH_INDEX_FILENAME}"),
      fuse_js: format!("{root}{FUSE_FILENAME}"),
      url_search: format!("{root}{SEARCH_FILENAME}"),
    }
  }
}

#[derive(Debug, Serialize, Clone)]
struct IndexCtx {
  html_head_ctx: HtmlHeadCtx,
  sidepanel_ctx: sidepanels::IndexSidepanelCtx,
  module_doc: Option<super::jsdoc::ModuleDocCtx>,
  breadcrumbs_ctx: BreadcrumbsCtx,
}

pub fn render_index(
  ctx: &GenerateCtx,
  specifier: Option<&ModuleSpecifier>,
  doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<DocNode>>,
  partitions: IndexMap<String, Vec<DocNodeWithContext>>,
  file: Option<String>,
) -> String {
  let sidepanel_ctx = sidepanels::IndexSidepanelCtx::new(
    ctx,
    specifier,
    doc_nodes_by_url,
    partitions,
    &file,
  );

  let short_path = specifier.map(|specifier| ctx.url_to_short_path(specifier));

  // will be default on index page with no main entrypoint
  let default = vec![];
  let doc_nodes = specifier
    .or(ctx.main_entrypoint.as_ref())
    .and_then(|specifier| doc_nodes_by_url.get(specifier))
    .unwrap_or(&default);

  let render_ctx = RenderContext::new(
    ctx,
    doc_nodes,
    if let Some(short_path) = &short_path {
      UrlResolveKind::File(short_path)
    } else {
      UrlResolveKind::Root
    },
    specifier,
  );

  let module_doc =
    super::jsdoc::ModuleDocCtx::new(&render_ctx, specifier, doc_nodes_by_url);

  let root = (ctx.url_resolver)(
    file
      .as_deref()
      .map_or(UrlResolveKind::Root, UrlResolveKind::File),
    UrlResolveKind::Root,
  );

  let html_head_ctx =
    HtmlHeadCtx::new(&root, "Index", ctx.package_name.as_ref(), None);

  let index_ctx = IndexCtx {
    html_head_ctx,
    sidepanel_ctx,
    module_doc,
    breadcrumbs_ctx: render_ctx.get_breadcrumbs(),
  };

  render_ctx.render("pages/index", &index_ctx)
}

#[derive(Serialize)]
struct AllSymbolsCtx {
  html_head_ctx: HtmlHeadCtx,
  namespace_ctx: super::namespace::NamespaceRenderCtx,
  breadcrumbs_ctx: BreadcrumbsCtx,
}

pub(crate) fn render_all_symbols_page(
  ctx: &GenerateCtx,
  partitions: &IndexMap<DocNodeKind, Vec<DocNodeWithContext>>,
) -> Result<String, anyhow::Error> {
  // TODO(@crowlKats): handle doc_nodes in all symbols page for each symbol
  let render_ctx =
    RenderContext::new(ctx, &[], UrlResolveKind::AllSymbols, None);
  let namespace_ctx =
    super::namespace::get_namespace_render_ctx(&render_ctx, partitions);

  let html_head_ctx =
    HtmlHeadCtx::new("./", "All Symbols", ctx.package_name.as_ref(), None);
  let all_symbols_ctx = AllSymbolsCtx {
    html_head_ctx,
    namespace_ctx,
    breadcrumbs_ctx: render_ctx.get_breadcrumbs(),
  };

  Ok(render_ctx.render("pages/all_symbols", &all_symbols_ctx))
}

pub fn generate_symbol_pages_for_module(
  ctx: &GenerateCtx,
  current_specifier: &ModuleSpecifier,
  short_path: &str,
  partitions_for_nodes: &IndexMap<String, Vec<DocNodeWithContext>>,
  doc_nodes: &[DocNode],
) -> Vec<(BreadcrumbsCtx, SidepanelCtx, SymbolGroupCtx)> {
  let name_partitions = partition_nodes_by_name(doc_nodes);

  generate_symbol_pages(
    ctx,
    doc_nodes,
    partitions_for_nodes,
    name_partitions,
    current_specifier,
    short_path,
    vec![],
  )
}

fn generate_symbol_pages(
  ctx: &GenerateCtx,
  doc_nodes_for_module: &[DocNode],
  partitions_for_nodes: &IndexMap<String, Vec<DocNodeWithContext>>,
  name_partitions: IndexMap<String, Vec<DocNode>>,
  current_specifier: &ModuleSpecifier,
  short_path: &str,
  namespace_paths: Vec<String>,
) -> Vec<(BreadcrumbsCtx, SidepanelCtx, SymbolGroupCtx)> {
  let mut generated_pages =
    Vec::with_capacity(name_partitions.values().len() * 2);

  for (name, doc_nodes) in name_partitions.iter() {
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
      current_specifier,
      short_path,
      &namespace_paths,
      &namespaced_name,
      doc_nodes,
    );

    generated_pages.push((breadcrumbs_ctx, sidepanel_ctx, symbol_group_ctx));

    if let Some(doc_node) = doc_nodes
      .iter()
      .find(|doc_node| doc_node.kind == DocNodeKind::Namespace)
    {
      let namespace = doc_node.namespace_def.as_ref().unwrap();

      let namespace_name_partitions =
        partition_nodes_by_name(&namespace.elements);

      let namespace_paths = {
        let mut ns_paths = namespace_paths.clone();
        ns_paths.push(name.to_string());
        ns_paths
      };

      let generated = generate_symbol_pages(
        ctx,
        doc_nodes_for_module,
        partitions_for_nodes,
        namespace_name_partitions,
        current_specifier,
        short_path,
        namespace_paths,
      );
      generated_pages.extend_from_slice(&generated);
    }
  }

  generated_pages
}

#[derive(Debug, Serialize)]
pub struct PageCtx {
  pub html_head_ctx: HtmlHeadCtx,
  pub sidepanel_ctx: SidepanelCtx,
  pub symbol_group_ctx: SymbolGroupCtx,
  pub breadcrumbs_ctx: BreadcrumbsCtx,
}

fn render_symbol_page(
  ctx: &GenerateCtx,
  doc_nodes_for_module: &[DocNode],
  current_specifier: &ModuleSpecifier,
  short_path: &str,
  namespace_paths: &[String],
  namespaced_name: &str,
  doc_nodes: &[DocNode],
) -> (BreadcrumbsCtx, SymbolGroupCtx) {
  let mut render_ctx = RenderContext::new(
    ctx,
    doc_nodes_for_module,
    UrlResolveKind::Symbol {
      file: short_path,
      symbol: namespaced_name,
    },
    Some(current_specifier),
  );
  if !namespace_paths.is_empty() {
    render_ctx = render_ctx.with_namespace(namespace_paths.to_vec())
  }

  // NOTE: `doc_nodes` should be sorted at this point.
  let symbol_group_ctx =
    SymbolGroupCtx::new(&render_ctx, doc_nodes, namespaced_name);

  (render_ctx.get_breadcrumbs(), symbol_group_ctx)
}
