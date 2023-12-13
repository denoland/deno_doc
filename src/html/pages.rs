use super::partition_nodes_by_name;
use super::sidepanels;
use super::DocNodeWithContext;
use super::GenerateCtx;
use super::NamespacedSymbols;
use super::RenderContext;
use super::UrlResolveKind;

use super::FUSE_FILENAME;
use super::PAGE_STYLESHEET_FILENAME;
use super::SEARCH_FILENAME;
use super::SEARCH_INDEX_FILENAME;
use super::STYLESHEET_FILENAME;

use crate::html::util::BreadcrumbsCtx;
use crate::DocNode;
use crate::DocNodeKind;
use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use serde::Serialize;

#[derive(Debug, Serialize, Clone)]
struct HtmlHeadCtx {
  title: String,
  current_file: String,
  stylesheet_url: String,
  page_stylesheet_url: String,
  url_search_index: String,
  fuse_js: String,
  url_search: String,
}

impl HtmlHeadCtx {
  fn new(
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
  // TODO(bartlomieju): needed because `tt` requires ctx for `call` blocks
  search_ctx: serde_json::Value,
}

pub fn render_index(
  ctx: &GenerateCtx,
  specifier: Option<&ModuleSpecifier>,
  doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<DocNode>>,
  partitions: IndexMap<String, Vec<DocNodeWithContext>>,
  all_symbols: NamespacedSymbols,
  file: Option<String>,
) -> Result<String, anyhow::Error> {
  let sidepanel_ctx = sidepanels::IndexSidepanelCtx::new(
    ctx,
    specifier,
    doc_nodes_by_url,
    partitions,
    &file,
  );

  let short_path = specifier.map(|specifier| ctx.url_to_short_path(specifier));

  let render_ctx = RenderContext::new(
    ctx,
    all_symbols,
    if let Some(short_path) = &short_path {
      UrlResolveKind::File(short_path)
    } else {
      UrlResolveKind::Root
    },
    specifier.cloned(),
  );

  let module_doc = super::jsdoc::ModuleDocCtx::new(
    ctx,
    &render_ctx,
    specifier,
    doc_nodes_by_url,
  );

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
    search_ctx: serde_json::Value::Null,
  };

  Ok(render_ctx.render("index.html", &index_ctx))
}

#[derive(Serialize)]
struct AllSymbolsCtx {
  html_head_ctx: HtmlHeadCtx,
  namespace_ctx: super::namespace::NamespaceRenderCtx,
  breadcrumbs_ctx: BreadcrumbsCtx,
  // TODO(bartlomieju): needed because `tt` requires ctx for `call` blocks
  search_ctx: serde_json::Value,
}

pub fn render_all_symbols(
  ctx: &GenerateCtx,
  partitions: &IndexMap<DocNodeKind, Vec<DocNodeWithContext>>,
  all_symbols: NamespacedSymbols,
) -> Result<String, anyhow::Error> {
  let render_ctx =
    RenderContext::new(ctx, all_symbols, UrlResolveKind::AllSymbols, None);
  let namespace_ctx =
    super::namespace::get_namespace_render_ctx(&render_ctx, partitions);

  let html_head_ctx =
    HtmlHeadCtx::new("./", "All Symbols", ctx.package_name.as_ref(), None);
  let all_symbols_ctx = AllSymbolsCtx {
    html_head_ctx,
    namespace_ctx,
    breadcrumbs_ctx: render_ctx.get_breadcrumbs(),
    search_ctx: serde_json::Value::Null,
  };

  Ok(render_ctx.render("all_symbols.html", &all_symbols_ctx))
}

pub fn generate_pages_for_file(
  ctx: &GenerateCtx,
  current_specifier: ModuleSpecifier,
  partitions_for_nodes: &IndexMap<String, Vec<DocNodeWithContext>>,
  short_path: String,
  doc_nodes: &[DocNode],
) -> Result<Vec<(String, String)>, anyhow::Error> {
  let name_partitions = partition_nodes_by_name(doc_nodes);
  let all_symbols = NamespacedSymbols::new(doc_nodes);

  generate_pages_inner(
    ctx,
    current_specifier,
    partitions_for_nodes,
    &short_path,
    name_partitions,
    vec![],
    all_symbols,
  )
}

pub fn generate_pages_inner(
  ctx: &GenerateCtx,
  current_specifier: ModuleSpecifier,
  partitions_for_nodes: &IndexMap<String, Vec<DocNodeWithContext>>,
  short_path: &str,
  name_partitions: IndexMap<String, Vec<DocNode>>,
  namespace_paths: Vec<String>,
  all_symbols: NamespacedSymbols,
) -> Result<Vec<(String, String)>, anyhow::Error> {
  let mut generated_pages =
    Vec::with_capacity(name_partitions.values().len() * 2);

  for (name, doc_nodes) in name_partitions.iter() {
    let file_name = if short_path.is_empty() {
      "."
    } else {
      short_path
    };
    let namespaced_name = if namespace_paths.is_empty() {
      name.to_owned()
    } else {
      format!("{}.{name}", namespace_paths.join("."))
    };
    let file_name = format!("{file_name}/~/{namespaced_name}.html");

    let sidepanel_ctx = sidepanels::SidepanelCtx::new(
      ctx,
      partitions_for_nodes,
      short_path,
      &namespaced_name,
    );

    let page = render_symbol_page(
      ctx,
      current_specifier.clone(),
      sidepanel_ctx,
      &namespace_paths,
      name,
      doc_nodes,
      all_symbols.clone(),
      short_path,
    )?;

    generated_pages.push((file_name, page));

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

      let generated = generate_pages_inner(
        ctx,
        current_specifier.clone(),
        partitions_for_nodes,
        short_path,
        namespace_name_partitions,
        namespace_paths,
        all_symbols.clone(),
      )?;
      generated_pages.extend_from_slice(&generated);
    }
  }

  Ok(generated_pages)
}

#[derive(Serialize)]
struct PageCtx {
  html_head_ctx: HtmlHeadCtx,
  sidepanel_ctx: sidepanels::SidepanelCtx,
  symbol_group_ctx: super::symbol::SymbolGroupCtx,
  breadcrumbs_ctx: BreadcrumbsCtx,
  // TODO(bartlomieju): needed because `tt` requires ctx for `call` blocks
  search_ctx: serde_json::Value,
}

fn render_symbol_page(
  ctx: &GenerateCtx,
  current_specifier: ModuleSpecifier,
  sidepanel_ctx: sidepanels::SidepanelCtx,
  namespace_paths: &[String],
  name: &str,
  doc_nodes: &[DocNode],
  all_symbols: NamespacedSymbols,
  file: &str,
) -> Result<String, anyhow::Error> {
  let namespaced_name = if namespace_paths.is_empty() {
    name.to_string()
  } else {
    format!("{}.{}", namespace_paths.join("."), name)
  };

  let mut render_ctx = RenderContext::new(
    ctx,
    all_symbols,
    UrlResolveKind::Symbol {
      file,
      symbol: &namespaced_name,
    },
    Some(current_specifier),
  );
  if !namespace_paths.is_empty() {
    render_ctx = render_ctx.with_namespace(namespace_paths.to_vec())
  }

  // NOTE: `doc_nodes` should be sorted at this point.
  let symbol_group_ctx = super::symbol::SymbolGroupCtx::new(
    &render_ctx,
    doc_nodes,
    &namespaced_name,
  );

  let root = (ctx.url_resolver)(
    UrlResolveKind::Symbol {
      file,
      symbol: &namespaced_name,
    },
    UrlResolveKind::Root,
  );

  let html_head_ctx = HtmlHeadCtx::new(
    &root,
    &namespaced_name,
    ctx.package_name.as_ref(),
    Some(file.to_string()),
  );
  let page_ctx = PageCtx {
    html_head_ctx,
    sidepanel_ctx,
    symbol_group_ctx,
    breadcrumbs_ctx: render_ctx.get_breadcrumbs(),
    search_ctx: serde_json::Value::Null,
  };

  Ok(render_ctx.render("symbol_page.html", &page_ctx))
}
