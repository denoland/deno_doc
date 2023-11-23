use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use serde::Serialize;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;
use tinytemplate::TinyTemplate;

use crate::html::jsdoc::render_markdown;
use crate::DocNode;
use crate::DocNodeKind;

mod jsdoc;
mod parameters;
mod search;
mod symbol;
mod symbols;
mod types;
mod util;

pub use search::generate_search_index;
pub use symbol::get_symbol_group_ctx;
pub use symbol::SymbolGroupCtx;
use symbols::namespace::NamespaceRenderCtx;

pub use self::symbols::namespace;
pub use self::util::DocNodeKindCtx;
pub use self::util::GlobalSymbolHrefResolver;
pub use self::util::NamespacedGlobalSymbols;
pub use self::util::NamespacedSymbols;
pub use self::util::RenderContext;

pub const STYLESHEET: &str = include_str!("./templates/styles.css");
pub const STYLESHEET_FILENAME: &str = "styles.css";

pub const PAGE_STYLESHEET: &str = include_str!("./templates/page.css");
pub const PAGE_STYLESHEET_FILENAME: &str = "page.css";

const SEARCH_INDEX_FILENAME: &str = "search_index.js";

const FUSE_JS: &str = include_str!("./templates/fuse.js");
const FUSE_FILENAME: &str = "fuse.js";

const SEARCH_JS: &str = include_str!("./templates/search.js");
const SEARCH_FILENAME: &str = "search.js";

pub enum UrlResolveKinds<'a> {
  Root,
  AllSymbols,
  Symbol {
    target_file: &'a str,
    target_symbol: &'a str,
  },
  File(&'a str),
}

pub type UrlResolver = Rc<dyn Fn(Option<&str>, UrlResolveKinds) -> String>;

pub fn default_url_resolver(
  current_file: Option<&str>,
  resolve: UrlResolveKinds,
) -> String {
  let backs = current_file
    .map(|current_file| "../".repeat(current_file.split('.').count() + 1))
    .unwrap_or_default();

  match resolve {
    UrlResolveKinds::Root => backs,
    UrlResolveKinds::AllSymbols => format!("{backs}./all_symbols.html"),
    UrlResolveKinds::Symbol {
      target_file,
      target_symbol,
    } => {
      format!("{backs}./{target_file}/~/{target_symbol}.html")
    }
    UrlResolveKinds::File(target_file) => {
      format!("{backs}./{target_file}/~/index.html")
    }
  }
}

#[derive(Clone)]
pub struct GenerateOptions {
  /// The name that is shown is the top-left corner, eg. "deno_std".
  pub package_name: Option<String>,
  /// The main entrypoint.
  /// If only a single file is specified during generation, this will always
  /// default to that file.
  pub main_entrypoint: Option<ModuleSpecifier>,
  pub global_symbols: NamespacedGlobalSymbols,
  pub global_symbol_href_resolver: GlobalSymbolHrefResolver,
  pub url_resolver: UrlResolver,
}

pub struct GenerateCtx<'ctx> {
  pub package_name: Option<String>,
  pub common_ancestor: Option<PathBuf>,
  pub tt: Rc<TinyTemplate<'ctx>>,
  pub global_symbols: NamespacedGlobalSymbols,
  pub global_symbol_href_resolver: GlobalSymbolHrefResolver,
  pub url_resolver: UrlResolver,
}

impl<'ctx> GenerateCtx<'ctx> {
  pub fn url_to_short_path(&self, url: &ModuleSpecifier) -> String {
    if url.scheme() != "file" {
      return url.to_string();
    }

    let url_file_path = url.to_file_path().unwrap();

    let Some(common_ancestor) = &self.common_ancestor else {
      return url_file_path.to_string_lossy().to_string();
    };

    let stripped_path = url_file_path
      .strip_prefix(common_ancestor)
      .unwrap_or(&url_file_path);

    stripped_path.to_string_lossy().to_string()
  }
}

#[derive(Clone, Debug)]
pub struct DocNodeWithContext {
  pub origin: Option<String>,
  pub doc_node: DocNode,
}

pub fn setup_tt<'t>() -> Result<Rc<TinyTemplate<'t>>, anyhow::Error> {
  let mut tt = TinyTemplate::new();
  tt.set_default_formatter(&tinytemplate::format_unescaped);
  tt.add_template(
    "html_head.html",
    include_str!("./templates/html_head.html"),
  )?;
  tt.add_template(
    "html_tail.html",
    include_str!("./templates/html_tail.html"),
  )?;
  tt.add_template(
    "all_symbols.html",
    include_str!("./templates/all_symbols.html"),
  )?;
  tt.add_template(
    "search_bar.html",
    include_str!("./templates/search_bar.html"),
  )?;
  tt.add_template(
    "search_results.html",
    include_str!("./templates/search_results.html"),
  )?;
  tt.add_template(
    "sidepanel.html",
    include_str!("./templates/sidepanel.html"),
  )?;
  tt.add_template("page.html", include_str!("./templates/page.html"))?;
  tt.add_template(
    "doc_entry.html",
    include_str!("./templates/doc_entry.html"),
  )?;
  tt.add_template("section.html", include_str!("./templates/section.html"))?;
  tt.add_template("index.html", include_str!("./templates/index.html"))?;
  tt.add_template(
    "index_sidepanel.html",
    include_str!("./templates/index_sidepanel.html"),
  )?;
  tt.add_template(
    "doc_node_kind_icon.html",
    include_str!("./templates/doc_node_kind_icon.html"),
  )?;
  tt.add_template(
    "namespace_section.html",
    include_str!("./templates/namespace_section.html"),
  )?;
  tt.add_template(
    "doc_block_subtitle.html",
    include_str!("./templates/doc_block_subtitle.html"),
  )?;
  tt.add_template("anchor.html", include_str!("./templates/anchor.html"))?;
  tt.add_template(
    "symbol_group.html",
    include_str!("./templates/symbol_group.html"),
  )?;
  tt.add_template("example.html", include_str!("./templates/example.html"))?;
  tt.add_template("function.html", include_str!("./templates/function.html"))?;
  tt.add_template(
    "module_doc.html",
    include_str!("./templates/module_doc.html"),
  )?;
  Ok(Rc::new(tt))
}

pub fn generate(
  mut options: GenerateOptions,
  doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<DocNode>>,
) -> Result<HashMap<String, String>, anyhow::Error> {
  if doc_nodes_by_url.len() == 1 && options.main_entrypoint.is_none() {
    options.main_entrypoint =
      Some(doc_nodes_by_url.keys().next().unwrap().clone());
  }

  let common_ancestor = find_common_ancestor(doc_nodes_by_url, false);
  let tt = setup_tt()?;
  let ctx = GenerateCtx {
    package_name: options.package_name,
    common_ancestor,
    tt,
    global_symbols: options.global_symbols,
    global_symbol_href_resolver: options.global_symbol_href_resolver,
    url_resolver: options.url_resolver,
  };
  let mut files = HashMap::new();

  // TODO(bartlomieju): remove
  let all_doc_nodes = doc_nodes_by_url
    .iter()
    .flat_map(|(specifier, nodes)| {
      nodes.iter().map(|node| DocNodeWithContext {
        origin: Some(ctx.url_to_short_path(specifier)),
        doc_node: node.clone(),
      })
    })
    .collect::<Vec<DocNodeWithContext>>();

  let all_symbols = NamespacedSymbols::new(
    &doc_nodes_by_url
      .values()
      .flatten()
      .cloned()
      .collect::<Vec<_>>(),
  );

  // Index page
  {
    let partitions_for_nodes = get_partitions_for_specifier(
      &ctx,
      options.main_entrypoint.as_ref(),
      doc_nodes_by_url,
    );

    let index = render_index(
      &ctx,
      options.main_entrypoint.as_ref(),
      doc_nodes_by_url,
      partitions_for_nodes,
      all_symbols.clone(),
      None,
    )?;
    files.insert("index.html".to_string(), index);
  }

  // All symbols (list of all symbols in all files)
  {
    let partitions_by_kind =
      namespace::partition_nodes_by_kind(&all_doc_nodes, true);

    let all_symbols_render =
      render_all_symbols(&ctx, &partitions_by_kind, all_symbols.clone())?;
    files.insert("all_symbols.html".to_string(), all_symbols_render);
  }

  // Pages for all discovered symbols
  {
    for (specifier, doc_nodes) in doc_nodes_by_url {
      let short_path = ctx.url_to_short_path(specifier);

      let partitions_for_nodes =
        get_partitions_for_file(doc_nodes, &short_path);

      let sidepanel_ctx = sidepanel_render_ctx(&ctx, &partitions_for_nodes);

      let index = render_index(
        &ctx,
        Some(specifier),
        doc_nodes_by_url,
        partitions_for_nodes,
        all_symbols.clone(),
        Some(short_path.clone()),
      )?;

      files.insert(format!("{short_path}/~/index.html"), index);

      files.extend(generate_pages_for_file(
        &ctx,
        &sidepanel_ctx,
        ctx.url_to_short_path(specifier),
        doc_nodes,
      )?);
    }
  }

  files.insert(STYLESHEET_FILENAME.into(), STYLESHEET.into());
  files.insert(PAGE_STYLESHEET_FILENAME.into(), PAGE_STYLESHEET.into());
  files.insert(
    SEARCH_INDEX_FILENAME.into(),
    search::get_search_index_file(&ctx, doc_nodes_by_url)?,
  );
  files.insert(FUSE_FILENAME.into(), FUSE_JS.into());
  files.insert(SEARCH_FILENAME.into(), SEARCH_JS.into());

  Ok(files)
}

pub fn get_partitions_for_file(
  doc_nodes: &[DocNode],
  short_path: &str,
) -> IndexMap<String, Vec<DocNodeWithContext>> {
  let doc_nodes_with_context = doc_nodes
    .iter()
    .map(|node| DocNodeWithContext {
      doc_node: node.clone(),
      origin: Some(short_path.to_owned()),
    })
    .collect::<Vec<_>>();

  let categories =
    namespace::partition_nodes_by_category(&doc_nodes_with_context, false);

  if categories.len() == 1 && categories.contains_key("Uncategorized") {
    namespace::partition_nodes_by_kind(&doc_nodes_with_context, false)
      .into_iter()
      .map(|(kind, nodes)| {
        let doc_node_kind_ctx: util::DocNodeKindCtx = kind.into();
        (doc_node_kind_ctx.title.to_string(), nodes)
      })
      .collect()
  } else {
    categories
  }
}

pub fn get_partitions_for_specifier(
  ctx: &GenerateCtx,
  main_entrypoint: Option<&ModuleSpecifier>,
  doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<DocNode>>,
) -> IndexMap<String, Vec<DocNodeWithContext>> {
  let doc_nodes = main_entrypoint
    .and_then(|main_entrypoint| doc_nodes_by_url.get(main_entrypoint));

  if let Some(doc_nodes) = doc_nodes {
    let doc_nodes_with_context = doc_nodes
      .iter()
      .map(|node| DocNodeWithContext {
        doc_node: node.clone(),
        origin: Some(ctx.url_to_short_path(main_entrypoint.as_ref().unwrap())),
      })
      .collect::<Vec<_>>();

    let categories =
      namespace::partition_nodes_by_category(&doc_nodes_with_context, false);

    if categories.len() == 1 && categories.contains_key("Uncategorized") {
      namespace::partition_nodes_by_kind(&doc_nodes_with_context, false)
        .into_iter()
        .map(|(kind, nodes)| {
          let doc_node_kind_ctx: util::DocNodeKindCtx = kind.into();
          (doc_node_kind_ctx.title.to_string(), nodes)
        })
        .collect()
    } else {
      categories
    }
  } else {
    Default::default()
  }
}

fn generate_pages_inner(
  ctx: &GenerateCtx,
  sidepanel_ctx: &SidepanelRenderCtx,
  file: &str,
  name_partitions: IndexMap<String, Vec<DocNode>>,
  namespace_paths: Vec<String>,
  all_symbols: NamespacedSymbols,
) -> Result<Vec<(String, String)>, anyhow::Error> {
  let mut generated_pages =
    Vec::with_capacity(name_partitions.values().len() * 2);

  for (name, doc_nodes) in name_partitions.iter() {
    let file_name = if namespace_paths.is_empty() {
      format!("{file}/~/{}.html", name)
    } else {
      format!("{file}/~/{}.{name}.html", namespace_paths.join("."))
    };

    let page = render_page(
      ctx,
      sidepanel_ctx,
      &namespace_paths,
      name,
      doc_nodes,
      all_symbols.clone(),
      file,
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
        sidepanel_ctx,
        file,
        namespace_name_partitions,
        namespace_paths,
        all_symbols.clone(),
      )?;
      generated_pages.extend_from_slice(&generated);
    }
  }

  Ok(generated_pages)
}

fn generate_pages_for_file(
  ctx: &GenerateCtx,
  sidepanel_ctx: &SidepanelRenderCtx,
  file: String,
  doc_nodes: &[DocNode],
) -> Result<Vec<(String, String)>, anyhow::Error> {
  let name_partitions = partition_nodes_by_name(doc_nodes);
  let all_symbols = NamespacedSymbols::new(doc_nodes);

  generate_pages_inner(
    ctx,
    sidepanel_ctx,
    &file,
    name_partitions,
    vec![],
    all_symbols,
  )
}

// TODO(bartlomieju): move a separate module?
#[derive(Debug, Serialize, Clone)]
struct HtmlHeadCtx {
  title: String,
  current_file: String,
  stylesheet_url: String,
  page_stylesheet_url: String,
}

#[derive(Debug, Serialize, Clone)]
struct HtmlTailCtx {
  url_search_index: String,
  fuse_js: String,
  url_search: String,
}

#[derive(Debug, Serialize, Clone)]
pub struct SidepanelPartitionNodeCtx {
  kind: util::DocNodeKindCtx,
  name: String,
  href: String,
}

#[derive(Debug, Serialize, Clone)]
pub struct SidepanelPartitionCtx {
  name: String,
  symbols: Vec<SidepanelPartitionNodeCtx>,
}

#[derive(Debug, Serialize, Clone)]
pub struct IndexSidepanelFileCtx {
  name: String,
  href: String,
}

#[derive(Debug, Serialize, Clone)]
pub struct IndexSidepanelCtx {
  package_name: Option<String>,
  root_url: String,
  all_symbols_url: String,
  kind_partitions: Vec<SidepanelPartitionCtx>,
  files: Vec<IndexSidepanelFileCtx>,
}

#[derive(Debug, Serialize, Clone)]
struct IndexCtx {
  html_head_ctx: HtmlHeadCtx,
  html_tail_ctx: HtmlTailCtx,
  sidepanel_ctx: IndexSidepanelCtx,
  module_doc: Option<ModuleDocCtx>,
  // TODO(bartlomieju): needed because `tt` requires ctx for `call` blocks
  search_ctx: serde_json::Value,
}

#[derive(Debug, Serialize, Clone)]
pub struct ModuleDocCtx {
  url: String,
  docs: String,
}

pub fn get_module_doc(
  ctx: &GenerateCtx,
  render_ctx: &RenderContext,
  specifier: Option<&ModuleSpecifier>,
  doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<DocNode>>,
) -> Option<ModuleDocCtx> {
  if let Some(main_entrypoint) = specifier {
    let module_doc_nodes = doc_nodes_by_url.get(main_entrypoint).unwrap();

    let docs = module_doc_nodes
      .iter()
      .find(|n| n.kind == DocNodeKind::ModuleDoc);

    docs
      .and_then(|node| node.js_doc.doc.as_ref())
      .map(|docs_md| {
        let rendered_docs = render_markdown(docs_md, render_ctx);

        ModuleDocCtx {
          url: ctx.url_to_short_path(main_entrypoint),
          docs: rendered_docs,
        }
      })
  } else {
    None
  }
}

pub fn get_index_sidepanel_ctx(
  ctx: &GenerateCtx,
  main_entrypoint: Option<&ModuleSpecifier>,
  doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<DocNode>>,
  partitions: IndexMap<String, Vec<DocNodeWithContext>>,
  current_file: &Option<String>,
) -> IndexSidepanelCtx {
  let files = doc_nodes_by_url
    .keys()
    .filter(|url| {
      main_entrypoint
        .map(|main_entrypoint| *url != main_entrypoint)
        .unwrap_or(true)
    })
    .map(|url| {
      let short_path = ctx.url_to_short_path(url);
      IndexSidepanelFileCtx {
        href: (ctx.url_resolver)(
          current_file.as_deref(),
          UrlResolveKinds::File(&short_path),
        ),
        name: short_path,
      }
    })
    .collect::<Vec<_>>();

  let kind_partitions = partitions
    .into_iter()
    .map(|(name, nodes)| {
      let symbols = nodes
        .into_iter()
        .map(|node| SidepanelPartitionNodeCtx {
          kind: node.doc_node.kind.into(),
          href: (ctx.url_resolver)(
            current_file.as_deref(),
            UrlResolveKinds::Symbol {
              target_file: node.origin.as_deref().unwrap(),
              target_symbol: &node.doc_node.name,
            },
          ),
          name: node.doc_node.name,
        })
        .collect::<Vec<_>>();
      SidepanelPartitionCtx { name, symbols }
    })
    .collect::<Vec<_>>();

  IndexSidepanelCtx {
    package_name: ctx.package_name.clone(),
    root_url: (ctx.url_resolver)(
      current_file.as_deref(),
      UrlResolveKinds::Root,
    ),
    all_symbols_url: (ctx.url_resolver)(
      current_file.as_deref(),
      UrlResolveKinds::AllSymbols,
    ),
    kind_partitions,
    files,
  }
}

fn render_index(
  ctx: &GenerateCtx,
  specifier: Option<&ModuleSpecifier>,
  doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<DocNode>>,
  partitions: IndexMap<String, Vec<DocNodeWithContext>>,
  all_symbols: NamespacedSymbols,
  file: Option<String>,
) -> Result<String, anyhow::Error> {
  let sidepanel_ctx = get_index_sidepanel_ctx(
    ctx,
    specifier,
    doc_nodes_by_url,
    partitions,
    &file,
  );

  let render_ctx = RenderContext::new(
    ctx,
    all_symbols,
    specifier.map(|specifier| ctx.url_to_short_path(specifier)),
  );

  let module_doc =
    get_module_doc(ctx, &render_ctx, specifier, doc_nodes_by_url);

  let root = (ctx.url_resolver)(file.as_deref(), UrlResolveKinds::Root);

  // TODO(bartlomieju): dedup with `render_page`
  let html_head_ctx = HtmlHeadCtx {
    title: format!(
      "Index - {}documentation",
      ctx
        .package_name
        .as_ref()
        .map(|package_name| format!("{package_name} "))
        .unwrap_or_default()
    ),
    current_file: "".to_string(),
    stylesheet_url: format!("{root}{STYLESHEET_FILENAME}"),
    page_stylesheet_url: format!("{root}{PAGE_STYLESHEET_FILENAME}"),
  };
  let html_tail_ctx = HtmlTailCtx {
    url_search_index: format!("{root}{SEARCH_INDEX_FILENAME}"),
    fuse_js: format!("{root}{FUSE_FILENAME}"),
    url_search: format!("{root}{SEARCH_FILENAME}"),
  };

  let index_ctx = IndexCtx {
    html_head_ctx,
    html_tail_ctx,
    sidepanel_ctx,
    module_doc,
    search_ctx: serde_json::Value::Null,
  };

  Ok(render_ctx.render("index.html", &index_ctx))
}

#[derive(Serialize)]
struct AllSymbolsCtx {
  html_head_ctx: HtmlHeadCtx,
  html_tail_ctx: HtmlTailCtx,
  namespace_ctx: NamespaceRenderCtx,
  // TODO(bartlomieju): needed because `tt` requires ctx for `call` blocks
  search_ctx: serde_json::Value,
}

fn render_all_symbols(
  ctx: &GenerateCtx,
  partitions: &IndexMap<DocNodeKind, Vec<DocNodeWithContext>>,
  all_symbols: NamespacedSymbols,
) -> Result<String, anyhow::Error> {
  let render_ctx = RenderContext::new(ctx, all_symbols, None);
  let namespace_ctx =
    namespace::get_namespace_render_ctx(&render_ctx, partitions);

  // TODO(bartlomieju): dedup with `render_page`
  let html_head_ctx = HtmlHeadCtx {
    title: format!(
      "All Symbols - {}documentation",
      ctx
        .package_name
        .as_ref()
        .map(|package_name| format!("{package_name} "))
        .unwrap_or_default()
    ),
    current_file: "".to_string(),
    stylesheet_url: format!("./{STYLESHEET_FILENAME}"),
    page_stylesheet_url: format!("./{PAGE_STYLESHEET_FILENAME}"),
  };
  let html_tail_ctx = HtmlTailCtx {
    url_search_index: format!("./{SEARCH_INDEX_FILENAME}"),
    fuse_js: format!("./{FUSE_FILENAME}"),
    url_search: format!("./{SEARCH_FILENAME}"),
  };
  let all_symbols_ctx = AllSymbolsCtx {
    html_head_ctx,
    html_tail_ctx,
    namespace_ctx,
    search_ctx: serde_json::Value::Null,
  };

  Ok(render_ctx.render("all_symbols.html", &all_symbols_ctx))
}

pub fn partition_nodes_by_name(
  doc_nodes: &[DocNode],
) -> IndexMap<String, Vec<DocNode>> {
  let mut partitions = IndexMap::default();

  for node in doc_nodes {
    if node.kind == DocNodeKind::ModuleDoc {
      continue;
    }

    partitions
      .entry(node.name.clone())
      .or_insert(vec![])
      .push(node.clone());
  }

  for val in partitions.values_mut() {
    val.sort_by_key(|n| n.name.to_string());
  }

  partitions.sort_by(|k1, _v1, k2, _v2| k1.cmp(k2));

  partitions
}

#[derive(Serialize)]
struct PageCtx {
  html_head_ctx: HtmlHeadCtx,
  html_tail_ctx: HtmlTailCtx,
  sidepanel_ctx: SidepanelRenderCtx,
  symbol_group_ctx: SymbolGroupCtx,
  // TODO(bartlomieju): needed because `tt` requires ctx for `call` blocks
  search_ctx: serde_json::Value,
}

fn render_page(
  ctx: &GenerateCtx,
  sidepanel_ctx: &SidepanelRenderCtx,
  namespace_paths: &[String],
  name: &str,
  doc_nodes: &[DocNode],
  all_symbols: NamespacedSymbols,
  file: &str,
) -> Result<String, anyhow::Error> {
  let mut render_ctx =
    RenderContext::new(ctx, all_symbols, Some(file.to_string()));
  if !namespace_paths.is_empty() {
    render_ctx = render_ctx.with_namespace(namespace_paths.to_vec())
  }

  let namespaced_name = if namespace_paths.is_empty() {
    name.to_string()
  } else {
    format!("{}.{}", namespace_paths.join("."), name)
  };

  // NOTE: `doc_nodes` should be sorted at this point.
  let symbol_group_ctx =
    get_symbol_group_ctx(&render_ctx, doc_nodes, &namespaced_name);

  let root = (ctx.url_resolver)(Some(file), UrlResolveKinds::Root);

  // TODO(bartlomieju): dedup with `render_page`
  let html_head_ctx = HtmlHeadCtx {
    title: format!(
      "{} - {} documentation",
      namespaced_name,
      ctx
        .package_name
        .as_ref()
        .map(|package_name| format!("{package_name} "))
        .unwrap_or_default()
    ),
    current_file: file.to_string(),
    stylesheet_url: format!("{root}{STYLESHEET_FILENAME}"),
    page_stylesheet_url: format!("{root}{PAGE_STYLESHEET_FILENAME}"),
  };
  let html_tail_ctx = HtmlTailCtx {
    url_search_index: format!("{root}{SEARCH_INDEX_FILENAME}"),
    fuse_js: format!("{root}{FUSE_FILENAME}"),
    url_search: format!("{root}{SEARCH_FILENAME}"),
  };
  let page_ctx = PageCtx {
    html_head_ctx,
    html_tail_ctx,
    sidepanel_ctx: sidepanel_ctx.clone(),
    symbol_group_ctx,
    search_ctx: serde_json::Value::Null,
  };

  Ok(render_ctx.render("page.html", &page_ctx))
}

#[derive(Debug, Serialize, Clone)]
pub struct SidepanelRenderCtx {
  package_name: Option<String>,
  partitions: Vec<SidepanelPartitionCtx>,
}

pub fn sidepanel_render_ctx(
  ctx: &GenerateCtx,
  partitions: &IndexMap<String, Vec<DocNodeWithContext>>,
) -> SidepanelRenderCtx {
  let partitions = partitions
    .into_iter()
    .map(|(name, nodes)| {
      let symbols = nodes
        .iter()
        .map(|node| SidepanelPartitionNodeCtx {
          kind: node.doc_node.kind.into(),
          href: format!("{}.html", node.doc_node.name),
          name: node.doc_node.name.clone(),
        })
        .collect::<Vec<_>>();
      SidepanelPartitionCtx {
        name: name.clone(),
        symbols,
      }
    })
    .collect();

  SidepanelRenderCtx {
    package_name: ctx.package_name.clone(),
    partitions,
  }
}

pub fn find_common_ancestor(
  doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<DocNode>>,
  single_file_is_common_ancestor: bool,
) -> Option<PathBuf> {
  let paths: Vec<PathBuf> = doc_nodes_by_url
    .keys()
    .filter_map(|url| {
      if url.scheme() == "file" {
        Some(url.to_file_path().unwrap())
      } else {
        None
      }
    })
    .collect();

  assert!(!paths.is_empty());

  if paths.len() == 1 && !single_file_is_common_ancestor {
    return None;
  }

  let shortest_path = paths
    .iter()
    .min_by_key(|path| path.components().count())
    .unwrap();

  let mut common_ancestor = PathBuf::new();

  for (index, component) in shortest_path.components().enumerate() {
    if paths.iter().all(|path| {
      path.components().count() > index
        && path.components().nth(index) == Some(component)
    }) {
      common_ancestor.push(component);
    } else {
      break;
    }
  }

  if common_ancestor.as_os_str().is_empty() {
    None
  } else {
    Some(common_ancestor)
  }
}
