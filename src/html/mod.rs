use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;
use tinytemplate::TinyTemplate;

use crate::DocNode;
use crate::DocNodeKind;

mod jsdoc;
pub mod pages;
mod parameters;
mod search;
pub mod sidepanels;
mod symbol;
mod symbols;
mod syntect_adapter;
mod types;
mod usage;
mod util;

pub use jsdoc::ModuleDocCtx;
pub use pages::generate_symbol_pages_for_module;
pub use search::generate_search_index;
pub use symbol::SymbolGroupCtx;
pub use symbols::namespace;
pub use util::compute_namespaced_symbols;
pub use util::DocNodeKindCtx;
pub use util::GlobalSymbolHrefResolver;
pub use util::ImportHrefResolver;
pub use util::NamespacedGlobalSymbols;
pub use util::RenderContext;

pub const STYLESHEET: &str = include_str!("./templates/styles.css");
pub const STYLESHEET_FILENAME: &str = "styles.css";

pub const PAGE_STYLESHEET: &str = include_str!("./templates/pages/page.css");
pub const PAGE_STYLESHEET_FILENAME: &str = "page.css";

const SEARCH_INDEX_FILENAME: &str = "search_index.js";

const FUSE_JS: &str = include_str!("./templates/pages/fuse.js");
const FUSE_FILENAME: &str = "fuse.js";

const SEARCH_JS: &str = include_str!("./templates/pages/search.js");
const SEARCH_FILENAME: &str = "search.js";

#[derive(Debug, Clone, Copy)]
pub enum UrlResolveKind<'a> {
  Root,
  AllSymbols,
  File(&'a str),
  Symbol { file: &'a str, symbol: &'a str },
}

impl UrlResolveKind<'_> {
  fn get_file(&self) -> Option<&str> {
    match self {
      UrlResolveKind::Root => None,
      UrlResolveKind::AllSymbols => None,
      UrlResolveKind::File(file) => Some(file),
      UrlResolveKind::Symbol { file, .. } => Some(file),
    }
  }
}

/// Arguments are current and target
pub type UrlResolver = Rc<dyn Fn(UrlResolveKind, UrlResolveKind) -> String>;

/// Argument is current specifier and current file
pub type UsageResolver = Rc<dyn Fn(&ModuleSpecifier, &str) -> String>;

pub fn default_url_resolver(
  current: UrlResolveKind,
  resolve: UrlResolveKind,
) -> String {
  let backs = match current {
    UrlResolveKind::Symbol { file, .. } | UrlResolveKind::File(file) => "../"
      .repeat(if file == "." {
        1
      } else {
        file.split('/').count() + 1
      }),
    UrlResolveKind::Root => String::new(),
    UrlResolveKind::AllSymbols => String::from("./"),
  };

  match resolve {
    UrlResolveKind::Root => backs,
    UrlResolveKind::AllSymbols => format!("{backs}./all_symbols.html"),
    UrlResolveKind::Symbol {
      file: target_file,
      symbol: target_symbol,
    } => {
      format!("{backs}./{target_file}/~/{target_symbol}.html")
    }
    UrlResolveKind::File(target_file) => {
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
  pub import_href_resolver: ImportHrefResolver,
  pub usage_resolver: UsageResolver,
  pub url_resolver: UrlResolver,
  pub rewrite_map: Option<IndexMap<ModuleSpecifier, String>>,
  pub hide_module_doc_title: bool,
}

pub struct GenerateCtx<'ctx> {
  pub package_name: Option<String>,
  pub common_ancestor: Option<PathBuf>,
  pub main_entrypoint: Option<ModuleSpecifier>,
  pub specifiers: Vec<ModuleSpecifier>,
  pub tt: TinyTemplate<'ctx>,
  pub syntect_adapter: syntect_adapter::SyntectAdapter,
  pub global_symbols: NamespacedGlobalSymbols,
  pub global_symbol_href_resolver: GlobalSymbolHrefResolver,
  pub import_href_resolver: ImportHrefResolver,
  pub usage_resolver: UsageResolver,
  pub url_resolver: UrlResolver,
  pub rewrite_map: Option<IndexMap<ModuleSpecifier, String>>,
  pub hide_module_doc_title: bool,
}

impl<'ctx> GenerateCtx<'ctx> {
  pub fn url_to_short_path(&self, url: &ModuleSpecifier) -> String {
    if let Some(rewrite) = self
      .rewrite_map
      .as_ref()
      .and_then(|rewrite_map| rewrite_map.get(url))
    {
      return rewrite.to_owned();
    }

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

    let path = stripped_path.to_string_lossy().to_string();

    if path.is_empty() {
      ".".to_string()
    } else {
      path
    }
  }
}

fn short_path_to_name(short_path: String) -> String {
  if short_path == "." {
    "main".to_string()
  } else {
    short_path
      .strip_prefix('.')
      .unwrap_or(&short_path)
      .strip_prefix('/')
      .unwrap_or(&short_path)
      .to_string()
  }
}

#[derive(Clone, Debug)]
pub struct DocNodeWithContext {
  pub origin: Option<String>,
  pub doc_node: DocNode,
}

pub fn setup_tt<'t>() -> Result<TinyTemplate<'t>, anyhow::Error> {
  let mut tt = TinyTemplate::new();
  tt.set_default_formatter(&tinytemplate::format_unescaped);
  tt.add_template(
    "sidepanel.html",
    include_str!("./templates/sidepanel.html"),
  )?;
  tt.add_template(
    "doc_entry.html",
    include_str!("./templates/doc_entry.html"),
  )?;
  tt.add_template("section.html", include_str!("./templates/section.html"))?;
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
  tt.add_template(
    "breadcrumbs.html",
    include_str!("./templates/breadcrumbs.html"),
  )?;
  tt.add_template("usage.html", include_str!("./templates/usage.html"))?;

  // pages
  tt.add_template(
    "pages/html_head.html",
    include_str!("./templates/pages/html_head.html"),
  )?;
  tt.add_template(
    "pages/all_symbols.html",
    include_str!("./templates/pages/all_symbols.html"),
  )?;
  tt.add_template(
    "pages/symbol.html",
    include_str!("./templates/pages/symbol.html"),
  )?;
  tt.add_template(
    "pages/index.html",
    include_str!("./templates/pages/index.html"),
  )?;
  tt.add_template(
    "pages/search_bar.html",
    include_str!("./templates/pages/search_bar.html"),
  )?;
  tt.add_template(
    "pages/search_results.html",
    include_str!("./templates/pages/search_results.html"),
  )?;

  // icons
  tt.add_template(
    "icons/copy.html",
    include_str!("./templates/icons/copy.html"),
  )?;
  Ok(tt)
}

pub fn setup_syntect() -> syntect_adapter::SyntectAdapter {
  let syntax_set: syntect::parsing::SyntaxSet =
    syntect::dumps::from_uncompressed_data(include_bytes!(
      "./default_newlines.packdump"
    ))
    .unwrap();

  syntect_adapter::SyntectAdapter {
    syntax_set,
    theme_set: syntect::highlighting::ThemeSet::load_defaults(),
  }
}

pub fn generate(
  mut options: GenerateOptions,
  doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<DocNode>>,
) -> Result<HashMap<String, String>, anyhow::Error> {
  if doc_nodes_by_url.len() == 1 && options.main_entrypoint.is_none() {
    options.main_entrypoint =
      Some(doc_nodes_by_url.keys().next().unwrap().clone());
  }

  let common_ancestor = find_common_ancestor(doc_nodes_by_url.keys(), true);
  let tt = setup_tt()?;
  let ctx = GenerateCtx {
    package_name: options.package_name,
    common_ancestor,
    main_entrypoint: options.main_entrypoint,
    specifiers: doc_nodes_by_url.keys().cloned().collect(),
    tt,
    syntect_adapter: setup_syntect(),
    global_symbols: options.global_symbols,
    global_symbol_href_resolver: options.global_symbol_href_resolver,
    import_href_resolver: options.import_href_resolver,
    usage_resolver: options.usage_resolver,
    url_resolver: options.url_resolver,
    rewrite_map: options.rewrite_map,
    hide_module_doc_title: options.hide_module_doc_title,
  };
  let mut files = HashMap::new();

  // Index page
  {
    let partitions_for_entrypoint_nodes =
      get_partitions_for_main_entrypoint(&ctx, doc_nodes_by_url);

    let index = pages::render_index(
      &ctx,
      ctx.main_entrypoint.as_ref(),
      doc_nodes_by_url,
      partitions_for_entrypoint_nodes,
      None,
    );
    files.insert("./index.html".to_string(), index);
  }

  // All symbols (list of all symbols in all files)
  {
    let all_doc_nodes = doc_nodes_by_url
      .iter()
      .flat_map(|(specifier, nodes)| {
        nodes.iter().map(|node| DocNodeWithContext {
          origin: Some(ctx.url_to_short_path(specifier)),
          doc_node: node.clone(),
        })
      })
      .collect::<Vec<DocNodeWithContext>>();

    let partitions_by_kind =
      namespace::partition_nodes_by_kind(&all_doc_nodes, true);

    let all_symbols_render =
      pages::render_all_symbols_page(&ctx, &partitions_by_kind)?;
    files.insert("./all_symbols.html".to_string(), all_symbols_render);
  }

  // Pages for all discovered symbols
  {
    for (specifier, doc_nodes) in doc_nodes_by_url {
      let short_path = ctx.url_to_short_path(specifier);

      let partitions_for_nodes =
        get_partitions_for_file(doc_nodes, &short_path);

      let symbol_pages = generate_symbol_pages_for_module(
        &ctx,
        specifier,
        &short_path,
        &partitions_for_nodes,
        doc_nodes,
      );

      files.extend(symbol_pages.into_iter().map(
        |(breadcrumbs_ctx, sidepanel_ctx, symbol_group_ctx)| {
          let root = (ctx.url_resolver)(
            UrlResolveKind::Symbol {
              file: &short_path,
              symbol: &symbol_group_ctx.name,
            },
            UrlResolveKind::Root,
          );

          let html_head_ctx = pages::HtmlHeadCtx::new(
            &root,
            &symbol_group_ctx.name,
            ctx.package_name.as_ref(),
            Some(short_path.clone()),
          );

          let file_name =
            format!("{short_path}/~/{}.html", symbol_group_ctx.name);

          let page_ctx = pages::PageCtx {
            html_head_ctx,
            sidepanel_ctx,
            symbol_group_ctx,
            breadcrumbs_ctx,
            search_ctx: serde_json::Value::Null,
          };

          let symbol_page =
            ctx.tt.render("pages/symbol.html", &page_ctx).unwrap();

          (file_name, symbol_page)
        },
      ));

      let index = pages::render_index(
        &ctx,
        Some(specifier),
        doc_nodes_by_url,
        partitions_for_nodes,
        Some(short_path.clone()),
      );

      files.insert(format!("{short_path}/~/index.html"), index);
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
        let doc_node_kind_ctx: DocNodeKindCtx = kind.into();
        (doc_node_kind_ctx.title.to_string(), nodes)
      })
      .collect()
  } else {
    categories
  }
}

pub fn get_partitions_for_main_entrypoint(
  ctx: &GenerateCtx,
  doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<DocNode>>,
) -> IndexMap<String, Vec<DocNodeWithContext>> {
  let doc_nodes = ctx
    .main_entrypoint
    .as_ref()
    .and_then(|main_entrypoint| doc_nodes_by_url.get(main_entrypoint));

  if let Some(doc_nodes) = doc_nodes {
    let doc_nodes_with_context = doc_nodes
      .iter()
      .map(|node| DocNodeWithContext {
        doc_node: node.clone(),
        origin: Some(
          ctx.url_to_short_path(ctx.main_entrypoint.as_ref().unwrap()),
        ),
      })
      .collect::<Vec<_>>();

    let categories =
      namespace::partition_nodes_by_category(&doc_nodes_with_context, false);

    if categories.len() == 1 && categories.contains_key("Uncategorized") {
      namespace::partition_nodes_by_kind(&doc_nodes_with_context, false)
        .into_iter()
        .map(|(kind, nodes)| {
          let doc_node_kind_ctx: DocNodeKindCtx = kind.into();
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

pub fn partition_nodes_by_name(
  doc_nodes: &[DocNode],
) -> IndexMap<String, Vec<DocNode>> {
  let mut partitions = IndexMap::default();

  for node in doc_nodes {
    if matches!(node.kind, DocNodeKind::ModuleDoc | DocNodeKind::Import)
      || node.declaration_kind != crate::node::DeclarationKind::Export
    {
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

pub fn find_common_ancestor<'a>(
  urls: impl Iterator<Item = &'a ModuleSpecifier>,
  single_file_is_common_ancestor: bool,
) -> Option<PathBuf> {
  let paths: Vec<PathBuf> = urls
    .filter_map(|url| {
      if url.scheme() == "file" {
        url.to_file_path().ok()
      } else {
        None
      }
    })
    .collect();

  if paths.is_empty() || paths.len() == 1 && !single_file_is_common_ancestor {
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

  if common_ancestor.as_os_str().is_empty()
    || common_ancestor == PathBuf::from("/")
  {
    None
  } else {
    Some(common_ancestor)
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[cfg(not(windows))]
  #[test]
  fn common_ancestor_root() {
    run_common_ancestor_test(
      &[
        "file:///bytes.ts",
        "file:///colors.ts",
        "file:///duration.ts",
        "file:///printf.ts",
      ],
      false,
      None,
    );
  }

  #[test]
  fn common_ancestor_single_file() {
    run_common_ancestor_test(&["file:///a/a.ts"], false, None);
  }

  #[test]
  fn common_ancestor_multiple_files() {
    run_common_ancestor_test(
      &["file:///a/a.ts", "file:///a/b.ts"],
      false,
      Some("file:///a/"),
    );
  }

  #[test]
  fn common_ancestor_single_file_single_mode() {
    run_common_ancestor_test(&["file:///a/a.ts"], true, Some("file:///a/a.ts"));
  }

  #[test]
  fn common_ancestor_multiple_file_single_mode() {
    run_common_ancestor_test(
      &["file:///a/a.ts", "file:///a/b.ts"],
      true,
      Some("file:///a/"),
    );
  }

  #[track_caller]
  fn run_common_ancestor_test(
    specifiers: &[&str],
    single_file_is_common_ancestor: bool,
    expected: Option<&str>,
  ) {
    let map = specifiers
      .iter()
      .map(|specifier| normalize_specifier(specifier))
      .collect::<Vec<_>>();

    let common_ancestor =
      find_common_ancestor(map.iter(), single_file_is_common_ancestor);
    assert_eq!(
      common_ancestor,
      expected.map(|e| normalize_specifier(e).to_file_path().unwrap()),
    );
  }

  fn normalize_specifier(specifier: &str) -> ModuleSpecifier {
    if cfg!(windows) {
      ModuleSpecifier::parse(&specifier.replace("file:///", "file:///c:/"))
        .unwrap()
    } else {
      ModuleSpecifier::parse(specifier).unwrap()
    }
  }
}
