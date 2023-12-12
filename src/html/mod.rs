use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;
use tinytemplate::TinyTemplate;

use crate::DocNode;
use crate::DocNodeKind;

mod jsdoc;
mod pages;
mod parameters;
mod search;
pub mod sidepanels;
mod symbol;
mod symbols;
mod types;
mod util;

pub use jsdoc::ModuleDocCtx;
pub use search::generate_search_index;
pub use symbol::SymbolGroupCtx;
pub use symbols::namespace;
pub use util::DocNodeKindCtx;
pub use util::GlobalSymbolHrefResolver;
pub use util::NamespacedGlobalSymbols;
pub use util::NamespacedSymbols;
pub use util::RenderContext;

pub const STYLESHEET: &str = include_str!("./templates/styles.css");
pub const STYLESHEET_FILENAME: &str = "styles.css";

pub const PAGE_STYLESHEET: &str = include_str!("./templates/page.css");
pub const PAGE_STYLESHEET_FILENAME: &str = "page.css";

const SEARCH_INDEX_FILENAME: &str = "search_index.js";

const FUSE_JS: &str = include_str!("./templates/fuse.js");
const FUSE_FILENAME: &str = "fuse.js";

const SEARCH_JS: &str = include_str!("./templates/search.js");
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

pub fn default_url_resolver(
  current: UrlResolveKind,
  resolve: UrlResolveKind,
) -> String {
  let backs = match current {
    UrlResolveKind::Symbol { file, .. } | UrlResolveKind::File(file) => "../"
      .repeat(if file == "." {
        1
      } else {
        file.split('.').count() + 1
      }),
    UrlResolveKind::Root | UrlResolveKind::AllSymbols => String::new(),
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
  pub url_resolver: UrlResolver,
  pub rewrite_map: Option<IndexMap<ModuleSpecifier, String>>,
}

pub struct GenerateCtx<'ctx> {
  pub package_name: Option<String>,
  pub common_ancestor: Option<PathBuf>,
  pub main_entrypoint: Option<ModuleSpecifier>,
  pub tt: Rc<TinyTemplate<'ctx>>,
  pub global_symbols: NamespacedGlobalSymbols,
  pub global_symbol_href_resolver: GlobalSymbolHrefResolver,
  pub url_resolver: UrlResolver,
  pub rewrite_map: Option<IndexMap<ModuleSpecifier, String>>,
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

    stripped_path.to_string_lossy().to_string()
  }
}

fn short_path_to_name(short_path: String) -> String {
  if short_path == "." || short_path.is_empty() {
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

  let common_ancestor = find_common_ancestor(doc_nodes_by_url.keys(), true);
  let tt = setup_tt()?;
  let ctx = GenerateCtx {
    package_name: options.package_name,
    common_ancestor,
    main_entrypoint: options.main_entrypoint,
    tt,
    global_symbols: options.global_symbols,
    global_symbol_href_resolver: options.global_symbol_href_resolver,
    url_resolver: options.url_resolver,
    rewrite_map: options.rewrite_map,
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
    let partitions_for_entrypoint_nodes =
      get_partitions_for_main_entrypoint(&ctx, doc_nodes_by_url);

    let index = pages::render_index(
      &ctx,
      ctx.main_entrypoint.as_ref(),
      doc_nodes_by_url,
      partitions_for_entrypoint_nodes,
      all_symbols.clone(),
      None,
    )?;
    files.insert("./index.html".to_string(), index);
  }

  // All symbols (list of all symbols in all files)
  {
    let partitions_by_kind =
      namespace::partition_nodes_by_kind(&all_doc_nodes, true);

    let all_symbols_render = pages::render_all_symbols(
      &ctx,
      &partitions_by_kind,
      all_symbols.clone(),
    )?;
    files.insert("./all_symbols.html".to_string(), all_symbols_render);
  }

  // Pages for all discovered symbols
  {
    for (specifier, doc_nodes) in doc_nodes_by_url {
      let short_path = ctx.url_to_short_path(specifier);

      let partitions_for_nodes =
        get_partitions_for_file(doc_nodes, &short_path);

      files.extend(pages::generate_pages_for_file(
        &ctx,
        &partitions_for_nodes,
        short_path.clone(),
        doc_nodes,
      )?);

      let index = pages::render_index(
        &ctx,
        Some(specifier),
        doc_nodes_by_url,
        partitions_for_nodes,
        all_symbols.clone(),
        Some(short_path.clone()),
      )?;

      files.insert(
        format!(
          "{}/~/index.html",
          if short_path.is_empty() {
            "."
          } else {
            &short_path
          }
        ),
        index,
      );
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
