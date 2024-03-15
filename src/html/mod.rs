use deno_ast::ModuleSpecifier;
use handlebars::handlebars_helper;
use handlebars::Handlebars;
use indexmap::IndexMap;
use std::borrow::Cow;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

use crate::DocNode;

pub mod comrak_adapters;
pub mod jsdoc;
pub mod pages;
mod parameters;
pub mod partition;
mod render_context;
mod search;
pub mod sidepanels;
mod symbols;
#[cfg(feature = "tree-sitter")]
mod tree_sitter;
mod types;
mod usage;
mod util;

//pub use pages::generate_symbol_page;
pub use pages::generate_symbol_pages_for_module;
pub use render_context::RenderContext;
pub use search::generate_search_index;
pub use symbols::namespace;
pub use symbols::SymbolContentCtx;
pub use symbols::SymbolGroupCtx;
pub use usage::usage_to_md;
pub use util::compute_namespaced_symbols;
pub use util::DocNodeKindCtx;
pub use util::HrefResolver;
pub use util::NamespacedGlobalSymbols;
pub use util::UrlResolveKind;

pub const STYLESHEET: &str = include_str!("./templates/styles.gen.css");
pub const STYLESHEET_FILENAME: &str = "styles.css";

pub const PAGE_STYLESHEET: &str =
  include_str!("./templates/pages/page.gen.css");
pub const PAGE_STYLESHEET_FILENAME: &str = "page.css";

const SEARCH_INDEX_FILENAME: &str = "search_index.js";

pub const SCRIPT_JS: &str = include_str!("./templates/script.js");
pub const SCRIPT_FILENAME: &str = "script.js";

const FUSE_JS: &str = include_str!("./templates/pages/fuse.js");
const FUSE_FILENAME: &str = "fuse.js";

const SEARCH_JS: &str = include_str!("./templates/pages/search.js");
const SEARCH_FILENAME: &str = "search.js";

pub type UsageComposer = Rc<
  dyn Fn(
    &RenderContext,
    &[DocNodeWithContext],
    String,
  ) -> IndexMap<String, String>,
>;

#[derive(Clone)]
pub struct GenerateOptions {
  /// The name that is shown is the top-left corner, eg. "deno_std".
  pub package_name: Option<String>,
  /// The main entrypoint.
  /// If only a single file is specified during generation, this will always
  /// default to that file.
  pub main_entrypoint: Option<ModuleSpecifier>,
  pub href_resolver: Rc<dyn HrefResolver>,
  pub usage_composer: Option<UsageComposer>,
  pub rewrite_map: Option<IndexMap<ModuleSpecifier, String>>,
  pub hide_module_doc_title: bool,
  pub sidebar_flatten_namespaces: bool,
}

pub struct GenerateCtx<'ctx> {
  pub package_name: Option<String>,
  pub common_ancestor: Option<PathBuf>,
  pub main_entrypoint: Option<ModuleSpecifier>,
  pub specifiers: Vec<ModuleSpecifier>,
  pub hbs: Handlebars<'ctx>,
  pub highlight_adapter: comrak_adapters::HighlightAdapter,
  #[cfg(feature = "ammonia")]
  pub url_rewriter: Option<comrak_adapters::URLRewriter>,
  pub href_resolver: Rc<dyn HrefResolver>,
  pub usage_composer: Option<UsageComposer>,
  pub rewrite_map: Option<IndexMap<ModuleSpecifier, String>>,
  pub hide_module_doc_title: bool,
  pub single_file_mode: bool,
  pub sidebar_hide_all_symbols: bool,
  pub sidebar_flatten_namespaces: bool,
}

impl<'ctx> GenerateCtx<'ctx> {
  pub fn url_to_short_path(&self, url: &ModuleSpecifier) -> ShortPath {
    if let Some(rewrite) = self
      .rewrite_map
      .as_ref()
      .and_then(|rewrite_map| rewrite_map.get(url))
    {
      return rewrite.to_owned().into();
    }

    if url.scheme() != "file" {
      return url.to_string().into();
    }

    let url_file_path = url.to_file_path().unwrap();

    let Some(common_ancestor) = &self.common_ancestor else {
      return url_file_path.to_string_lossy().to_string().into();
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
    .into()
  }

  pub fn doc_nodes_by_url_add_context<'a>(
    &self,
    doc_nodes_by_url: &'a IndexMap<ModuleSpecifier, Vec<DocNode>>,
  ) -> IndexMap<ModuleSpecifier, Vec<DocNodeWithContext<'a>>> {
    doc_nodes_by_url
      .iter()
      .map(|(specifier, nodes)| {
        (
          specifier.clone(),
          nodes
            .iter()
            .map(|node| DocNodeWithContext {
              origin: Rc::new(self.url_to_short_path(specifier)),
              ns_qualifiers: Rc::new(vec![]),
              inner: Cow::Borrowed(node),
            })
            .collect::<Vec<_>>(),
        )
      })
      .collect::<IndexMap<_, _>>()
  }
}

#[derive(Clone, Debug)]
pub struct ShortPath(String);

impl ShortPath {
  pub fn to_name(&self) -> String {
    if self.0.is_empty() || self.0 == "." {
      "main".to_string()
    } else {
      self
        .0
        .strip_prefix('.')
        .unwrap_or(&self.0)
        .strip_prefix('/')
        .unwrap_or(&self.0)
        .to_string()
    }
  }

  pub fn as_str(&self) -> &str {
    &self.0
  }
}

impl From<String> for ShortPath {
  fn from(value: String) -> Self {
    ShortPath(value)
  }
}

#[derive(Clone, Debug)]
pub struct DocNodeWithContext<'a> {
  pub origin: Rc<ShortPath>,
  pub ns_qualifiers: Rc<Vec<String>>,
  pub inner: Cow<'a, DocNode>,
}

impl<'a> core::ops::Deref for DocNodeWithContext<'a> {
  type Target = DocNode;

  fn deref(&self) -> &Self::Target {
    &*self.inner
  }
}

pub fn setup_hbs<'t>() -> Result<Handlebars<'t>, anyhow::Error> {
  let mut reg = Handlebars::new();
  reg.register_escape_fn(|str| html_escape::encode_safe(str).to_string());
  reg.set_strict_mode(true);

  #[cfg(debug_assertions)]
  reg.set_dev_mode(true);

  handlebars_helper!(concat: |a: str, b: str| format!("{a}{b}"));
  reg.register_helper("concat", Box::new(concat));

  handlebars_helper!(print: |a: Json| println!("{a:#?}"));
  reg.register_helper("print", Box::new(print));

  reg.register_template_string(
    "sidepanel_common",
    include_str!("./templates/sidepanel_common.hbs"),
  )?;
  reg.register_template_string(
    "sidepanel",
    include_str!("./templates/sidepanel.hbs"),
  )?;
  reg.register_template_string(
    "doc_entry",
    include_str!("./templates/doc_entry.hbs"),
  )?;
  reg.register_template_string(
    "section",
    include_str!("./templates/section.hbs"),
  )?;
  reg.register_template_string(
    "index_sidepanel",
    include_str!("./templates/index_sidepanel.hbs"),
  )?;
  reg.register_template_string(
    "doc_node_kind_icon",
    include_str!("./templates/doc_node_kind_icon.hbs"),
  )?;
  reg.register_template_string(
    "namespace_section",
    include_str!("./templates/namespace_section.hbs"),
  )?;
  reg.register_template_string(
    "doc_block_subtitle_class",
    include_str!("./templates/doc_block_subtitle_class.hbs"),
  )?;
  reg.register_template_string(
    "doc_block_subtitle_interface",
    include_str!("./templates/doc_block_subtitle_interface.hbs"),
  )?;
  reg.register_template_string(
    "anchor",
    include_str!("./templates/anchor.hbs"),
  )?;
  reg.register_template_string(
    "symbol_group",
    include_str!("./templates/symbol_group.hbs"),
  )?;
  reg.register_template_string(
    "symbol_content",
    include_str!("./templates/symbol_content.hbs"),
  )?;
  reg.register_template_string(
    "example",
    include_str!("./templates/example.hbs"),
  )?;
  reg.register_template_string(
    "function",
    include_str!("./templates/function.hbs"),
  )?;
  reg.register_template_string(
    "module_doc",
    include_str!("./templates/module_doc.hbs"),
  )?;
  reg.register_template_string(
    "breadcrumbs",
    include_str!("./templates/breadcrumbs.hbs"),
  )?;
  reg.register_template_string(
    "usages",
    include_str!("./templates/usages.hbs"),
  )?;
  reg.register_template_string("tag", include_str!("./templates/tag.hbs"))?;
  reg.register_template_string(
    "source_button",
    include_str!("./templates/source_button.hbs"),
  )?;
  reg.register_template_string(
    "deprecated",
    include_str!("./templates/deprecated.hbs"),
  )?;
  reg.register_template_string(
    "index_signature",
    include_str!("./templates/index_signature.hbs"),
  )?;

  // pages
  reg.register_template_string(
    "pages/html_head",
    include_str!("./templates/pages/html_head.hbs"),
  )?;
  reg.register_template_string(
    "pages/all_symbols",
    include_str!("./templates/pages/all_symbols.hbs"),
  )?;
  reg.register_template_string(
    "pages/symbol",
    include_str!("./templates/pages/symbol.hbs"),
  )?;
  reg.register_template_string(
    "pages/index",
    include_str!("./templates/pages/index.hbs"),
  )?;
  reg.register_template_string(
    "pages/search_bar",
    include_str!("./templates/pages/search_bar.hbs"),
  )?;
  reg.register_template_string(
    "pages/search_results",
    include_str!("./templates/pages/search_results.hbs"),
  )?;

  // icons
  reg.register_template_string(
    "icons/copy",
    include_str!("./templates/icons/copy.svg"),
  )?;
  reg.register_template_string(
    "icons/link",
    include_str!("./templates/icons/link.svg"),
  )?;
  reg.register_template_string(
    "icons/source",
    include_str!("./templates/icons/source.svg"),
  )?;
  reg.register_template_string(
    "icons/menu",
    include_str!("./templates/icons/menu.svg"),
  )?;

  Ok(reg)
}

pub fn setup_highlighter(
  show_line_numbers: bool,
) -> comrak_adapters::HighlightAdapter {
  comrak_adapters::HighlightAdapter {
    #[cfg(feature = "syntect")]
    syntax_set: syntect::dumps::from_uncompressed_data(include_bytes!(
      "./default_newlines.packdump"
    ))
    .unwrap(),
    #[cfg(feature = "syntect")]
    theme_set: syntect::highlighting::ThemeSet::load_defaults(),
    #[cfg(feature = "tree-sitter")]
    language_cb: tree_sitter::tree_sitter_language_cb,
    show_line_numbers,
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
  let ctx = GenerateCtx {
    package_name: options.package_name,
    common_ancestor,
    main_entrypoint: options.main_entrypoint,
    specifiers: doc_nodes_by_url.keys().cloned().collect(),
    hbs: setup_hbs()?,
    highlight_adapter: setup_highlighter(false),
    #[cfg(feature = "ammonia")]
    url_rewriter: None,
    href_resolver: options.href_resolver,
    usage_composer: options.usage_composer,
    rewrite_map: options.rewrite_map,
    hide_module_doc_title: options.hide_module_doc_title,
    single_file_mode: doc_nodes_by_url.len() == 1,
    sidebar_hide_all_symbols: false,
    sidebar_flatten_namespaces: options.sidebar_flatten_namespaces,
  };
  let mut files = HashMap::new();

  let doc_nodes_by_url = ctx.doc_nodes_by_url_add_context(doc_nodes_by_url);

  // Index page
  {
    let partitions_for_entrypoint_nodes =
      partition::get_partitions_for_main_entrypoint(&ctx, &doc_nodes_by_url);

    let index = pages::render_index(
      &ctx,
      ctx.main_entrypoint.as_ref(),
      &doc_nodes_by_url,
      partitions_for_entrypoint_nodes,
      None,
    );
    files.insert("./index.html".to_string(), index);
  }

  // All symbols (list of all symbols in all files)
  {
    let all_doc_nodes = doc_nodes_by_url
      .values()
      .flatten()
      .cloned()
      .collect::<Vec<_>>();

    let partitions_by_kind =
      partition::partition_nodes_by_kind(&all_doc_nodes, true);

    let all_symbols_render =
      pages::render_all_symbols_page(&ctx, partitions_by_kind);
    files.insert("./all_symbols.html".to_string(), all_symbols_render);
  }

  // Pages for all discovered symbols
  {
    for (specifier, doc_nodes) in &doc_nodes_by_url {
      let short_path = ctx.url_to_short_path(specifier);

      let partitions_for_nodes =
        partition::get_partitions_for_file(&ctx, doc_nodes);

      let symbol_pages = generate_symbol_pages_for_module(
        &ctx,
        specifier,
        &short_path,
        &partitions_for_nodes,
        doc_nodes,
      );

      files.extend(symbol_pages.into_iter().map(
        |(breadcrumbs_ctx, sidepanel_ctx, symbol_group_ctx)| {
          let root = ctx.href_resolver.resolve_path(
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
            format!("{}/~/{}.html", short_path.as_str(), symbol_group_ctx.name);

          let page_ctx = pages::PageCtx {
            html_head_ctx,
            sidepanel_ctx,
            symbol_group_ctx,
            breadcrumbs_ctx,
          };

          let symbol_page = ctx.hbs.render("pages/symbol", &page_ctx).unwrap();

          (file_name, symbol_page)
        },
      ));

      let index = pages::render_index(
        &ctx,
        Some(specifier),
        &doc_nodes_by_url,
        partitions_for_nodes,
        Some(short_path.clone()),
      );

      files.insert(format!("{}/~/index.html", short_path.as_str()), index);
    }
  }

  files.insert(STYLESHEET_FILENAME.into(), STYLESHEET.into());
  files.insert(PAGE_STYLESHEET_FILENAME.into(), PAGE_STYLESHEET.into());
  files.insert(
    SEARCH_INDEX_FILENAME.into(),
    search::get_search_index_file(&ctx, &doc_nodes_by_url)?,
  );
  files.insert(SCRIPT_FILENAME.into(), SCRIPT_JS.into());
  files.insert(FUSE_FILENAME.into(), FUSE_JS.into());
  files.insert(SEARCH_FILENAME.into(), SEARCH_JS.into());

  Ok(files)
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
