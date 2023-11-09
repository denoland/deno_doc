use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use serde::Serialize;
use serde_json::json;
use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;
use tinytemplate::TinyTemplate;

use crate::html::jsdoc::render_markdown;
use crate::html::util::RenderContext;
use crate::node::Location;
use crate::{DocNode, DocNodeKind};

mod jsdoc;
mod parameters;
mod symbol;
mod symbols;
mod types;
mod util;

use symbol::SymbolGroupCtx;
use symbols::namespace::NamespaceRenderCtx;

use self::util::NamespacedSymbols;

const STYLESHEET: &str = include_str!("./templates/styles.css");
const STYLESHEET_FILENAME: &str = "styles.css";

const SEARCH_INDEX_FILENAME: &str = "search_index.js";

const FUSE_JS: &str = include_str!("./templates/fuse.js");
const FUSE_FILENAME: &str = "fuse.js";

const SEARCH_JS: &str = include_str!("./templates/search.js");
const SEARCH_FILENAME: &str = "search.js";

#[derive(Debug, Clone)]
pub struct GenerateOptions {
  /// The name that is shown is the top-left corner, eg. "deno_std".
  pub package_name: String,
}

struct GenerateCtx<'ctx> {
  package_name: String,
  common_ancestor: Option<PathBuf>,
  tt: Rc<TinyTemplate<'ctx>>,
}

impl<'ctx> GenerateCtx<'ctx> {
  fn url_to_short_path(&self, url: &ModuleSpecifier) -> String {
    if url.scheme() != "file" {
      return url.to_string();
    }

    let Some(common_ancestor) = &self.common_ancestor else {
      return url.to_string();
    };

    let url_file_path = url.to_file_path().unwrap();

    let stripped_path = url_file_path
      .strip_prefix(common_ancestor)
      .unwrap_or(&url_file_path);

    stripped_path.to_string_lossy().to_string()
  }
}

fn setup_tt<'t>() -> Result<Rc<TinyTemplate<'t>>, anyhow::Error> {
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
  Ok(Rc::new(tt))
}

pub fn generate(
  options: GenerateOptions,
  doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<DocNode>>,
) -> Result<HashMap<String, String>, anyhow::Error> {
  let file_paths: Vec<PathBuf> = doc_nodes_by_url
    .keys()
    .filter_map(|url| {
      if url.scheme() == "file" {
        Some(url.to_file_path().unwrap())
      } else {
        None
      }
    })
    .collect();
  let common_ancestor = find_common_ancestor(file_paths);
  let tt = setup_tt()?;
  let ctx = GenerateCtx {
    package_name: options.package_name,
    common_ancestor,
    tt,
  };
  let mut files = HashMap::new();

  // TODO(bartlomieju): remove
  let doc_nodes = doc_nodes_by_url
    .values()
    .flatten()
    .cloned()
    .collect::<Vec<_>>();

  let all_symbols = NamespacedSymbols::new(&doc_nodes);

  // Index page
  {
    // TODO(bartlomieju): this is fake - we should pass the actual main entrypoint
    let main_entrypoint = doc_nodes_by_url.keys().next().unwrap().clone();

    let doc_nodes = doc_nodes_by_url.get(&main_entrypoint).cloned().unwrap();
    let categories =
      symbols::namespace::partition_nodes_by_category(&doc_nodes, true);

    let partitions_for_main_entrypoint =
      if categories.len() == 1 && categories.contains_key("Uncategorized") {
        symbols::namespace::partition_nodes_by_kind(&doc_nodes, true)
          .into_iter()
          .map(|(kind, nodes)| {
            let doc_node_kind_ctx: util::DocNodeKindCtx = (&kind).into();
            (doc_node_kind_ctx.title.to_string(), nodes)
          })
          .collect()
      } else {
        categories
      };

    let index = render_index(
      &ctx,
      main_entrypoint,
      doc_nodes_by_url,
      partitions_for_main_entrypoint,
      all_symbols.clone(),
    )?;
    files.insert("index.html".to_string(), index);
  }

  let partitions_by_kind =
    symbols::namespace::partition_nodes_by_kind(&doc_nodes, true);

  // All symbols (list of all symbols in all files)
  {
    let all_symbols_render =
      render_all_symbols(&ctx, &partitions_by_kind, all_symbols.clone())?;
    files.insert("all_symbols.html".to_string(), all_symbols_render);
  }

  let sidepanel_ctx = sidepanel_render_ctx(&ctx, &partitions_by_kind);

  // Pages for all discovered symbols
  {
    let generated_pages =
      generate_pages(&ctx, &sidepanel_ctx, &doc_nodes, all_symbols)?;
    for (file_name, content) in generated_pages {
      files.insert(file_name, content);
    }
  }

  files.insert(STYLESHEET_FILENAME.into(), STYLESHEET.into());
  files.insert(
    SEARCH_INDEX_FILENAME.into(),
    generate_search_index(&ctx, doc_nodes_by_url)?,
  );
  files.insert(FUSE_FILENAME.into(), FUSE_JS.into());
  files.insert(SEARCH_FILENAME.into(), SEARCH_JS.into());

  Ok(files)
}

fn generate_pages_inner(
  ctx: &GenerateCtx,
  sidepanel_ctx: &SidepanelRenderCtx,
  name_partitions: IndexMap<String, Vec<DocNode>>,
  namespace_paths: Vec<String>,
  all_symbols: NamespacedSymbols,
) -> Result<Vec<(String, String)>, anyhow::Error> {
  let mut generated_pages =
    Vec::with_capacity(name_partitions.values().len() * 2);

  for (name, doc_nodes) in name_partitions.iter() {
    let file_name = if namespace_paths.is_empty() {
      format!("{}.html", name)
    } else {
      format!("{}/{name}.html", namespace_paths.join("/"))
    };

    let page = render_page(
      ctx,
      sidepanel_ctx,
      &namespace_paths,
      name,
      doc_nodes,
      all_symbols.clone(),
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
        namespace_name_partitions,
        namespace_paths,
        all_symbols.clone(),
      )?;
      generated_pages.extend_from_slice(&generated);
    }
  }

  Ok(generated_pages)
}

fn generate_pages(
  ctx: &GenerateCtx,
  sidepanel_ctx: &SidepanelRenderCtx,
  doc_nodes: &[DocNode],
  all_symbols: NamespacedSymbols,
) -> Result<Vec<(String, String)>, anyhow::Error> {
  let name_partitions = partition_nodes_by_name(doc_nodes);

  generate_pages_inner(ctx, sidepanel_ctx, name_partitions, vec![], all_symbols)
}

// TODO(bartlomieju): move a separate module?
#[derive(Serialize)]
struct HtmlHeadCtx {
  title: String,
  current_symbol: String,
  stylesheet_url: String,
}

#[derive(Serialize)]
struct HtmlTailCtx {
  url_search_index: String,
  fuse_js: String,
  url_search: String,
}

#[derive(Serialize)]
struct IndexSidepanelPartitionNodeCtx {
  kind: util::DocNodeKindCtx,
  name: String,
  href: String,
}

#[derive(Serialize)]
struct IndexSidepanelPartitionCtx {
  name: String,
  symbols: Vec<IndexSidepanelPartitionNodeCtx>,
}

#[derive(Serialize)]
struct IndexSidepanelCtx {
  kind_partitions: Vec<IndexSidepanelPartitionCtx>,
  files: Vec<String>,
  base_url: String,
  package_name: String,
}

#[derive(Serialize)]
struct IndexCtx {
  html_head_ctx: HtmlHeadCtx,
  html_tail_ctx: HtmlTailCtx,
  sidepanel_ctx: IndexSidepanelCtx,
  // TODO: use stronly typed value
  module_doc: serde_json::Value,
  // TODO(bartlomieju): needed because `tt` requires ctx for `call` blocks
  search_ctx: serde_json::Value,
}

fn render_index(
  ctx: &GenerateCtx,
  main_entrypoint: ModuleSpecifier,
  doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<DocNode>>,
  partitions: IndexMap<String, Vec<DocNode>>,
  all_symbols: NamespacedSymbols,
) -> Result<String, anyhow::Error> {
  let files = doc_nodes_by_url
    .keys()
    .filter(|url| *url != &main_entrypoint)
    .map(|url| ctx.url_to_short_path(url))
    .collect::<Vec<_>>();

  // TODO(@crowlKats): if files is empty, dont render "Modules" section in sidepanel

  let kind_partitions = partitions
    .into_iter()
    .map(|(name, nodes)| {
      let symbols = nodes
        .iter()
        .map(|node| IndexSidepanelPartitionNodeCtx {
          kind: (&node.kind).into(),
          name: node.name.to_string(),
          href: "TODO".to_string(),
        })
        .collect::<Vec<_>>();
      IndexSidepanelPartitionCtx { name, symbols }
    })
    .collect::<Vec<_>>();

  let sidepanel_ctx = IndexSidepanelCtx {
    kind_partitions,
    files,
    base_url: "./".to_string(),
    package_name: ctx.package_name.to_string(),
  };

  let render_ctx = RenderContext::new(ctx.tt.clone(), all_symbols);

  let module_doc_nodes = doc_nodes_by_url.get(&main_entrypoint).unwrap();

  let module_doc = {
    let docs = module_doc_nodes
      .iter()
      .find(|n| n.kind == DocNodeKind::ModuleDoc)
      .cloned();
    let docs_md = docs
      .and_then(|node| node.js_doc.doc.clone())
      .unwrap_or_default();
    let rendered_docs = render_markdown(&docs_md, &render_ctx);

    json!({
      "url": ctx.url_to_short_path(&main_entrypoint),
      "docs": rendered_docs,
    })
  };

  // TODO(bartlomieju): dedup with `render_page`
  let html_head_ctx = HtmlHeadCtx {
    title: format!("Index - {} documentation", ctx.package_name),
    current_symbol: "".to_string(),
    stylesheet_url: format!("./{}", STYLESHEET_FILENAME),
  };
  let html_tail_ctx = HtmlTailCtx {
    url_search_index: format!("./{}", SEARCH_INDEX_FILENAME),
    fuse_js: format!("./{}", FUSE_FILENAME),
    url_search: format!("./{}", SEARCH_FILENAME),
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
  partitions: &IndexMap<DocNodeKind, Vec<DocNode>>,
  all_symbols: NamespacedSymbols,
) -> Result<String, anyhow::Error> {
  let render_ctx = RenderContext::new(ctx.tt.clone(), all_symbols.clone());
  let namespace_ctx =
    symbols::namespace::get_namespace_render_ctx(&render_ctx, partitions);

  // TODO(bartlomieju): dedup with `render_page`
  let html_head_ctx = HtmlHeadCtx {
    title: format!("All Symbols - {} documentation", ctx.package_name),
    current_symbol: "".to_string(),
    stylesheet_url: format!("./{}", STYLESHEET_FILENAME),
  };
  let html_tail_ctx = HtmlTailCtx {
    url_search_index: format!("./{}", SEARCH_INDEX_FILENAME),
    fuse_js: format!("./{}", FUSE_FILENAME),
    url_search: format!("./{}", SEARCH_FILENAME),
  };
  let all_symbols_ctx = AllSymbolsCtx {
    html_head_ctx,
    html_tail_ctx,
    namespace_ctx,
    search_ctx: serde_json::Value::Null,
  };

  Ok(render_ctx.render("all_symbols.html", &all_symbols_ctx))
}

fn partition_nodes_by_name(
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
  base_url: String,
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
) -> Result<String, anyhow::Error> {
  let mut render_ctx = RenderContext::new(ctx.tt.clone(), all_symbols);
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
    symbol::get_symbol_group_ctx(&render_ctx, doc_nodes, &namespaced_name);

  let backs = namespaced_name
    .split('.')
    .skip(1)
    .map(|_| "../")
    .collect::<String>();

  let sidepanel_ctx = SidepanelRenderCtx {
    base_url: backs.clone(),
    ..sidepanel_ctx.clone()
  };
  // TODO(bartlomieju): dedup with `render_page`
  let html_head_ctx = HtmlHeadCtx {
    title: format!("{} - {} documentation", namespaced_name, ctx.package_name),
    current_symbol: namespaced_name.to_string(),
    stylesheet_url: format!("./{}{}", backs, STYLESHEET_FILENAME),
  };
  let html_tail_ctx = HtmlTailCtx {
    url_search_index: format!("./{}{}", backs, SEARCH_INDEX_FILENAME),
    fuse_js: format!("./{}{}", backs, FUSE_FILENAME),
    url_search: format!("./{}{}", backs, SEARCH_FILENAME),
  };
  let page_ctx = PageCtx {
    html_head_ctx,
    html_tail_ctx,
    sidepanel_ctx,
    base_url: format!("./{backs}index.html"),
    symbol_group_ctx,
    search_ctx: serde_json::Value::Null,
  };

  Ok(render_ctx.render("page.html", &page_ctx))
}

#[derive(Debug, Serialize, Clone)]
struct SidepanelPartition {
  kind: util::DocNodeKindCtx,
  doc_nodes: Vec<String>,
}

#[derive(Debug, Serialize, Clone)]
struct SidepanelRenderCtx {
  base_url: String,
  package_name: String,
  partitions: Vec<SidepanelPartition>,
}

fn sidepanel_render_ctx(
  ctx: &GenerateCtx,
  partitions: &IndexMap<DocNodeKind, Vec<DocNode>>,
) -> SidepanelRenderCtx {
  let mut partitions: Vec<SidepanelPartition> = partitions
    .into_iter()
    .map(|(kind, doc_nodes)| SidepanelPartition {
      kind: kind.into(),
      doc_nodes: doc_nodes
        .iter()
        .map(|doc_node| doc_node.name.to_string())
        .collect::<Vec<_>>(),
    })
    .collect();
  partitions.sort_by_key(|part| part.kind.kind.clone());

  SidepanelRenderCtx {
    base_url: "./".to_string(),
    package_name: ctx.package_name.to_string(),
    partitions,
  }
}

fn join_qualifiers<S>(qualifiers: &[String], s: S) -> Result<S::Ok, S::Error>
where
  S: serde::Serializer,
{
  s.serialize_str(&qualifiers.join("."))
}

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct SearchIndexNode {
  kind: DocNodeKind,
  name: String,
  #[serde(serialize_with = "join_qualifiers")]
  ns_qualifiers: Vec<String>,
  location: Location,
  declaration_kind: crate::node::DeclarationKind,
}

fn doc_node_into_search_index_nodes_inner(
  ctx: &GenerateCtx,
  doc_node: &DocNode,
  ns_qualifiers: Vec<String>,
) -> Vec<SearchIndexNode> {
  if !matches!(doc_node.kind, DocNodeKind::Namespace) {
    let mut location = doc_node.location.clone();
    let location_url = ModuleSpecifier::parse(&location.filename).unwrap();
    let location_url_str = ctx.url_to_short_path(&location_url);
    location.filename = location_url_str;

    return vec![SearchIndexNode {
      kind: doc_node.kind,
      name: doc_node.name.to_string(),
      ns_qualifiers: ns_qualifiers.clone(),
      location,
      declaration_kind: doc_node.declaration_kind,
    }];
  }

  let ns_def = doc_node.namespace_def.as_ref().unwrap();
  let mut nodes = Vec::with_capacity(1 + ns_def.elements.len());
  let ns_name = doc_node.name.to_string();

  let mut location = doc_node.location.clone();
  let location_url = ModuleSpecifier::parse(&location.filename).unwrap();
  let location_url_str = ctx.url_to_short_path(&location_url);
  location.filename = location_url_str;

  nodes.push(SearchIndexNode {
    kind: doc_node.kind,
    name: doc_node.name.to_string(),
    ns_qualifiers: ns_qualifiers.clone(),
    location,
    declaration_kind: doc_node.declaration_kind,
  });

  for el in &ns_def.elements {
    let mut ns_qualifiers_ = ns_qualifiers.clone();
    ns_qualifiers_.push(ns_name.to_string());

    let mut location = el.location.clone();
    let location_url = ModuleSpecifier::parse(&location.filename).unwrap();
    let location_url_str = ctx.url_to_short_path(&location_url);
    location.filename = location_url_str;

    nodes.push(SearchIndexNode {
      kind: el.kind,
      name: el.name.to_string(),
      ns_qualifiers: ns_qualifiers_,
      location,
      declaration_kind: el.declaration_kind,
    });

    if el.kind == DocNodeKind::Namespace {
      nodes.extend_from_slice(&doc_node_into_search_index_nodes_inner(
        ctx,
        el,
        ns_qualifiers.clone(),
      ));
    }
  }

  nodes
}

/// A single DocNode can produce multiple SearchIndexNode - eg. a namespace
/// node is flattened into a list of its elements.
fn doc_node_into_search_index_nodes(
  ctx: &GenerateCtx,
  doc_node: &DocNode,
) -> Vec<SearchIndexNode> {
  doc_node_into_search_index_nodes_inner(ctx, doc_node, vec![])
}

fn generate_search_index(
  ctx: &GenerateCtx,
  doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<DocNode>>,
) -> Result<String, anyhow::Error> {
  // TODO(bartlomieju): remove
  let doc_nodes = doc_nodes_by_url
    .values()
    .flatten()
    .cloned()
    .collect::<Vec<_>>();

  let doc_nodes = doc_nodes.iter().fold(
    Vec::with_capacity(doc_nodes.len()),
    |mut output, node| {
      output.extend_from_slice(&doc_node_into_search_index_nodes(ctx, node));
      output
    },
  );

  let search_index = json!({
    "nodes": doc_nodes
  });
  let search_index_str = serde_json::to_string(&search_index)?;

  let index = format!(
    r#"(function () {{
  window.DENO_DOC_SEARCH_INDEX = {};
}})()"#,
    search_index_str
  );
  Ok(index)
}

fn find_common_ancestor(paths: Vec<PathBuf>) -> Option<PathBuf> {
  assert!(!paths.is_empty());

  let shortest_path = paths.iter().min_by_key(|path| path.components().count());

  match shortest_path {
    Some(shortest) => {
      let mut common_ancestor = PathBuf::new();

      for (index, component) in shortest.components().enumerate() {
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
    None => None, // No paths in the vector.
  }
}
