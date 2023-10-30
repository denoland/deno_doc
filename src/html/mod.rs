use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use serde::Serialize;
use serde_json::json;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use tinytemplate::TinyTemplate;

use crate::html::jsdoc::markdown_to_html;
use crate::html::util::RenderContext;
use crate::node::Location;
use crate::{DocNode, DocNodeKind};

mod class;
mod r#enum;
mod function;
mod interface;
mod jsdoc;
mod namespace;
mod parameters;
mod symbol;
mod type_alias;
mod types;
mod util;
mod variable;

pub const STYLESHEET: &str = include_str!("./styles.css");
pub const STYLESHEET_FILENAME: &str = "styles.css";

pub const SEARCH_INDEX_FILENAME: &str = "search_index.js";

pub const SEARCH_JS: &str = include_str!("./search.js");
pub const SEARCH_FILENAME: &str = "search.js";

const SEARCH_BAR: &str = r#"
<input type="text" id="searchbar" style="display: none;" />
"#;

#[derive(Debug, Clone)]
pub struct GenerateOptions {
  /// The name that is shown is the top-left corner, eg. "deno_std".
  pub package_name: String,
}

struct GenerateCtx<'ctx> {
  package_name: String,
  tt: TinyTemplate<'ctx>,
}

fn setup_tt<'t>() -> Result<TinyTemplate<'t>, anyhow::Error> {
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
    "index_list.html",
    include_str!("./templates/index_list.html"),
  )?;
  tt.add_template(
    "sidepanel.html",
    include_str!("./templates/sidepanel.html"),
  )?;
  tt.add_template("page.html", include_str!("./templates/page.html"))?;
  tt.add_template(
    "compound_index.html",
    include_str!("./templates/compound_index.html"),
  )?;
  tt.add_template(
    "compound_sidepanel.html",
    include_str!("./templates/compound_sidepanel.html"),
  )?;
  tt.add_template(
    "doc_node_kind_icon.html",
    include_str!("./templates/doc_node_kind_icon.html"),
  )?;
  tt.add_template("anchor.html", include_str!("./templates/anchor.html"))?;
  Ok(tt)
}

pub fn generate(
  options: GenerateOptions,
  doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<DocNode>>,
) -> Result<HashMap<String, String>, anyhow::Error> {
  let tt = setup_tt()?;
  let ctx = GenerateCtx {
    package_name: options.package_name,
    tt,
  };
  let mut files = HashMap::new();

  // TODO(bartlomieju): remove
  let doc_nodes = doc_nodes_by_url
    .values()
    .flatten()
    .cloned()
    .collect::<Vec<_>>();

  let current_symbols = Rc::new(get_current_symbols(&doc_nodes, vec![]));

  let partitions_by_kind =
    namespace::partition_nodes_by_kind_dedup_overloads(&doc_nodes);
  let sidepanel_ctx = sidepanel_render_ctx(&ctx, &partitions_by_kind);

  // Index page (list of all symbols in all files)
  {
    let index = render_index(
      &ctx,
      &sidepanel_ctx,
      &partitions_by_kind,
      current_symbols.clone(),
    )?;
    files.insert("index.html".to_string(), index);
  }

  // "Compound index" page (list of all files and symbol kinds)
  {
    let compound_index =
      render_compound_index(&ctx, doc_nodes_by_url, &partitions_by_kind)?;
    files.insert("compound_index.html".to_string(), compound_index);
  }

  // Pages for all discovered symbols
  {
    let generated_pages =
      generate_pages(&ctx, &sidepanel_ctx, &doc_nodes, current_symbols)?;
    for (file_name, content) in generated_pages {
      files.insert(file_name, content);
    }
  }

  Ok(files)
}

fn generate_pages_inner(
  ctx: &GenerateCtx,
  sidepanel_ctx: &SidepanelRenderCtx,
  name_partitions: IndexMap<String, Vec<DocNode>>,
  base: Option<Vec<String>>,
  current_symbols: Rc<HashSet<Vec<String>>>,
) -> Result<Vec<(String, String)>, anyhow::Error> {
  let mut generated_pages =
    Vec::with_capacity(name_partitions.values().len() * 2);

  for (name, doc_nodes) in name_partitions.iter() {
    let file_name = base.as_ref().map_or_else(
      || format!("{}.html", name),
      |base| format!("{}/{name}.html", base.join("/")),
    );
    let symbol_name = base.as_ref().map_or(name.to_string(), |base| {
      format!("{}.{name}", base.join("."))
    });

    let page = render_page(
      ctx,
      sidepanel_ctx,
      &symbol_name,
      doc_nodes,
      current_symbols.clone(),
    )?;

    generated_pages.push((file_name, page));

    if let Some(doc_node) = doc_nodes
      .iter()
      .find(|doc_node| doc_node.kind == DocNodeKind::Namespace)
    {
      let namespace = doc_node.namespace_def.as_ref().unwrap();

      let namespace_name_partitions =
        partition_nodes_by_name(&namespace.elements);

      let new_base = if let Some(mut base) = base.clone() {
        base.push(name.to_string());
        base
      } else {
        vec![name.to_string()]
      };

      let generated = generate_pages_inner(
        ctx,
        sidepanel_ctx,
        namespace_name_partitions,
        Some(new_base),
        current_symbols.clone(),
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
  current_symbols: Rc<HashSet<Vec<String>>>,
) -> Result<Vec<(String, String)>, anyhow::Error> {
  let name_partitions = partition_nodes_by_name(doc_nodes);

  generate_pages_inner(
    ctx,
    sidepanel_ctx,
    name_partitions,
    None,
    current_symbols,
  )
}

fn render_compound_index(
  ctx: &GenerateCtx,
  doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<DocNode>>,
  partitions: &IndexMap<DocNodeKind, Vec<DocNode>>,
) -> Result<String, anyhow::Error> {
  let files = doc_nodes_by_url
    .keys()
    .map(|url| url.as_str())
    .collect::<Vec<_>>();

  let sidepanel_ctx = json!({
    "node_kinds": partitions.keys().map(|k| format!("{:?}", k)).collect::<Vec<_>>(),
    "files": files,
    "base_url": "./".to_string(),
    "package_name": ctx.package_name.to_string(),
  });

  let module_docs = doc_nodes_by_url
    .iter()
    .map(|(url, nodes)| {
      let docs = nodes
        .iter()
        .find(|node| node.kind == DocNodeKind::ModuleDoc);
      let docs_md = docs
        .and_then(|node| node.js_doc.doc.clone())
        .unwrap_or_default();
      let rendered_docs = markdown_to_html(
        &docs_md,
        false,
        &RenderContext {
          additional_css: Rc::new(RefCell::new("".to_string())),
          namespace: None,
          current_symbols: Default::default(),
          current_type_params: Default::default(),
        },
      );

      json!({
        "url": url.as_str(),
        "docs": rendered_docs,
      })
    })
    .collect::<Vec<_>>();

  Ok(ctx.tt.render(
    "compound_index.html",
    &json!({
      // TODO(bartlomieju): dedup with `render_page`
      "html_head": {
        "additional_css": "",
        "stylesheet_url": format!("./{}", STYLESHEET_FILENAME),
      },
      "html_tail": {
        "url_search_index": format!("./{}", SEARCH_INDEX_FILENAME),
        "url_search": format!("./{}", SEARCH_FILENAME),
      },
      "sidepanel": sidepanel_ctx,
      "search_bar": SEARCH_BAR,
      "module_docs": module_docs
    }),
  )?)
}

fn render_index(
  ctx: &GenerateCtx,
  sidepanel_ctx: &SidepanelRenderCtx,
  partitions: &IndexMap<DocNodeKind, Vec<DocNode>>,
  current_symbols: Rc<HashSet<Vec<String>>>,
) -> Result<String, anyhow::Error> {
  let content = namespace::doc_node_kind_sections(
    partitions,
    &RenderContext {
      additional_css: Rc::new(RefCell::new("".to_string())),
      namespace: None,
      current_symbols: current_symbols.clone(),
      current_type_params: Default::default(),
    },
  );

  Ok(ctx.tt.render(
    "index_list.html",
    &json!({
      // TODO(bartlomieju): dedup with `render_page`
      "html_head": {
        "additional_css": "",
        "stylesheet_url": format!("./{}", STYLESHEET_FILENAME),
      },
      "html_tail": {
        "url_search_index": format!("./{}", SEARCH_INDEX_FILENAME),
        "url_search": format!("./{}", SEARCH_FILENAME),
      },
      "sidepanel": sidepanel_ctx,
      "search_bar": SEARCH_BAR,
      "content": content
    }),
  )?)
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

fn get_current_symbols(
  doc_nodes: &[DocNode],
  base: Vec<String>,
) -> HashSet<Vec<String>> {
  let mut current_symbols = HashSet::new();

  for doc_node in doc_nodes {
    if doc_node.kind == DocNodeKind::ModuleDoc {
      continue;
    }

    let mut name_path = base.clone();
    name_path.push(doc_node.name.clone());

    current_symbols.insert(name_path.clone());

    if doc_node.kind == DocNodeKind::Namespace {
      let namespace_def = doc_node.namespace_def.as_ref().unwrap();
      current_symbols
        .extend(get_current_symbols(&namespace_def.elements, name_path))
    }
  }

  current_symbols
}

fn render_page(
  ctx: &GenerateCtx,
  sidepanel_ctx: &SidepanelRenderCtx,
  name: &str,
  doc_nodes: &[DocNode],
  current_symbols: Rc<HashSet<Vec<String>>>,
) -> Result<String, anyhow::Error> {
  let render_ctx = RenderContext {
    additional_css: Rc::new(RefCell::new("".to_string())),
    namespace: name
      .rsplit_once('.')
      .map(|(namespace, _symbol)| namespace.to_string()),
    current_symbols,
    current_type_params: Default::default(),
  };

  // NOTE: `doc_nodes` should be sorted at this point.
  let symbol_group = symbol::render_symbol_group(doc_nodes, name, &render_ctx);

  let backs = name.split('.').skip(1).map(|_| "../").collect::<String>();

  let sidepanel_ctx = SidepanelRenderCtx {
    base_url: backs.clone(),
    ..sidepanel_ctx.clone()
  };

  let additional_css = render_ctx.additional_css.borrow();
  let additional_css: &str = additional_css.as_ref();

  Ok(ctx.tt.render(
    "page.html",
    &json!({
      // TODO(bartlomieju): dedup with `render_index`
      "html_head": {
        "additional_css": additional_css,
        "stylesheet_url": format!("./{backs}{STYLESHEET_FILENAME}"),
      },
      "html_tail": {
        "url_search_index": format!("./{backs}{SEARCH_INDEX_FILENAME}"),
        "url_search": format!("./{backs}{SEARCH_FILENAME}"),
      },
      "sidepanel": sidepanel_ctx,
      "search_bar": SEARCH_BAR,
      "base_url": format!("./{backs}index.html"),
      "symbol_group": symbol_group
    }),
  )?)
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

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
struct SearchIndexNode {
  kind: DocNodeKind,
  name: String,
  #[serde(skip_serializing_if = "Vec::is_empty")]
  ns_qualifiers: Vec<String>,
  // TODO(bartlomieju): location should be processed based on the entry point of
  // the graph to node include `file:///base/dir/other/dir/`.
  location: Location,
  declaration_kind: crate::node::DeclarationKind,
}

fn doc_node_into_search_index_nodes_inner(
  doc_node: &DocNode,
  ns_qualifiers: Vec<String>,
) -> Vec<SearchIndexNode> {
  if !matches!(doc_node.kind, DocNodeKind::Namespace) {
    return vec![SearchIndexNode {
      kind: doc_node.kind,
      name: doc_node.name.to_string(),
      ns_qualifiers: ns_qualifiers.clone(),
      location: doc_node.location.clone(),
      declaration_kind: doc_node.declaration_kind,
    }];
  }

  let ns_def = doc_node.namespace_def.as_ref().unwrap();
  let mut nodes = Vec::with_capacity(1 + ns_def.elements.len());
  let ns_name = doc_node.name.to_string();

  nodes.push(SearchIndexNode {
    kind: doc_node.kind,
    name: doc_node.name.to_string(),
    ns_qualifiers: ns_qualifiers.clone(),
    location: doc_node.location.clone(),
    declaration_kind: doc_node.declaration_kind,
  });

  for el in &ns_def.elements {
    let mut ns_qualifiers_ = ns_qualifiers.clone();
    ns_qualifiers_.push(ns_name.to_string());

    nodes.push(SearchIndexNode {
      kind: el.kind,
      name: el.name.to_string(),
      ns_qualifiers: ns_qualifiers_,
      location: el.location.clone(),
      declaration_kind: el.declaration_kind,
    });

    if el.kind == DocNodeKind::Namespace {
      nodes.extend_from_slice(&doc_node_into_search_index_nodes_inner(
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
  doc_node: &DocNode,
) -> Vec<SearchIndexNode> {
  doc_node_into_search_index_nodes_inner(doc_node, vec![])
}

pub fn generate_search_index(
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
      output.extend_from_slice(&doc_node_into_search_index_nodes(node));
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
