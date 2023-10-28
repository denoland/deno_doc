use indexmap::IndexMap;
use serde::Serialize;
use serde_json::json;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use tinytemplate::TinyTemplate;

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

// TODO(bartlomieju): reference STYLESHEET_FILENAME below
const HTML_TAIL: &str = r#"
</body>
<script>
</script>
<script type="module" src="./search_index.js" defer></script>
<script type="module" src="./search.js" defer></script>
</html>"#;
const SEARCH_BAR: &str = r#"
<input type="text" placeholder="Search..." id="searchbar" style="display: none;" />
"#;

#[derive(Debug, Clone)]
pub struct GenerateCtx {
  /// The name that is shown is the top-left corner, eg. "deno_std".
  pub package_name: String,

  /// Pathname that all links will refer to, eg. "/docs".
  pub base_url: String,
}

impl GenerateCtx {
  fn url(&self, path: String) -> String {
    format!("{}{}", self.base_url, path)
  }

  fn url_with_html(&self, path: String) -> String {
    format!("{}{}.html", self.base_url, path)
  }
}

pub fn generate(
  ctx: GenerateCtx,
  doc_nodes: &[crate::DocNode],
) -> Result<HashMap<String, String>, anyhow::Error> {
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

  let mut files = HashMap::new();

  let current_symbols = Rc::new(get_current_symbols(doc_nodes, vec![]));

  // FIXME(bartlomieju): functions can have duplicates because of overloads
  let partitions = namespace::partition_nodes_by_kind(doc_nodes);
  let name_partitions = partition_nodes_by_name(doc_nodes);

  // TODO(bartlomieju): flatten all of this into a single template call
  let sidepanel_ctx = sidepanel_render_ctx(&ctx, &partitions);
  let index =
    render_index(&tt, &sidepanel_ctx, partitions, current_symbols.clone())?;
  files.insert("index".to_string(), index);

  let sidepanel = tt.render("sidepanel.html", &sidepanel_ctx)?;
  generate_pages(
    name_partitions,
    &mut files,
    &ctx,
    &sidepanel,
    None,
    current_symbols,
  );

  Ok(files)
}

fn generate_pages(
  name_partitions: IndexMap<String, Vec<crate::DocNode>>,
  files: &mut HashMap<String, String>,
  ctx: &GenerateCtx,
  sidepanel: &str,
  base: Option<Vec<String>>,
  current_symbols: Rc<HashSet<Vec<String>>>,
) {
  for (name, doc_nodes) in name_partitions.iter() {
    let file_name = base.as_ref().map_or(name.to_string(), |base| {
      format!("{}/{name}", base.join("/"))
    });
    let symbol_name = base.as_ref().map_or(name.to_string(), |base| {
      format!("{}.{name}", base.join("."))
    });

    let page = render_page(
      ctx,
      sidepanel,
      &symbol_name,
      doc_nodes,
      current_symbols.clone(),
    );

    files.insert(file_name, page);

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

      generate_pages(
        namespace_name_partitions,
        files,
        ctx,
        sidepanel,
        Some(new_base),
        current_symbols.clone(),
      );
    }
  }
}

fn render_index(
  tt: &TinyTemplate,
  sidepanel_ctx: &serde_json::Value,
  partitions: IndexMap<DocNodeKind, Vec<crate::DocNode>>,
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

  Ok(tt.render(
    "index_list.html",
    &json!({
      "html_head": "",
      "html_tail": "",
      "sidepanel": sidepanel_ctx,
      "search_bar": SEARCH_BAR,
      "content": content
    }),
  )?)
}

fn partition_nodes_by_name(
  doc_nodes: &[crate::DocNode],
) -> IndexMap<String, Vec<crate::DocNode>> {
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

  partitions
}

fn get_current_symbols(
  doc_nodes: &[crate::DocNode],
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
  sidepanel: &str,
  name: &str,
  doc_nodes: &[crate::DocNode],
  current_symbols: Rc<HashSet<Vec<String>>>,
) -> String {
  let context = RenderContext {
    additional_css: Rc::new(RefCell::new("".to_string())),
    namespace: name
      .rsplit_once('.')
      .map(|(namespace, _symbol)| namespace.to_string()),
    current_symbols,
    current_type_params: Default::default(),
  };

  // FIXME: don't clone here
  let symbol_group =
    symbol::render_symbol_group(doc_nodes.to_vec(), name, &context);

  let backs = name.split('.').skip(1).map(|_| "../").collect::<String>();

  format!(
    r##"<html>
<head>
  <link rel="stylesheet" href="./{backs}{STYLESHEET_FILENAME}">
</head>
<style>{}</style>
<div style="display: flex;">
  {sidepanel}
  <div style="padding: 30px; width: 100%;">
    <a href="{}"><- Index</a>
  <div>
  {SEARCH_BAR}
</div>
{symbol_group}
<div id="searchResults"></div>
</div>
</div>
{HTML_TAIL}"##,
    context.additional_css.borrow(),
    ctx.base_url,
  )
}

fn sidepanel_render_ctx(
  ctx: &GenerateCtx,
  partitions: &IndexMap<DocNodeKind, Vec<DocNode>>,
) -> serde_json::Value {
  let partitions: Vec<serde_json::Value> = partitions
    .into_iter()
    .map(|(kind, doc_nodes)| {
      serde_json::json!({
        "kind": format!("{:?}", kind),
        "doc_nodes": doc_nodes.iter().map(|doc_node| {
          serde_json::json!({
            "url": ctx.url_with_html(doc_node.name.to_string()),
            "name": doc_node.name.to_string()
          })
        }).collect::<Vec<serde_json::Value>>()
      })
    })
    .collect();

  json!({
    "base_url": ctx.base_url.to_string(),
    "package_name": ctx.package_name.to_string(),
    "partitions": partitions,
  })
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

/// A single DocNode can produce multiple SearchIndexNode - eg. a namespace
/// node is flattened into a list of its elements.
fn doc_node_into_search_index_nodes(
  doc_node: &crate::DocNode,
) -> Vec<SearchIndexNode> {
  if !matches!(doc_node.kind, DocNodeKind::Namespace) {
    return vec![SearchIndexNode {
      kind: doc_node.kind,
      name: doc_node.name.to_string(),
      ns_qualifiers: vec![],
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
    ns_qualifiers: vec![],
    location: doc_node.location.clone(),
    declaration_kind: doc_node.declaration_kind,
  });

  // TODO(bartlomieju): doesn't handle nested namespaces
  for el in &ns_def.elements {
    nodes.push(SearchIndexNode {
      kind: el.kind,
      name: el.name.to_string(),
      ns_qualifiers: vec![ns_name.to_string()],
      location: el.location.clone(),
      declaration_kind: el.declaration_kind,
    });
  }

  nodes
}

pub fn generate_search_index(
  doc_nodes: &[crate::DocNode],
) -> Result<String, anyhow::Error> {
  let doc_nodes = doc_nodes.iter().fold(
    Vec::with_capacity(doc_nodes.len()),
    |mut output, node| {
      output.extend_from_slice(&doc_node_into_search_index_nodes(node));
      output
    },
  );

  let search_index = serde_json::json!({
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
