use indexmap::IndexMap;
use std::collections::HashMap;
use tinytemplate::TinyTemplate;

use crate::DocNodeKind;

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

const HTML_TEMPLATE: &str = r#"
<html>
<head>
  <link rel="stylesheet" href="./styles.css">
</head>
<body>
  {content}
</body>
<script type="module" src="./search_index.js" defer></script>
<script type="module" src="./search.js" defer></script>
</html>"#;

// TODO(bartlomieju): reference STYLESHEET_FILENAME below
const HTML_HEAD: &str = r#"
<html>
<head>
  <link rel="stylesheet" href="./styles.css">
</head>
<body>
"#;
const HTML_TAIL: &str = r#"
</body>
<script>
</script>
<script type="module" src="./search_index.js" defer></script>
<script type="module" src="./search.js" defer></script>
</html>"#;
const SEARCH_BAR: &str = r#"
<input type="text" placeholder="Search..." id="searchbar" />
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
}

pub fn generate_search_index(
  doc_nodes: &[crate::DocNode],
) -> Result<String, anyhow::Error> {
  let mut search_index = serde_json::json!({
    "nodes": doc_nodes
  });

  for el in search_index
    .get_mut("nodes")
    .unwrap()
    .as_array_mut()
    .unwrap()
  {
    el["jsDoc"].take();
    match el.get("kind").unwrap().as_str().unwrap() {
      "function" => el["functionDef"].take(),
      "variable" => el["variableDef"].take(),
      "enum" => el["enumDef"].take(),
      "class" => el["classDef"].take(),
      "typeAlias" => el["typeAliasDef"].take(),
      "namespace" => el["namespaceDef"].take(),
      "interface" => el["interfaceDef"].take(),
      _ => serde_json::Value::Null,
    };
  }

  let search_index_str = serde_json::to_string(&search_index)?;

  let index = format!(
    r#"(function () {{
  window.DENO_DOC_SEARCH_INDEX = {};
}})()"#,
    search_index_str
  );
  Ok(index)
}

pub fn generate(
  ctx: GenerateCtx,
  doc_nodes: &[crate::DocNode],
) -> Result<HashMap<String, String>, anyhow::Error> {
  let mut tt = TinyTemplate::new();
  tt.set_default_formatter(&tinytemplate::format_unescaped);
  tt.add_template("html", HTML_TEMPLATE)?;

  let mut files = HashMap::new();

  // FIXME(bartlomieju): functions can have duplicates because of overloads
  let partitions = namespace::partition_nodes_by_kind(doc_nodes);
  let name_partitions = partition_nodes_by_name(doc_nodes);

  let sidepanel = render_sidepanel(&ctx, &partitions);
  let index_content = render_index(&ctx, &sidepanel, partitions);
  files.insert(
    "index".to_string(),
    tt.render(
      "html",
      &serde_json::json!({
        "content": index_content
      }),
    )?,
  );

  generate_pages(name_partitions, &mut files, &ctx, &sidepanel, None);

  Ok(files)
}

fn generate_pages(
  name_partitions: IndexMap<String, Vec<crate::DocNode>>,
  files: &mut HashMap<String, String>,
  ctx: &GenerateCtx,
  sidepanel: &str,
  base: Option<Vec<String>>,
) {
  for (name, doc_nodes) in name_partitions.iter() {
    let file_name = base.as_ref().map_or(name.to_string(), |base| {
      format!("{}/{name}", base.join("/"))
    });
    let symbol_name = base.as_ref().map_or(name.to_string(), |base| {
      format!("{}.{name}", base.join("."))
    });

    let page = render_page(ctx, sidepanel, &symbol_name, doc_nodes);

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
      );
    }
  }
}

fn render_index(
  ctx: &GenerateCtx,
  sidepanel: &str,
  partitions: IndexMap<DocNodeKind, Vec<crate::DocNode>>,
) -> String {
  let mut content = String::with_capacity(32 * 1024);

  content.push_str(HTML_HEAD);
  content.push_str(&format!(
    r#"<div style="display: flex;">{sidepanel}<div style="padding: 30px;"><h1>Index</h1><div>{SEARCH_BAR}</div>"#
  ));

  content.push_str(r#"<div id="mainContent">"#);
  for (kind, doc_nodes) in partitions {
    content.push_str(&format!(r#"<h2>{:?}</h2><ul>"#, kind));

    for doc_node in doc_nodes {
      content.push_str(&format!(
        r##"<li><a href="{}.html">{}</a>{}</li>"##,
        ctx.url(doc_node.name.to_string()),
        doc_node.name,
        jsdoc::render_docs(&doc_node.js_doc, false, false),
      ));
    }

    content.push_str(r#"</ul>"#);
  }
  content.push_str(r#"</div><div id="searchResults"></div>"#);

  content.push_str(&format!(r#"</div></div>{HTML_TAIL}"#));

  content
}

fn partition_nodes_by_name(
  doc_nodes: &[crate::DocNode],
) -> IndexMap<String, Vec<crate::DocNode>> {
  let mut partitions = IndexMap::default();

  for node in doc_nodes {
    partitions
      .entry(node.name.clone())
      .or_insert(vec![])
      .push(node.clone());
  }

  partitions
}

fn render_page(
  ctx: &GenerateCtx,
  sidepanel: &str,
  name: &str,
  doc_nodes: &[crate::DocNode],
) -> String {
  let context = util::RenderContext {
    additional_css: std::rc::Rc::new(std::cell::RefCell::new("".to_string())),
    namespace: name
      .rsplit_once('.')
      .map(|(namespace, _symbol)| namespace.to_string()),
  };

  // FIXME: don't clone here
  let symbol_group =
    symbol::render_symbol_group(doc_nodes.to_vec(), name, &context);

  let backs = name.split('.').skip(1).map(|_| "../").collect::<String>();

  format!(
    r##"<html><head><link rel="stylesheet" href="./{backs}{STYLESHEET_FILENAME}"></head><style>{}</style><div style="display: flex;">{sidepanel}<div style="padding: 30px;"><a href="{}"><- Index</a><div>{SEARCH_BAR}</div>{symbol_group}</div></div>{HTML_TAIL}"##,
    context.additional_css.borrow(),
    ctx.base_url,
  )
}

fn render_sidepanel(
  ctx: &GenerateCtx,
  partitions: &IndexMap<DocNodeKind, Vec<crate::DocNode>>,
) -> String {
  let mut sidepanel = String::with_capacity(1024);

  sidepanel.push_str(r#"<div id="sidepanel">"#);
  sidepanel.push_str(&format!(
    r#"<h2><a href="{}">{}</a></h2>"#,
    ctx.base_url, ctx.package_name
  ));
  for (kind, doc_nodes) in partitions.iter() {
    sidepanel.push_str(&format!(r#"<h3>{:?}</h3><ul>"#, kind));

    for doc_node in doc_nodes {
      sidepanel.push_str(&format!(
        r##"<li><a href="{}.html">{}</a></li>"##,
        ctx.url(doc_node.name.to_string()),
        doc_node.name
      ));
    }

    sidepanel.push_str(r#"</ul>"#);
  }
  sidepanel.push_str(r#"</div>"#);

  sidepanel
}
