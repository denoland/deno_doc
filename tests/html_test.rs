use deno_ast::ModuleSpecifier;
use deno_doc::html::*;
use deno_doc::{DocNode, DocParser, DocParserOptions};
use deno_graph::source::{LoadFuture, LoadResponse, Loader};
use deno_graph::{
  BuildOptions, CapturingModuleAnalyzer, GraphKind, ModuleGraph,
};
use futures::future;
use indexmap::IndexMap;
use std::fs::read_dir;
use std::fs::read_to_string;
use std::rc::Rc;

struct SourceFileLoader {}

impl Loader for SourceFileLoader {
  fn load(
    &mut self,
    specifier: &ModuleSpecifier,
    _is_dynamic: bool,
    _cache_setting: deno_graph::source::CacheSetting,
  ) -> LoadFuture {
    let result = if specifier.scheme() == "file" {
      let path = specifier.to_file_path().unwrap();
      read_to_string(path)
        .map(|content| {
          Some(LoadResponse::Module {
            specifier: specifier.clone(),
            maybe_headers: None,
            content: content.into(),
          })
        })
        .map_err(|err| err.into())
    } else {
      Ok(None)
    };
    Box::pin(future::ready(result))
  }
}

async fn get_files(subpath: &str) -> IndexMap<ModuleSpecifier, Vec<DocNode>> {
  let files = read_dir(
    std::env::current_dir()
      .unwrap()
      .join("tests")
      .join("testdata")
      .join(subpath),
  )
  .unwrap();

  let source_files: Vec<ModuleSpecifier> = files
    .into_iter()
    .map(|entry| {
      let entry = entry.unwrap();
      ModuleSpecifier::from_file_path(entry.path()).unwrap()
    })
    .collect();

  let mut loader = SourceFileLoader {};
  let analyzer = CapturingModuleAnalyzer::default();
  let mut graph = ModuleGraph::new(GraphKind::TypesOnly);
  graph
    .build(
      source_files.clone(),
      &mut loader,
      BuildOptions {
        module_analyzer: Some(&analyzer),
        ..Default::default()
      },
    )
    .await;

  let parser = DocParser::new(
    &graph,
    analyzer.as_capturing_parser(),
    DocParserOptions {
      diagnostics: false,
      private: false,
    },
  )
  .unwrap();

  let mut source_files = source_files.clone();
  source_files.sort();
  let mut doc_nodes_by_url = IndexMap::with_capacity(source_files.len());
  for source_file in source_files {
    let nodes = parser.parse_with_reexports(&source_file).unwrap();
    doc_nodes_by_url.insert(source_file, nodes);
  }

  doc_nodes_by_url
}

#[tokio::test]
async fn html_doc_files() {
  let files = generate(
    GenerateOptions {
      package_name: None,
      main_entrypoint: None,
      global_symbols: Default::default(),
      global_symbol_href_resolver: Rc::new(|_, _| String::new()),
      url_resolver: Rc::new(default_url_resolver),
    },
    &get_files("single").await,
  )
  .unwrap();

  let mut file_names = files.keys().collect::<Vec<_>>();
  file_names.sort();

  assert_eq!(
    file_names,
    [
      "./all_symbols.html",
      "./index.html",
      "./~/Bar.html",
      "./~/Foo.html",
      "./~/index.html",
      "fuse.js",
      "page.css",
      "search.js",
      "search_index.js",
      "styles.css",
    ]
  );
}

#[test]
fn common_ancestor_root() {
  let map = [
    "file:///bytes.ts",
    "file:///colors.ts",
    "file:///duration.ts",
    "file:///printf.ts",
  ]
  .into_iter()
  .map(|path| {
    let specifier = ModuleSpecifier::parse(path).unwrap();
    (specifier, vec![])
  })
  .collect();

  let common_ancestor = find_common_ancestor(&map, false);
  assert_eq!(common_ancestor, None);
}

#[tokio::test]
async fn common_ancestor_single_file() {
  let common_ancestor = find_common_ancestor(&get_files("single").await, false);
  assert_eq!(common_ancestor, None);
}

#[tokio::test]
async fn common_ancestor_multiple_files() {
  let common_ancestor =
    find_common_ancestor(&get_files("multiple").await, false);
  assert_eq!(
    common_ancestor,
    Some(
      std::env::current_dir()
        .unwrap()
        .join("tests")
        .join("testdata")
        .join("multiple")
    )
  );
}

#[tokio::test]
async fn common_ancestor_single_file_single_mode() {
  let common_ancestor = find_common_ancestor(&get_files("single").await, true);
  assert_eq!(
    common_ancestor,
    Some(
      std::env::current_dir()
        .unwrap()
        .join("tests")
        .join("testdata")
        .join("single")
        .join("a.ts")
    )
  );
}

#[tokio::test]
async fn common_ancestor_multiple_file_single_mode() {
  let common_ancestor =
    find_common_ancestor(&get_files("multiple").await, false);
  assert_eq!(
    common_ancestor,
    Some(
      std::env::current_dir()
        .unwrap()
        .join("tests")
        .join("testdata")
        .join("multiple")
    )
  );
}
