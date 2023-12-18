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
      import_href_resolver: Rc::new(|_, _| None),
      url_resolver: Rc::new(default_url_resolver),
      rewrite_map: None,
      hide_module_doc_title: false,
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

#[tokio::test]
async fn html_doc_files_rewrite() {
  let multiple_dir = std::env::current_dir()
    .unwrap()
    .join("tests")
    .join("testdata")
    .join("multiple");
  let mut rewrite_map = IndexMap::new();
  rewrite_map.insert(
    ModuleSpecifier::from_file_path(multiple_dir.join("a.ts")).unwrap(),
    ".".to_string(),
  );
  rewrite_map.insert(
    ModuleSpecifier::from_file_path(multiple_dir.join("b.ts")).unwrap(),
    "foo".to_string(),
  );

  let files = generate(
    GenerateOptions {
      package_name: None,
      main_entrypoint: None,
      global_symbols: Default::default(),
      global_symbol_href_resolver: Rc::new(|_, _| String::new()),
      import_href_resolver: Rc::new(|_, _| None),
      url_resolver: Rc::new(default_url_resolver),
      rewrite_map: Some(rewrite_map),
      hide_module_doc_title: false,
    },
    &get_files("multiple").await,
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
      "foo/~/index.html",
      "foo/~/x.html",
      "fuse.js",
      "page.css",
      "search.js",
      "search_index.js",
      "styles.css"
    ]
  );
}

#[tokio::test]
async fn symbol_group() {
  let multiple_dir = std::env::current_dir()
    .unwrap()
    .join("tests")
    .join("testdata")
    .join("multiple");

  let doc_nodes_by_url = get_files("multiple").await;

  let mut rewrite_map = IndexMap::new();
  rewrite_map.insert(
    ModuleSpecifier::from_file_path(multiple_dir.join("a.ts")).unwrap(),
    ".".to_string(),
  );
  rewrite_map.insert(
    ModuleSpecifier::from_file_path(multiple_dir.join("b.ts")).unwrap(),
    "foo".to_string(),
  );

  let ctx = GenerateCtx {
    package_name: None,
    common_ancestor: None,
    main_entrypoint: Some(
      ModuleSpecifier::from_file_path(multiple_dir.join("a.ts")).unwrap(),
    ),
    specifiers: rewrite_map.keys().cloned().collect(),
    tt: setup_tt().unwrap(),
    global_symbols: Default::default(),
    global_symbol_href_resolver: Rc::new(|_, _| String::new()),
    import_href_resolver: Rc::new(|_, _| None),
    url_resolver: Rc::new(default_url_resolver),
    rewrite_map: Some(rewrite_map),
    hide_module_doc_title: false,
  };

  let mut files = vec![];

  {
    for (specifier, doc_nodes) in &doc_nodes_by_url {
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

          let page_ctx = pages::PageCtx {
            html_head_ctx,
            sidepanel_ctx,
            symbol_group_ctx,
            breadcrumbs_ctx,
            search_ctx: serde_json::Value::Null,
          };

          page_ctx
        },
      ));
    }
  }

  let files_json = serde_json::to_string(&files).unwrap();

  let symbol_group_json_path = std::env::current_dir()
    .unwrap()
    .join("tests")
    .join("testdata")
    .join("symbol_group.json");

  let symbol_group_json = read_to_string(symbol_group_json_path).unwrap();

  assert_eq!(files_json, symbol_group_json);
}
