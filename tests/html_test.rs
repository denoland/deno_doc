use deno_ast::ModuleSpecifier;
use deno_doc::html::*;
use deno_doc::{DocNode, DocParser, DocParserOptions};
use deno_graph::source::{LoadFuture, LoadResponse, Loader};
use deno_graph::{
  BuildOptions, CapturingModuleAnalyzer, GraphKind, ModuleGraph,
};
use futures::future;
use indexmap::IndexMap;
use std::fs;
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
      fs::read(path)
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

struct EmptyResolver {}

impl HrefResolver for EmptyResolver {
  fn resolve_global_symbol(&self, _symbol: &[String]) -> Option<String> {
    None
  }

  fn resolve_import_href(
    &self,
    _symbol: &[String],
    _src: &str,
  ) -> Option<String> {
    None
  }

  fn resolve_usage(
    &self,
    _current_specifier: &ModuleSpecifier,
    current_file: &str,
  ) -> Option<String> {
    Some(current_file.to_string())
  }

  fn resolve_source(&self, _location: &deno_doc::Location) -> Option<String> {
    None
  }
}

async fn get_files(subpath: &str) -> IndexMap<ModuleSpecifier, Vec<DocNode>> {
  let files = fs::read_dir(
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
        module_parser: Some(&analyzer),
        ..Default::default()
      },
    )
    .await;

  let parser = DocParser::new(
    &graph,
    &analyzer,
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
      href_resolver: Rc::new(EmptyResolver {}),
      usage_composer: None,
      rewrite_map: None,
      hide_module_doc_title: false,
      sidebar_flatten_namespaces: false,
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
      href_resolver: Rc::new(EmptyResolver {}),
      usage_composer: None,
      rewrite_map: Some(rewrite_map),
      hide_module_doc_title: false,
      sidebar_flatten_namespaces: false,
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
    hbs: setup_hbs().unwrap(),
    syntect_adapter: setup_syntect(false),
    href_resolver: Rc::new(EmptyResolver {}),
    usage_composer: None,
    rewrite_map: Some(rewrite_map),
    hide_module_doc_title: false,
    single_file_mode: false,
    sidebar_flatten_namespaces: false,
  };

  let mut files = vec![];

  {
    for (specifier, doc_nodes) in &doc_nodes_by_url {
      let short_path = ctx.url_to_short_path(specifier);

      let partitions_for_nodes =
        get_partitions_for_file(&ctx, doc_nodes, &short_path);

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

          pages::PageCtx {
            html_head_ctx,
            sidepanel_ctx,
            symbol_group_ctx,
            breadcrumbs_ctx,
          }
        },
      ));
    }
  }

  let mut files_json = serde_json::to_string_pretty(&files).unwrap();
  files_json.push('\n');

  let symbol_group_json_path = std::env::current_dir()
    .unwrap()
    .join("tests")
    .join("testdata")
    .join("symbol_group.json");

  // uncomment to regenerate symbol_group.json
  //std::fs::write(&symbol_group_json_path, &files_json);

  let symbol_group_json = fs::read_to_string(symbol_group_json_path).unwrap();

  assert_eq!(files_json, symbol_group_json);
}

#[tokio::test]
async fn symbol_search() {
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
    hbs: setup_hbs().unwrap(),
    syntect_adapter: setup_syntect(false),
    href_resolver: Rc::new(EmptyResolver {}),
    usage_composer: None,
    rewrite_map: Some(rewrite_map),
    hide_module_doc_title: false,
    single_file_mode: false,
    sidebar_flatten_namespaces: false,
  };

  let search_index = generate_search_index(&ctx, &doc_nodes_by_url);
  let mut file_json = serde_json::to_string_pretty(&search_index).unwrap();
  file_json.push('\n');

  let symbol_search_json_path = std::env::current_dir()
    .unwrap()
    .join("tests")
    .join("testdata")
    .join("symbol_search.json");

  // uncomment to regenerate symbol_search.json
  //std::fs::write(&symbol_search_json_path, &file_json);

  let symbol_search_json = fs::read_to_string(symbol_search_json_path).unwrap();

  assert_eq!(file_json, symbol_search_json);
}

#[tokio::test]
async fn module_doc() {
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
    hbs: setup_hbs().unwrap(),
    syntect_adapter: setup_syntect(false),
    href_resolver: Rc::new(EmptyResolver {}),
    usage_composer: None,
    rewrite_map: Some(rewrite_map),
    hide_module_doc_title: false,
    single_file_mode: true,
    sidebar_flatten_namespaces: false,
  };

  let mut module_docs = vec![];

  for specifier in &ctx.specifiers {
    let short_path = ctx.url_to_short_path(specifier);
    let doc_nodes = doc_nodes_by_url.get(specifier).unwrap();
    let render_ctx = RenderContext::new(
      &ctx,
      doc_nodes,
      UrlResolveKind::File(&short_path),
      Some(specifier),
    );
    let module_doc =
      ModuleDocCtx::new(&render_ctx, Some(specifier), &doc_nodes_by_url);

    module_docs.push(module_doc);
  }

  let mut file_json = serde_json::to_string_pretty(&module_docs).unwrap();
  file_json.push('\n');

  let module_docs_json_path = std::env::current_dir()
    .unwrap()
    .join("tests")
    .join("testdata")
    .join("module_doc.json");

  // uncomment to regenerate symbol_search.json
  //std::fs::write(&module_docs_json_path, &file_json);

  let module_docs_json = fs::read_to_string(module_docs_json_path).unwrap();
  assert_eq!(file_json, module_docs_json);
}
