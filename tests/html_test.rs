// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use deno_ast::ModuleSpecifier;
use deno_doc::DocParser;
use deno_doc::DocParserOptions;
use deno_doc::ParseOutput;
use deno_doc::diff::DocDiff;
use deno_doc::html::pages::SymbolPage;
use deno_doc::html::*;
use deno_graph::BuildOptions;
use deno_graph::GraphKind;
use deno_graph::ModuleGraph;
use deno_graph::ast::CapturingModuleAnalyzer;
use deno_graph::source::LoadError;
use deno_graph::source::LoadFuture;
use deno_graph::source::LoadOptions;
use deno_graph::source::LoadResponse;
use deno_graph::source::Loader;
use deno_graph::source::MemoryLoader;
use deno_graph::source::Source;
use futures::future;
use indexmap::IndexMap;
use std::fs;
use std::rc::Rc;
use std::sync::Arc;

struct SourceFileLoader {}

impl Loader for SourceFileLoader {
  fn load(
    &self,
    specifier: &ModuleSpecifier,
    _options: LoadOptions,
  ) -> LoadFuture {
    let result = if specifier.scheme() == "file" {
      let path = specifier.to_file_path().unwrap();
      fs::read(path)
        .map(|content| {
          Some(LoadResponse::Module {
            specifier: specifier.clone(),
            maybe_headers: None,
            mtime: None,
            content: content.into(),
          })
        })
        .map_err(|err| LoadError::Other(Arc::new(err)))
    } else {
      Ok(None)
    };
    Box::pin(future::ready(result))
  }
}

struct EmptyResolver;

impl HrefResolver for EmptyResolver {
  fn resolve_path(
    &self,
    current: UrlResolveKind,
    target: UrlResolveKind,
  ) -> String {
    href_path_resolve(current, target)
  }

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

  fn resolve_source(&self, _location: &deno_doc::Location) -> Option<String> {
    None
  }

  fn resolve_external_jsdoc_module(
    &self,
    _module: &str,
    _symbol: Option<&str>,
  ) -> Option<(String, String)> {
    None
  }
}

impl UsageComposer for EmptyResolver {
  fn is_single_mode(&self) -> bool {
    true
  }

  fn compose(
    &self,
    current_resolve: UrlResolveKind,
    usage_to_md: UsageToMd,
  ) -> IndexMap<UsageComposerEntry, String> {
    current_resolve
      .get_file()
      .map(|current_file| {
        IndexMap::from([(
          UsageComposerEntry {
            name: "".to_string(),
            icon: None,
          },
          usage_to_md(current_file.path.as_str(), None),
        )])
      })
      .unwrap_or_default()
  }
}

async fn get_files(subpath: &str) -> ParseOutput {
  let files = fs::read_dir(
    std::env::current_dir()
      .unwrap()
      .join("tests")
      .join("testdata")
      .join(subpath),
  )
  .unwrap();

  let mut source_files: Vec<ModuleSpecifier> = files
    .into_iter()
    .map(|entry| {
      let entry = entry.unwrap();
      ModuleSpecifier::from_file_path(entry.path()).unwrap()
    })
    .collect();
  source_files.sort();

  let loader = SourceFileLoader {};
  let analyzer = CapturingModuleAnalyzer::default();
  let mut graph = ModuleGraph::new(GraphKind::TypesOnly);
  graph
    .build(
      source_files.clone(),
      Vec::new(),
      &loader,
      BuildOptions {
        module_analyzer: &analyzer,
        ..Default::default()
      },
    )
    .await;

  DocParser::new(
    &graph,
    &analyzer,
    &source_files,
    DocParserOptions {
      diagnostics: false,
      private: false,
    },
  )
  .unwrap()
  .parse()
  .unwrap()
}

#[tokio::test]
async fn html_doc_dts() {
  let ctx = GenerateCtx::create_basic(
    GenerateOptions {
      package_name: None,
      main_entrypoint: None,
      href_resolver: Rc::new(EmptyResolver),
      usage_composer: Rc::new(EmptyResolver),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Rc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
    },
    get_files("dts").await,
  )
  .unwrap();
  let files = generate(ctx).unwrap();

  let mut file_names = files.keys().collect::<Vec<_>>();
  file_names.sort();

  assert_eq!(
    file_names,
    [
      "./all_symbols.html",
      "./index.html",
      "./~/ResponseInit.html",
      "./~/ResponseInit.status.html",
      "./~/ResponseInit.statusText.html",
      "./~/WebSocket.OPEN.html",
      "./~/WebSocket.bufferedAmount.html",
      "./~/WebSocket.html",
      "./~/WebSocket.prototype.html",
      "comrak.css",
      "darkmode_toggle.js",
      "fuse.js",
      "page.css",
      "reset.css",
      "script.js",
      "search.js",
      "search_index.js",
      "styles.css"
    ]
  );

  for file_name in file_names {
    if !file_name.ends_with(".css") {
      insta::assert_snapshot!(files.get(file_name).unwrap());
    }
  }
}

#[tokio::test]
async fn html_doc_files_single() {
  let ctx = GenerateCtx::create_basic(
    GenerateOptions {
      package_name: None,
      main_entrypoint: None,
      href_resolver: Rc::new(EmptyResolver),
      usage_composer: Rc::new(EmptyResolver),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Rc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
    },
    get_files("single").await,
  )
  .unwrap();
  let files = generate(ctx).unwrap();

  let mut file_names = files.keys().collect::<Vec<_>>();
  file_names.sort();

  assert_eq!(
    file_names,
    [
      "./all_symbols.html",
      "./index.html",
      "./~/Bar.html",
      "./~/Bar.prototype.html",
      "./~/Foo.html",
      "./~/Foo.prototype.html",
      "./~/Foobar.html",
      "./~/Foobar.prototype.html",
      "comrak.css",
      "darkmode_toggle.js",
      "fuse.js",
      "page.css",
      "reset.css",
      "script.js",
      "search.js",
      "search_index.js",
      "styles.css",
    ]
  );

  for file_name in file_names {
    if !file_name.ends_with(".css") {
      insta::assert_snapshot!(files.get(file_name).unwrap());
    }
  }
}

#[tokio::test]
async fn html_doc_files_multiple() {
  let multiple_dir = std::env::current_dir()
    .unwrap()
    .join("tests")
    .join("testdata")
    .join("multiple");
  let mut rewrite_map = IndexMap::new();
  let main_specifier =
    ModuleSpecifier::from_file_path(multiple_dir.join("a.ts")).unwrap();
  rewrite_map.insert(main_specifier.clone(), ".".to_string());
  rewrite_map.insert(
    ModuleSpecifier::from_file_path(multiple_dir.join("b.ts")).unwrap(),
    "foo".to_string(),
  );
  rewrite_map.insert(
    ModuleSpecifier::from_file_path(multiple_dir.join("c.ts")).unwrap(),
    "c".to_string(),
  );
  rewrite_map.insert(
    ModuleSpecifier::from_file_path(multiple_dir.join("_d.ts")).unwrap(),
    "d".to_string(),
  );

  let ctx = GenerateCtx::create_basic(
    GenerateOptions {
      package_name: None,
      main_entrypoint: Some(main_specifier),
      href_resolver: Rc::new(EmptyResolver),
      usage_composer: Rc::new(EmptyResolver),
      rewrite_map: Some(rewrite_map),
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Rc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
    },
    get_files("multiple").await,
  )
  .unwrap();
  let files = generate(ctx).unwrap();

  let mut file_names = files.keys().collect::<Vec<_>>();
  file_names.sort();

  assert_eq!(
    file_names,
    [
      "./all_symbols.html",
      "./index.html",
      "./~/A.html",
      "./~/A.prototype.html",
      "./~/AbstractClass.html",
      "./~/AbstractClass.prototype.foo.html",
      "./~/AbstractClass.prototype.getter.html",
      "./~/AbstractClass.prototype.html",
      "./~/AbstractClass.prototype.method.html",
      "./~/B.html",
      "./~/B.prototype.html",
      "./~/Bar.html",
      "./~/Bar.prototype.html",
      "./~/Baz.bar.html",
      "./~/Baz.foo.html",
      "./~/Baz.html",
      "./~/CompoundType.bufferedAmount.html",
      "./~/CompoundType.html",
      "./~/EmptyInterface.html",
      "./~/Enum.html",
      "./~/Enum2.html",
      "./~/Foo.bar.html",
      "./~/Foo.html",
      "./~/Foo.prototype.\"><img src=x onerror=alert(1)>.html",
      "./~/Foo.prototype.[Symbol.iterator].html",
      "./~/Foo.prototype.foo.html",
      "./~/Foo.prototype.getter.html",
      "./~/Foo.prototype.getterAndSetter.html",
      "./~/Foo.prototype.html",
      "./~/Foo.prototype.methodWithOverloads.html",
      "./~/Foo.prototype.protectedProperty.html",
      "./~/Foo.prototype.readonlyProperty.html",
      "./~/Foo.prototype.setter.html",
      "./~/Foo.prototype.test.html",
      "./~/Foo.staticMethod.html",
      "./~/Foo.staticSetter.html",
      "./~/Foobar.html",
      "./~/Foobar.prototype.html",
      "./~/Hello.ab.html",
      "./~/Hello.computedMethod.html",
      "./~/Hello.html",
      "./~/Hello.optionalMethod.html",
      "./~/Hello.test.html",
      "./~/Hello.world.html",
      "./~/Hello.x.html",
      "./~/InterfaceWithIndexSignature.html",
      "./~/Testing.externalFunction.html",
      "./~/Testing.html",
      "./~/Testing.prototype.html",
      "./~/Testing.t.html",
      "./~/Testing.x.html",
      "./~/TypeAlias.html",
      "./~/anotherVariable.bar.html",
      "./~/anotherVariable.foo.html",
      "./~/anotherVariable.html",
      "./~/c.html",
      "./~/d.html",
      "./~/functionWithOptionalParameters.html",
      "./~/qaz.html",
      "./~/someVariable.html",
      "./~/x.html",
      "c/index.html",
      "c/~/x.html",
      "comrak.css",
      "d/index.html",
      "d/~/externalFunction.html",
      "darkmode_toggle.js",
      "foo/index.html",
      "foo/~/default.html",
      "foo/~/x.html",
      "fuse.js",
      "page.css",
      "reset.css",
      "script.js",
      "search.js",
      "search_index.js",
      "styles.css"
    ]
  );

  for file_name in file_names {
    if !file_name.ends_with(".css") {
      insta::assert_snapshot!(files.get(file_name).unwrap());
    }
  }
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
  rewrite_map.insert(
    ModuleSpecifier::from_file_path(multiple_dir.join("c.ts")).unwrap(),
    "c".to_string(),
  );
  rewrite_map.insert(
    ModuleSpecifier::from_file_path(multiple_dir.join("_d.ts")).unwrap(),
    "d".to_string(),
  );

  let ctx = GenerateCtx::new(
    GenerateOptions {
      package_name: None,
      main_entrypoint: Some(
        ModuleSpecifier::from_file_path(multiple_dir.join("a.ts")).unwrap(),
      ),
      href_resolver: Rc::new(EmptyResolver),
      usage_composer: Rc::new(EmptyResolver),
      rewrite_map: Some(rewrite_map),
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Rc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
    },
    None,
    Default::default(),
    doc_nodes_by_url,
    None,
  )
  .unwrap();

  let mut files = vec![];

  {
    for (short_path, doc_nodes) in &ctx.doc_nodes {
      let symbol_pages =
        generate_symbol_pages_for_module(&ctx, short_path, doc_nodes);

      files.extend(symbol_pages.into_iter().map(
        |symbol_page| match symbol_page {
          SymbolPage::Symbol {
            breadcrumbs_ctx,
            symbol_group_ctx,
            toc_ctx,
            categories_panel,
          } => {
            let root = ctx.resolve_path(
              UrlResolveKind::Symbol {
                file: short_path,
                symbol: &symbol_group_ctx.name,
              },
              UrlResolveKind::Root,
            );

            let html_head_ctx = pages::HtmlHeadCtx::new(
              &ctx,
              &root,
              Some(&symbol_group_ctx.name),
              Some(short_path),
            );

            Some(pages::SymbolPageCtx {
              html_head_ctx,
              symbol_group_ctx,
              breadcrumbs_ctx,
              toc_ctx,
              disable_search: false,
              categories_panel,
            })
          }
          SymbolPage::Redirect { .. } => None,
        },
      ));
    }
  }

  insta::assert_json_snapshot!(files);
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
  rewrite_map.insert(
    ModuleSpecifier::from_file_path(multiple_dir.join("c.ts")).unwrap(),
    "c".to_string(),
  );
  rewrite_map.insert(
    ModuleSpecifier::from_file_path(multiple_dir.join("_d.ts")).unwrap(),
    "_d".to_string(),
  );

  let ctx = GenerateCtx::new(
    GenerateOptions {
      package_name: None,
      main_entrypoint: Some(
        ModuleSpecifier::from_file_path(multiple_dir.join("a.ts")).unwrap(),
      ),
      href_resolver: Rc::new(EmptyResolver),
      usage_composer: Rc::new(EmptyResolver),
      rewrite_map: Some(rewrite_map),
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Rc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
    },
    None,
    Default::default(),
    doc_nodes_by_url,
    None,
  )
  .unwrap();

  let search_index = generate_search_index(&ctx);

  insta::assert_json_snapshot!(search_index);
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
  rewrite_map.insert(
    ModuleSpecifier::from_file_path(multiple_dir.join("c.ts")).unwrap(),
    "c".to_string(),
  );
  rewrite_map.insert(
    ModuleSpecifier::from_file_path(multiple_dir.join("_d.ts")).unwrap(),
    "d".to_string(),
  );

  let ctx = GenerateCtx::new(
    GenerateOptions {
      package_name: None,
      main_entrypoint: Some(
        ModuleSpecifier::from_file_path(multiple_dir.join("a.ts")).unwrap(),
      ),
      href_resolver: Rc::new(EmptyResolver),
      usage_composer: Rc::new(EmptyResolver),
      rewrite_map: Some(rewrite_map),
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Rc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
    },
    None,
    FileMode::Single,
    doc_nodes_by_url,
    None,
  )
  .unwrap();

  let mut module_docs = vec![];

  for (short_path, doc_nodes) in &ctx.doc_nodes {
    let render_ctx = RenderContext::new(
      &ctx,
      doc_nodes,
      UrlResolveKind::File { file: short_path },
    );
    let module_doc = jsdoc::ModuleDocCtx::new(
      &render_ctx,
      short_path,
      !short_path.is_main,
      false,
    );

    module_docs.push(module_doc);
  }

  insta::assert_json_snapshot!(module_docs);
}

async fn parse_source(source: &str) -> ParseOutput {
  let specifier = ModuleSpecifier::parse("file:///mod.ts").unwrap();
  let mut loader = MemoryLoader::default();
  loader.add_source(
    specifier.clone(),
    Source::Module {
      specifier: specifier.to_string(),
      maybe_headers: None,
      content: source.to_string(),
    },
  );

  let analyzer = CapturingModuleAnalyzer::default();
  let mut graph = ModuleGraph::new(GraphKind::TypesOnly);
  graph
    .build(
      vec![specifier.clone()],
      Vec::new(),
      &loader,
      BuildOptions {
        module_analyzer: &analyzer,
        ..Default::default()
      },
    )
    .await;

  DocParser::new(
    &graph,
    &analyzer,
    &[specifier],
    DocParserOptions {
      private: false,
      diagnostics: false,
    },
  )
  .unwrap()
  .parse()
  .unwrap()
}

async fn parse_file(path: &std::path::Path) -> ParseOutput {
  let content = fs::read_to_string(path).unwrap();
  parse_source(&content).await
}

#[tokio::test]
async fn diff_kind_change() {
  let test_dir = std::env::current_dir()
    .unwrap()
    .join("tests")
    .join("testdata")
    .join("diff_kind_change");

  let old_docs = parse_file(&test_dir.join("old.ts")).await;
  let new_docs = parse_file(&test_dir.join("new.ts")).await;

  let diff = DocDiff::diff(&old_docs, &new_docs);

  let ctx = GenerateCtx::create_basic_with_diff(
    GenerateOptions {
      package_name: None,
      main_entrypoint: None,
      href_resolver: Rc::new(EmptyResolver),
      usage_composer: Rc::new(EmptyResolver),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Rc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
    },
    new_docs,
    diff,
  )
  .unwrap();

  let json_output = generate_json(ctx).unwrap();

  let mut keys: Vec<_> = json_output.keys().collect();
  keys.sort();

  let pages: Vec<_> = keys
    .iter()
    .filter(|k| k.ends_with(".json"))
    .map(|k| (k.to_string(), json_output.get(*k).unwrap().clone()))
    .collect();

  insta::assert_json_snapshot!(pages);
}

#[tokio::test]
async fn diff_comprehensive() {
  let test_dir = std::env::current_dir()
    .unwrap()
    .join("tests")
    .join("testdata")
    .join("diff_comprehensive");

  let old_docs = parse_file(&test_dir.join("old.ts")).await;
  let new_docs = parse_file(&test_dir.join("new.ts")).await;

  let diff = DocDiff::diff(&old_docs, &new_docs);

  // Test with diff_only: false (full output with diff annotations)
  let ctx = GenerateCtx::create_basic_with_diff(
    GenerateOptions {
      package_name: None,
      main_entrypoint: None,
      href_resolver: Rc::new(EmptyResolver),
      usage_composer: Rc::new(EmptyResolver),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Rc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
    },
    new_docs.clone(),
    diff.clone(),
  )
  .unwrap();

  let json_output = generate_json(ctx).unwrap();

  let mut keys: Vec<_> = json_output.keys().collect();
  keys.sort();

  let pages: Vec<_> = keys
    .iter()
    .filter(|k| k.ends_with(".json"))
    .map(|k| (k.to_string(), json_output.get(*k).unwrap().clone()))
    .collect();

  insta::assert_json_snapshot!("diff_comprehensive_full", pages);

  // Test with diff_only: true (only changed content)
  let ctx = GenerateCtx::create_basic_with_diff(
    GenerateOptions {
      package_name: None,
      main_entrypoint: None,
      href_resolver: Rc::new(EmptyResolver),
      usage_composer: Rc::new(EmptyResolver),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Rc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: true,
    },
    new_docs,
    diff,
  )
  .unwrap();

  let json_output = generate_json(ctx).unwrap();

  let mut keys: Vec<_> = json_output.keys().collect();
  keys.sort();

  let pages: Vec<_> = keys
    .iter()
    .filter(|k| k.ends_with(".json"))
    .map(|k| (k.to_string(), json_output.get(*k).unwrap().clone()))
    .collect();

  insta::assert_json_snapshot!("diff_comprehensive_diff_only", pages);
}
