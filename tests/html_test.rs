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
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
    },
    get_files("dts").await,
    None,
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
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
    },
    get_files("single").await,
    None,
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
async fn html_doc_import_linking() {
  let ctx = GenerateCtx::create_basic(
    GenerateOptions {
      package_name: None,
      main_entrypoint: None,
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
    },
    get_files("import_linking").await,
    None,
  )
  .unwrap();
  let files = generate(ctx).unwrap();

  // the non-exported `Internal` type alias must not get its own page
  assert!(!files.keys().any(|file| file.contains("Internal")));

  // `import type * as t` references link to the other entrypoint
  let expression = files.get("mod.ts/~/expression.html").unwrap();
  assert!(
    expression.contains(
      r##"<a href="../.././types.ts/~/Expression.html" class="link td-ref">t.Expression</a>"##
    ),
    "namespace import type reference is not linked: {expression}"
  );

  // `{@link t.Expression}` in the jsdoc links as well
  assert!(
    expression.matches("types.ts/~/Expression.html").count() > 1,
    "namespace import jsdoc link is not linked: {expression}"
  );

  // `import type { Statement as Stmt }` references link to the original
  // symbol in the other entrypoint
  let statement = files.get("mod.ts/~/statement.html").unwrap();
  assert!(
    statement.contains(
      r##"<a href="../.././types.ts/~/Statement.html" class="link td-ref">Stmt</a>"##
    ),
    "aliased import type reference is not linked: {statement}"
  );

  // a reference to a symbol that doesn't exist in the imported module must
  // not be linked
  let missing = files.get("mod.ts/~/missing.html").unwrap();
  assert!(
    missing.contains(r#"<span class="td-ref">t.DoesNotExist</span>"#),
    "nonexistent symbol must not be linked: {missing}"
  );

  // a reference to a non-exported symbol must not be linked
  let alias = files.get("types.ts/~/Scope.Alias.html").unwrap();
  assert!(
    alias.contains(r#"<span class="td-ref">Internal</span>"#),
    "internal symbol must not be linked: {alias}"
  );
}

#[tokio::test]
async fn html_doc_import_linking_internal_file() {
  let dir = std::env::current_dir()
    .unwrap()
    .join("tests")
    .join("testdata")
    .join("import_linking_internal");
  let source_files =
    vec![ModuleSpecifier::from_file_path(dir.join("mod.ts")).unwrap()];

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

  let parse_output = DocParser::new(
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
  .unwrap();

  let ctx = GenerateCtx::create_basic(
    GenerateOptions {
      package_name: None,
      main_entrypoint: None,
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
    },
    parse_output,
    None,
  )
  .unwrap();
  let files = generate(ctx).unwrap();

  // `API.Audio` references a file that isn't a documented entrypoint, but
  // the package re-exports `Audio`, so the reference links to that
  // re-export.
  let client = files.get("./~/Client.html").unwrap();
  assert!(
    client.contains(
      r##"<a href="../././~/Audio.html" class="link td-ref">API.Audio</a>"##
    ),
    "reference to a symbol of an internal file must link to the package's re-export: {client}"
  );
}

#[tokio::test]
async fn html_doc_empty_sections() {
  let ctx = GenerateCtx::create_basic(
    GenerateOptions {
      package_name: None,
      main_entrypoint: None,
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
    },
    get_files("empty_sections").await,
    None,
  )
  .unwrap();
  let files = generate(ctx).unwrap();

  // the module's only symbol is private, so no "Functions" section header
  // (or matching ToC entry) may be rendered (jsr-io/jsr#918)
  let index = files.get("./index.html").unwrap();
  assert!(!index.contains("Functions"), "{index}");
  let all_symbols = files.get("./all_symbols.html").unwrap();
  assert!(!all_symbols.contains("Functions"), "{all_symbols}");
}

#[tokio::test]
async fn html_doc_symbol_listing_limit() {
  async fn generate_with_limit(
    limit: Option<usize>,
  ) -> std::collections::HashMap<String, String> {
    let ctx = GenerateCtx::create_basic(
      GenerateOptions {
        package_name: None,
        main_entrypoint: None,
        href_resolver: Arc::new(EmptyResolver),
        usage_composer: Some(Arc::new(EmptyResolver)),
        rewrite_map: None,
        category_docs: None,
        disable_search: false,
        symbol_redirect_map: None,
        default_symbol_map: None,
        markdown_renderer: comrak::create_renderer(None, None, None),
        markdown_stripper: Arc::new(comrak::strip),
        head_inject: None,
        id_prefix: None,
        diff_only: false,
        symbol_listing_limit: limit,
      },
      get_files("symbol_listing_limit").await,
      None,
    )
    .unwrap();
    generate(ctx).unwrap()
  }

  // without a limit everything is listed
  let files = generate_with_limit(None).await;
  let index = files.get("mod.ts/index.html").unwrap();
  assert!(index.contains(r#"id="namespace_outer_inner_innerfnone""#));
  assert!(!index.contains("omitted from this overview"));
  // the "Symbols" panel lists namespace members under their qualified name
  // and links to the qualified page (jsr-io/jsr#1301)
  let panel = index.split(r#"<nav class="topSymbols">"#).nth(1).unwrap();
  assert!(
    panel.contains(r#"title="outer.outerFnOne""#),
    "the Symbols panel must contain the qualified member: {panel}"
  );
  assert!(
    panel.contains("outer.outerFnOne.html"),
    "the Symbols panel must link the qualified page: {panel}"
  );
  assert!(!index.contains(r#"title="outerFnOne""#));

  // the flattened listing has 10 rows; with a limit of 8, the deepest
  // namespace members are dropped first
  let files = generate_with_limit(Some(8)).await;
  let index = files.get("mod.ts/index.html").unwrap();
  assert!(index.contains(r#"id="namespace_toplevelfn""#));
  assert!(index.contains(r#"id="namespace_outer_outerfnone""#));
  assert!(!index.contains(r#"id="namespace_outer_inner_innerfnone""#));
  assert!(index.contains("omitted from this overview"));

  // levels are dropped whole: limit 4 also falls through to the 3
  // top-level rows, since keeping any of the 4 depth-1 rows would list the
  // `outer` namespace only partially
  let files = generate_with_limit(Some(4)).await;
  let index = files.get("mod.ts/index.html").unwrap();
  assert!(index.contains(r#"id="namespace_toplevelfn""#));
  assert!(!index.contains(r#"id="namespace_outer_outerfnone""#));
  assert!(index.contains("omitted from this overview"));

  // top-level symbols are always rendered, even over the limit
  let files = generate_with_limit(Some(2)).await;
  let index = files.get("mod.ts/index.html").unwrap();
  assert!(index.contains(r#"id="namespace_toplevelfn""#));
  assert!(index.contains(r#"id="namespace_toplevelinterface""#));
  assert!(!index.contains(r#"id="namespace_outer_outerfnone""#));
  assert!(index.contains("omitted from this overview"));
}

#[tokio::test]
async fn html_doc_signature_examples() {
  let ctx = GenerateCtx::create_basic(
    GenerateOptions {
      package_name: None,
      main_entrypoint: None,
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
    },
    get_files("signature_examples").await,
    None,
  )
  .unwrap();
  let files = generate(ctx).unwrap();

  // the @example on an interface construct signature renders inline, since
  // construct signatures have no dedicated page
  let page = files.get("./~/ExampleConstructor.html").unwrap();
  assert!(
    page.contains("const example = new Example(1);"),
    "construct signature example is not rendered: {page}"
  );

  // same for call signatures
  let page = files.get("./~/ExampleCallable.html").unwrap();
  assert!(
    page.contains("exampleCallable(2);"),
    "call signature example is not rendered: {page}"
  );
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
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: Some(rewrite_map),
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
    },
    get_files("multiple").await,
    None,
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
      "./~/AbstractClass.html",
      "./~/AbstractClass.prototype.foo.html",
      "./~/AbstractClass.prototype.getter.html",
      "./~/AbstractClass.prototype.html",
      "./~/AbstractClass.prototype.method.html",
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
      "./~/Foo.prototype.%22%3E%3Cimg%20src=x%20onerror=alert(1)%3E.html",
      "./~/Foo.prototype.[Symbol.iterator].html",
      "./~/Foo.prototype.divergentAccessor.html",
      "./~/Foo.prototype.foo.html",
      "./~/Foo.prototype.getter.html",
      "./~/Foo.prototype.getterAndSetter.html",
      "./~/Foo.prototype.html",
      "./~/Foo.prototype.protectedProperty.html",
      "./~/Foo.prototype.readonlyProperty.html",
      "./~/Foo.prototype.setter.html",
      "./~/Foo.prototype.test.html",
      "./~/Foo.staticMethod.html",
      "./~/Foo.staticSetter.html",
      "./~/Foobar.html",
      "./~/Foobar.prototype.html",
      "./~/Hello.ab.html",
      "./~/Hello.accessor.html",
      "./~/Hello.computedMethod.html",
      "./~/Hello.divergentAccessor.html",
      "./~/Hello.html",
      "./~/Hello.optionalMethod.html",
      "./~/Hello.readonlyAccessor.html",
      "./~/Hello.test.html",
      "./~/Hello.world.html",
      "./~/Hello.writeonlyAccessor.html",
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

  // Every relative link in the generated output must point at a file that was
  // actually written. Regression test for #835, where links to
  // namespace-qualified re-exports (`export * as`) pointed at pages that were
  // never generated.
  fn resolve_relative(base_file: &str, href: &str) -> Option<String> {
    let mut segments = base_file
      .split('/')
      .filter(|s| !s.is_empty() && *s != ".")
      .collect::<Vec<_>>();
    // pop the file name to get the base directory
    segments.pop();

    for segment in href.split('/') {
      match segment {
        "" | "." => {}
        ".." => {
          segments.pop()?;
        }
        _ => segments.push(segment),
      }
    }

    if href.ends_with('/') || href == ".." || href == "." {
      segments.push("index.html");
    }

    Some(segments.join("/"))
  }

  fn extract_attr_values<'a>(
    content: &'a str,
    attr: &str,
  ) -> impl Iterator<Item = &'a str> {
    let needle = format!("{attr}=\"");
    content
      .match_indices::<&str>(&needle)
      .filter(|(index, _)| {
        // require preceding whitespace so e.g. `data-id="` doesn't match `id="`
        index
          .checked_sub(1)
          .is_some_and(|prev| content.as_bytes()[prev].is_ascii_whitespace())
      })
      .map(|(index, needle)| {
        let value = &content[index + needle.len()..];
        &value[..value.find('"').unwrap()]
      })
      .collect::<Vec<_>>()
      .into_iter()
  }

  let mut links = Vec::new();
  let mut ids_per_file =
    std::collections::HashMap::<&str, std::collections::HashSet<&str>>::new();
  for (file_name, content) in &files {
    if !file_name.ends_with(".html") {
      continue;
    }
    for attr in ["href", "src"] {
      for value in extract_attr_values(content, attr) {
        links.push((file_name.as_str(), value.replace("&#x2F;", "/")));
      }
    }
    ids_per_file.insert(
      file_name.as_str(),
      extract_attr_values(content, "id").collect(),
    );
  }
  // search index urls are relative to the root
  let search_index = files.get("search_index.js").unwrap();
  for (index, _) in search_index.match_indices("\"url\":\"") {
    let url = &search_index[index + "\"url\":\"".len()..];
    let url = &url[..url.find('"').unwrap()];
    links.push(("search_index.js", url.to_string()));
  }

  for (file_name, link) in links {
    // skip absolute urls (https://...) and scheme-prefixed links (mailto:)
    if link.split(['/', '#', '?']).next().unwrap().contains(':') {
      continue;
    }
    let (path, fragment) = match link.split_once('#') {
      Some((path, fragment)) => (path, Some(fragment)),
      None => (link.as_str(), None),
    };

    let target_file = if path.is_empty() {
      // fragment-only link into the current file
      file_name.to_string()
    } else {
      let resolved = resolve_relative(file_name, path).unwrap_or_else(|| {
        panic!("link {link} in {file_name} escapes the root")
      });
      [resolved.clone(), format!("./{resolved}")]
        .into_iter()
        .find(|resolved| files.contains_key(resolved))
        .unwrap_or_else(|| {
          panic!(
            "broken link {link} in {file_name}: {resolved} was not generated"
          )
        })
    };

    // a fragment must point at an id the target file declares
    if let Some(fragment) = fragment
      && !fragment.is_empty()
      && let Some(ids) = ids_per_file.get(target_file.as_str())
    {
      assert!(
        ids.contains(fragment),
        "broken fragment {link} in {file_name}: {target_file} has no id \"{fragment}\""
      );
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
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: Some(rewrite_map),
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
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
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: Some(rewrite_map),
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
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
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: Some(rewrite_map),
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
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

async fn parse_sources_multi(sources: &[(&str, &str)]) -> ParseOutput {
  let mut loader = MemoryLoader::default();
  let mut roots = Vec::new();
  for (specifier, source) in sources {
    let specifier = ModuleSpecifier::parse(specifier).unwrap();
    loader.add_source(
      specifier.clone(),
      Source::Module {
        specifier: specifier.to_string(),
        maybe_headers: None,
        content: source.to_string(),
      },
    );
    roots.push(specifier);
  }

  let analyzer = CapturingModuleAnalyzer::default();
  let mut graph = ModuleGraph::new(GraphKind::TypesOnly);
  graph
    .build(
      roots.clone(),
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
    &roots,
    DocParserOptions {
      private: false,
      diagnostics: false,
    },
  )
  .unwrap()
  .parse()
  .unwrap()
}

// Regression test for https://github.com/denoland/deno_doc/issues/724:
// a member whose name comes from a string literal can contain characters that
// are invalid in a file name (Windows reserves `<>:"/\|?*`) or that break a
// URL. The generated page path must be filesystem- and URL-safe, and the link
// to it must use the same encoded form so it still resolves.
#[tokio::test]
async fn html_symbol_name_unsafe_chars_in_path() {
  let source = r#"
export class Foo {
  "a/b<c>": number = 0;
}
"#;

  let ctx = GenerateCtx::create_basic(
    GenerateOptions {
      package_name: None,
      main_entrypoint: None,
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
    },
    parse_source(source).await,
    None,
  )
  .unwrap();

  let files = generate(ctx).unwrap();

  // No generated file path may contain characters that are reserved on common
  // filesystems or that need escaping in a URL.
  for name in files.keys() {
    assert!(
      !name.contains(['"', '<', '>', '|', '?', '*', '\\', ' ']),
      "generated file path is not filesystem/URL safe: {name}"
    );
  }

  // The member still gets a page, under a percent-encoded name
  // (`a/b<c>` -> `a%2Fb%3Cc%3E`).
  assert!(
    files
      .keys()
      .any(|k| k.ends_with("/~/Foo.prototype.a%2Fb%3Cc%3E.html")),
    "expected a percent-encoded page for the member, got: {:?}",
    files.keys().collect::<Vec<_>>()
  );

  // No rendered page may contain the raw (unencoded) link, which would both
  // 404 and inject markup into the surrounding attribute.
  assert!(
    files.values().all(|content| !content.contains("a/b<c>")),
    "a rendered page contains an unsafe, unencoded symbol link"
  );
}

// Regression test for https://github.com/denoland/deno_doc/issues/801:
// `@event`, `@fires`/`@emits`, and `@listens` JSDoc tags (parsed since #800)
// must be rendered in the HTML output. They previously parsed but were dropped
// from the rendered symbol page.
#[tokio::test]
async fn html_event_tags_rendered() {
  let source = r#"
/**
 * A clickable button.
 *
 * @event click - fired when the button is clicked
 * @fires submit
 * @emits change
 * @listens keydown
 */
export class Button {}
"#;

  let ctx = GenerateCtx::create_basic(
    GenerateOptions {
      package_name: None,
      main_entrypoint: None,
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
    },
    parse_source(source).await,
    None,
  )
  .unwrap();

  let files = generate(ctx).unwrap();

  let button_page = files
    .iter()
    .find(|(k, _)| k.ends_with("/~/Button.html"))
    .map(|(_, v)| v)
    .expect("expected a generated page for Button");

  // Each tag family renders as a labeled section, and every event name (with
  // `@emits` treated the same as `@fires`) appears in the page.
  for needle in [
    "Events", "Fires", "Listens", "click", "submit", "change", "keydown",
  ] {
    assert!(
      button_page.contains(needle),
      "Button page is missing {needle:?}"
    );
  }
}

// Regression test for https://github.com/denoland/deno_doc/issues/590:
// `@internal` symbols are excluded from the rendered listings but were still
// emitted into the search index, leaving them findable. This applied to both
// named and default exports. `@ignore` symbols are already dropped entirely.
#[tokio::test]
async fn html_internal_symbols_excluded_from_search() {
  let source = r#"
/** @internal */
export function internalNamed(): void {}

/** A visible function. */
export function visible(): void {}

/** @internal */
export default function (): void {}
"#;

  let ctx = GenerateCtx::create_basic(
    GenerateOptions {
      package_name: None,
      main_entrypoint: Some(ModuleSpecifier::parse("file:///mod.ts").unwrap()),
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
    },
    parse_source(source).await,
    None,
  )
  .unwrap();

  let files = generate(ctx).unwrap();
  let search = files
    .get("search_index.js")
    .expect("search index should be generated");

  // The visible export is searchable.
  assert!(
    search.contains("/~/visible.html"),
    "visible export should be in the search index"
  );
  // `@internal` named and default exports must not be searchable.
  assert!(
    !search.contains("/~/internalNamed.html"),
    "@internal named export leaked into the search index"
  );
  assert!(
    !search.contains("/~/default.html"),
    "@internal default export leaked into the search index"
  );
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

  let ctx = GenerateCtx::create_basic(
    GenerateOptions {
      package_name: None,
      main_entrypoint: None,
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
    },
    new_docs,
    Some(diff),
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
  let ctx = GenerateCtx::create_basic(
    GenerateOptions {
      package_name: None,
      main_entrypoint: None,
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
    },
    new_docs.clone(),
    Some(diff.clone()),
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
  let ctx = GenerateCtx::create_basic(
    GenerateOptions {
      package_name: None,
      main_entrypoint: None,
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: true,
      symbol_listing_limit: None,
    },
    new_docs,
    Some(diff),
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

// A namespace re-export (`export * as`) of another documented module produces
// reference declarations; their resolved nodes get the documenting module as
// `origin` (so links resolve, see #835) while diff data stays keyed by the
// declaring module (`declared_origin`). This exercises both at once: the diff
// annotations of the re-exported symbol must survive the origin rewrite.
#[tokio::test]
async fn diff_namespace_reexport() {
  let entrypoint = "export * as dep from \"./dep.ts\";\n";
  let old = parse_sources_multi(&[
    ("file:///mod.ts", entrypoint),
    (
      "file:///dep.ts",
      "export function depFunc(a: string): void {}\n",
    ),
  ])
  .await;
  let new = parse_sources_multi(&[
    ("file:///mod.ts", entrypoint),
    (
      "file:///dep.ts",
      "export function depFunc(a: string, b: number): void {}\n",
    ),
  ])
  .await;

  let diff = DocDiff::diff(&old, &new);

  let ctx = GenerateCtx::create_basic(
    GenerateOptions {
      package_name: None,
      main_entrypoint: Some(ModuleSpecifier::parse("file:///mod.ts").unwrap()),
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
    },
    new,
    Some(diff),
  )
  .unwrap();

  let json_output = generate_json(ctx).unwrap();

  // The namespace-qualified page is written under the documenting module and
  // the namespace listing links to it there (#835).
  let namespace_page = json_output.get("mod.ts/~/dep.json").unwrap();
  assert!(
    namespace_page.contains(r#""href":"../.././mod.ts/~/dep.depFunc.html""#),
    "namespace listing does not link to the documenting module's page: {namespace_page}"
  );
  assert!(json_output.contains_key("mod.ts/~/dep.depFunc.json"));

  // The re-exported symbol still carries its declaration-level diff: the
  // added parameter `b` must be annotated, which requires looking up the diff
  // index by the declaring module (dep.ts), not the documenting one.
  let qualified_page = json_output.get("mod.ts/~/dep.depFunc.json").unwrap();
  assert!(
    qualified_page.contains(r#""diff_status":{"kind":"added"}"#),
    "re-exported symbol lost its declaration-level diff annotations: {qualified_page}"
  );
  assert!(
    qualified_page.contains(r#""diff_status":{"kind":"modified"}"#),
    "re-exported symbol lost its modified status: {qualified_page}"
  );
}

/// `.repr` is package-controlled and must never reach the HTML/JSON output
/// unescaped (reported privately, fixed in #834). A markup-carrying
/// string-literal type is placed on every surface that interpolates `.repr`:
/// `@throws` types (content, modified old value, removed entry), type param
/// constraint/default old values, and `super_type_params_added`/`_removed`.
#[tokio::test]
async fn repr_markup_is_escaped() {
  let old_source = r#"
export class Base<T> {}

/**
 * @throws {"<img src=x onerror=alert(1)>/mod-old"} modified throws
 * @throws {"<img src=x onerror=alert(1)>/removed"} removed throws
 */
export function thrower(): void {}

export class Holder<
  T extends "<img src=x onerror=alert(1)>/old-constraint" =
    "<img src=x onerror=alert(1)>/old-default",
> extends Base<"<img src=x onerror=alert(1)>/old-super"> {}
"#;

  let new_source = r#"
export class Base<T> {}

/**
 * @throws {"<img src=x onerror=alert(1)>/mod-new"} modified throws
 */
export function thrower(): void {}

export class Holder<T extends "new-constraint" = "new-default">
  extends Base<"<img src=x onerror=alert(1)>/new-super"> {}
"#;

  let old_docs = parse_source(old_source).await;
  let new_docs = parse_source(new_source).await;
  let diff = DocDiff::diff(&old_docs, &new_docs);

  let make_ctx = || {
    GenerateCtx::create_basic(
      GenerateOptions {
        package_name: None,
        main_entrypoint: None,
        href_resolver: Arc::new(EmptyResolver),
        usage_composer: Some(Arc::new(EmptyResolver)),
        rewrite_map: None,
        category_docs: None,
        disable_search: false,
        symbol_redirect_map: None,
        default_symbol_map: None,
        markdown_renderer: comrak::create_renderer(None, None, None),
        markdown_stripper: Arc::new(comrak::strip),
        head_inject: None,
        id_prefix: None,
        diff_only: false,
        symbol_listing_limit: None,
      },
      new_docs.clone(),
      Some(diff.clone()),
    )
    .unwrap()
  };

  let files = generate(make_ctx()).unwrap();
  for (name, content) in &files {
    assert!(
      !content.contains("<img src=x"),
      "unescaped .repr in generated file {name}"
    );
  }
  assert!(
    files
      .values()
      .any(|content| content.contains("&lt;img src=x")),
    "expected escaped .repr in HTML output"
  );

  let json_output = generate_json(make_ctx()).unwrap();
  let serialized = serde_json::to_string(&json_output).unwrap();
  assert!(
    !serialized.contains("<img src=x"),
    "unescaped .repr in JSON output"
  );
  assert!(
    serialized.contains("&lt;img src=x"),
    "expected escaped .repr in JSON output"
  );
}

/// Verify that README headings in the module doc TOC:
/// 1. Appear before @example entries (matching the rendered page order)
/// 2. Are not inflated to deeper nesting levels by the offset state
#[tokio::test]
async fn readme_toc_order_with_examples() {
  let source = r#"
/**
 * ## Installation
 *
 * Install the library.
 *
 * ## Usage
 *
 * Use the library.
 *
 * ## API Reference
 *
 * The API reference.
 *
 * @example My Example
 * ```ts
 * hello();
 * ```
 *
 * @module
 */

/** A simple function. */
export function hello(): string {
  return "hello";
}
"#;

  let doc_nodes_by_url = parse_source(source).await;

  let specifier = ModuleSpecifier::parse("file:///mod.ts").unwrap();

  let ctx = GenerateCtx::create_basic(
    GenerateOptions {
      package_name: None,
      main_entrypoint: Some(specifier),
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
    },
    doc_nodes_by_url,
    None,
  )
  .unwrap();

  let files = generate(ctx).unwrap();
  let index_html = files.get("./index.html").unwrap();

  // README headings should appear before the Examples section in the TOC,
  // matching the page layout where the markdown body comes before @example sections.
  let readme_heading_pos = index_html
    .find("title=\"Installation\"")
    .expect("Installation heading not found in TOC");
  let examples_pos = index_html
    .find("title=\"Examples\"")
    .expect("Examples heading not found in TOC");

  assert!(
    readme_heading_pos < examples_pos,
    "README headings should appear before Examples in the TOC"
  );

  // Verify README headings are in document order
  let headings = ["Installation", "Usage", "API Reference"];
  let positions: Vec<usize> = headings
    .iter()
    .map(|h| {
      index_html
        .find(&format!("title=\"{}\"", h))
        .unwrap_or_else(|| panic!("heading '{}' not found in TOC", h))
    })
    .collect();

  for window in positions.windows(2) {
    assert!(
      window[0] < window[1],
      "TOC headings are not in document order"
    );
  }

  // Verify heading levels aren't inflated: README h2 headings should NOT be
  // nested deeper than the Examples section (level 1). If the offset leaked,
  // they'd be at level 4 and appear as deeply nested sub-items.
  // In the correct output, README headings at level 2 nest directly under the
  // top-level list, not under a third-level nested list.
  let nav_start = index_html.find("documentNavigation").unwrap();
  let nav_section = &index_html[nav_start..];
  let nav_end = nav_section.find("</nav>").unwrap();
  let nav_html = &nav_section[..nav_end];

  // Count nesting depth of the first README heading (Installation).
  // It should be in at most one <ul> nesting (the root <ul> + one sub-<ul>
  // for level 2), not two or more sub-<ul>s which would indicate inflated levels.
  let before_installation = &nav_html[..nav_html.find("Installation").unwrap()];
  let ul_depth = before_installation.matches("<ul>").count();
  assert!(
    ul_depth <= 2,
    "README headings are nested too deeply (depth {}), offset likely leaked from Examples",
    ul_depth
  );
}

// Parse every generated HTML file with a real HTML5 parser and assert it has
// no parse errors (missing closing tags, mismatched/invalid markup, etc.).
// See issue #634.
fn assert_generated_html_is_valid(
  files: &std::collections::HashMap<String, String>,
) {
  use html5ever::parse_document;
  use html5ever::tendril::TendrilSink;
  use markup5ever_rcdom::RcDom;

  let mut names: Vec<_> = files.keys().collect();
  names.sort();

  for name in names {
    if !name.ends_with(".html") {
      continue;
    }
    let content = &files[name];
    let dom = parse_document(RcDom::default(), Default::default())
      .from_utf8()
      .read_from(&mut content.as_bytes())
      .unwrap();
    assert!(
      dom.errors.is_empty(),
      "generated HTML for {name} is not valid: {:?}",
      dom.errors
    );
  }
}

#[tokio::test]
async fn html_output_is_valid() {
  // Validate the "multiple" fixture: it exercises the widest range of output
  // (classes, interfaces, enums, type aliases, namespaces, drilldown member
  // pages, redirects, and the all-symbols/index pages).
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
      main_entrypoint: Some(
        ModuleSpecifier::from_file_path(multiple_dir.join("a.ts")).unwrap(),
      ),
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: Some(rewrite_map),
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
    },
    get_files("multiple").await,
    None,
  )
  .unwrap();
  let files = generate(ctx).unwrap();

  assert_generated_html_is_valid(&files);
}

/// The page `<title>` should only escape characters that are unsafe in text
/// content (`&`, `<`, `>`). Characters like `/` are harmless and must not be
/// over-escaped into entities like `&#x2F;` (regression test for `I/O`
/// rendering as `I&#x2F;O`).
#[tokio::test]
async fn title_does_not_over_escape_slash() {
  let source = r#"
/** A simple function. */
export function hello(): string {
  return "hello";
}
"#;

  let doc_nodes_by_url = parse_source(source).await;

  let specifier = ModuleSpecifier::parse("file:///mod.ts").unwrap();

  let ctx = GenerateCtx::create_basic(
    GenerateOptions {
      // A scoped package name naturally contains a `/`.
      package_name: Some("@deno/cool".to_string()),
      main_entrypoint: Some(specifier),
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
    },
    doc_nodes_by_url,
    None,
  )
  .unwrap();

  let files = generate(ctx).unwrap();
  let index_html = files.get("./index.html").unwrap();

  // Scope the check to the `<title>` element itself: `&#x2F;` is still expected
  // elsewhere on the page (e.g. in attribute/URL values escaped by the
  // registry-wide escaper), but the title must read `@deno/cool`, not
  // `@deno&#x2F;cool`.
  let title_start = index_html.find("<title>").expect("title tag");
  let title_end = index_html.find("</title>").expect("title close tag");
  let title = &index_html[title_start..title_end];

  assert_eq!(
    title, "<title>@deno/cool documentation",
    "title should contain an unescaped `/`"
  );
  assert!(
    !title.contains("&#x2F;"),
    "title should not over-escape `/` into `&#x2F;`"
  );
}

/// Symbol names containing HTML-special characters must still be escaped in
/// the `<title>` so they cannot break out of the element.
#[tokio::test]
async fn title_escapes_html_special_chars() {
  let source = r#"
/** A class with a dangerous property name. */
export class Foo {
  "<script>" = 1;
}
"#;

  let doc_nodes_by_url = parse_source(source).await;

  let specifier = ModuleSpecifier::parse("file:///mod.ts").unwrap();

  let ctx = GenerateCtx::create_basic(
    GenerateOptions {
      package_name: None,
      main_entrypoint: Some(specifier),
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
    },
    doc_nodes_by_url,
    None,
  )
  .unwrap();

  let files = generate(ctx).unwrap();

  // Find the generated page whose `<title>` is built from the dangerous
  // property name, regardless of the exact file name.
  let title_page = files
    .iter()
    .filter(|(name, _)| name.ends_with(".html"))
    .map(|(_, content)| content)
    .find(|content| {
      content
        .lines()
        .any(|line| line.contains("<title>") && line.contains("script"))
    })
    .expect("a page whose title references the property should exist");

  assert!(
    !title_page.contains("<title>Foo.prototype.\"<script>\""),
    "raw `<script>` must not appear unescaped in the title"
  );
  assert!(
    title_page.contains("&lt;script&gt;"),
    "`<` and `>` in the title must be escaped"
  );
}

// Regression test for https://github.com/denoland/deno_doc/issues/552:
// `@since <version>` is parsed into `JsDocTag::Since` and printed by the
// terminal printer, but the HTML output dropped it entirely. It now renders as
// a tag chip, both on the symbol page itself and next to class/interface
// members.
#[tokio::test]
async fn html_since_tag_is_rendered() {
  let source = r#"
/**
 * A thing.
 *
 * @since 1.2.0
 */
export class Foo {
  /**
   * A method.
   *
   * @since 2.0.0
   */
  bar(): void {}
}

/** @since */
export function noVersion(): void {}
"#;

  let specifier = ModuleSpecifier::parse("file:///mod.ts").unwrap();

  let ctx = GenerateCtx::create_basic(
    GenerateOptions {
      package_name: None,
      main_entrypoint: Some(specifier),
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
    },
    parse_source(source).await,
    None,
  )
  .unwrap();

  let files = generate(ctx).unwrap();

  let foo = files
    .get("./~/Foo.html")
    .expect("a page for `Foo` should exist");

  // The class' own `@since`, in the large chip on the symbol page.
  assert!(
    foo.contains("Since 1.2.0"),
    "the symbol page should show a `Since 1.2.0` chip"
  );
  // The method's `@since`, in the small chip next to the member.
  assert!(
    foo.contains("Since 2.0.0"),
    "the member should show a `Since 2.0.0` chip"
  );

  // A bare `@since` carries no version, so it renders no chip.
  let no_version = files
    .get("./~/noVersion.html")
    .expect("a page for `noVersion` should exist");
  assert!(
    !no_version.contains("text-other"),
    "a `@since` without a version should not render a chip"
  );
}

// The anchorizer's character class keeps `"`, `<` and `&` (its ` -_` is a
// range over U+0020..=U+005F, not three literals), so a markdown heading can
// carry them into the anchor. Every other anchor is emitted through
// handlebars, which escapes; the heading adapter writes its attributes by hand
// and so has to escape them itself.
#[tokio::test]
async fn html_heading_anchor_is_escaped() {
  let source = r#"
/**
 * ## x"onmouseover=alert(1) y
 *
 * body
 */
export function foo(): void {}
"#;

  let ctx = GenerateCtx::create_basic(
    GenerateOptions {
      package_name: None,
      main_entrypoint: None,
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
    },
    parse_source(source).await,
    None,
  )
  .unwrap();

  let files = generate(ctx).unwrap();
  let page = files
    .get("./~/foo.html")
    .expect("function symbol page should be generated");

  // Both attribute contexts the anchor lands in must be escaped: the heading
  // itself, and the table-of-contents link that points at it.
  assert!(
    page.contains(r#"id="x&quot;onmouseover=alert(1)-y"#),
    "the heading anchor should be escaped for attribute context"
  );
  assert!(
    page.contains(r##"href="#x&quot;onmouseover=alert(1)-y"##),
    "the permalink href should be escaped the same way"
  );
}

// Regression test for https://github.com/denoland/deno_doc/issues/574:
// `@param` documentation must render for rest/spread parameters. The rendered
// parameter name carries a `...` prefix, but the JSDoc `@param` tag name does
// not, so the doc lookup has to match on the bare identifier.
#[tokio::test]
async fn html_rest_param_jsdoc() {
  let source = r#"
/**
 * Sums numbers.
 *
 * @param first the leading number
 * @param rest the trailing numbers
 */
export function sum(first: number, ...rest: number[]): number {
  return first + rest.reduce((a, b) => a + b, 0);
}
"#;

  let ctx = GenerateCtx::create_basic(
    GenerateOptions {
      package_name: None,
      main_entrypoint: None,
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
    },
    parse_source(source).await,
    None,
  )
  .unwrap();

  let files = generate(ctx).unwrap();

  let sum_page = files
    .get("./~/sum.html")
    .expect("function symbol page should be generated");

  // The non-rest parameter has always rendered its doc.
  assert!(
    sum_page.contains("the leading number"),
    "expected the first parameter's @param doc to render"
  );
  // The rest parameter's @param doc must render too (the bug in #574).
  assert!(
    sum_page.contains("the trailing numbers"),
    "expected the rest parameter's @param doc to render"
  );
}

// Follow-up to #574: matching `@param` tags to parameters is done by the bare
// identifier the parameter binds, unwrapping rest/default parameters. A
// destructuring pattern binds no single name, so it must not be matched by name
// (it would otherwise attach an unrelated tag's doc). A rest parameter that
// coexists with destructuring must still resolve correctly.
#[tokio::test]
async fn html_param_jsdoc_destructuring() {
  let source = r#"
/**
 * @param opts the options bag
 * @param rest the trailing numbers
 */
export function f(
  { a, b }: { a: number; b: number },
  ...rest: number[]
): void {}
"#;

  let ctx = GenerateCtx::create_basic(
    GenerateOptions {
      package_name: None,
      main_entrypoint: None,
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
    },
    parse_source(source).await,
    None,
  )
  .unwrap();

  let files = generate(ctx).unwrap();
  let page = files
    .get("./~/f.html")
    .expect("function symbol page should be generated");

  // The rest parameter still resolves its doc by bare identifier.
  assert!(
    page.contains("the trailing numbers"),
    "expected the rest parameter's @param doc to render"
  );
  // The destructuring parameter binds no name, so the `@param opts` tag (whose
  // name matches no parameter) must not be attached to it.
  assert!(
    !page.contains("the options bag"),
    "an unrelated @param tag was wrongly attached to a destructuring parameter"
  );
}

// Anchors are stored unescaped and escaped by whoever writes them into markup.
// Two sites write them by hand -- the table of contents and the markdown
// heading adapter -- and the anchorizer keeps `"`, `<` and `&` (its character
// class has ` -_` as a range over U+0020..=U+005F, not three literals). An
// unescaped anchor there breaks out of the attribute; escaping it twice instead
// leaves the `href` unable to match the `id` it points at.
#[tokio::test]
async fn html_anchors_are_escaped_once_everywhere() {
  let source = r#"
/**
 * A class.
 *
 * ## h"eading
 *
 * body
 */
export class A {
  '"><img src=x onerror=alert(1)>' = 0;
}
"#;

  let ctx = GenerateCtx::create_basic(
    GenerateOptions {
      package_name: None,
      main_entrypoint: None,
      href_resolver: Arc::new(EmptyResolver),
      usage_composer: Some(Arc::new(EmptyResolver)),
      rewrite_map: None,
      category_docs: None,
      disable_search: false,
      symbol_redirect_map: None,
      default_symbol_map: None,
      markdown_renderer: comrak::create_renderer(None, None, None),
      markdown_stripper: Arc::new(comrak::strip),
      head_inject: None,
      id_prefix: None,
      diff_only: false,
      symbol_listing_limit: None,
    },
    parse_source(source).await,
    None,
  )
  .unwrap();

  let files = generate(ctx).unwrap();
  let page = files
    .get("./~/A.html")
    .expect("a page for `A` should exist");

  // Nothing may break out of an attribute: every `"` from the source has to
  // have become an entity by the time it lands in an `id` or `href`.
  assert!(
    !page.contains(r##"href="#h"eading"##),
    "the table-of-contents href broke out of its attribute"
  );
  assert!(
    !page.contains(r#"id="h"eading"#),
    "the heading id broke out of its attribute"
  );

  // Escaped exactly once, so the fragment still resolves to the element.
  assert!(
    !page.contains("&amp;quot;"),
    "an anchor was escaped twice, leaving hrefs unable to match their ids"
  );

  // Each anchor that is linked to must exist as an id on the page, spelled
  // identically -- for a markdown heading and for a symbol name alike.
  for anchor in [
    "h&quot;eading",
    "property_&quot;&gt;&lt;img-src=x-onerror=alert(1)&gt;",
  ] {
    assert!(
      page.contains(&format!(r#"id="{anchor}""#)),
      "expected an element with id {anchor:?}"
    );
    assert!(
      page.contains(&format!(r##"href="#{anchor}""##)),
      "expected a link to #{anchor}"
    );
  }
}
