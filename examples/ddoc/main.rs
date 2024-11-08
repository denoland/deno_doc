// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use clap::App;
use clap::Arg;
use deno_doc::find_nodes_by_name_recursively;
use deno_doc::html::UrlResolveKind;
use deno_doc::html::{
  DocNodeWithContext, HrefResolver, UsageComposer, UsageComposerEntry,
};
use deno_doc::DocNodeKind;
use deno_doc::DocParser;
use deno_doc::DocParserOptions;
use deno_doc::DocPrinter;
use deno_graph::source::LoadFuture;
use deno_graph::source::LoadResponse;
use deno_graph::source::Loader;
use deno_graph::BuildOptions;
use deno_graph::CapturingModuleAnalyzer;
use deno_graph::GraphKind;
use deno_graph::ModuleGraph;
use deno_graph::ModuleSpecifier;
use futures::executor::block_on;
use futures::future;
use indexmap::IndexMap;
use std::env::current_dir;
use std::rc::Rc;

struct SourceFileLoader {}

impl Loader for SourceFileLoader {
  fn load(
    &self,
    specifier: &ModuleSpecifier,
    _options: deno_graph::source::LoadOptions,
  ) -> LoadFuture {
    let result = if specifier.scheme() == "file" {
      let path = specifier.to_file_path().unwrap();
      std::fs::read(path)
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

async fn run() -> anyhow::Result<()> {
  let matches = App::new("ddoc")
    .arg(
      Arg::with_name("html")
        .long("html")
        .requires_all(&["output"]),
    )
    .arg(Arg::with_name("name").long("name").takes_value(true))
    .arg(
      Arg::with_name("main_entrypoint")
        .long("main_entrypoint")
        .takes_value(true),
    )
    .arg(Arg::with_name("output").long("output").takes_value(true))
    .arg(Arg::with_name("source_files").required(true).multiple(true))
    .arg(
      Arg::with_name("filter")
        .long("filter")
        .conflicts_with("html"),
    )
    .arg(Arg::with_name("private").long("private"))
    .get_matches();
  let source_files = matches.values_of("source_files").unwrap();
  let html = matches.is_present("html");
  let name = if html {
    matches.value_of("name").map(|name| name.to_string())
  } else {
    None
  };
  let main_entrypoint = if html {
    matches.value_of("main_entrypoint").map(|main_entrypoint| {
      ModuleSpecifier::from_directory_path(current_dir().unwrap())
        .unwrap()
        .join(main_entrypoint)
        .unwrap()
    })
  } else {
    None
  };
  let output_dir = if html {
    matches.value_of("output").unwrap().to_string()
  } else {
    "".to_string()
  };
  let maybe_filter = matches.value_of("filter");
  let private = matches.is_present("private");
  let source_files: Vec<ModuleSpecifier> = source_files
    .into_iter()
    .map(|source_file| {
      ModuleSpecifier::from_directory_path(current_dir().unwrap())
        .unwrap()
        .join(source_file)
        .unwrap()
    })
    .collect();
  let loader = SourceFileLoader {};
  let analyzer = CapturingModuleAnalyzer::default();
  let mut graph = ModuleGraph::new(GraphKind::TypesOnly);
  graph
    .build(
      source_files.clone(),
      &loader,
      BuildOptions {
        module_analyzer: &analyzer,
        ..Default::default()
      },
    )
    .await;

  let parser = DocParser::new(
    &graph,
    &analyzer,
    DocParserOptions {
      diagnostics: false,
      private,
    },
  )?;

  if html {
    let mut source_files = source_files.clone();
    source_files.sort();
    let mut doc_nodes_by_url = IndexMap::with_capacity(source_files.len());
    for source_file in source_files {
      let nodes = parser.parse_with_reexports(&source_file)?;
      doc_nodes_by_url.insert(source_file, nodes);
    }
    generate_docs_directory(
      name,
      output_dir,
      main_entrypoint,
      doc_nodes_by_url,
    )?;
    return Ok(());
  }

  let mut doc_nodes = Vec::with_capacity(1024);
  for source_file in source_files {
    let nodes = parser.parse_with_reexports(&source_file)?;
    doc_nodes.extend(nodes);
  }

  doc_nodes.retain(|doc_node| doc_node.kind() != DocNodeKind::Import);
  if let Some(filter) = maybe_filter {
    doc_nodes = find_nodes_by_name_recursively(doc_nodes, filter);
  }

  let result = DocPrinter::new(&doc_nodes, true, false);
  println!("{}", result);
  Ok(())
}

fn main() {
  let future = async move {
    if let Err(err) = run().await {
      eprintln!("{}", err);
      std::process::exit(1);
    }
  };

  block_on(future);
}

struct EmptyResolver;

impl HrefResolver for EmptyResolver {
  fn resolve_path(
    &self,
    current: UrlResolveKind,
    target: UrlResolveKind,
  ) -> String {
    deno_doc::html::href_path_resolve(current, target)
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

  fn resolve_source(&self, location: &deno_doc::Location) -> Option<String> {
    Some(location.filename.to_string())
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
    nodes: &[DocNodeWithContext],
    current_resolve: UrlResolveKind,
    usage_to_md: deno_doc::html::UsageToMd,
  ) -> IndexMap<UsageComposerEntry, String> {
    current_resolve
      .get_file()
      .map(|current_file| {
        IndexMap::from([(
          UsageComposerEntry {
            name: "".to_string(),
            icon: None,
          },
          usage_to_md(nodes, current_file.specifier.as_str(), None),
        )])
      })
      .unwrap_or_default()
  }
}

fn generate_docs_directory(
  package_name: Option<String>,
  output_dir: String,
  main_entrypoint: Option<ModuleSpecifier>,
  doc_nodes_by_url: IndexMap<ModuleSpecifier, Vec<deno_doc::DocNode>>,
) -> Result<(), anyhow::Error> {
  let cwd = current_dir().unwrap();
  let output_dir_resolved = cwd.join(output_dir);

  let mut index_map = IndexMap::new();
  if let Some(main_entrypoint) = main_entrypoint.as_ref() {
    index_map.insert(main_entrypoint.clone(), String::from("."));
  }

  let options = deno_doc::html::GenerateOptions {
    package_name,
    main_entrypoint,
    href_resolver: Rc::new(EmptyResolver),
    usage_composer: Rc::new(EmptyResolver),
    rewrite_map: Some(index_map),
    category_docs: None,
    disable_search: false,
    symbol_redirect_map: None,
    default_symbol_map: None,
    markdown_renderer: deno_doc::html::comrak::create_renderer(
      None, None, None,
    ),
    markdown_stripper: Rc::new(deno_doc::html::comrak::strip),
    head_inject: Some(Rc::new(|root| {
      format!(
        r#"<link rel="stylesheet" href="{root}{}">"#,
        deno_doc::html::comrak::COMRAK_STYLESHEET_FILENAME
      )
    })),
  };
  let html = deno_doc::html::generate(options, doc_nodes_by_url)?;

  let path = &output_dir_resolved;
  let _ = std::fs::remove_dir_all(path);
  std::fs::create_dir(path)?;

  for (name, content) in html {
    let this_path = path.join(name);
    let prefix = this_path.parent().unwrap();
    std::fs::create_dir_all(prefix).unwrap();
    std::fs::write(this_path, content).unwrap();
  }

  Ok(())
}
