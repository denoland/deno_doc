// Copyright 2020-2022 the Deno authors. All rights reserved. MIT license.

use clap::App;
use clap::Arg;
use deno_doc::find_nodes_by_name_recursively;
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

async fn run() -> anyhow::Result<()> {
  env_logger::init();

  let matches = App::new("ddoc")
    .arg(
      Arg::with_name("html")
        .long("html")
        .requires_all(&["name", "output"]),
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
    matches.value_of("name").unwrap().to_string()
  } else {
    "".to_string()
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
      &doc_nodes_by_url,
    )?;
    return Ok(());
  }

  let mut doc_nodes = Vec::with_capacity(1024);
  for source_file in source_files {
    let nodes = parser.parse_with_reexports(&source_file)?;
    doc_nodes.extend_from_slice(&nodes);
  }

  doc_nodes.retain(|doc_node| doc_node.kind != DocNodeKind::Import);
  if let Some(filter) = maybe_filter {
    doc_nodes = find_nodes_by_name_recursively(doc_nodes, filter.to_string());
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

fn generate_docs_directory(
  name: String,
  output_dir: String,
  main_entrypoint: Option<ModuleSpecifier>,
  doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<deno_doc::DocNode>>,
) -> Result<(), anyhow::Error> {
  let cwd = current_dir().unwrap();
  let output_dir_resolved = cwd.join(output_dir);

  let mut index_map = IndexMap::new();
  if let Some(main_entrypoint) = main_entrypoint.as_ref() {
    index_map.insert(main_entrypoint.clone(), String::from("."));
  }

  let options = deno_doc::html::GenerateOptions {
    package_name: Some(name),
    main_entrypoint,
    global_symbols: Default::default(),
    global_symbol_href_resolver: Rc::new(|_, _| String::new()),
    import_href_resolver: Rc::new(|_, _| None),
    usage_resolver: Rc::new(|specifier, _file| specifier.to_string()),
    url_resolver: Rc::new(deno_doc::html::default_url_resolver),
    rewrite_map: Some(index_map),
    hide_module_doc_title: false,
  };
  let html = deno_doc::html::generate(options.clone(), doc_nodes_by_url)?;

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
