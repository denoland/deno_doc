// Copyright 2020-2022 the Deno authors. All rights reserved. MIT license.

use clap::App;
use clap::Arg;
use deno_doc::find_nodes_by_name_recursively;
use deno_doc::DocNodeKind;
use deno_doc::DocParser;
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
  let matches = App::new("ddoc")
    .arg(
      Arg::with_name("html")
        .long("html")
        .requires_all(&["name", "output"]),
    )
    .arg(Arg::with_name("name").long("name").takes_value(true))
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
    generate_docs_directory(name, output_dir, &doc_nodes_by_url)?;
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
  doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<deno_doc::DocNode>>,
) -> Result<(), anyhow::Error> {
  let cwd = std::env::current_dir().unwrap();
  let output_dir_resolved = cwd.join(output_dir);
  let output_dir_relative = output_dir_resolved.strip_prefix(&cwd).unwrap();
  let mut output_dir_relative_str =
    output_dir_relative.to_string_lossy().to_string();
  output_dir_relative_str = output_dir_relative_str
    .strip_prefix('/')
    .unwrap_or(&output_dir_relative_str)
    .to_string();
  output_dir_relative_str = output_dir_relative_str
    .strip_suffix('/')
    .unwrap_or(&output_dir_relative_str)
    .to_string();
  // TODO: make `base_url` configurable?
  let base_url = format!("/{}/", output_dir_relative_str);

  let options = deno_doc::html::GenerateOptions {
    package_name: name,
    base_url,
  };
  let html = deno_doc::html::generate(options.clone(), doc_nodes_by_url)?;

  let path = &output_dir_resolved;
  let _ = std::fs::remove_dir_all(path);
  std::fs::create_dir(path)?;

  std::fs::write(
    path.join(deno_doc::html::STYLESHEET_FILENAME),
    deno_doc::html::STYLESHEET,
  )
  .unwrap();
  std::fs::write(
    path.join(deno_doc::html::SEARCH_INDEX_FILENAME),
    deno_doc::html::generate_search_index(doc_nodes_by_url)?,
  )
  .unwrap();
  std::fs::write(
    path.join(deno_doc::html::SEARCH_FILENAME),
    deno_doc::html::SEARCH_JS,
  )
  .unwrap();
  for (name, content) in html {
    let this_path = path.join(name);
    let prefix = this_path.parent().unwrap();
    std::fs::create_dir_all(prefix).unwrap();
    std::fs::write(this_path, content).unwrap();
  }

  Ok(())
}
