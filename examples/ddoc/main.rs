// Copyright 2020-2022 the Deno authors. All rights reserved. MIT license.

use anyhow;
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

fn main() {
  let matches = App::new("ddoc")
    .arg(Arg::with_name("html").long("html").requires_all(&["name"]))
    .arg(Arg::with_name("name").long("name").takes_value(true))
    .arg(Arg::with_name("source_file").required(true))
    .arg(Arg::with_name("filter"))
    .get_matches();

  let source_file = matches.value_of("source_file").unwrap();
  let html = matches.is_present("html");
  let name = if html {
    matches.value_of("name").unwrap().to_string()
  } else {
    "".to_string()
  };
  let maybe_filter = matches.value_of("filter");
  let source_file =
    ModuleSpecifier::from_directory_path(current_dir().unwrap())
      .unwrap()
      .join(source_file)
      .unwrap();

  let mut loader = SourceFileLoader {};
  let future = async move {
    let analyzer = CapturingModuleAnalyzer::default();
    let mut graph = ModuleGraph::new(GraphKind::TypesOnly);
    graph
      .build(
        vec![source_file.clone()],
        &mut loader,
        BuildOptions {
          module_analyzer: Some(&analyzer),
          ..Default::default()
        },
      )
      .await;
    let parser = DocParser::new(graph, false, analyzer.as_capturing_parser());
    let parse_result = parser.parse_with_reexports(&source_file);

    let mut doc_nodes = match parse_result {
      Ok(nodes) => nodes,
      Err(e) => {
        eprintln!("{}", e);
        std::process::exit(1);
      }
    };

    doc_nodes.retain(|doc_node| doc_node.kind != DocNodeKind::Import);
    if let Some(filter) = maybe_filter {
      doc_nodes = find_nodes_by_name_recursively(doc_nodes, filter.to_string());
    }

    if !html {
      let result = DocPrinter::new(&doc_nodes, true, false);
      println!("{}", result);
      return;
    }

    generate_docs_directory(name, &doc_nodes).unwrap();
  };

  block_on(future);
}

fn generate_docs_directory(
  name: String,
  doc_nodes: &[deno_doc::DocNode],
) -> Result<(), anyhow::Error> {
  let ctx = deno_doc::html::GenerateCtx {
    package_name: name,
    // TODO: don't hardcode the path
    base_url: "/generated_docs/".to_string(),
  };
  let html = deno_doc::html::generate(ctx.clone(), doc_nodes);

  // TODO: don't hardcode the path
  let base_path = format!(
    "./{}",
    &ctx.base_url.strip_prefix("/").unwrap_or(&ctx.base_url)
  );
  let _ = std::fs::remove_dir_all(&base_path);
  std::fs::create_dir(&base_path)?;

  std::fs::write(
    format!("{base_path}/{}", deno_doc::html::STYLESHEET_FILENAME),
    deno_doc::html::STYLESHEET,
  )
  .unwrap();
  for (name, content) in html {
    std::fs::write(format!("{base_path}/{name}.html"), content).unwrap();
  }

  Ok(())
}
