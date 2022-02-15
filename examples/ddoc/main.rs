// Copyright 2020-2022 the Deno authors. All rights reserved. MIT license.

use clap::App;
use clap::Arg;
use deno_doc::find_nodes_by_name_recursively;
use deno_doc::DocNodeKind;
use deno_doc::DocParser;
use deno_doc::DocPrinter;
use deno_graph::create_type_graph;
use deno_graph::source::LoadFuture;
use deno_graph::source::LoadResponse;
use deno_graph::source::Loader;
use deno_graph::DefaultSourceParser;
use deno_graph::ModuleSpecifier;
use futures::executor::block_on;
use futures::future;
use std::env::current_dir;
use std::fs::read_to_string;
use std::sync::Arc;

struct SourceFileLoader {}

impl Loader for SourceFileLoader {
  fn load(
    &mut self,
    specifier: &ModuleSpecifier,
    _is_dynamic: bool,
  ) -> LoadFuture {
    let result = if specifier.scheme() == "file" {
      let path = specifier.to_file_path().unwrap();
      read_to_string(path)
        .map(|content| {
          Some(LoadResponse {
            specifier: specifier.clone(),
            maybe_headers: None,
            content: Arc::new(content),
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
    .arg(Arg::with_name("source_file").required(true))
    .arg(Arg::with_name("filter"))
    .get_matches();

  let source_file = matches.value_of("source_file").unwrap();
  let maybe_filter = matches.value_of("filter");
  let source_file =
    ModuleSpecifier::from_directory_path(current_dir().unwrap())
      .unwrap()
      .join(source_file)
      .unwrap();

  let mut loader = SourceFileLoader {};
  let future = async move {
    let source_parser = DefaultSourceParser::new();
    let graph = create_type_graph(
      vec![source_file.clone()],
      false,
      None,
      &mut loader,
      None,
      None,
      Some(&source_parser),
      None,
    )
    .await;
    let parser = DocParser::new(graph, false, &source_parser);
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
    let result = DocPrinter::new(&doc_nodes, true, false);
    println!("{}", result);
  };

  block_on(future);
}
