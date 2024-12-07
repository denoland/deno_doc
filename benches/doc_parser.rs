// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use criterion::async_executor::FuturesExecutor;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;

use deno_doc::DocNode;
use deno_doc::DocParser;
use deno_doc::DocParserOptions;
use deno_graph::source::MemoryLoader;
use deno_graph::source::Source;
use deno_graph::BuildOptions;
use deno_graph::CapturingModuleAnalyzer;
use deno_graph::GraphKind;
use deno_graph::ModuleGraph;
use deno_graph::ModuleSpecifier;
use indexmap::IndexMap;

async fn parse() -> IndexMap<ModuleSpecifier, Vec<DocNode>> {
  let source = std::fs::read_to_string("./benches/fixtures/deno.d.ts").unwrap();
  let sources = vec![(
    "file:///test/fixtures/deno.d.ts",
    Source::Module {
      specifier: "file:///test/fixtures/deno.d.ts",
      maybe_headers: None,
      content: source.as_str(),
    },
  )];
  let memory_loader = MemoryLoader::new(sources, vec![]);
  let root = ModuleSpecifier::parse("file:///test/fixtures/deno.d.ts").unwrap();

  let analyzer = CapturingModuleAnalyzer::default();
  let mut graph = ModuleGraph::new(GraphKind::TypesOnly);
  graph
    .build(
      vec![root.clone()],
      &memory_loader,
      BuildOptions {
        module_analyzer: &analyzer,
        ..Default::default()
      },
    )
    .await;
  DocParser::new(&graph, &analyzer, &[root], DocParserOptions::default())
    .unwrap()
    .parse()
    .unwrap()
}

fn doc_parser(c: &mut Criterion) {
  c.bench_function("parse large", |b| {
    b.to_async(FuturesExecutor).iter_with_large_drop(parse)
  });
}

criterion_group!(benches, doc_parser);
criterion_main!(benches);
