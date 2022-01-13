// Copyright 2020-2021 the Deno authors. All rights reserved. MIT license.

use criterion::async_executor::FuturesExecutor;
use criterion::criterion_group;
use criterion::criterion_main;
use criterion::Criterion;

use deno_doc::DocNode;
use deno_doc::DocParser;
use deno_graph::create_graph;
use deno_graph::source::MemoryLoader;
use deno_graph::ModuleSpecifier;

async fn parse_with_reexports() -> Vec<DocNode> {
  let source = std::fs::read_to_string("./benches/fixtures/deno.d.ts").unwrap();
  let sources = vec![(
    "file:///test/fixtures/deno.d.ts",
    Ok(("file:///test/fixtures/deno.d.ts", None, source.as_str())),
  )];
  let mut memory_loader = MemoryLoader::new(sources, vec![]);
  let root = ModuleSpecifier::parse("file:///test/fixtures/deno.d.ts").unwrap();
  let graph = create_graph(
    vec![root.clone()],
    false,
    None,
    &mut memory_loader,
    None,
    None,
    None,
    None,
  )
  .await;
  let source_parser = deno_graph::DefaultSourceParser::new();
  DocParser::new(graph, false, &source_parser)
    .parse_with_reexports(&root)
    .unwrap()
}

fn doc_parser(c: &mut Criterion) {
  c.bench_function("parse_with_rexports large", |b| {
    b.to_async(FuturesExecutor)
      .iter_with_large_drop(parse_with_reexports)
  });
}

criterion_group!(benches, doc_parser);
criterion_main!(benches);
