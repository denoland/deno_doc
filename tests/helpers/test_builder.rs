// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use deno_ast::ModuleSpecifier;
use deno_doc::DocDiagnostic;
use deno_doc::DocParser;
use deno_doc::DocPrinter;
use deno_graph::source::MemoryLoader;
use deno_graph::BuildOptions;
use deno_graph::CapturingModuleAnalyzer;
use deno_graph::DefaultModuleParser;
use deno_graph::GraphKind;

pub struct BuildResult {
  pub json_output: serde_json::Value,
  pub text_output: String,
  pub diagnostics: Vec<DocDiagnostic>,
}

pub struct TestBuilder {
  loader: MemoryLoader,
  private: bool,
  entry_point: String,
}

impl TestBuilder {
  pub fn new() -> Self {
    Self {
      private: false,
      loader: Default::default(),
      entry_point: "file:///mod.ts".to_string(),
    }
  }

  pub fn with_loader(
    &mut self,
    mut action: impl FnMut(&mut MemoryLoader),
  ) -> &mut Self {
    action(&mut self.loader);
    self
  }

  pub fn set_private(&mut self, value: bool) -> &mut Self {
    self.private = value;
    self
  }

  pub async fn build(&mut self) -> BuildResult {
    let analyzer = create_analyzer();
    let mut graph = deno_graph::ModuleGraph::new(GraphKind::TypesOnly);
    let entry_point_url = ModuleSpecifier::parse(&self.entry_point).unwrap();
    let roots = vec![entry_point_url.clone()];
    graph
      .build(
        roots.clone(),
        &mut self.loader,
        BuildOptions {
          module_analyzer: Some(&analyzer),
          ..Default::default()
        },
      )
      .await;
    let parser =
      DocParser::new(&graph, self.private, analyzer.as_capturing_parser())
        .unwrap();
    let entries = parser.parse(&entry_point_url).unwrap();

    let doc = DocPrinter::new(&entries, false, self.private).to_string();
    let diagnostics = parser.diagnostics();

    BuildResult {
      diagnostics,
      json_output: serde_json::to_value(entries).unwrap(),
      text_output: doc,
    }
  }
}

fn create_analyzer() -> CapturingModuleAnalyzer {
  let source_parser = DefaultModuleParser::new_for_analysis();
  CapturingModuleAnalyzer::new(Some(Box::new(source_parser)), None)
}
