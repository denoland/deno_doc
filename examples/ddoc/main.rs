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
    .arg(Arg::with_name("html").long("html"))
    .arg(Arg::with_name("source_file").required(true))
    .arg(Arg::with_name("filter"))
    .get_matches();

  let source_file = matches.value_of("source_file").unwrap();
  let html = matches.is_present("html");
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

    generate_docs_directory(&doc_nodes).unwrap();
  };

  block_on(future);
}

fn generate_docs_directory(
  doc_nodes: &[deno_doc::DocNode],
) -> Result<(), anyhow::Error> {
  let html = generate_html(doc_nodes)?;

  // TODO: don't hardcode the path
  std::fs::create_dir("generated_docs/")?;
  // TODO: don't hardcode the path
  std::fs::write("generated_docs/foo.html", html)?;
  Ok(())
}

const HTML_HEAD: &str = r#"
<html>
<head></head>
<body>
"#;
const HTML_TAIL: &str = r#"
</body>
<script>
</script>
</html>"#;

fn generate_html(
  doc_nodes: &[deno_doc::DocNode],
) -> Result<String, anyhow::Error> {
  let mut parts = vec![HTML_HEAD.to_string()];

  parts.push("<ul>".to_string());

  for doc_node in doc_nodes {
    parts.push(render_doc_node(doc_node)?);
  }

  parts.push("</ul>".to_string());
  parts.push(HTML_TAIL.to_string());
  Ok(parts.join(""))
}

fn render_doc_node(
  doc_node: &deno_doc::DocNode,
) -> Result<String, anyhow::Error> {
  let tpl = format!("<li>{} ({:?})</li>", doc_node.name, doc_node.kind);

  match doc_node.kind {
    DocNodeKind::ModuleDoc => {}
    DocNodeKind::Function => {}
    DocNodeKind::Variable => {}
    DocNodeKind::Class => {}
    DocNodeKind::Enum => {}
    DocNodeKind::Interface => {}
    DocNodeKind::TypeAlias => {}
    DocNodeKind::Namespace => {}
    DocNodeKind::Import => {}
  }

  Ok(tpl)
}
