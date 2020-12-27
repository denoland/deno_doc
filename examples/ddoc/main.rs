use clap::{App, Arg};
use deno_doc::parser::DocFileLoader;
use deno_doc::{DocError, DocNodeKind, DocParser, DocPrinter, find_nodes_by_name_recursively};
use futures::executor::block_on;
use futures::FutureExt;
use std::env::current_dir;
use std::fs::read_to_string;
use swc_ecmascript::parser::{Syntax, TsConfig};
use tokio::macros::support::{Future, Pin};
use url::Url;

struct SourceFileFetcher {}

impl DocFileLoader for SourceFileFetcher {
  fn resolve(
    &self,
    specifier: &str,
    referrer: &str,
  ) -> Result<String, DocError> {
    let base =
      Url::parse(referrer).map_err(|e| DocError::Resolve(e.to_string()))?;
    let resolved = base
      .join(specifier)
      .map_err(|e| DocError::Resolve(e.to_string()))?;
    Ok(resolved.to_string())
  }

  fn load_source_code(
    &self,
    specifier: &str,
  ) -> Pin<Box<dyn Future<Output = Result<String, DocError>>>> {
    let module_url = Url::parse(specifier).unwrap();
    async move {
      let url_scheme = module_url.scheme();
      let is_local_file = url_scheme == "file";

      if is_local_file {
        let path = module_url.to_file_path().unwrap();
        Ok(read_to_string(path).unwrap())
      } else {
        Err(DocError::Resolve(
          "Fetching remote modules is not supported.".to_string(),
        ))
      }
    }
    .boxed_local()
  }
}

fn main() {
  let matches = App::new("ddoc")
    .arg(Arg::with_name("source_file").required(true))
    .arg(Arg::with_name("filter"))
    .get_matches();

  let source_file = matches.value_of("source_file").unwrap();
  let maybe_filter = matches.value_of("filter");
  let source_file = Url::from_directory_path(current_dir().unwrap())
    .unwrap()
    .join(source_file)
    .unwrap();

  let loader = Box::new(SourceFileFetcher {});
  let parser = DocParser::new(loader, false);
  let future = async move {
    let parse_result = parser
      .parse_with_reexports(
        source_file.as_str(),
        Syntax::Typescript(TsConfig::default()),
      )
      .await;

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
