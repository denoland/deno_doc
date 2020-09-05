use deno_doc::parser::DocFileLoader;
use deno_doc::{DocError, DocNodeKind, DocParser, DocPrinter};
use futures::executor::block_on;
use futures::FutureExt;
use std::env::{args, current_dir};
use std::fs::read_to_string;
use swc_ecmascript::parser::{Syntax, TsConfig};
use tokio::macros::support::{Future, Pin};
use url::Url;

#[derive(Clone)]
struct SourceFileFetcher {}

impl SourceFileFetcher {
  pub fn new() -> Box<Self> {
    Box::new(Self {})
  }

  fn fetch_local_file(&self, module_url: &Url) -> String {
    let path = module_url.to_file_path().unwrap();
    read_to_string(path).unwrap()
  }

  fn fetch_remote_source(&self, _module_url: &Url) -> String {
    unimplemented!();
  }
}

impl DocFileLoader for SourceFileFetcher {
  fn resolve(
    &self,
    specifier: &str,
    referrer: &str,
  ) -> Result<String, DocError> {
    Ok(
      Url::parse(referrer)
        .unwrap()
        .join(specifier)
        .unwrap()
        .to_string(),
    )
  }

  fn load_source_code(
    &self,
    specifier: &str,
  ) -> Pin<Box<dyn Future<Output = Result<String, DocError>>>> {
    let fetcher = self.clone();
    let module_url = Url::parse(specifier).unwrap();
    async move {
      let url_scheme = module_url.scheme();
      let is_local_file = url_scheme == "file";

      if is_local_file {
        Ok(fetcher.fetch_local_file(&module_url))
      } else {
        Ok(fetcher.fetch_remote_source(&module_url))
      }
    }
    .boxed_local()
  }
}

fn main() {
  let args: Vec<String> = args().collect();
  let target = args.get(1).unwrap();
  let target = Url::from_directory_path(current_dir().unwrap())
    .unwrap()
    .join(target)
    .unwrap();

  let loader = SourceFileFetcher::new();
  let parser = DocParser::new(loader, false);
  let future = async move {
    let mut doc_nodes = parser
      .parse_with_reexports(
        target.as_str(),
        Syntax::Typescript(TsConfig::default()),
      )
      .await
      .unwrap();

    doc_nodes.retain(|doc_node| doc_node.kind != DocNodeKind::Import);
    let result = DocPrinter::new(&doc_nodes, true, false);
    println!("{}", result);
  };

  block_on(future);
}
