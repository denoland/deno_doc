// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use clap::Parser;
use clap::Subcommand;
use deno_doc::DocParser;
use deno_doc::DocParserOptions;
use deno_doc::DocPrinter;
use deno_doc::ParseOutput;
use deno_doc::diff;
use deno_doc::find_nodes_by_name_recursively;
use deno_doc::html::GenerateCtx;
use deno_doc::html::HrefResolver;
use deno_doc::html::UrlResolveKind;
use deno_doc::html::UsageComposer;
use deno_doc::html::UsageComposerEntry;
use deno_doc::node::DocNodeDef;
use deno_graph::BuildOptions;
use deno_graph::GraphKind;
use deno_graph::ModuleGraph;
use deno_graph::ModuleSpecifier;
use deno_graph::ast::CapturingModuleAnalyzer;
use deno_graph::source::LoadFuture;
use deno_graph::source::LoadResponse;
use deno_graph::source::Loader;
use futures::executor::block_on;
use futures::future;
use indexmap::IndexMap;
use std::env::current_dir;
use std::path::PathBuf;
use std::rc::Rc;

#[derive(Parser)]
#[command(name = "ddoc")]
#[command(about = "Generate documentation for Deno/TypeScript modules")]
#[command(version)]
struct Cli {
  #[command(subcommand)]
  command: Commands,
}

#[derive(Subcommand)]
enum Commands {
  /// Generate documentation for source files
  Doc {
    /// Source files to document
    #[arg(required = true)]
    files: Vec<PathBuf>,

    /// Output format: json
    #[arg(long, conflicts_with = "html")]
    json: bool,

    /// Treat input files as JSON ParseOutput instead of source files
    #[arg(long)]
    json_input: bool,

    /// Generate HTML documentation
    #[arg(long, requires = "output")]
    html: bool,

    /// Output directory for HTML documentation
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Package name for HTML documentation
    #[arg(long)]
    name: Option<String>,

    /// Main entrypoint for HTML documentation
    #[arg(long)]
    main_entrypoint: Option<PathBuf>,

    /// Filter documentation by name
    #[arg(short, long, conflicts_with = "html")]
    filter: Option<String>,

    /// Include private items
    #[arg(long)]
    private: bool,
  },

  /// Compare documentation between two versions
  ///
  /// Usage: ddoc diff <OLD_FILES>... -- <NEW_FILES>...
  Diff {
    /// Old source files (before --)
    #[arg(required = true)]
    old_files: Vec<PathBuf>,

    /// New source files (after --)
    #[arg(last = true, required = true)]
    new_files: Vec<PathBuf>,

    /// Include private items
    #[arg(long)]
    private: bool,

    /// Treat input files as JSON ParseOutput instead of source files
    #[arg(long)]
    json_input: bool,
  },
}

struct SourceFileLoader;

impl Loader for SourceFileLoader {
  fn load(
    &self,
    specifier: &ModuleSpecifier,
    _options: deno_graph::source::LoadOptions,
  ) -> LoadFuture {
    let result = if specifier.scheme() == "file" {
      let path = specifier.to_file_path().unwrap();
      std::fs::read(path)
        .map(|content| {
          Some(LoadResponse::Module {
            specifier: specifier.clone(),
            mtime: None,
            maybe_headers: None,
            content: content.into(),
          })
        })
        .map_err(|err| {
          deno_graph::source::LoadError::Other(std::sync::Arc::new(err))
        })
    } else {
      Ok(None)
    };
    Box::pin(future::ready(result))
  }
}

fn path_to_specifier(path: &PathBuf) -> ModuleSpecifier {
  let cwd = current_dir().unwrap();
  let absolute = if path.is_absolute() {
    path.clone()
  } else {
    cwd.join(path)
  };
  ModuleSpecifier::from_file_path(absolute).unwrap()
}

async fn parse_sources(
  source_files: Vec<ModuleSpecifier>,
  private: bool,
) -> anyhow::Result<ParseOutput> {
  let loader = SourceFileLoader;
  let analyzer = CapturingModuleAnalyzer::default();
  let mut graph = ModuleGraph::new(GraphKind::TypesOnly);
  graph
    .build(
      source_files.clone(),
      Vec::new(),
      &loader,
      BuildOptions {
        module_analyzer: &analyzer,
        ..Default::default()
      },
    )
    .await;

  let mut source_files = source_files;
  source_files.sort();

  let parser = DocParser::new(
    &graph,
    &analyzer,
    &source_files,
    DocParserOptions {
      diagnostics: false,
      private,
    },
  )?;

  Ok(parser.parse()?)
}

async fn run() -> anyhow::Result<()> {
  let cli = Cli::parse();

  match cli.command {
    Commands::Doc {
      files,
      json,
      json_input,
      html,
      output,
      name,
      main_entrypoint,
      filter,
      private,
    } => {
      let doc_nodes_by_url = if json_input {
        assert_eq!(files.len(), 1);
        serde_json::from_reader(std::fs::File::open(&files[0])?)?
      } else {
        let source_files = files.iter().map(path_to_specifier).collect();
        parse_sources(source_files, private).await?
      };

      if html {
        let output_dir = output.unwrap();
        let main_entrypoint = main_entrypoint.map(|p| path_to_specifier(&p));
        generate_docs_directory(
          name,
          output_dir,
          main_entrypoint,
          doc_nodes_by_url,
        )?;
        return Ok(());
      }

      let mut doc_nodes =
        doc_nodes_by_url.into_values().flatten().collect::<Vec<_>>();

      doc_nodes
        .retain(|doc_node| !matches!(doc_node.def, DocNodeDef::Import { .. }));

      if let Some(filter) = filter {
        doc_nodes = find_nodes_by_name_recursively(doc_nodes, &filter);
      }

      if json {
        serde_json::to_writer_pretty(std::io::stdout(), &doc_nodes)?;
        println!();
      } else {
        let result = DocPrinter::new(&doc_nodes, true, false);
        println!("{result}");
      }
    }

    Commands::Diff {
      old_files,
      new_files,
      private,
      json_input,
    } => {
      let (old, new) = if json_input {
        assert_eq!(old_files.len(), 1);
        assert_eq!(new_files.len(), 1);

        let old_docs: ParseOutput =
          serde_json::from_reader(std::fs::File::open(&old_files[0])?)?;
        let new_docs: ParseOutput =
          serde_json::from_reader(std::fs::File::open(&new_files[0])?)?;

        (old_docs, new_docs)
      } else {
        let old_specifiers =
          old_files.iter().map(path_to_specifier).collect::<Vec<_>>();
        let new_specifiers =
          new_files.iter().map(path_to_specifier).collect::<Vec<_>>();

        let old_docs = parse_sources(old_specifiers.clone(), private).await?;
        let new_docs = parse_sources(new_specifiers.clone(), private).await?;

        // Match modules by position: old[0] -> new[0], old[1] -> new[1], etc.
        // This allows comparing renamed modules.
        let old_by_original = old_specifiers
          .iter()
          .zip(old_docs.into_iter())
          .map(|(orig, (_, nodes))| (orig.clone(), nodes))
          .collect::<IndexMap<_, _>>();

        let new_by_original = new_specifiers
          .iter()
          .zip(new_docs.into_iter())
          .map(|(orig, (_, nodes))| (orig.clone(), nodes))
          .collect::<IndexMap<_, _>>();

        // Create normalized maps using new specifiers as canonical keys
        let mut old_normalized = IndexMap::new();
        let mut new_normalized = IndexMap::new();

        for (i, new_spec) in new_specifiers.iter().enumerate() {
          if let Some(old_spec) = old_specifiers.get(i)
            && let Some(old_nodes) = old_by_original.get(old_spec)
          {
            old_normalized.insert(new_spec.clone(), old_nodes.clone());
          }
          if let Some(new_nodes) = new_by_original.get(new_spec) {
            new_normalized.insert(new_spec.clone(), new_nodes.clone());
          }
        }

        // Handle extra old modules (removed)
        for old_spec in old_specifiers.iter().skip(new_specifiers.len()) {
          if let Some(old_nodes) = old_by_original.get(old_spec) {
            old_normalized.insert(old_spec.clone(), old_nodes.clone());
          }
        }

        (old_normalized, new_normalized)
      };

      let doc_diff = diff::DocDiff::diff(&old, &new);
      serde_json::to_writer_pretty(std::io::stdout(), &doc_diff)?;
      println!();
    }
  }

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

struct EmptyResolver;

impl HrefResolver for EmptyResolver {
  fn resolve_path(
    &self,
    current: UrlResolveKind,
    target: UrlResolveKind,
  ) -> String {
    deno_doc::html::href_path_resolve(current, target)
  }

  fn resolve_global_symbol(&self, _symbol: &[String]) -> Option<String> {
    None
  }

  fn resolve_import_href(
    &self,
    _symbol: &[String],
    _src: &str,
  ) -> Option<String> {
    None
  }

  fn resolve_source(&self, location: &deno_doc::Location) -> Option<String> {
    Some(location.filename.to_string())
  }

  fn resolve_external_jsdoc_module(
    &self,
    _module: &str,
    _symbol: Option<&str>,
  ) -> Option<(String, String)> {
    None
  }
}

impl UsageComposer for EmptyResolver {
  fn is_single_mode(&self) -> bool {
    true
  }

  fn compose(
    &self,
    current_resolve: UrlResolveKind,
    usage_to_md: deno_doc::html::UsageToMd,
  ) -> IndexMap<UsageComposerEntry, String> {
    current_resolve
      .get_file()
      .map(|current_file| {
        IndexMap::from([(
          UsageComposerEntry {
            name: "".to_string(),
            icon: None,
          },
          usage_to_md(current_file.specifier.as_str(), None),
        )])
      })
      .unwrap_or_default()
  }
}

fn generate_docs_directory(
  package_name: Option<String>,
  output_dir: PathBuf,
  main_entrypoint: Option<ModuleSpecifier>,
  doc_nodes_by_url: IndexMap<ModuleSpecifier, Vec<deno_doc::DocNode>>,
) -> Result<(), anyhow::Error> {
  let cwd = current_dir().unwrap();
  let output_dir_resolved = cwd.join(output_dir);

  let mut index_map = IndexMap::new();
  if let Some(main_entrypoint) = main_entrypoint.as_ref() {
    index_map.insert(main_entrypoint.clone(), String::from("."));
  }

  let options = deno_doc::html::GenerateOptions {
    package_name,
    main_entrypoint,
    href_resolver: Rc::new(EmptyResolver),
    usage_composer: Some(Rc::new(EmptyResolver)),
    rewrite_map: Some(index_map),
    category_docs: None,
    disable_search: false,
    symbol_redirect_map: None,
    default_symbol_map: None,
    markdown_renderer: deno_doc::html::comrak::create_renderer(
      None, None, None,
    ),
    markdown_stripper: Rc::new(deno_doc::html::comrak::strip),
    head_inject: Some(Rc::new(|root| {
      format!(
        r#"<link rel="stylesheet" href="{root}{}">"#,
        deno_doc::html::comrak::COMRAK_STYLESHEET_FILENAME
      )
    })),
    id_prefix: None,
    diff_only: false,
  };
  let ctx = GenerateCtx::create_basic(options, doc_nodes_by_url)?;
  let html = deno_doc::html::generate(ctx)?;

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
