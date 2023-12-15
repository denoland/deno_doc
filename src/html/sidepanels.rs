use super::DocNodeWithContext;
use super::GenerateCtx;
use super::UrlResolveKind;
use super::{short_path_to_name, DocNodeKindCtx};
use crate::DocNode;
use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use serde::Serialize;

#[derive(Debug, Serialize, Clone)]
struct SidepanelPartitionNodeCtx {
  kind: DocNodeKindCtx,
  name: String,
  href: String,
}

#[derive(Debug, Serialize, Clone)]
struct SidepanelPartitionCtx {
  name: String,
  symbols: Vec<SidepanelPartitionNodeCtx>,
}

#[derive(Debug, Serialize, Clone)]
pub struct SidepanelCtx {
  package_name: Option<String>,
  partitions: Vec<SidepanelPartitionCtx>,
}

impl SidepanelCtx {
  pub fn new(
    ctx: &GenerateCtx,
    partitions: &IndexMap<String, Vec<DocNodeWithContext>>,
    file: &str,
    symbol: &str,
  ) -> Self {
    let partitions = partitions
      .into_iter()
      .map(|(name, nodes)| {
        let symbols = nodes
          .iter()
          .map(|node| SidepanelPartitionNodeCtx {
            kind: node.doc_node.kind.into(),
            href: (ctx.url_resolver)(
              UrlResolveKind::Symbol { file, symbol },
              UrlResolveKind::Symbol {
                file: node.origin.as_deref().unwrap(),
                symbol: &node.doc_node.name,
              },
            ),
            name: node.doc_node.name.clone(),
          })
          .collect::<Vec<_>>();
        SidepanelPartitionCtx {
          name: name.clone(),
          symbols,
        }
      })
      .collect();

    Self {
      package_name: ctx.package_name.clone(),
      partitions,
    }
  }
}

#[derive(Debug, Serialize, Clone)]
struct IndexSidepanelFileCtx {
  name: String,
  href: String,
}

#[derive(Debug, Serialize, Clone)]
pub struct IndexSidepanelCtx {
  package_name: Option<String>,
  root_url: String,
  all_symbols_url: String,
  kind_partitions: Vec<SidepanelPartitionCtx>,
  files: Vec<IndexSidepanelFileCtx>,
  expand_files: Vec<IndexSidepanelFileCtx>,
}

impl IndexSidepanelCtx {
  pub fn new(
    ctx: &GenerateCtx,
    current_entrypoint: Option<&ModuleSpecifier>,
    doc_nodes_by_url: &IndexMap<ModuleSpecifier, Vec<DocNode>>,
    partitions: IndexMap<String, Vec<DocNodeWithContext>>,
    current_file: &Option<String>,
  ) -> Self {
    let files = doc_nodes_by_url
      .keys()
      .filter(|url| {
        current_entrypoint
          .map(|current_entrypoint| *url != current_entrypoint)
          .unwrap_or(true)
      })
      .map(|url| {
        let short_path = ctx.url_to_short_path(url);
        IndexSidepanelFileCtx {
          href: (ctx.url_resolver)(
            current_file
              .as_deref()
              .map_or(UrlResolveKind::Root, UrlResolveKind::File),
            if ctx.main_entrypoint.is_some()
              && ctx.main_entrypoint.as_ref() == Some(url)
            {
              UrlResolveKind::Root
            } else {
              UrlResolveKind::File(&short_path)
            },
          ),
          name: short_path_to_name(short_path),
        }
      })
      .collect::<Vec<_>>();

    let kind_partitions = partitions
      .into_iter()
      .map(|(name, nodes)| {
        let symbols = nodes
          .into_iter()
          .map(|node| SidepanelPartitionNodeCtx {
            kind: node.doc_node.kind.into(),
            href: (ctx.url_resolver)(
              current_file
                .as_deref()
                .map_or(UrlResolveKind::Root, UrlResolveKind::File),
              UrlResolveKind::Symbol {
                file: node.origin.as_deref().unwrap(),
                symbol: &node.doc_node.name,
              },
            ),
            name: node.doc_node.name,
          })
          .collect::<Vec<_>>();
        SidepanelPartitionCtx { name, symbols }
      })
      .collect::<Vec<_>>();

    let (files, expand_files) = if files.len() > 4 {
      let (files, expand_files) = files.split_at(3);
      (files.to_vec(), expand_files.to_vec())
    } else {
      (files, vec![])
    };

    Self {
      package_name: ctx.package_name.clone(),
      root_url: (ctx.url_resolver)(
        current_file
          .as_deref()
          .map_or(UrlResolveKind::Root, UrlResolveKind::File),
        UrlResolveKind::Root,
      ),
      all_symbols_url: (ctx.url_resolver)(
        current_file
          .as_deref()
          .map_or(UrlResolveKind::Root, UrlResolveKind::File),
        UrlResolveKind::AllSymbols,
      ),
      kind_partitions,
      files,
      expand_files,
    }
  }
}