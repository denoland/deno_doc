use super::short_path_to_name;
use super::DocNodeKindCtx;
use super::DocNodeWithContext;
use super::GenerateCtx;
use super::UrlResolveKind;
use crate::js_doc::JsDocTag;
use crate::DocNode;
use deno_ast::ModuleSpecifier;
use indexmap::IndexMap;
use serde::Serialize;

#[derive(Debug, Serialize, Clone)]
struct SidepanelPartitionSymbolCtx {
  kind: Vec<DocNodeKindCtx>,
  name: String,
  href: String,
  active: bool,
  deprecated: bool,
}

impl SidepanelPartitionSymbolCtx {
  fn new(
    nodes: &[&DocNodeWithContext],
    active: bool,
    href: String,
    name: String,
  ) -> Self {
    Self {
      kind: nodes.iter().map(|node| node.doc_node.kind.into()).collect(),
      name,
      href,
      active,
      deprecated: nodes.iter().all(|node| {
        node
          .doc_node
          .js_doc
          .tags
          .iter()
          .any(|tag| matches!(tag, JsDocTag::Deprecated { .. }))
      }),
    }
  }
}

#[derive(Debug, Serialize, Clone)]
struct SidepanelPartitionCtx {
  name: String,
  symbols: Vec<SidepanelPartitionSymbolCtx>,
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
        let mut grouped_nodes = IndexMap::new();

        for node in nodes {
          let entry = grouped_nodes
            .entry(node.doc_node.name.clone())
            .or_insert(vec![]);
          entry.push(node);
        }

        let symbols = grouped_nodes
          .into_iter()
          .map(|(node_name, nodes)| {
            SidepanelPartitionSymbolCtx::new(
              &nodes,
              symbol == node_name,
              (ctx.url_resolver)(
                UrlResolveKind::Symbol { file, symbol },
                UrlResolveKind::Symbol {
                  file: nodes[0].origin.as_deref().unwrap(),
                  symbol: &node_name,
                },
              ),
              node_name,
            )
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
          name: short_path_to_name(&short_path),
        }
      })
      .collect::<Vec<_>>();

    let kind_partitions = partitions
      .into_iter()
      .map(|(name, nodes)| {
        let mut grouped_nodes = IndexMap::new();

        for node in &nodes {
          let entry = grouped_nodes
            .entry(node.doc_node.name.clone())
            .or_insert(vec![]);
          entry.push(node);
        }

        let symbols = grouped_nodes
          .into_iter()
          .map(|(node_name, nodes)| {
            SidepanelPartitionSymbolCtx::new(
              &nodes,
              false,
              (ctx.url_resolver)(
                current_file
                  .as_deref()
                  .map_or(UrlResolveKind::Root, UrlResolveKind::File),
                UrlResolveKind::Symbol {
                  file: nodes[0].origin.as_deref().unwrap(),
                  symbol: &node_name,
                },
              ),
              node_name,
            )
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
