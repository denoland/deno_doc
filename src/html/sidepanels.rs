use super::partition::Partition;
use super::DocNodeKindCtx;
use super::DocNodeWithContext;
use super::GenerateCtx;
use super::ShortPath;
use super::UrlResolveKind;
use indexmap::IndexMap;
use serde::Serialize;
use std::rc::Rc;

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
      kind: nodes
        .iter()
        .map(|node| node.kind_with_drilldown.into())
        .collect(),
      name,
      href,
      active,
      deprecated: super::util::all_deprecated(nodes),
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
  partitions: Vec<SidepanelPartitionCtx>,
}

impl SidepanelCtx {
  pub const TEMPLATE: &'static str = "sidepanel";

  pub fn new(
    ctx: &GenerateCtx,
    partitions: &Partition,
    file: &ShortPath,
    symbol: &str,
  ) -> Self {
    let partitions = partitions
      .into_iter()
      .map(|(name, nodes)| {
        let mut grouped_nodes = IndexMap::new();

        for node in nodes {
          let name = if !node.ns_qualifiers.is_empty() {
            format!("{}.{}", node.ns_qualifiers.join("."), node.get_name())
          } else {
            node.get_name().to_string()
          };

          let entry = grouped_nodes.entry(name).or_insert(vec![]);
          entry.push(node);
        }

        let symbols = grouped_nodes
          .into_iter()
          .map(|(node_name, nodes)| {
            SidepanelPartitionSymbolCtx::new(
              &nodes,
              symbol == node_name,
              ctx.href_resolver.resolve_path(
                UrlResolveKind::Symbol { file, symbol },
                UrlResolveKind::Symbol {
                  file: &nodes[0].origin,
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

    Self { partitions }
  }
}

#[derive(Debug, Serialize, Clone)]
struct IndexSidepanelFileCtx {
  name: String,
  href: String,
  active: bool,
}

#[derive(Debug, Serialize, Clone)]
pub struct IndexSidepanelCtx {
  root_url: String,
  all_symbols_url: Option<String>,
  kind_partitions: Vec<SidepanelPartitionCtx>,
  files: Vec<IndexSidepanelFileCtx>,
}

impl IndexSidepanelCtx {
  pub const TEMPLATE: &'static str = "index_sidepanel";

  pub fn new(
    ctx: &GenerateCtx,
    current_file: Option<Rc<ShortPath>>,
    doc_nodes_by_url: &super::ContextDocNodesByShortPath,
    partitions: Partition,
  ) -> Self {
    let current_resolve_kind = current_file
      .as_deref()
      .map_or(UrlResolveKind::Root, ShortPath::as_resolve_kind);

    let main_short_path = doc_nodes_by_url
      .keys()
      .find(|short_path| short_path.is_main);

    let files = doc_nodes_by_url
      .keys()
      .filter(|short_path| {
        main_short_path
          .map(|main_short_path| short_path != &main_short_path)
          .unwrap_or(true)
      })
      .map(|short_path| IndexSidepanelFileCtx {
        href: ctx
          .href_resolver
          .resolve_path(current_resolve_kind, short_path.as_resolve_kind()),
        name: short_path.display_name(),
        active: current_file
          .as_ref()
          .is_some_and(|current_file| current_file == short_path),
      })
      .collect::<Vec<_>>();

    let kind_partitions = partitions
      .into_iter()
      .map(|(name, nodes)| {
        let mut grouped_nodes = IndexMap::new();

        for node in &nodes {
          let name = if !node.ns_qualifiers.is_empty() {
            format!("{}.{}", node.ns_qualifiers.join("."), node.get_name())
          } else {
            node.get_name().to_string()
          };

          let entry = grouped_nodes.entry(name).or_insert(vec![]);
          entry.push(node);
        }

        let symbols = grouped_nodes
          .into_iter()
          .map(|(node_name, nodes)| {
            SidepanelPartitionSymbolCtx::new(
              &nodes,
              false,
              ctx.href_resolver.resolve_path(
                current_resolve_kind,
                UrlResolveKind::Symbol {
                  file: &nodes[0].origin,
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

    Self {
      root_url: ctx
        .href_resolver
        .resolve_path(current_resolve_kind, UrlResolveKind::Root),
      all_symbols_url: (!ctx.sidebar_hide_all_symbols).then(|| {
        ctx
          .href_resolver
          .resolve_path(current_resolve_kind, UrlResolveKind::AllSymbols)
      }),
      kind_partitions,
      files,
    }
  }
}
