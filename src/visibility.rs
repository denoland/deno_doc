// Copyright 2020-2023 the Deno authors. All rights reserved. MIT license.

use crate::parser::DocError;
use crate::util::graph::resolve_deno_graph_module;
use crate::util::symbol::get_module_info;
use crate::util::symbol::symbol_has_ignorable_js_doc_tag;

use deno_ast::SourceRange;
use deno_graph::ModuleGraph;
use deno_graph::symbols::DefinitionPathNode;
use deno_graph::symbols::DefinitionPathNodeResolved;
use deno_graph::symbols::ResolveDepsMode;
use deno_graph::symbols::ResolvedSymbolDepEntry;
use deno_graph::symbols::SymbolDecl;
use deno_graph::symbols::UniqueSymbolId;
use indexmap::IndexMap;
use indexmap::IndexSet;

use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Debug)]
pub struct SymbolDeclDeps {
  pub symbol_id: UniqueSymbolId,
  pub decl_range: SourceRange,
  pub deps: IndexSet<UniqueSymbolId>,
  /// If the path to this declaration had an ignorable js doc tag.
  pub had_ignorable_tag: bool,
}

#[derive(Default, Debug)]
pub struct SymbolDeps(IndexMap<SourceRange, Vec<SymbolDeclDeps>>);

impl SymbolDeps {
  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  pub fn iter(&self) -> impl Iterator<Item = &SymbolDeclDeps> {
    self.0.values().flatten()
  }

  fn add(
    &mut self,
    symbol_id: UniqueSymbolId,
    symbol_decl: &SymbolDecl,
    dep_id: UniqueSymbolId,
    had_ignorable_tag: bool,
  ) {
    self
      .0
      .entry(symbol_decl.range)
      .or_default()
      .push(SymbolDeclDeps {
        symbol_id,
        decl_range: symbol_decl.range,
        deps: IndexSet::from([dep_id]),
        had_ignorable_tag,
      });
  }
}

pub struct SymbolVisibility {
  root_exported_ids: HashMap<UniqueSymbolId, SymbolDeps>,
  /// Symbol identifiers that are not exported, but are referenced
  /// by exported symbols.
  non_exported_public_ids: HashSet<UniqueSymbolId>,
}

impl SymbolVisibility {
  pub fn build(
    graph: &ModuleGraph,
    root_symbol: &deno_graph::symbols::RootSymbol,
  ) -> Result<Self, DocError> {
    // get all the symbols that are exported from the roots of the graph
    let mut root_exported_ids = analyze_root_exported_ids(graph, root_symbol)?;

    // now analyze for all the non-exported types that are referenced by exported types
    // along with filling in any non-exported dependencies of root exported types
    let mut non_exported_public_ids = HashSet::new();
    let mut pending_symbol_ids =
      root_exported_ids.keys().copied().collect::<Vec<_>>();
    while let Some(original_id) = pending_symbol_ids.pop() {
      let module_info =
        root_symbol.module_from_id(original_id.module_id).unwrap();
      let symbol = module_info.symbol(original_id.symbol_id).unwrap();
      let decl_deps = symbol
        .decls()
        .iter()
        .filter(|d| !d.has_overloads())
        .flat_map(|decl| {
          decl
            .deps(ResolveDepsMode::TypesOnly)
            .into_iter()
            .map(move |dep| (decl, dep))
        })
        .map(|(decl, dep)| (symbol, decl, dep))
        .chain(
          symbol
            .members()
            .iter()
            .map(|symbol_id| module_info.symbol(*symbol_id).unwrap())
            .filter(|symbol| !symbol.is_private_member())
            .flat_map(|symbol| {
              symbol
                .decls()
                .iter()
                .filter(|m| !m.has_overloads())
                .flat_map(move |decl| {
                  decl
                    .deps(ResolveDepsMode::TypesOnly)
                    .into_iter()
                    .map(move |dep| (symbol, decl, dep))
                })
            }),
        );
      for (decl_symbol, decl, dep) in decl_deps {
        let mut path_has_ignorable_tag =
          symbol_has_ignorable_js_doc_tag(module_info, decl_symbol);
        let mut dep_symbol_ids = Vec::new();
        let mut pending_entries =
          root_symbol.resolve_symbol_dep(module_info, &dep);
        while let Some(entry) = pending_entries.pop() {
          match entry {
            ResolvedSymbolDepEntry::Path(path_node) => {
              let path = match path_node {
                DefinitionPathNode::Resolved(path) => path,
                DefinitionPathNode::Unresolved(_) => continue,
              };
              let Some(path_symbol) = path.symbol() else {
                continue;
              };
              if !path_has_ignorable_tag
                && symbol_has_ignorable_js_doc_tag(path.module(), path_symbol)
              {
                path_has_ignorable_tag = true;
              }

              // only analyze declarations
              if path_symbol.is_decl() {
                dep_symbol_ids.push(path_symbol.unique_id());
              }

              // queue the next parts
              match path {
                DefinitionPathNodeResolved::Link(link) => {
                  pending_entries.extend(
                    link.next.into_iter().map(ResolvedSymbolDepEntry::Path),
                  );
                }
                DefinitionPathNodeResolved::Definition(_) => {}
              }
            }
            ResolvedSymbolDepEntry::ImportType(_) => {
              // this is an import type with no property access, ignore it for now
            }
          }
        }

        for dep_symbol_id in dep_symbol_ids {
          if !root_exported_ids.contains_key(&dep_symbol_id)
            && non_exported_public_ids.insert(dep_symbol_id)
          {
            if let Some(dep_ids) = root_exported_ids.get_mut(&original_id) {
              dep_ids.add(
                decl_symbol.unique_id(),
                decl,
                dep_symbol_id,
                path_has_ignorable_tag,
              );
            }

            // examine the private types of this private type
            pending_symbol_ids.push(dep_symbol_id);
          }
        }
      }
    }

    Ok(SymbolVisibility {
      root_exported_ids,
      non_exported_public_ids,
    })
  }

  pub fn get_root_exported_deps(
    &self,
    id: &UniqueSymbolId,
  ) -> Option<&SymbolDeps> {
    self.root_exported_ids.get(id)
  }

  pub fn has_non_exported_public(&self, id: &UniqueSymbolId) -> bool {
    self.non_exported_public_ids.contains(id)
  }
}

fn analyze_root_exported_ids(
  graph: &ModuleGraph,
  root_symbol: &deno_graph::symbols::RootSymbol<'_>,
) -> Result<HashMap<UniqueSymbolId, SymbolDeps>, DocError> {
  // this returns a HashMap instead of a HashSet because we'll fill the values
  // above, so this avoids re-allocating the HashSet into a HashMap
  let mut root_exported_ids: HashMap<UniqueSymbolId, SymbolDeps> =
    Default::default();
  {
    let mut pending_symbols = Vec::new();
    for root in &graph.roots {
      let module = resolve_deno_graph_module(graph, root)?;
      let module_info = get_module_info(root_symbol, module.specifier())?;
      let exports = module_info.exports(root_symbol);
      for (_name, export) in &exports.resolved {
        let export = export.as_resolved_export();
        let symbol = export.module.symbol(export.symbol_id).unwrap();
        let definitions = root_symbol.go_to_definitions(export.module, symbol);
        for definition in definitions {
          if root_exported_ids
            .insert(definition.symbol.unique_id(), Default::default())
            .is_none()
          {
            pending_symbols.push(definition);
          }
        }
      }
    }
    // analyze the pending symbols
    while let Some(definition) = pending_symbols.pop() {
      for (_name, export_id) in definition.symbol.exports() {
        let export_symbol = definition.module.symbol(*export_id).unwrap();
        let definitions =
          root_symbol.go_to_definitions(definition.module, export_symbol);
        for definition in definitions {
          if root_exported_ids
            .insert(definition.symbol.unique_id(), Default::default())
            .is_none()
          {
            pending_symbols.push(definition);
          }
        }
      }
    }
  }
  Ok(root_exported_ids)
}
