// Copyright 2020-2023 the Deno authors. All rights reserved. MIT license.

use deno_ast::ModuleSpecifier;
use deno_graph::ModuleGraph;

use crate::DocError;

/// Resolve a deno_graph module redirecting to the types dependency if available.
pub fn resolve_deno_graph_module<'a>(
  graph: &'a ModuleGraph,
  specifier: &ModuleSpecifier,
) -> Result<&'a deno_graph::Module, DocError> {
  graph
    .try_get_prefer_types(specifier)
    .map_err(|err| DocError::Resolve(err.to_string()))?
    .ok_or_else(|| {
      DocError::Resolve(format!("Unable to load specifier: \"{}\"", specifier))
    })
}
