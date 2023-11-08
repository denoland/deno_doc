// Copyright 2020-2023 the Deno authors. All rights reserved. MIT license.

use deno_graph::symbols::ModuleInfoRef;
use deno_graph::symbols::Symbol;
use deno_graph::symbols::SymbolDecl;

use crate::swc_util::has_ignorable_js_doc_tag;
use crate::swc_util::js_doc_for_range_include_ignore;

pub fn fully_qualified_symbol_name(
  module: ModuleInfoRef,
  symbol: &Symbol,
) -> Option<String> {
  let mut text = String::new();
  let mut next = Some(symbol);
  while let Some(symbol) = next {
    if symbol.parent_id().is_none() {
      break; // ignore the source file
    }
    if !text.is_empty() {
      text = format!("{}.{}", symbol.maybe_name()?, text);
    } else {
      text = symbol.maybe_name()?.to_string();
    }
    next = symbol.parent_id().and_then(|id| module.symbol(id));
  }
  if text.is_empty() {
    None
  } else {
    Some(text)
  }
}

pub fn symbol_has_ignorable_js_doc_tag(
  module: ModuleInfoRef,
  symbol: &Symbol,
) -> bool {
  if symbol
    .decls()
    .iter()
    .any(|decl| decl_has_ignorable_js_doc_tag(module, decl))
  {
    return true;
  }
  let parent_symbol = symbol.parent_id().and_then(|id| module.symbol(id));
  if let Some(parent_symbol) = parent_symbol {
    symbol_has_ignorable_js_doc_tag(module, parent_symbol)
  } else {
    false
  }
}

fn decl_has_ignorable_js_doc_tag(
  module: ModuleInfoRef,
  decl: &SymbolDecl,
) -> bool {
  let Some(module) = module.esm() else {
    return false;
  };
  let js_doc = js_doc_for_range_include_ignore(module.source(), &decl.range);
  has_ignorable_js_doc_tag(&js_doc)
}
