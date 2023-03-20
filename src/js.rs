// Copyright 2020-2022 the Deno authors. All rights reserved. MIT license.
// https://github.com/rustwasm/wasm-bindgen/issues/2774
#![allow(clippy::unused_unit)]

use crate::parser::DocParser;

use anyhow::anyhow;
use anyhow::Result;
use deno_graph::source::LoadFuture;
use deno_graph::source::LoadResponse;
use deno_graph::source::Loader;
use deno_graph::source::Resolver;
use deno_graph::BuildOptions;
use deno_graph::CapturingModuleAnalyzer;
use deno_graph::GraphKind;
use deno_graph::ModuleGraph;
use deno_graph::ModuleSpecifier;
use import_map::ImportMap;
use serde::Serialize;
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::JsFuture;

#[wasm_bindgen]
extern "C" {
  #[wasm_bindgen(js_namespace = console)]
  fn warn(s: &str);
}

macro_rules! console_warn {
  ($($t:tt)*) => (warn(&format_args!($($t)*).to_string()))
}

struct JsLoader {
  load: js_sys::Function,
}

impl JsLoader {
  pub fn new(load: js_sys::Function) -> Self {
    Self { load }
  }
}

impl Loader for JsLoader {
  fn load(
    &mut self,
    specifier: &ModuleSpecifier,
    is_dynamic: bool,
  ) -> LoadFuture {
    let this = JsValue::null();
    let arg0 = JsValue::from(specifier.to_string());
    let arg1 = JsValue::from(is_dynamic);
    let result = self.load.call2(&this, &arg0, &arg1);
    let f = async move {
      let response = match result {
        Ok(result) => JsFuture::from(js_sys::Promise::resolve(&result)).await,
        Err(err) => Err(err),
      };
      response
        .map(|value| serde_wasm_bindgen::from_value(value).unwrap())
        .map_err(|_| anyhow!("load rejected or errored"))
    };
    Box::pin(f)
  }
}

#[derive(Debug)]
pub struct ImportMapResolver(ImportMap);

impl ImportMapResolver {
  pub fn new(import_map: ImportMap) -> Self {
    Self(import_map)
  }
}

impl Resolver for ImportMapResolver {
  fn resolve(
    &self,
    specifier: &str,
    referrer: &ModuleSpecifier,
  ) -> Result<ModuleSpecifier> {
    Ok(self.0.resolve(specifier, referrer)?)
  }
}

#[derive(Debug)]
pub struct JsResolver {
  resolve: js_sys::Function,
}

impl JsResolver {
  pub fn new(resolve: js_sys::Function) -> Self {
    Self { resolve }
  }
}

impl Resolver for JsResolver {
  fn resolve(
    &self,
    specifier: &str,
    referrer: &ModuleSpecifier,
  ) -> Result<ModuleSpecifier> {
    let this = JsValue::null();
    let arg0 = JsValue::from(specifier);
    let arg1 = JsValue::from(referrer.to_string());
    let value = match self.resolve.call2(&this, &arg0, &arg1) {
      Ok(value) => value,
      Err(_) => return Err(anyhow!("JavaScript resolve() function threw.")),
    };
    let value: String = serde_wasm_bindgen::from_value(value)
      .map_err(|err| anyhow!("{}", err))?;
    Ok(ModuleSpecifier::parse(&value)?)
  }
}

#[wasm_bindgen]
pub async fn doc(
  root_specifier: String,
  include_all: bool,
  load: js_sys::Function,
  maybe_resolve: Option<js_sys::Function>,
  maybe_import_map: Option<String>,
) -> Result<JsValue, JsValue> {
  console_error_panic_hook::set_once();
  let root_specifier = ModuleSpecifier::parse(&root_specifier)
    .map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))?;
  let mut loader = JsLoader::new(load);
  let maybe_resolver: Option<Box<dyn Resolver>> = if let Some(import_map) =
    maybe_import_map
  {
    if maybe_resolve.is_some() {
      console_warn!("An import map is specified as well as a resolve function, ignoring resolve function.");
    }
    let import_map_specifier = ModuleSpecifier::parse(&import_map)
      .map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))?;
    if let Some(LoadResponse::Module {
      content, specifier, ..
    }) = loader
      .load(&import_map_specifier, false)
      .await
      .map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))?
    {
      let result = import_map::parse_from_json(&specifier, content.as_ref())
        .map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))?;
      if !result.diagnostics.is_empty() {
        console_warn!(
          "Import map diagnostics:\n{}",
          result
            .diagnostics
            .into_iter()
            .map(|d| format!("  - {}", d))
            .collect::<Vec<_>>()
            .join("\n")
        );
      }
      Some(Box::new(ImportMapResolver::new(result.import_map)))
    } else {
      None
    }
  } else {
    maybe_resolve.map(|res| Box::new(JsResolver::new(res)) as Box<dyn Resolver>)
  };
  let analyzer = CapturingModuleAnalyzer::default();
  let mut graph = ModuleGraph::new(GraphKind::TypesOnly);
  graph
    .build(
      vec![root_specifier.clone()],
      &mut loader,
      BuildOptions {
        module_analyzer: Some(&analyzer),
        resolver: maybe_resolver.as_ref().map(|r| r.as_ref()),
        ..Default::default()
      },
    )
    .await;
  let entries =
    DocParser::new(graph, include_all, analyzer.as_capturing_parser())
      .parse_with_reexports(&root_specifier)
      .map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))?;
  let serializer =
    serde_wasm_bindgen::Serializer::new().serialize_maps_as_objects(true);
  Ok(entries.serialize(&serializer).unwrap())
}
