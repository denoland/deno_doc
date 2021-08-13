// Copyright 2020-2021 the Deno authors. All rights reserved. MIT license.

use crate::parser::DocParser;

use anyhow::anyhow;
use anyhow::Result;
use deno_graph::create_graph;
use deno_graph::source::LoadFuture;
use deno_graph::source::Loader;
use deno_graph::source::Resolver;
use deno_graph::ModuleSpecifier;
use serde::Serialize;
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::JsFuture;

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
    let specifier = specifier.clone();
    let this = JsValue::null();
    let arg0 = JsValue::from(specifier.to_string());
    let arg1 = JsValue::from(is_dynamic);
    let result = self.load.call2(&this, &arg0, &arg1).unwrap();
    let f = async move {
      let response = JsFuture::from(js_sys::Promise::resolve(&result)).await;
      (
        specifier,
        response
          .map(|value| value.into_serde().unwrap())
          .map_err(|_| anyhow!("promise rejected")),
      )
    };
    Box::pin(f)
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
    let value = self
      .resolve
      .call2(&this, &arg0, &arg1)
      .map_err(|_| anyhow!("JavaScript resolve() function threw."))?;
    let value: String = value.into_serde()?;
    let resolved_specifier = ModuleSpecifier::parse(&value)?;
    Ok(resolved_specifier)
  }
}

#[wasm_bindgen]
pub async fn doc(
  root_specifier: String,
  load: js_sys::Function,
  maybe_resolve: Option<js_sys::Function>,
) -> JsValue {
  let root_specifier = ModuleSpecifier::parse(&root_specifier).unwrap();
  let loader = Box::new(JsLoader::new(load));
  let maybe_resolver: Option<Box<dyn Resolver>> =
    if let Some(resolve) = maybe_resolve {
      Some(Box::new(JsResolver::new(resolve)))
    } else {
      None
    };
  let graph =
    create_graph(root_specifier.clone(), loader, maybe_resolver, None).await;
  let entries = DocParser::new(graph, false)
    .parse_with_reexports(&root_specifier)
    .unwrap();
  let serializer =
    serde_wasm_bindgen::Serializer::new().serialize_maps_as_objects(true);
  entries.serialize(&serializer).unwrap()
}
