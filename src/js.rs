// Copyright 2020-2022 the Deno authors. All rights reserved. MIT license.

use crate::parser::DocParser;

use anyhow::anyhow;
use anyhow::Result;
use deno_graph::create_type_graph;
use deno_graph::source::LoadFuture;
use deno_graph::source::Loader;
use deno_graph::source::ResolveResponse;
use deno_graph::source::Resolver;
use deno_graph::ModuleSpecifier;
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
        .map(|value| value.into_serde().unwrap())
        .map_err(|_| anyhow!("load rejected or errored"))
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
  ) -> ResolveResponse {
    let this = JsValue::null();
    let arg0 = JsValue::from(specifier);
    let arg1 = JsValue::from(referrer.to_string());
    let value = match self.resolve.call2(&this, &arg0, &arg1) {
      Ok(value) => value,
      Err(_) => {
        return ResolveResponse::Err(anyhow!(
          "JavaScript resolve() function threw."
        ))
      }
    };
    let value: String = match value.into_serde() {
      Ok(value) => value,
      Err(err) => return ResolveResponse::Err(err.into()),
    };
    match ModuleSpecifier::parse(&value) {
      Ok(specifier) => ResolveResponse::Specifier(specifier),
      Err(err) => ResolveResponse::Err(err.into()),
    }
  }
}

// https://github.com/rustwasm/wasm-bindgen/issues/2774
#[allow(clippy::unused_unit)]
#[wasm_bindgen]
pub async fn doc(
  root_specifier: String,
  include_all: bool,
  load: js_sys::Function,
  maybe_resolve: Option<js_sys::Function>,
) -> Result<JsValue, JsValue> {
  console_error_panic_hook::set_once();
  let root_specifier = ModuleSpecifier::parse(&root_specifier)
    .map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))?;
  let mut loader = JsLoader::new(load);
  let maybe_resolver = maybe_resolve.map(JsResolver::new);
  let graph = create_type_graph(
    vec![root_specifier.clone()],
    false,
    None,
    &mut loader,
    maybe_resolver.as_ref().map(|r| r as &dyn Resolver),
    None,
    None,
    None,
  )
  .await;
  let source_parser = deno_graph::DefaultSourceParser::new();
  let entries = DocParser::new(graph, include_all, &source_parser)
    .parse_with_reexports(&root_specifier)
    .map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))?;
  JsValue::from_serde(&entries)
    .map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))
}
