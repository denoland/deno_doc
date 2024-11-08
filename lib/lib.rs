// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use anyhow::anyhow;
use anyhow::Context;
use deno_doc::html::{
  DocNodeWithContext, UrlResolveKind, UsageComposerEntry, UsageToMd,
};
use deno_doc::DocParser;
use deno_graph::source::CacheSetting;
use deno_graph::source::LoadFuture;
use deno_graph::source::LoadOptions;
use deno_graph::source::LoadResponse;
use deno_graph::source::Loader;
use deno_graph::source::ResolveError;
use deno_graph::source::Resolver;
use deno_graph::BuildOptions;
use deno_graph::CapturingModuleAnalyzer;
use deno_graph::GraphKind;
use deno_graph::ModuleGraph;
use deno_graph::ModuleSpecifier;
use deno_graph::Range;
use import_map::ImportMap;
use import_map::ImportMapOptions;
use indexmap::IndexMap;
use serde::Serialize;
use std::rc::Rc;
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
    &self,
    specifier: &ModuleSpecifier,
    options: LoadOptions,
  ) -> LoadFuture {
    #[derive(Serialize)]
    #[serde(rename_all = "camelCase")]
    struct JsLoadOptions {
      pub is_dynamic: bool,
      pub cache_setting: &'static str,
      pub checksum: Option<String>,
    }

    let this = JsValue::null();
    let arg0 = JsValue::from(specifier.to_string());
    let arg1 = serde_wasm_bindgen::to_value(&JsLoadOptions {
      is_dynamic: options.is_dynamic,
      cache_setting: options.cache_setting.as_js_str(),
      checksum: options.maybe_checksum.map(|c| c.into_string()),
    })
    .unwrap();
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
    referrer_range: &Range,
    _mode: deno_graph::source::ResolutionMode,
  ) -> Result<ModuleSpecifier, ResolveError> {
    self
      .0
      .resolve(specifier, &referrer_range.specifier)
      .map_err(|err| ResolveError::Other(err.into()))
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
    referrer_range: &Range,
    _mode: deno_graph::source::ResolutionMode,
  ) -> Result<ModuleSpecifier, ResolveError> {
    use ResolveError::*;
    let this = JsValue::null();
    let arg0 = JsValue::from(specifier);
    let arg1 = JsValue::from(referrer_range.specifier.to_string());
    let value = match self.resolve.call2(&this, &arg0, &arg1) {
      Ok(value) => value,
      Err(_) => {
        return Err(Other(anyhow!("JavaScript resolve() function threw.")))
      }
    };
    let value: String = serde_wasm_bindgen::from_value(value)
      .map_err(|err| anyhow!("{}", err))?;
    ModuleSpecifier::parse(&value).map_err(|err| Other(err.into()))
  }
}

#[wasm_bindgen]
pub async fn doc(
  root_specifier: String,
  include_all: bool,
  load: js_sys::Function,
  maybe_resolve: Option<js_sys::Function>,
  maybe_import_map: Option<String>,
  print_import_map_diagnostics: bool,
) -> anyhow::Result<JsValue, JsValue> {
  console_error_panic_hook::set_once();
  inner_doc(
    root_specifier,
    include_all,
    load,
    maybe_resolve,
    maybe_import_map,
    print_import_map_diagnostics,
  )
  .await
  .map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))
}

async fn inner_doc(
  root_specifier: String,
  include_all: bool,
  load: js_sys::Function,
  maybe_resolve: Option<js_sys::Function>,
  maybe_import_map: Option<String>,
  print_import_map_diagnostics: bool,
) -> Result<JsValue, anyhow::Error> {
  let root_specifier = ModuleSpecifier::parse(&root_specifier)?;
  let mut loader = JsLoader::new(load);
  let maybe_resolver: Option<Box<dyn Resolver>> = if let Some(import_map) =
    maybe_import_map
  {
    if print_import_map_diagnostics && maybe_resolve.is_some() {
      console_warn!("An import map is specified as well as a resolve function, ignoring resolve function.");
    }
    let import_map_specifier = ModuleSpecifier::parse(&import_map)?;
    if let Some(LoadResponse::Module {
      content, specifier, ..
    }) = loader
      .load(
        &import_map_specifier,
        LoadOptions {
          is_dynamic: false,
          was_dynamic_root: false,
          cache_setting: CacheSetting::Use,
          maybe_checksum: None,
        },
      )
      .await?
    {
      let text = String::from_utf8(content.to_vec())
        .context("Failed decoding import map.")?;
      let result = import_map::parse_from_json_with_options(
        specifier,
        &text,
        ImportMapOptions {
          address_hook: None,
          // always do this for simplicity
          expand_imports: true,
        },
      )?;
      if print_import_map_diagnostics && !result.diagnostics.is_empty() {
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
        module_analyzer: &analyzer,
        resolver: maybe_resolver.as_ref().map(|r| r.as_ref()),
        ..Default::default()
      },
    )
    .await;
  let entries = DocParser::new(
    &graph,
    &analyzer,
    deno_doc::DocParserOptions {
      diagnostics: false,
      private: include_all,
    },
  )?
  .parse_with_reexports(&root_specifier)?;
  let serializer =
    serde_wasm_bindgen::Serializer::new().serialize_maps_as_objects(true);
  Ok(entries.serialize(&serializer).unwrap())
}

#[allow(clippy::too_many_arguments)]
#[wasm_bindgen]
pub fn generate_html(
  package_name: Option<String>,
  main_entrypoint: Option<String>,

  usage_composer_single_mode: bool,
  usage_composer_compose: js_sys::Function,

  rewrite_map: JsValue,
  category_docs: JsValue,
  disable_search: bool,
  symbol_redirect_map: JsValue,
  default_symbol_map: JsValue,

  resolve_path: js_sys::Function,
  resolve_global_symbol: js_sys::Function,
  resolve_import_href: js_sys::Function,
  resolve_source: js_sys::Function,
  resolve_external_jsdoc_module: js_sys::Function,

  markdown_renderer: js_sys::Function,
  markdown_stripper: js_sys::Function,

  doc_nodes_by_url: JsValue,
) -> Result<JsValue, JsValue> {
  console_error_panic_hook::set_once();

  generate_html_inner(
    package_name,
    main_entrypoint,
    usage_composer_single_mode,
    usage_composer_compose,
    rewrite_map,
    category_docs,
    disable_search,
    symbol_redirect_map,
    default_symbol_map,
    resolve_path,
    resolve_global_symbol,
    resolve_import_href,
    resolve_source,
    resolve_external_jsdoc_module,
    markdown_renderer,
    markdown_stripper,
    doc_nodes_by_url,
  )
  .map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))
}

struct JsHrefResolver {
  resolve_path: js_sys::Function,
  resolve_global_symbol: js_sys::Function,
  resolve_import_href: js_sys::Function,
  resolve_source: js_sys::Function,
  resolve_external_jsdoc_module: js_sys::Function,
}

impl deno_doc::html::HrefResolver for JsHrefResolver {
  fn resolve_path(
    &self,
    current: UrlResolveKind,
    target: UrlResolveKind,
  ) -> String {
    let this = JsValue::null();

    let current = serde_wasm_bindgen::to_value(&current).unwrap();
    let target = serde_wasm_bindgen::to_value(&target).unwrap();

    let global_symbol = self
      .resolve_path
      .call2(&this, &current, &target)
      .expect("resolve_path errored");

    serde_wasm_bindgen::from_value(global_symbol)
      .expect("resolve_path returned an invalid value")
  }

  fn resolve_global_symbol(&self, symbol: &[String]) -> Option<String> {
    let this = JsValue::null();

    let symbol = serde_wasm_bindgen::to_value(&symbol).unwrap();

    let global_symbol = self
      .resolve_global_symbol
      .call1(&this, &symbol)
      .expect("resolve_global_symbol errored");

    serde_wasm_bindgen::from_value(global_symbol)
      .expect("resolve_global_symbol returned an invalid value")
  }

  fn resolve_import_href(
    &self,
    symbol: &[String],
    src: &str,
  ) -> Option<String> {
    let this = JsValue::null();

    let symbol = serde_wasm_bindgen::to_value(&symbol).unwrap();
    let src = serde_wasm_bindgen::to_value(&src).unwrap();

    let global_symbol = self
      .resolve_import_href
      .call2(&this, &symbol, &src)
      .expect("resolve_import_href errored");

    serde_wasm_bindgen::from_value(global_symbol)
      .expect("resolve_import_href returned an invalid value")
  }

  fn resolve_source(&self, location: &deno_doc::Location) -> Option<String> {
    let this = JsValue::null();

    let location = serde_wasm_bindgen::to_value(&location).unwrap();

    let global_symbol = self
      .resolve_source
      .call1(&this, &location)
      .expect("resolve_source errored");

    serde_wasm_bindgen::from_value(global_symbol)
      .expect("resolve_source returned an invalid value")
  }

  fn resolve_external_jsdoc_module(
    &self,
    module: &str,
    symbol: Option<&str>,
  ) -> Option<(String, String)> {
    let this = JsValue::null();

    let module = serde_wasm_bindgen::to_value(&module).unwrap();
    let symbol = serde_wasm_bindgen::to_value(&symbol).unwrap();

    let global_symbol = self
      .resolve_external_jsdoc_module
      .call2(&this, &module, &symbol)
      .expect("resolve_external_jsdoc_module errored");

    serde_wasm_bindgen::from_value(global_symbol)
      .expect("resolve_external_jsdoc_module returned an invalid value")
  }
}

struct JsUsageComposer {
  single_mode: bool,
  compose: js_sys::Function,
}

impl deno_doc::html::UsageComposer for JsUsageComposer {
  fn is_single_mode(&self) -> bool {
    self.single_mode
  }

  fn compose(
    &self,
    doc_nodes: &[DocNodeWithContext],
    current_resolve: UrlResolveKind,
    usage_to_md: UsageToMd,
  ) -> IndexMap<UsageComposerEntry, String> {
    let this = JsValue::null();

    let doc_nodes = serde_wasm_bindgen::to_value(&doc_nodes).unwrap();
    let current_resolve =
      serde_wasm_bindgen::to_value(&current_resolve).unwrap();

    let global_symbol = self
      .compose
      .call3(&this, &doc_nodes, &current_resolve, &usage_to_md)
      .expect("compose errored");

    serde_wasm_bindgen::from_value(global_symbol)
      .expect("compose returned an invalid value")
  }
}

#[allow(clippy::too_many_arguments)]
fn generate_html_inner(
  package_name: Option<String>,
  main_entrypoint: Option<String>,

  usage_composer_single_mode: bool,
  usage_composer_compose: js_sys::Function,

  rewrite_map: JsValue,
  category_docs: JsValue,
  disable_search: bool,
  symbol_redirect_map: JsValue,
  default_symbol_map: JsValue,

  resolve_path: js_sys::Function,
  resolve_global_symbol: js_sys::Function,
  resolve_import_href: js_sys::Function,
  resolve_source: js_sys::Function,
  resolve_external_jsdoc_module: js_sys::Function,

  markdown_renderer: js_sys::Function,
  markdown_stripper: js_sys::Function,

  doc_nodes_by_url: JsValue,
) -> Result<JsValue, anyhow::Error> {
  let main_entrypoint = main_entrypoint
    .map(|s| ModuleSpecifier::parse(&s))
    .transpose()?;

  let rewrite_map = serde_wasm_bindgen::from_value::<
    Option<IndexMap<ModuleSpecifier, String>>,
  >(rewrite_map)
  .map_err(|err| anyhow!("{}", err))?;

  let category_docs = serde_wasm_bindgen::from_value::<
    Option<IndexMap<String, Option<String>>>,
  >(category_docs)
  .map_err(|err| anyhow!("{}", err))?;

  let symbol_redirect_map = serde_wasm_bindgen::from_value::<
    Option<IndexMap<String, IndexMap<String, String>>>,
  >(symbol_redirect_map)
  .map_err(|err| anyhow!("{}", err))?;

  let default_symbol_map = serde_wasm_bindgen::from_value::<
    Option<IndexMap<String, String>>,
  >(default_symbol_map)
  .map_err(|err| anyhow!("{}", err))?;

  let doc_nodes_by_url: IndexMap<ModuleSpecifier, Vec<deno_doc::DocNode>> =
    serde_wasm_bindgen::from_value(doc_nodes_by_url)
      .map_err(|err| anyhow!("{}", err))?;

  let markdown_renderer = Rc::new(
    move |md: &str,
          title_only: bool,
          file_path: Option<deno_doc::html::ShortPath>,
          anchorizer: deno_doc::html::jsdoc::Anchorizer| {
      let this = JsValue::null();
      let md = serde_wasm_bindgen::to_value(md).unwrap();
      let title_only = serde_wasm_bindgen::to_value(&title_only).unwrap();
      let file_path = serde_wasm_bindgen::to_value(&file_path).unwrap();

      let html = markdown_renderer
        .apply(
          &this,
          &js_sys::Array::of4(&md, &title_only, &file_path, &anchorizer),
        )
        .expect("markdown_renderer errored");

      serde_wasm_bindgen::from_value(html)
        .expect("markdown_renderer returned an invalid value")
    },
  );

  let markdown_stripper = Rc::new(move |md: &str| {
    let this = JsValue::null();
    let md = serde_wasm_bindgen::to_value(md).unwrap();

    let stripped = markdown_stripper
      .call1(&this, &md)
      .expect("markdown_stripper errored");

    serde_wasm_bindgen::from_value(stripped)
      .expect("markdown_stripper returned an invalid value")
  });

  let files = deno_doc::html::generate(
    deno_doc::html::GenerateOptions {
      package_name,
      main_entrypoint,
      href_resolver: Rc::new(JsHrefResolver {
        resolve_path,
        resolve_global_symbol,
        resolve_import_href,
        resolve_source,
        resolve_external_jsdoc_module,
      }),
      usage_composer: Rc::new(JsUsageComposer {
        single_mode: usage_composer_single_mode,
        compose: usage_composer_compose,
      }),
      rewrite_map,
      category_docs,
      disable_search,
      symbol_redirect_map,
      default_symbol_map,
      markdown_renderer,
      markdown_stripper,
      head_inject: None,
    },
    doc_nodes_by_url,
  )?;

  let serializer =
    serde_wasm_bindgen::Serializer::new().serialize_maps_as_objects(true);

  files
    .serialize(&serializer)
    .map_err(|err| anyhow!("{}", err))
}
