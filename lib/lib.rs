// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use anyhow::anyhow;
use anyhow::Context;
use deno_doc::html::UrlResolveKind;
use deno_doc::html::UsageComposerEntry;
use deno_doc::html::UsageToMd;
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
use std::ffi::c_void;
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

thread_local! {
  static CURRENT: std::cell::RefCell<*const c_void> = const { std::cell::RefCell::new(std::ptr::null()) };
  static TARGET: std::cell::RefCell<*const c_void> = const { std::cell::RefCell::new(std::ptr::null()) };
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
    _kind: deno_graph::source::ResolutionKind,
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
    _kind: deno_graph::source::ResolutionKind,
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
  root_specifiers: Vec<String>,
  include_all: bool,
  load: js_sys::Function,
  maybe_resolve: Option<js_sys::Function>,
  maybe_import_map: Option<String>,
  print_import_map_diagnostics: bool,
) -> anyhow::Result<JsValue, JsValue> {
  console_error_panic_hook::set_once();
  inner_doc(
    root_specifiers,
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
  root_specifiers: Vec<String>,
  include_all: bool,
  load: js_sys::Function,
  maybe_resolve: Option<js_sys::Function>,
  maybe_import_map: Option<String>,
  print_import_map_diagnostics: bool,
) -> Result<JsValue, anyhow::Error> {
  let root_specifiers = root_specifiers
    .into_iter()
    .map(|root_specifier| ModuleSpecifier::parse(&root_specifier))
    .collect::<Result<Vec<_>, _>>()?;
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
      root_specifiers.clone(),
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
    &root_specifiers,
    deno_doc::DocParserOptions {
      diagnostics: false,
      private: include_all,
    },
  )?
  .parse()?;
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

  resolve_path: Option<js_sys::Function>,
  resolve_global_symbol: js_sys::Function,
  resolve_import_href: js_sys::Function,
  resolve_source: js_sys::Function,
  resolve_external_jsdoc_module: js_sys::Function,

  markdown_renderer: js_sys::Function,
  markdown_stripper: js_sys::Function,
  head_inject: Option<js_sys::Function>,

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
    head_inject,
    doc_nodes_by_url,
  )
  .map_err(|err| JsValue::from(js_sys::Error::new(&err.to_string())))
}

struct JsHrefResolver {
  resolve_path: Option<js_sys::Function>,
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
    if let Some(resolve_path) = &self.resolve_path {
      let this = JsValue::null();

      let new_current = current.clone();
      let new_target = target.clone();

      {
        let current_ptr =
          &new_current as *const UrlResolveKind as *const c_void;
        CURRENT.set(current_ptr);
        let target = &new_target as *const UrlResolveKind as *const c_void;
        TARGET.set(target);
      }

      let default_closure = Box::new(move || {
        CURRENT.with(|current| {
          let current_ptr = *current.borrow() as *const UrlResolveKind;
          assert!(!current_ptr.is_null());
          // SAFETY: this pointer is valid until destroyed, which is done
          //  after compose is called
          let current_val = unsafe { &*current_ptr };

          let path = TARGET.with(|target| {
            let target_ptr = *target.borrow() as *const UrlResolveKind;
            assert!(!target_ptr.is_null());
            // SAFETY: this pointer is valid until destroyed, which is done
            //  after compose is called
            let target_val = unsafe { &*target_ptr };

            let path =
              deno_doc::html::href_path_resolve(*current_val, *target_val);

            *target.borrow_mut() =
              target_val as *const UrlResolveKind as *const c_void;

            path
          });

          *current.borrow_mut() =
            current_val as *const UrlResolveKind as *const c_void;

          path
        })
      });

      let default_closure =
        Closure::wrap(Box::new(default_closure) as Box<dyn Fn() -> String>);
      let default_closure =
        JsCast::unchecked_ref::<js_sys::Function>(default_closure.as_ref());

      let current = serde_wasm_bindgen::to_value(&current).unwrap();
      let target = serde_wasm_bindgen::to_value(&target).unwrap();

      let global_symbol = resolve_path
        .call3(&this, &current, &target, default_closure)
        .expect("resolve_path errored");

      {
        let current =
          CURRENT.replace(std::ptr::null()) as *const UrlResolveKind;
        // SAFETY: take the pointer and drop it
        let _ = unsafe { &*current };

        let target = TARGET.replace(std::ptr::null()) as *const UrlResolveKind;
        // SAFETY: take the pointer and drop it
        let _ = unsafe { &*target };
      }

      serde_wasm_bindgen::from_value(global_symbol)
        .expect("resolve_path returned an invalid value")
    } else {
      deno_doc::html::href_path_resolve(current, target)
    }
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
    current_resolve: UrlResolveKind,
    usage_to_md: UsageToMd,
  ) -> IndexMap<UsageComposerEntry, String> {
    let this = JsValue::null();

    let current_resolve =
      serde_wasm_bindgen::to_value(&current_resolve).unwrap();

    let global_symbol = self
      .compose
      .call2(&this, &current_resolve, &usage_to_md)
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

  resolve_path: Option<js_sys::Function>,
  resolve_global_symbol: js_sys::Function,
  resolve_import_href: js_sys::Function,
  resolve_source: js_sys::Function,
  resolve_external_jsdoc_module: js_sys::Function,

  markdown_renderer: js_sys::Function,
  markdown_stripper: js_sys::Function,
  head_inject: Option<js_sys::Function>,

  doc_nodes_by_url: JsValue,
) -> Result<JsValue, anyhow::Error> {
  let main_entrypoint = main_entrypoint
    .map(|s| ModuleSpecifier::parse(&s))
    .transpose()
    .map_err(|e| anyhow::Error::from(e).context("mainEntrypoint"))?;

  let rewrite_map = serde_wasm_bindgen::from_value::<
    Option<IndexMap<ModuleSpecifier, String>>,
  >(rewrite_map)
  .map_err(|err| anyhow!("rewriteMap: {}", err))?;

  let category_docs = serde_wasm_bindgen::from_value::<
    Option<IndexMap<String, Option<String>>>,
  >(category_docs)
  .map_err(|err| anyhow!("categoryDocs: {}", err))?;

  let symbol_redirect_map = serde_wasm_bindgen::from_value::<
    Option<IndexMap<String, IndexMap<String, String>>>,
  >(symbol_redirect_map)
  .map_err(|err| anyhow!("symbolRedirectMap: {}", err))?;

  let default_symbol_map = serde_wasm_bindgen::from_value::<
    Option<IndexMap<String, String>>,
  >(default_symbol_map)
  .map_err(|err| anyhow!("defaultSymbolMap: {}", err))?;

  let doc_nodes_by_url: IndexMap<ModuleSpecifier, Vec<deno_doc::DocNode>> =
    serde_wasm_bindgen::from_value(doc_nodes_by_url)
      .map_err(|err| anyhow!("docNodesByUrl: {}", err))?;

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

  let head_inject: Option<Rc<dyn Fn(&str) -> String + 'static>> =
    if let Some(head_inject) = head_inject {
      let head_inject = Rc::new(move |root: &str| {
        let this = JsValue::null();
        let root = serde_wasm_bindgen::to_value(root).unwrap();

        let inject = head_inject
          .call1(&this, &root)
          .expect("head_inject errored");

        serde_wasm_bindgen::from_value::<String>(inject)
          .expect("head_inject returned an invalid value")
      });

      Some(head_inject)
    } else {
      None
    };

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
      head_inject,
    },
    doc_nodes_by_url,
  )?;

  let serializer =
    serde_wasm_bindgen::Serializer::new().serialize_maps_as_objects(true);

  files
    .serialize(&serializer)
    .map_err(|err| anyhow!("{}", err))
}
