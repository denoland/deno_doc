use deno_doc::html::pages::SymbolPage;
use deno_doc::html::*;
use indexmap::IndexMap;
use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::Instant;

/// Simple tracking allocator for single-threaded memory profiling.
/// Uses Relaxed ordering — not accurate under multi-threaded contention.
struct TrackingAllocator;

static ALLOCATED: AtomicUsize = AtomicUsize::new(0);
static PEAK: AtomicUsize = AtomicUsize::new(0);

unsafe impl GlobalAlloc for TrackingAllocator {
  unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
    let ret = unsafe { System.alloc(layout) };
    if !ret.is_null() {
      let current =
        ALLOCATED.fetch_add(layout.size(), Ordering::Relaxed) + layout.size();
      PEAK.fetch_max(current, Ordering::Relaxed);
    }
    ret
  }

  unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
    unsafe { System.dealloc(ptr, layout) };
    ALLOCATED.fetch_sub(layout.size(), Ordering::Relaxed);
  }
}

#[global_allocator]
static GLOBAL: TrackingAllocator = TrackingAllocator;

fn reset_peak() {
  PEAK.store(ALLOCATED.load(Ordering::Relaxed), Ordering::Relaxed);
}

fn current_allocated() -> usize {
  ALLOCATED.load(Ordering::Relaxed)
}

fn peak_allocated() -> usize {
  PEAK.load(Ordering::Relaxed)
}

fn mb(bytes: usize) -> f64 {
  bytes as f64 / (1024.0 * 1024.0)
}

struct EmptyResolver;

impl HrefResolver for EmptyResolver {
  fn resolve_path(
    &self,
    current: UrlResolveKind,
    target: UrlResolveKind,
  ) -> String {
    href_path_resolve(current, target)
  }
  fn resolve_global_symbol(&self, _symbol: &[String]) -> Option<String> {
    None
  }
  fn resolve_import_href(
    &self,
    _symbol: &[String],
    _src: &str,
  ) -> Option<String> {
    None
  }
  fn resolve_source(&self, _location: &deno_doc::Location) -> Option<String> {
    None
  }
  fn resolve_external_jsdoc_module(
    &self,
    _module: &str,
    _symbol: Option<&str>,
  ) -> Option<(String, String)> {
    None
  }
}

impl UsageComposer for EmptyResolver {
  fn is_single_mode(&self) -> bool {
    true
  }
  fn compose(
    &self,
    _current_resolve: UrlResolveKind,
    _usage_to_md: UsageToMd,
  ) -> IndexMap<UsageComposerEntry, String> {
    IndexMap::new()
  }
}

fn make_options() -> GenerateOptions {
  GenerateOptions {
    package_name: Some("@zod/zod".to_string()),
    main_entrypoint: None,
    href_resolver: Arc::new(EmptyResolver),
    usage_composer: Some(Arc::new(EmptyResolver)),
    rewrite_map: None,
    category_docs: None,
    disable_search: false,
    symbol_redirect_map: None,
    default_symbol_map: None,
    markdown_renderer: comrak::create_renderer(None, None, None),
    markdown_stripper: Arc::new(comrak::strip),
    head_inject: None,
    id_prefix: None,
    diff_only: false,
  }
}

fn main() {
  let raw = std::fs::read_to_string(
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
      .join("tests/testdata/@zod_zod_4.3.6_raw.json"),
  )
  .unwrap();

  eprintln!("=== Memory Profile: deno_doc HTML generator (zod fixture) ===\n");

  let baseline = current_allocated();
  eprintln!("Baseline allocation: {:.2} MB", mb(baseline));

  // Parse the fixture into ParseOutput
  reset_peak();
  let before = current_allocated();
  let t0 = Instant::now();

  let fixture: serde_json::Map<String, serde_json::Value> =
    serde_json::from_str(&raw).unwrap();

  let mut doc_nodes_by_url: deno_doc::ParseOutput = IndexMap::new();
  for (url, v1_nodes) in fixture {
    let specifier = deno_ast::ModuleSpecifier::parse(&url).unwrap();
    let doc = deno_doc::docnodes_v1_to_v2(v1_nodes);
    doc_nodes_by_url.insert(specifier, doc);
  }

  let parse_time = t0.elapsed();
  let after_parse = current_allocated();
  let peak_parse = peak_allocated();
  eprintln!(
    "ParseOutput: retained={:.2} MB, peak={:.2} MB, time={:.1}ms",
    mb(after_parse - before),
    mb(peak_parse - before),
    parse_time.as_secs_f64() * 1000.0
  );

  let num_modules = doc_nodes_by_url.len();
  let num_top_symbols: usize =
    doc_nodes_by_url.values().map(|d| d.symbols.len()).sum();

  fn count_recursive(symbols: &[std::sync::Arc<deno_doc::Symbol>]) -> usize {
    let mut count = symbols.len();
    for symbol in symbols {
      for decl in &symbol.declarations {
        if let Some(ns) = decl.namespace_def() {
          count += count_recursive(&ns.elements);
        }
      }
    }
    count
  }
  let num_recursive: usize = doc_nodes_by_url
    .values()
    .map(|d| count_recursive(&d.symbols))
    .sum();
  eprintln!(
    "  modules={num_modules}, top_level_symbols={num_top_symbols}, total_recursive={num_recursive}"
  );

  // === JSR-like pattern: create GenerateCtx + render single page ===
  eprintln!("\n--- JSR-like single page rendering ---");

  // Simulate JSR: clone ParseOutput from cache (JSR caches Arc<ParseOutput>)
  reset_peak();
  let before_clone = current_allocated();
  let tc = Instant::now();
  let cached_output = doc_nodes_by_url.clone();
  let clone_time = tc.elapsed();
  eprintln!(
    "ParseOutput clone: retained={:.2} MB, peak={:.2} MB, time={:.1}ms",
    mb(current_allocated() - before_clone),
    mb(peak_allocated() - before_clone),
    clone_time.as_secs_f64() * 1000.0
  );

  reset_peak();
  let before_ctx = current_allocated();
  let t1 = Instant::now();

  let mut options = make_options();
  options.main_entrypoint = Some(cached_output.keys().next().unwrap().clone());

  let ctx = GenerateCtx::create_basic(options, cached_output, None).unwrap();

  let ctx_time = t1.elapsed();
  let after_ctx = current_allocated();
  let peak_ctx = peak_allocated();
  eprintln!(
    "GenerateCtx::create_basic: retained={:.2} MB, peak={:.2} MB, time={:.1}ms",
    mb(after_ctx - before_ctx),
    mb(peak_ctx - before_ctx),
    ctx_time.as_secs_f64() * 1000.0
  );

  let num_doc_nodes: usize = ctx.doc_nodes.values().map(|v| v.len()).sum();
  eprintln!("  doc_nodes entries={num_doc_nodes}");

  // Render a single symbol page (like JSR DocsRequest::Symbol)
  reset_peak();
  let before_render = current_allocated();
  let t2 = Instant::now();

  let (short_path, doc_nodes) = ctx.doc_nodes.iter().next().unwrap();
  let symbol_pages =
    generate_symbol_pages_for_module(&ctx, short_path, doc_nodes);

  let render_time = t2.elapsed();
  let after_render = current_allocated();
  let peak_render = peak_allocated();
  let num_pages = symbol_pages.len();
  eprintln!(
    "generate_symbol_pages_for_module: retained={:.2} MB, peak={:.2} MB, time={:.1}ms",
    mb(after_render - before_render),
    mb(peak_render - before_render),
    render_time.as_secs_f64() * 1000.0
  );
  eprintln!("  symbol pages generated: {num_pages}");

  // Count symbol vs redirect pages
  let mut symbol_count = 0usize;
  let mut redirect_count = 0usize;
  for page in &symbol_pages {
    match page {
      SymbolPage::Symbol { .. } => symbol_count += 1,
      SymbolPage::Redirect { .. } => redirect_count += 1,
    }
  }
  eprintln!("  symbols: {symbol_count}, redirects: {redirect_count}");

  // Drop everything and show cleanup
  drop(symbol_pages);
  let after_drop_pages = current_allocated();

  drop(ctx);
  let after_drop_ctx = current_allocated();

  eprintln!("\n--- Cleanup ---");
  eprintln!("After drop symbol_pages: {:.2} MB", mb(after_drop_pages));
  eprintln!("After drop GenerateCtx: {:.2} MB", mb(after_drop_ctx));

  // Drop everything from previous test to get clean baseline
  drop(doc_nodes_by_url);
  eprintln!("After cleanup: {:.2} MB\n", mb(current_allocated()));

  // === generate_json_with (streaming) — fresh run ===
  eprintln!("--- generate_json_with (streaming) ---");

  let raw2 = std::fs::read_to_string(
    std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
      .join("tests/testdata/@zod_zod_4.3.6_raw.json"),
  )
  .unwrap();
  let fixture2: serde_json::Map<String, serde_json::Value> =
    serde_json::from_str(&raw2).unwrap();
  let mut doc_nodes2: deno_doc::ParseOutput = IndexMap::new();
  for (url, v1_nodes) in fixture2 {
    let specifier = deno_ast::ModuleSpecifier::parse(&url).unwrap();
    let doc = deno_doc::docnodes_v1_to_v2(v1_nodes);
    doc_nodes2.insert(specifier, doc);
  }

  reset_peak();
  let before_full = current_allocated();
  let t4 = Instant::now();

  let mut options2 = make_options();
  options2.main_entrypoint = Some(doc_nodes2.keys().next().unwrap().clone());
  let ctx2 = GenerateCtx::create_basic(options2, doc_nodes2, None).unwrap();

  let mut file_count = 0usize;
  let mut total_json_size = 0usize;
  let mut largest_file = (String::new(), 0usize);
  let mut seen = std::collections::HashSet::new();
  let mut dup_count = 0usize;
  generate_json_with(ctx2, |name, content| {
    let len = content.len();
    total_json_size += len;
    file_count += 1;
    if !seen.insert(name.clone()) {
      dup_count += 1;
    }
    if len > largest_file.1 {
      largest_file = (name, len);
    }
    // Drop content immediately — this is the streaming benefit
  })
  .unwrap();

  let full_time = t4.elapsed();
  let peak_full = peak_allocated();
  eprintln!(
    "generate_json_with (streaming): peak={:.2} MB, time={:.1}ms",
    mb(peak_full - before_full),
    full_time.as_secs_f64() * 1000.0
  );
  eprintln!(
    "  output files={file_count} (unique={}, dups={dup_count})",
    seen.len()
  );
  eprintln!("  total JSON output size={:.2} MB", mb(total_json_size));
  eprintln!(
    "  largest file: {:.1} MB  {}",
    mb(largest_file.1),
    largest_file.0,
  );
}
