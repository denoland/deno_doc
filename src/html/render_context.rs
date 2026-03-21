use crate::html::DocNodeWithContext;
use crate::html::GenerateCtx;
use crate::html::UrlResolveKind;
use crate::html::util::BreadcrumbCtx;
use crate::html::util::BreadcrumbsCtx;
use crate::html::util::NamespacedSymbols;
use deno_graph::ModuleSpecifier;
use serde::Deserialize;
use serde::Serialize;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;
use std::sync::Mutex;

#[derive(Clone)]
pub struct RenderContext<'ctx> {
  pub ctx: &'ctx GenerateCtx,
  scoped_symbols: NamespacedSymbols,
  current_imports: Arc<HashMap<String, String>>,
  current_type_params: Arc<HashSet<&'ctx str>>,
  current_resolve: UrlResolveKind<'ctx>,
  /// A vector of parts of the current namespace, eg. `vec!["Deno", "errors"]`.
  namespace_parts: Arc<[String]>,
  /// Only some when in `FileMode::SingleDts` and using categories
  pub category: Option<&'ctx str>,
  pub toc: HeadingToCAdapter,
  pub disable_links: bool,
}

impl<'ctx> RenderContext<'ctx> {
  pub fn new(
    ctx: &'ctx GenerateCtx,
    doc_nodes: &[DocNodeWithContext],
    current_resolve: UrlResolveKind<'ctx>,
  ) -> Self {
    let current_imports = current_resolve
      .get_file()
      .and_then(|file| ctx.imports.get(file))
      .map(|imports| get_current_imports(imports))
      .unwrap_or_default();

    Self {
      ctx,
      scoped_symbols: NamespacedSymbols::new(ctx, doc_nodes),
      current_imports: Arc::new(current_imports),
      current_type_params: Default::default(),
      current_resolve,
      namespace_parts: Arc::new([]),
      category: None,
      toc: Default::default(),
      disable_links: false,
    }
  }

  pub fn with_current_type_params(
    &self,
    current_type_params: HashSet<&'ctx str>,
  ) -> Self {
    Self {
      current_type_params: Arc::new(current_type_params),
      ..self.clone()
    }
  }

  pub fn with_namespace(&self, namespace_parts: Arc<[String]>) -> Self {
    Self {
      namespace_parts,
      ..self.clone()
    }
  }

  pub fn with_current_resolve(
    &self,
    current_resolve: UrlResolveKind<'ctx>,
  ) -> Self {
    Self {
      current_resolve,
      toc: Default::default(),
      ..self.clone()
    }
  }

  pub fn with_category(&self, category: Option<&'ctx str>) -> Self {
    Self {
      category,
      toc: Default::default(),
      ..self.clone()
    }
  }

  pub fn with_disable_links(&self, disable_links: bool) -> Self {
    Self {
      disable_links,
      ..self.clone()
    }
  }

  pub fn contains_type_param(&self, name: &str) -> bool {
    self.current_type_params.contains(name)
  }

  pub fn get_current_resolve(&self) -> UrlResolveKind<'_> {
    self.current_resolve
  }

  pub fn lookup_symbol_href(&self, target_symbol: &str) -> Option<String> {
    let target_symbol_parts = target_symbol
      .split('.')
      .map(String::from)
      .collect::<Vec<_>>();

    if !self.namespace_parts.is_empty() {
      let ns_len = self.namespace_parts.len();
      let target_len = target_symbol_parts.len();
      // Reusable buffer: [ns_prefix..., target_parts...]
      let mut lookup = Vec::with_capacity(ns_len + target_len);

      // Try progressively shorter namespace prefixes: ns_len, ns_len-1, ..., 1
      for prefix_len in (1..=ns_len).rev() {
        lookup.clear();
        lookup.extend(self.namespace_parts[..prefix_len].iter().cloned());
        lookup.extend_from_slice(&target_symbol_parts);

        if let Some(origin) = self.scoped_symbols.get(&lookup) {
          return Some(
            self.ctx.resolve_path(
              self.get_current_resolve(),
              UrlResolveKind::Symbol {
                file: self
                  .get_current_resolve()
                  .get_file()
                  .or_else(|| origin.as_ref().map(|origin| &**origin))
                  .unwrap(),
                symbol: &lookup.join("."),
              },
            ),
          );
        }
      }
    }

    if let Some(origin) = self.scoped_symbols.get(&target_symbol_parts) {
      return Some(
        self.ctx.resolve_path(
          self.get_current_resolve(),
          UrlResolveKind::Symbol {
            file: self
              .get_current_resolve()
              .get_file()
              .or_else(|| origin.as_ref().map(|origin| &**origin))
              .unwrap_or_else(|| {
                &(**self.ctx.main_entrypoint.as_ref().unwrap())
              }),
            symbol: target_symbol,
          },
        ),
      );
    }

    if let Some(src) = self.current_imports.get(target_symbol) {
      if let Ok(module_specifier) = ModuleSpecifier::parse(src)
        && let Some(short_path) = self
          .ctx
          .doc_nodes
          .keys()
          .find(|short_path| short_path.specifier == module_specifier)
      {
        return Some(self.ctx.resolve_path(
          self.get_current_resolve(),
          UrlResolveKind::Symbol {
            file: short_path,
            symbol: target_symbol,
          },
        ));
      }

      return self
        .ctx
        .href_resolver
        .resolve_import_href(&target_symbol_parts, src);
    }

    self
      .ctx
      .href_resolver
      .resolve_global_symbol(&target_symbol_parts)
  }

  pub fn get_breadcrumbs(&self) -> BreadcrumbsCtx {
    let root = BreadcrumbCtx {
      name: self.ctx.package_name.clone().unwrap_or("index".to_string()),
      href: self
        .ctx
        .resolve_path(self.current_resolve, UrlResolveKind::Root),
    };
    let mut current_entrypoint = None;

    let mut entrypoints = self
      .ctx
      .doc_nodes
      .keys()
      .map(|short_path| BreadcrumbCtx {
        name: short_path.display_name().to_string(),
        href: self.ctx.resolve_path(
          self.current_resolve,
          UrlResolveKind::File { file: short_path },
        ),
      })
      .collect::<Vec<_>>();

    entrypoints.insert(
      0,
      BreadcrumbCtx {
        name: "all symbols".to_string(),
        href: self
          .ctx
          .resolve_path(self.current_resolve, UrlResolveKind::AllSymbols),
      },
    );

    let mut symbols = vec![];

    match self.current_resolve {
      UrlResolveKind::Root => {
        entrypoints = vec![];
      }
      UrlResolveKind::AllSymbols => {
        current_entrypoint = Some(BreadcrumbCtx {
          name: "all symbols".to_string(),
          href: self
            .ctx
            .resolve_path(self.current_resolve, UrlResolveKind::AllSymbols),
        });
      }
      UrlResolveKind::Category { category } => {
        current_entrypoint = Some(BreadcrumbCtx {
          name: category.to_owned(),
          href: super::util::slugify(category),
        });
      }
      UrlResolveKind::File { file } => {
        current_entrypoint = Some(BreadcrumbCtx {
          name: file.display_name().to_string(),
          href: self
            .ctx
            .resolve_path(self.current_resolve, UrlResolveKind::File { file }),
        });
      }
      UrlResolveKind::Symbol { file, symbol } => {
        if let Some(category) = self.category {
          current_entrypoint = Some(BreadcrumbCtx {
            name: category.to_string(),
            href: self.ctx.resolve_path(
              self.current_resolve,
              UrlResolveKind::Category { category },
            ),
          });
        } else {
          current_entrypoint = Some(BreadcrumbCtx {
            name: file.display_name().to_string(),
            href: self.ctx.resolve_path(
              self.current_resolve,
              UrlResolveKind::File { file },
            ),
          });
        }

        let (_, symbol_parts) = split_with_brackets(symbol).into_iter().fold(
          (vec![], vec![]),
          |(mut symbol_parts, mut breadcrumbs), symbol_part| {
            symbol_parts.push(symbol_part.clone());
            let breadcrumb = BreadcrumbCtx {
              name: symbol_part,
              href: self.ctx.resolve_path(
                self.current_resolve,
                UrlResolveKind::Symbol {
                  file,
                  symbol: &symbol_parts.join("."),
                },
              ),
            };
            breadcrumbs.push(breadcrumb);

            (symbol_parts, breadcrumbs)
          },
        );

        symbols = symbol_parts;
      }
    }

    BreadcrumbsCtx {
      root,
      current_entrypoint,
      entrypoints,
      symbol: symbols,
    }
  }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ToCEntry {
  pub level: u8,
  pub content: String,
  pub anchor: String,
}

#[derive(Default)]
pub struct Anchorizer {
  map: HashMap<String, i32>,
  itoa_buffer: itoa::Buffer,
}

impl Anchorizer {
  /// Returns a String that has been converted into an anchor using the GFM algorithm.
  /// This replaces comrak's implementation to improve the performance.
  /// @see https://docs.rs/comrak/latest/comrak/struct.Anchorizer.html#method.anchorize
  pub fn anchorize(&mut self, s: &str) -> String {
    let mut s = REJECTED_CHARS
      .replace_all(&s.to_lowercase(), "")
      .replace(' ', "-");

    if let Some(count) = self.map.get_mut(&s) {
      let a = self.itoa_buffer.format(*count);
      s.push('-');
      s.push_str(a);

      *count += 1;
    } else {
      self.map.insert(s.clone(), 1);
    }

    s
  }
}

#[derive(Clone)]
pub struct HeadingToCAdapter {
  pub toc: Arc<Mutex<Vec<ToCEntry>>>,
  pub offset: Arc<Mutex<u8>>,
  pub anchorizer: Arc<Mutex<Anchorizer>>,
}

impl Default for HeadingToCAdapter {
  fn default() -> Self {
    Self {
      toc: Arc::new(Mutex::new(vec![])),
      anchorizer: Arc::new(Mutex::new(Default::default())),
      offset: Arc::new(Mutex::new(0)),
    }
  }
}

lazy_static! {
  static ref REJECTED_CHARS: regex::Regex =
    regex::Regex::new(r"[^\p{L}\p{M}\p{N}\p{Pc} -_/]").unwrap();
}

impl HeadingToCAdapter {
  pub fn anchorize(&self, content: &str) -> super::util::Id {
    let mut anchorizer = self.anchorizer.lock().unwrap();
    super::util::Id::from_raw(anchorizer.anchorize(content))
  }

  /// Apply the same GFM sanitization as anchorize but without registering
  /// in the deduplication map. Use for IDs referencing anchors on other pages.
  pub fn sanitize(&self, content: &str) -> super::util::Id {
    let s = REJECTED_CHARS
      .replace_all(&content.to_lowercase(), "")
      .replace(' ', "-");
    super::util::Id::from_raw(s)
  }

  pub fn add_entry(&self, level: u8, content: &str, anchor: &super::util::Id) {
    let mut toc = self.toc.lock().unwrap();
    let mut offset = self.offset.lock().unwrap();

    *offset = level;

    if toc.last().is_none_or(|toc| toc.content != content) {
      toc.push(ToCEntry {
        level,
        content: content.to_owned(),
        anchor: anchor.as_str().to_owned(),
      });
    }
  }

  pub fn render(&self) -> Option<String> {
    let toc = self.toc.lock().unwrap();

    if toc.is_empty() {
      return None;
    }

    let mut toc_content = vec!["<ul>".to_string()];
    let mut current_level = toc.iter().map(|entry| entry.level).min().unwrap();

    let mut level_diff = 0;
    for entry in toc.iter() {
      match current_level.cmp(&entry.level) {
        Ordering::Equal => {}
        Ordering::Less => {
          level_diff += 1;
          toc_content.push(r#"<li><ul>"#.to_string());
          current_level = entry.level;
        }
        Ordering::Greater => {
          level_diff -= 1;
          toc_content.push("</ul></li>".to_string());
          current_level = entry.level;
        }
      }

      toc_content.push(format!(
        r##"<li><a href="#{}" title="{}">{}</a></li>"##,
        entry.anchor,
        html_escape::encode_double_quoted_attribute(&entry.content),
        entry.content
      ));
    }

    for _ in 0..level_diff {
      toc_content.push("</ul></li>".to_string());
    }

    toc_content.push(String::from("</ul>"));

    Some(toc_content.join(""))
  }
}

fn split_with_brackets(s: &str) -> Vec<String> {
  let mut result = Vec::new();
  let mut current = String::new();
  let mut bracket = false;

  for c in s.chars() {
    if c == ']' {
      bracket = false;
      current.push(c);
      result.push(current.clone());
      current.clear();
    } else if c == '[' {
      bracket = true;
      if !current.is_empty() {
        result.push(current.clone());
        current.clear();
      }
      current.push(c);
    } else if c == '.' && !bracket {
      if !current.is_empty() {
        result.push(current.clone());
        current.clear();
      }
    } else {
      current.push(c);
    }
  }

  if !current.is_empty() {
    result.push(current.clone());
  }

  result
}

fn get_current_imports(
  imports: &[crate::node::Import],
) -> HashMap<String, String> {
  let mut imports_out = HashMap::new();

  for import in imports {
    // TODO: handle import aliasing
    if import.original_name.as_deref() == Some(&*import.imported_name) {
      imports_out.insert(import.imported_name.to_string(), import.src.clone());
    }
  }

  imports_out
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::Location;
  use crate::html::HrefResolver;
  use crate::html::{
    GenerateOptions, UsageComposer, UsageComposerEntry, UsageToMd,
  };
  use crate::node::Document;
  use crate::node::Import;
  use indexmap::IndexMap;

  struct TestResolver;

  impl HrefResolver for TestResolver {
    fn resolve_path(
      &self,
      current: UrlResolveKind,
      target: UrlResolveKind,
    ) -> String {
      crate::html::href_path_resolve(current, target)
    }

    fn resolve_global_symbol(&self, symbol: &[String]) -> Option<String> {
      if symbol == ["bar"] {
        Some("global$bar".to_string())
      } else {
        None
      }
    }

    fn resolve_import_href(
      &self,
      symbol: &[String],
      src: &str,
    ) -> Option<String> {
      Some(format!("{src}/{}", symbol.join(".")))
    }

    fn resolve_source(&self, location: &Location) -> Option<String> {
      Some(location.filename.clone().into_string())
    }

    fn resolve_external_jsdoc_module(
      &self,
      _module: &str,
      _symbol: Option<&str>,
    ) -> Option<(String, String)> {
      None
    }
  }

  impl UsageComposer for TestResolver {
    fn is_single_mode(&self) -> bool {
      true
    }

    fn compose(
      &self,
      current_resolve: UrlResolveKind,
      usage_to_md: UsageToMd,
    ) -> IndexMap<UsageComposerEntry, String> {
      current_resolve
        .get_file()
        .map(|current_file| {
          IndexMap::from([(
            UsageComposerEntry {
              name: "".to_string(),
              icon: None,
            },
            usage_to_md(current_file.specifier.as_str(), None),
          )])
        })
        .unwrap_or_default()
    }
  }

  #[test]
  fn lookup_symbol_href() {
    let doc_nodes_by_url = indexmap::IndexMap::from([(
      ModuleSpecifier::parse("file:///mod.ts").unwrap(),
      Document {
        module_doc: Default::default(),
        imports: vec![Import {
          imported_name: "foo".into(),
          js_doc: Default::default(),
          src: "b".to_string(),
          original_name: Some("foo".to_string()),
        }],
        symbols: vec![],
      },
    )]);

    let ctx = GenerateCtx::new(
      GenerateOptions {
        package_name: None,
        main_entrypoint: None,
        href_resolver: Arc::new(TestResolver),
        usage_composer: Some(Arc::new(TestResolver)),
        rewrite_map: None,
        category_docs: None,
        disable_search: false,
        symbol_redirect_map: None,
        default_symbol_map: None,
        markdown_renderer: crate::html::comrak::create_renderer(
          None, None, None,
        ),
        markdown_stripper: Arc::new(crate::html::comrak::strip),
        head_inject: None,
        id_prefix: None,
        diff_only: false,
      },
      None,
      Default::default(),
      doc_nodes_by_url,
      None,
    )
    .unwrap();

    let (short_path, doc_nodes) = ctx.doc_nodes.first().unwrap();

    // globals
    let render_ctx = RenderContext::new(&ctx, doc_nodes, UrlResolveKind::Root);
    assert_eq!(render_ctx.lookup_symbol_href("bar").unwrap(), "global$bar");

    // imports (only available when current resolve is a file)
    let render_ctx = RenderContext::new(
      &ctx,
      doc_nodes,
      UrlResolveKind::File { file: short_path },
    );
    assert_eq!(render_ctx.lookup_symbol_href("foo").unwrap(), "b/foo");
  }
}
