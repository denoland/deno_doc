use crate::html::DocNodeWithContext;
use crate::html::GenerateCtx;
use crate::html::UrlResolveKind;
use crate::html::util::BreadcrumbCtx;
use crate::html::util::BreadcrumbsCtx;
use crate::html::util::NamespacedSymbols;
use crate::node::DocNodeDef;
use deno_graph::ModuleSpecifier;
use serde::Deserialize;
use serde::Serialize;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::Mutex;

#[derive(Clone)]
pub struct RenderContext<'ctx> {
  pub ctx: &'ctx GenerateCtx,
  scoped_symbols: NamespacedSymbols,
  current_imports: Rc<HashMap<String, String>>,
  current_type_params: Rc<HashSet<&'ctx str>>,
  current_resolve: UrlResolveKind<'ctx>,
  /// A vector of parts of the current namespace, eg. `vec!["Deno", "errors"]`.
  namespace_parts: Rc<[String]>,
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
    Self {
      ctx,
      scoped_symbols: NamespacedSymbols::new(ctx, doc_nodes),
      current_imports: Rc::new(get_current_imports(doc_nodes)),
      current_type_params: Default::default(),
      current_resolve,
      namespace_parts: Rc::new([]),
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
      current_type_params: Rc::new(current_type_params),
      ..self.clone()
    }
  }

  pub fn with_namespace(&self, namespace_parts: Rc<[String]>) -> Self {
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
      // TODO: clean this up to not clone and to_vec
      let mut parts = self.namespace_parts.to_vec();
      while !parts.is_empty() {
        let mut current_parts = parts.clone();
        current_parts.extend_from_slice(&target_symbol_parts);

        if let Some(origin) = self.scoped_symbols.get(&current_parts) {
          return Some(
            self.ctx.resolve_path(
              self.get_current_resolve(),
              UrlResolveKind::Symbol {
                file: self
                  .get_current_resolve()
                  .get_file()
                  .or_else(|| origin.as_ref().map(|origin| &**origin))
                  .unwrap(),
                symbol: &current_parts.join("."),
              },
            ),
          );
        }

        parts.pop();
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

#[derive(Clone)]
pub struct Anchorized(pub String);

impl HeadingToCAdapter {
  pub fn anchorize(&self, content: &str) -> Anchorized {
    let mut anchorizer = self.anchorizer.lock().unwrap();
    Anchorized(anchorizer.anchorize(content))
  }

  pub fn add_entry(&self, level: u8, content: &str, anchor: &Anchorized) {
    let mut toc = self.toc.lock().unwrap();
    let mut offset = self.offset.lock().unwrap();

    *offset = level;

    if toc.last().is_none_or(|toc| toc.content != content) {
      toc.push(ToCEntry {
        level,
        content: content.to_owned(),
        anchor: anchor.0.to_owned(),
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
  doc_nodes: &[DocNodeWithContext],
) -> HashMap<String, String> {
  let mut imports = HashMap::new();

  for doc_node in doc_nodes {
    if let DocNodeDef::Import { import_def } = &doc_node.def {
      // TODO: handle import aliasing
      if import_def.imported.as_deref() == Some(doc_node.get_name()) {
        imports.insert(doc_node.get_name().to_string(), import_def.src.clone());
      }
    }
  }

  imports
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::DocNode;
  use crate::Location;
  use crate::html::HrefResolver;
  use crate::html::{
    GenerateOptions, UsageComposer, UsageComposerEntry, UsageToMd,
  };
  use crate::node::DeclarationKind;
  use crate::node::ImportDef;
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
      vec![DocNode {
        name: "foo".into(),
        is_default: None,
        location: Location {
          filename: "a".into(),
          line: 0,
          col: 0,
          byte_index: 0,
        },
        declaration_kind: DeclarationKind::Private,
        js_doc: Default::default(),
        def: crate::node::DocNodeDef::Import {
          import_def: ImportDef {
            src: "b".to_string(),
            imported: Some("foo".to_string()),
          },
        },
      }],
    )]);

    let ctx = GenerateCtx::new(
      GenerateOptions {
        package_name: None,
        main_entrypoint: None,
        href_resolver: Rc::new(TestResolver),
        usage_composer: Rc::new(TestResolver),
        rewrite_map: None,
        category_docs: None,
        disable_search: false,
        symbol_redirect_map: None,
        default_symbol_map: None,
        markdown_renderer: crate::html::comrak::create_renderer(
          None, None, None,
        ),
        markdown_stripper: Rc::new(crate::html::comrak::strip),
        head_inject: None,
        id_prefix: None,
      },
      None,
      Default::default(),
      doc_nodes_by_url,
    )
    .unwrap();

    let (short_path, doc_nodes) = ctx.doc_nodes.first().unwrap();

    // globals
    let render_ctx = RenderContext::new(&ctx, doc_nodes, UrlResolveKind::Root);
    assert_eq!(render_ctx.lookup_symbol_href("bar").unwrap(), "global$bar");

    // imports
    let render_ctx = RenderContext::new(&ctx, doc_nodes, UrlResolveKind::Root);
    assert_eq!(render_ctx.lookup_symbol_href("foo").unwrap(), "b/foo");

    let render_ctx = RenderContext::new(
      &ctx,
      doc_nodes,
      UrlResolveKind::File { file: short_path },
    );
    assert_eq!(render_ctx.lookup_symbol_href("foo").unwrap(), "b/foo");
  }
}
