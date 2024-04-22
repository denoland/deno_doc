use crate::html::util::BreadcrumbCtx;
use crate::html::util::BreadcrumbsCtx;
use crate::html::util::NamespacedSymbols;
use crate::html::ShortPath;
use crate::html::UrlResolveKind;
use crate::html::{DocNodeWithContext, GenerateCtx};
use crate::DocNodeKind;
use deno_graph::ModuleSpecifier;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

#[derive(Clone)]
pub struct RenderContext<'ctx> {
  pub ctx: &'ctx GenerateCtx<'ctx>,
  current_exports: NamespacedSymbols,
  current_imports: Rc<HashMap<String, String>>,
  current_type_params: Rc<HashSet<&'ctx str>>,
  current_resolve: UrlResolveKind<'ctx>,
  current_specifier: Option<&'ctx ModuleSpecifier>,
  /// A vector of parts of the current namespace, eg. `vec!["Deno", "errors"]`.
  namespace_parts: Rc<Vec<&'ctx str>>,
}

impl<'ctx> RenderContext<'ctx> {
  pub fn new(
    ctx: &'ctx GenerateCtx<'ctx>,
    doc_nodes: &[DocNodeWithContext],
    current_resolve: UrlResolveKind<'ctx>,
    current_specifier: Option<&'ctx ModuleSpecifier>,
  ) -> Self {
    Self {
      ctx,
      current_exports: NamespacedSymbols::new(doc_nodes),
      current_imports: Rc::new(get_current_imports(doc_nodes)),
      current_type_params: Default::default(),
      current_resolve,
      current_specifier,
      namespace_parts: Rc::new(vec![]),
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

  pub fn with_namespace(&self, namespace_parts: Vec<&'ctx str>) -> Self {
    Self {
      namespace_parts: Rc::new(namespace_parts),
      ..self.clone()
    }
  }

  pub fn contains_type_param(&self, name: &str) -> bool {
    self.current_type_params.contains(name)
  }

  pub fn get_namespace_parts(&self) -> &[&str] {
    &self.namespace_parts
  }

  pub fn get_current_resolve(&self) -> UrlResolveKind {
    self.current_resolve
  }

  pub fn get_current_specifier(&self) -> Option<&ModuleSpecifier> {
    self.current_specifier
  }

  pub fn lookup_symbol_href(&self, target_symbol: &str) -> Option<String> {
    let target_symbol_parts = target_symbol
      .split('.')
      .map(String::from)
      .collect::<Vec<_>>();

    if !self.namespace_parts.is_empty() {
      let mut parts = self
        .namespace_parts
        .iter()
        .map(|part| part.to_string())
        .collect::<Vec<String>>();
      while !parts.is_empty() {
        let mut current_parts = parts.clone();
        current_parts.extend_from_slice(&target_symbol_parts);

        if self.current_exports.contains(&current_parts) {
          return Some(self.ctx.href_resolver.resolve_path(
            self.get_current_resolve(),
            UrlResolveKind::Symbol {
              file: self.get_current_resolve().get_file().unwrap(),
              symbol: &current_parts.join("."),
            },
          ));
        }

        parts.pop();
      }
    }

    if self.current_exports.contains(&target_symbol_parts) {
      return Some(
        self.ctx.href_resolver.resolve_path(
          self.get_current_resolve(),
          UrlResolveKind::Symbol {
            file: &self
              .get_current_resolve()
              .get_file()
              .cloned()
              .unwrap_or_else(|| ShortPath::from(String::from("."))),
            symbol: target_symbol,
          },
        ),
      );
    }

    if let Some(src) = self.current_imports.get(target_symbol) {
      if let Ok(module_specifier) = ModuleSpecifier::parse(src) {
        if self.ctx.specifiers.contains(&module_specifier) {
          return Some(self.ctx.href_resolver.resolve_path(
            self.get_current_resolve(),
            UrlResolveKind::Symbol {
              file: &self.ctx.url_to_short_path(&module_specifier),
              symbol: target_symbol,
            },
          ));
        }
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
    let index_name =
      self.ctx.package_name.clone().unwrap_or("index".to_string());

    let parts = match self.current_resolve {
      UrlResolveKind::Root => vec![BreadcrumbCtx {
        name: index_name,
        href: "".to_string(),
        is_symbol: false,
        is_first_symbol: false,
      }],
      UrlResolveKind::AllSymbols => {
        vec![
          BreadcrumbCtx {
            name: index_name,
            href: self
              .ctx
              .href_resolver
              .resolve_path(self.current_resolve, UrlResolveKind::Root),
            is_symbol: false,
            is_first_symbol: false,
          },
          BreadcrumbCtx {
            name: "all symbols".to_string(),
            href: "".to_string(),
            is_symbol: false,
            is_first_symbol: false,
          },
        ]
      }
      UrlResolveKind::File(file) => {
        if self.current_specifier == self.ctx.main_entrypoint.as_ref() {
          vec![BreadcrumbCtx {
            name: index_name,
            href: "".to_string(),
            is_symbol: false,
            is_first_symbol: false,
          }]
        } else {
          vec![
            BreadcrumbCtx {
              name: index_name,
              href: self
                .ctx
                .href_resolver
                .resolve_path(self.current_resolve, UrlResolveKind::Root),
              is_symbol: false,
              is_first_symbol: false,
            },
            BreadcrumbCtx {
              name: file.to_name(),
              href: "".to_string(),
              is_symbol: false,
              is_first_symbol: false,
            },
          ]
        }
      }
      UrlResolveKind::Symbol { file, symbol } => {
        let mut parts = vec![BreadcrumbCtx {
          name: index_name,
          href: self
            .ctx
            .href_resolver
            .resolve_path(self.current_resolve, UrlResolveKind::Root),
          is_symbol: false,
          is_first_symbol: false,
        }];

        if self.current_specifier != self.ctx.main_entrypoint.as_ref() {
          parts.push(BreadcrumbCtx {
            name: file.to_name(),
            href: self
              .ctx
              .href_resolver
              .resolve_path(self.current_resolve, UrlResolveKind::File(file)),
            is_symbol: false,
            is_first_symbol: false,
          });
        }

        let (_, symbol_parts) = symbol.split('.').enumerate().fold(
          (vec![], vec![]),
          |(mut symbol_parts, mut breadcrumbs), (i, symbol_part)| {
            symbol_parts.push(symbol_part);
            let breadcrumb = BreadcrumbCtx {
              name: symbol_part.to_string(),
              href: self.ctx.href_resolver.resolve_path(
                self.current_resolve,
                UrlResolveKind::Symbol {
                  file,
                  symbol: &symbol_parts.join("."),
                },
              ),
              is_symbol: true,
              is_first_symbol: i == 0,
            };
            breadcrumbs.push(breadcrumb);

            (symbol_parts, breadcrumbs)
          },
        );

        parts.extend(symbol_parts);

        parts
      }
    };

    BreadcrumbsCtx { parts }
  }
}

fn get_current_imports(
  doc_nodes: &[DocNodeWithContext],
) -> HashMap<String, String> {
  let mut imports = HashMap::new();

  for doc_node in doc_nodes {
    if doc_node.kind == DocNodeKind::Import {
      let import_def = doc_node.import_def.as_ref().unwrap();
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
  use crate::html::setup_hbs;
  use crate::html::setup_highlighter;
  use crate::html::DocNodeKindWithDrilldown;
  use crate::html::HrefResolver;
  use crate::node::DeclarationKind;
  use crate::node::ImportDef;
  use crate::DocNode;
  use crate::Location;
  use std::sync::Arc;

  struct TestResolver();

  impl HrefResolver for TestResolver {
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

    fn resolve_usage(
      &self,
      current_specifier: &deno_ast::ModuleSpecifier,
      _current_file: Option<&ShortPath>,
    ) -> Option<String> {
      Some(current_specifier.to_string())
    }

    fn resolve_source(&self, location: &Location) -> Option<String> {
      Some(location.filename.clone())
    }
  }

  #[test]
  fn lookup_symbol_href() {
    let ctx = GenerateCtx {
      package_name: None,
      common_ancestor: None,
      main_entrypoint: None,
      specifiers: vec![],
      hbs: setup_hbs().unwrap(),
      highlight_adapter: setup_highlighter(false),
      url_rewriter: None,
      href_resolver: Rc::new(TestResolver()),
      usage_composer: None,
      rewrite_map: None,
      hide_module_doc_title: false,
      file_mode: Default::default(),
      sidebar_hide_all_symbols: false,
    };

    let doc_nodes = vec![DocNodeWithContext {
      origin: Rc::new(
        ctx.url_to_short_path(
          &ModuleSpecifier::parse("file:///mod.ts").unwrap(),
        ),
      ),
      ns_qualifiers: Rc::new(vec![]),
      drilldown_parent_kind: None,
      kind_with_drilldown: DocNodeKindWithDrilldown::Other(DocNodeKind::Import),
      inner: Arc::new(DocNode {
        kind: DocNodeKind::Import,
        name: "foo".to_string(),
        location: Location {
          filename: "a".to_string(),
          line: 0,
          col: 0,
          byte_index: 0,
        },
        declaration_kind: DeclarationKind::Private,
        js_doc: Default::default(),
        function_def: None,
        variable_def: None,
        enum_def: None,
        class_def: None,
        type_alias_def: None,
        namespace_def: None,
        interface_def: None,
        import_def: Some(ImportDef {
          src: "b".to_string(),
          imported: Some("foo".to_string()),
        }),
      }),
    }];

    // globals
    let render_ctx =
      RenderContext::new(&ctx, &doc_nodes, UrlResolveKind::Root, None);
    assert_eq!(render_ctx.lookup_symbol_href("bar").unwrap(), "global$bar");

    // imports
    let render_ctx =
      RenderContext::new(&ctx, &doc_nodes, UrlResolveKind::Root, None);
    assert_eq!(render_ctx.lookup_symbol_href("foo").unwrap(), "b/foo");

    let short_path = ShortPath::from("a".to_string());
    let render_ctx = RenderContext::new(
      &ctx,
      &doc_nodes,
      UrlResolveKind::File(&short_path),
      None,
    );
    assert_eq!(render_ctx.lookup_symbol_href("foo").unwrap(), "b/foo");
  }
}
