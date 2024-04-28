use crate::html::util::BreadcrumbCtx;
use crate::html::util::BreadcrumbsCtx;
use crate::html::util::NamespacedSymbols;
use crate::html::DocNodeWithContext;
use crate::html::GenerateCtx;
use crate::html::UrlResolveKind;
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
  /// A vector of parts of the current namespace, eg. `vec!["Deno", "errors"]`.
  namespace_parts: Rc<Vec<String>>,
}

impl<'ctx> RenderContext<'ctx> {
  pub fn new(
    ctx: &'ctx GenerateCtx<'ctx>,
    doc_nodes: &[DocNodeWithContext],
    current_resolve: UrlResolveKind<'ctx>,
  ) -> Self {
    Self {
      ctx,
      current_exports: NamespacedSymbols::new(doc_nodes),
      current_imports: Rc::new(get_current_imports(doc_nodes)),
      current_type_params: Default::default(),
      current_resolve,
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

  pub fn with_namespace(&self, namespace_parts: Rc<Vec<String>>) -> Self {
    Self {
      namespace_parts,
      ..self.clone()
    }
  }

  pub fn contains_type_param(&self, name: &str) -> bool {
    self.current_type_params.contains(name)
  }

  pub fn get_current_resolve(&self) -> UrlResolveKind {
    self.current_resolve
  }

  pub fn lookup_symbol_href(&self, target_symbol: &str) -> Option<String> {
    let target_symbol_parts = target_symbol
      .split('.')
      .map(String::from)
      .collect::<Vec<_>>();

    if !self.namespace_parts.is_empty() {
      let mut parts = (*self.namespace_parts).clone();
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
              .unwrap_or_else(|| {
                (**self
                  .ctx
                  .doc_nodes
                  .keys()
                  .find(|short_path| short_path.is_main)
                  .unwrap())
                .clone()
              }),
            symbol: target_symbol,
          },
        ),
      );
    }

    if let Some(src) = self.current_imports.get(target_symbol) {
      if let Ok(module_specifier) = ModuleSpecifier::parse(src) {
        if let Some(short_path) = self
          .ctx
          .doc_nodes
          .keys()
          .find(|short_path| short_path.specifier == module_specifier)
        {
          return Some(self.ctx.href_resolver.resolve_path(
            self.get_current_resolve(),
            UrlResolveKind::Symbol {
              file: short_path,
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
        if file.is_main {
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
              name: file.display_name(),
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

        if !file.is_main {
          parts.push(BreadcrumbCtx {
            name: file.display_name(),
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
  use crate::html::GenerateOptions;
  use crate::html::HrefResolver;
  use crate::node::DeclarationKind;
  use crate::node::ImportDef;
  use crate::DocNode;
  use crate::Location;

  struct TestResolver();

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

    fn resolve_usage(&self, current_resolve: UrlResolveKind) -> Option<String> {
      current_resolve
        .get_file()
        .map(|current_file| current_file.specifier.to_string())
    }

    fn resolve_source(&self, location: &Location) -> Option<String> {
      Some(location.filename.clone())
    }
  }

  #[test]
  fn lookup_symbol_href() {
    let doc_nodes_by_url = indexmap::IndexMap::from([(
      ModuleSpecifier::parse("file:///mod.ts").unwrap(),
      vec![DocNode {
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
      }],
    )]);

    let ctx = GenerateCtx::new(
      GenerateOptions {
        package_name: None,
        main_entrypoint: None,
        href_resolver: Rc::new(TestResolver()),
        usage_composer: None,
        rewrite_map: None,
        composable_output: false,
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

    let render_ctx =
      RenderContext::new(&ctx, doc_nodes, UrlResolveKind::File(short_path));
    assert_eq!(render_ctx.lookup_symbol_href("foo").unwrap(), "b/foo");
  }
}
