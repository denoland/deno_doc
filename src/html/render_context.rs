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
  pub ctx: &'ctx GenerateCtx,
  scoped_symbols: NamespacedSymbols,
  current_imports: Rc<HashMap<String, String>>,
  current_type_params: Rc<HashSet<&'ctx str>>,
  current_resolve: UrlResolveKind<'ctx>,
  /// A vector of parts of the current namespace, eg. `vec!["Deno", "errors"]`.
  namespace_parts: Rc<[String]>,
  /// Only some when in `FileMode::SingleDts` and using categories
  pub category: Option<&'ctx str>,
  pub toc: crate::html::comrak_adapters::HeadingToCAdapter,
}

impl<'ctx> RenderContext<'ctx> {
  pub fn new(
    ctx: &'ctx GenerateCtx,
    doc_nodes: &[DocNodeWithContext],
    current_resolve: UrlResolveKind<'ctx>,
  ) -> Self {
    Self {
      ctx,
      scoped_symbols: NamespacedSymbols::new(doc_nodes),
      current_imports: Rc::new(get_current_imports(doc_nodes)),
      current_type_params: Default::default(),
      current_resolve,
      namespace_parts: Rc::new([]),
      category: None,
      toc: Default::default(),
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
      // TODO: clean this up to not clone and to_vec
      let mut parts = self.namespace_parts.to_vec();
      while !parts.is_empty() {
        let mut current_parts = parts.clone();
        current_parts.extend_from_slice(&target_symbol_parts);

        if self.scoped_symbols.contains(&current_parts) {
          return Some(self.ctx.resolve_path(
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

    if self.scoped_symbols.contains(&target_symbol_parts) {
      return Some(
        self.ctx.resolve_path(
          self.get_current_resolve(),
          UrlResolveKind::Symbol {
            file: &self
              .get_current_resolve()
              .get_file()
              .cloned()
              .unwrap_or_else(|| {
                (**self.ctx.main_entrypoint.as_ref().unwrap()).clone()
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
          return Some(self.ctx.resolve_path(
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
      UrlResolveKind::Category(category) => {
        vec![
          BreadcrumbCtx {
            name: index_name,
            href: self
              .ctx
              .resolve_path(self.current_resolve, UrlResolveKind::Root),
            is_symbol: false,
            is_first_symbol: false,
          },
          BreadcrumbCtx {
            name: category.to_owned(),
            href: super::util::slugify(category),
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
                .resolve_path(self.current_resolve, UrlResolveKind::Root),
              is_symbol: false,
              is_first_symbol: false,
            },
            BreadcrumbCtx {
              name: file.display_name().to_string(),
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
            .resolve_path(self.current_resolve, UrlResolveKind::Root),
          is_symbol: false,
          is_first_symbol: false,
        }];

        if !file.is_main {
          parts.push(BreadcrumbCtx {
            name: file.display_name().to_string(),
            href: self
              .ctx
              .resolve_path(self.current_resolve, UrlResolveKind::File(file)),
            is_symbol: false,
            is_first_symbol: false,
          });
        } else if let Some(category) = self.category {
          parts.push(BreadcrumbCtx {
            name: category.to_string(),
            href: self.ctx.resolve_path(
              self.current_resolve,
              UrlResolveKind::Category(category),
            ),
            is_symbol: false,
            is_first_symbol: false,
          });
        }

        let (_, symbol_parts) =
          split_with_brackets(symbol).into_iter().enumerate().fold(
            (vec![], vec![]),
            |(mut symbol_parts, mut breadcrumbs), (i, symbol_part)| {
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
    if doc_node.kind() == DocNodeKind::Import {
      let import_def = doc_node.import_def().unwrap();
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
        href_resolver: Rc::new(TestResolver()),
        usage_composer: None,
        rewrite_map: None,
        composable_output: false,
        category_docs: None,
        disable_search: false,
        symbol_redirect_map: None,
        default_symbol_map: None,
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
