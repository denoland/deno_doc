use crate::html::util::BreadcrumbCtx;
use crate::html::util::BreadcrumbsCtx;
use crate::html::util::NamespacedSymbols;
use crate::html::GenerateCtx;
use crate::html::UrlResolveKind;
use deno_graph::ModuleSpecifier;
use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Clone)]
pub struct RenderContext<'ctx> {
  pub ctx: &'ctx GenerateCtx<'ctx>,
  current_exports: NamespacedSymbols,
  current_imports: HashMap<String, String>,
  current_type_params: HashSet<String>,
  current_resolve: UrlResolveKind<'ctx>,
  current_specifier: Option<&'ctx ModuleSpecifier>,
  /// A vector of parts of the current namespace, eg. `vec!["Deno", "errors"]`.
  namespace_parts: Vec<String>,
}

impl<'ctx> RenderContext<'ctx> {
  pub fn new(
    ctx: &'ctx GenerateCtx<'ctx>,
    doc_nodes: &[crate::DocNode],
    current_resolve: UrlResolveKind<'ctx>,
    current_specifier: Option<&'ctx ModuleSpecifier>,
  ) -> Self {
    Self {
      ctx,
      current_exports: NamespacedSymbols::new(doc_nodes),
      current_imports: crate::html::util::get_current_imports(doc_nodes),
      current_type_params: Default::default(),
      current_resolve,
      current_specifier,
      namespace_parts: vec![],
    }
  }

  pub fn with_current_type_params(
    &self,
    current_type_params: HashSet<String>,
  ) -> Self {
    Self {
      current_type_params,
      ..self.clone()
    }
  }

  pub fn with_namespace(&self, namespace_parts: Vec<String>) -> Self {
    Self {
      namespace_parts,
      ..self.clone()
    }
  }

  pub fn contains_type_param(&self, name: &str) -> bool {
    self.current_type_params.contains(name)
  }

  pub fn get_namespace_parts(&self) -> Vec<String> {
    self.namespace_parts.clone()
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
      let mut parts = self.namespace_parts.clone();
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
            file: self
              .get_current_resolve()
              .get_file()
              .expect("is in file because has exports"),
            symbol: &target_symbol_parts.join("."),
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
              symbol: &target_symbol_parts.join("."),
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
    let parts = match self.current_resolve {
      UrlResolveKind::Root => vec![BreadcrumbCtx {
        name: "index".to_string(),
        href: "".to_string(),
        is_symbol: false,
        is_first_symbol: false,
        is_all_symbols_part: false,
      }],
      UrlResolveKind::AllSymbols => {
        vec![
          BreadcrumbCtx {
            name: "index".to_string(),
            href: self
              .ctx
              .href_resolver
              .resolve_path(self.current_resolve, UrlResolveKind::Root),
            is_symbol: false,
            is_first_symbol: false,
            is_all_symbols_part: false,
          },
          BreadcrumbCtx {
            name: "all symbols".to_string(),
            href: "".to_string(),
            is_symbol: false,
            is_first_symbol: false,
            is_all_symbols_part: true,
          },
        ]
      }
      UrlResolveKind::File(file) => {
        if self.current_specifier == self.ctx.main_entrypoint.as_ref() {
          vec![BreadcrumbCtx {
            name: "index".to_string(),
            href: "".to_string(),
            is_symbol: false,
            is_first_symbol: false,
            is_all_symbols_part: false,
          }]
        } else {
          vec![
            BreadcrumbCtx {
              name: "index".to_string(),
              href: self
                .ctx
                .href_resolver
                .resolve_path(self.current_resolve, UrlResolveKind::Root),
              is_symbol: false,
              is_first_symbol: false,
              is_all_symbols_part: false,
            },
            BreadcrumbCtx {
              name: file.to_name(),
              href: "".to_string(),
              is_symbol: false,
              is_first_symbol: false,
              is_all_symbols_part: false,
            },
          ]
        }
      }
      UrlResolveKind::Symbol { file, symbol } => {
        let mut parts = vec![BreadcrumbCtx {
          name: "index".to_string(),
          href: self
            .ctx
            .href_resolver
            .resolve_path(self.current_resolve, UrlResolveKind::Root),
          is_symbol: false,
          is_first_symbol: false,
          is_all_symbols_part: false,
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
            is_all_symbols_part: false,
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
              is_all_symbols_part: false,
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
