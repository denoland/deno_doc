// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use deno_ast::ModuleItemRef;
use deno_ast::SourceRange;
use deno_ast::SourceRanged;
use deno_ast::SourceRangedForSpanned;
use deno_ast::swc::ast::ClassDecl;
use deno_ast::swc::ast::Decl;
use deno_ast::swc::ast::DefaultDecl;
use deno_ast::swc::ast::ExportDecl;
use deno_ast::swc::ast::ExportDefaultDecl;
use deno_ast::swc::ast::ExportDefaultExpr;
use deno_ast::swc::ast::FnDecl;
use deno_ast::swc::ast::Ident;
use deno_ast::swc::ast::ModuleDecl;
use deno_ast::swc::ast::TsEnumDecl;
use deno_ast::swc::ast::TsInterfaceDecl;
use deno_ast::swc::ast::TsModuleDecl;
use deno_ast::swc::ast::TsModuleName;
use deno_ast::swc::ast::TsTypeAliasDecl;
use deno_ast::swc::ast::VarDecl;
use deno_ast::swc::ast::VarDeclKind;
use deno_ast::swc::ast::VarDeclarator;
use deno_graph::Module;
use deno_graph::ModuleGraph;
use deno_graph::ModuleSpecifier;
use deno_graph::ast::EsParser;
use deno_graph::symbols::EsModuleInfo;
use deno_graph::symbols::ExpandoPropertyRef;
use deno_graph::symbols::ExportDeclRef;
use deno_graph::symbols::ModuleInfoRef;
use deno_graph::symbols::Symbol as GraphSymbol;
use deno_graph::symbols::SymbolDecl;
use deno_graph::symbols::SymbolNodeRef;
use deno_graph::symbols::UniqueSymbolId;
use indexmap::IndexMap;
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashSet;
use std::error::Error;
use std::fmt;
use std::rc::Rc;
use std::sync::Arc;

use crate::Location;
use crate::diagnostics::DiagnosticsCollector;
use crate::diagnostics::DocDiagnostic;
use crate::js_doc::JsDoc;
use crate::node::DeclarationDef;
use crate::node::DeclarationKind;
use crate::node::Document;
use crate::node::Import;
use crate::node::NamespaceDef;
use crate::node::ReferenceDef;
use crate::ts_type::PropertyDef;
use crate::ts_type::TsTypeDef;
use crate::ts_type::TsTypeDefKind;
use crate::ts_type::TsTypeLiteralDef;
use crate::ts_type::infer_simple_ts_type_from_init;
use crate::util::graph::resolve_deno_graph_module;
use crate::util::swc::get_location;
use crate::util::swc::get_text_info_location;
use crate::util::swc::js_doc_for_range;
use crate::util::swc::module_export_name_value;
use crate::util::swc::module_js_doc_for_source;
use crate::util::symbol::get_module_info;
use crate::variable::VariableDef;
use crate::visibility::SymbolVisibility;
use crate::{Declaration, Symbol};

#[derive(Debug)]
pub enum DocError {
  Resolve(String),
  #[allow(dead_code)]
  Io(std::io::Error),
  Parse(deno_ast::ParseDiagnostic),
}

impl Error for DocError {}

impl fmt::Display for DocError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let m = match self {
      Self::Resolve(s) => s.to_string(),
      Self::Io(err) => err.to_string(),
      Self::Parse(err) => err.to_string(),
    };
    f.pad(&m)
  }
}

impl From<deno_ast::ParseDiagnostic> for DocError {
  fn from(error: deno_ast::ParseDiagnostic) -> DocError {
    DocError::Parse(error)
  }
}

pub type ParseOutput = IndexMap<ModuleSpecifier, Document>;

#[derive(Default, Clone)]
pub struct DocParserOptions {
  /// Whether diagnostics should be collected.
  pub diagnostics: bool,
  /// Included private nodes in the output.
  ///
  /// Note: Private nodes that are referenced by public nodes
  /// are always included.
  pub private: bool,
}

pub struct DocParser<'a> {
  graph: &'a ModuleGraph,
  private: bool,
  root_symbol: Rc<deno_graph::symbols::RootSymbol<'a>>,
  visibility: SymbolVisibility,
  specifiers: &'a [ModuleSpecifier],
  diagnostics: Option<RefCell<DiagnosticsCollector<'a>>>,
}

impl<'a> DocParser<'a> {
  pub fn new(
    graph: &'a ModuleGraph,
    parser: &'a dyn EsParser,
    specifiers: &'a [ModuleSpecifier],
    options: DocParserOptions,
  ) -> Result<Self, anyhow::Error> {
    let root_symbol =
      Rc::new(deno_graph::symbols::RootSymbol::new(graph, parser));
    let visibility = SymbolVisibility::build(graph, &root_symbol)?;

    let diagnostics = if options.diagnostics {
      Some(RefCell::new(DiagnosticsCollector::new(root_symbol.clone())))
    } else {
      None
    };
    Ok(DocParser {
      graph,
      private: options.private,
      root_symbol,
      visibility,
      specifiers,
      diagnostics,
    })
  }

  /// Gets diagnostics found during any of the previous parses.
  pub fn take_diagnostics(&self) -> Vec<DocDiagnostic> {
    if let Some(diagnostics) = &self.diagnostics {
      diagnostics.borrow_mut().take_diagnostics()
    } else {
      debug_assert!(
        self.diagnostics.is_some(),
        "diagnostics were not enabled, but they were taken"
      );
      Vec::new()
    }
  }

  fn get_module_info(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Result<ModuleInfoRef<'_>, DocError> {
    get_module_info(&self.root_symbol, specifier)
  }

  pub fn parse(&self) -> Result<ParseOutput, DocError> {
    let mut symbols_by_url = self
      .specifiers
      .iter()
      .map(|specifier| {
        Ok((
          specifier.clone(),
          self.parse_with_reexports_inner(specifier, HashSet::new())?,
        ))
      })
      .collect::<Result<IndexMap<_, _>, DocError>>()?;

    let mut all_locations = symbols_by_url
      .values()
      .flat_map(|doc| doc.symbols.iter())
      .flat_map(|symbol| {
        fn walk_arc_symbols(symbol: &[Arc<Symbol>]) -> Vec<Location> {
          symbol
            .iter()
            .flat_map(|symbol| {
              symbol.declarations.iter().flat_map(|decl| {
                if let Some(namespace) = decl.namespace_def() {
                  walk_arc_symbols(&namespace.elements)
                } else {
                  vec![decl.location.clone()]
                }
              })
            })
            .collect()
        }

        symbol
          .declarations
          .iter()
          .flat_map(|decl| {
            if let Some(namespace) = decl.namespace_def() {
              walk_arc_symbols(&namespace.elements)
            } else {
              vec![decl.location.clone()]
            }
          })
          .collect::<Vec<_>>()
      })
      .collect::<HashSet<_>>();

    for (specifier, document) in symbols_by_url.iter_mut() {
      self.resolve_references_for_nodes(
        specifier,
        &mut document.symbols,
        &[],
        &mut all_locations,
      )?;
    }

    for (_, document) in &symbols_by_url {
      // TODO: jsdoc
      self.collect_diagnostics_for_symbols(&document.symbols);
    }

    Ok(symbols_by_url)
  }

  fn resolve_references_for_nodes(
    &self,
    specifier: &ModuleSpecifier,
    symbols: &mut Vec<Symbol>,
    name_path: &[String],
    all_locations: &mut HashSet<Location>,
  ) -> Result<(), DocError> {
    let mut i = 0;
    'outer: while i < symbols.len() {
      let name = symbols[i].name.to_string();

      for decl in &mut symbols[i].declarations {
        match &mut decl.def {
          DeclarationDef::Namespace(namespace_def) => {
            let mut namespace_elements = namespace_def
              .elements
              .iter()
              .map(|node| node.as_ref().clone())
              .collect::<Vec<_>>();

            let mut new_name_path = Vec::with_capacity(name_path.len() + 1);
            new_name_path.extend_from_slice(name_path);
            new_name_path.push(name.clone());

            self.resolve_references_for_nodes(
              specifier,
              &mut namespace_elements,
              &new_name_path,
              all_locations,
            )?;

            namespace_def.elements =
              namespace_elements.into_iter().map(Arc::new).collect();
          }
          DeclarationDef::Reference(reference_def) => {
            let mut new_name_path = Vec::with_capacity(name_path.len() + 1);
            new_name_path.extend_from_slice(name_path);
            new_name_path.push(name.clone());

            if !all_locations.contains(&reference_def.target)
              && let Some(new_symbol) = self.resolve_dangling_reference(
                specifier,
                reference_def,
                new_name_path,
                false,
              )?
            {
              if let Some(new_symbol) = new_symbol {
                symbols[i] = new_symbol;
                all_locations.extend(symbols.iter().flat_map(|symbol| {
                  symbol.declarations.iter().map(|decl| decl.location.clone())
                }));
              } else {
                symbols.remove(i);
              }
              continue 'outer;
            }
          }

          _ => {}
        }
      }

      i += 1;
    }
    Ok(())
  }

  fn resolve_dangling_reference(
    &self,
    specifier: &ModuleSpecifier,
    reference_def: &ReferenceDef,
    mut name_path: Vec<String>,
    star: bool,
  ) -> Result<Option<Option<Symbol>>, DocError> {
    let module = resolve_deno_graph_module(self.graph, specifier)?;

    match module {
      Module::Js(_) | Module::Json(_) | Module::Wasm(_) => {
        let module_info = self.get_module_info(module.specifier())?;
        let exports = module_info.exports(&self.root_symbol);
        if name_path.is_empty() {
          return Ok(Some(None));
        }
        let root_name = name_path.remove(0);
        let Some((mut export_name, export)) = exports
          .resolved
          .iter()
          .find(|(name, _)| &&root_name == name)
        else {
          return Ok(Some(None));
        };

        let export = export.as_resolved_export();
        let mut export_symbol = export.module.symbol(export.symbol_id).unwrap();

        let mut definitions = self
          .root_symbol
          .go_to_definitions(export.module, export_symbol)
          .collect::<Vec<_>>();

        if let Some(first_def) = definitions.first() {
          use deno_graph::symbols::DefinitionKind;
          match first_def.kind {
            DefinitionKind::ExportStar(file_dep) => {
              debug_assert_eq!(definitions.len(), 1);
              let specifier = self.resolve_dependency(
                &file_dep.specifier,
                first_def.module.specifier(),
              )?;
              return self.resolve_dangling_reference(
                specifier,
                reference_def,
                name_path,
                true,
              );
            }
            DefinitionKind::Definition => {
              for (i, name_path_item) in name_path.iter().enumerate() {
                let Some((name, id)) = export_symbol
                  .exports()
                  .iter()
                  .find(|(name, _)| &name_path_item == name)
                else {
                  let definitions = self
                    .root_symbol
                    .go_to_definitions(export.module, export_symbol)
                    .collect::<Vec<_>>();

                  return self.resolve_dangling_reference(
                    definitions.first().unwrap().module.specifier(),
                    reference_def,
                    // -1 to include the root
                    if i > 1 {
                      name_path[i - 1..].to_vec()
                    } else {
                      let mut out = vec![root_name];
                      out.extend_from_slice(&name_path);
                      out
                    },
                    false,
                  );
                };

                export_name = name;
                export_symbol = export.module.symbol(*id).unwrap();
              }

              definitions = self
                .root_symbol
                .go_to_definitions(export.module, export_symbol)
                .collect::<Vec<_>>();
            }
          }
        }

        let original_range = &export_symbol.decls().first().unwrap().range;

        if let Some(first_def) = definitions.first() {
          use deno_graph::symbols::DefinitionKind;
          match first_def.kind {
            DefinitionKind::ExportStar(_) => {}
            DefinitionKind::Definition => {
              if (star
                || first_def.module.specifier() != module_info.specifier())
                && definitions
                  .iter()
                  .any(|def| definition_location(def) == reference_def.target)
              {
                let mut declarations = vec![];
                for definition in &definitions {
                  let decl = definition.symbol_decl;
                  let mut decls = self.decls_for_maybe_node(
                    export_name,
                    definition.module,
                    definition.symbol,
                    decl.maybe_node(),
                    module_info.esm(),
                    first_def.module.specifier(),
                    Some(decl),
                    Some(original_range),
                  );
                  for d in &mut decls {
                    d.declaration_kind = DeclarationKind::Export;
                  }
                  declarations.extend(decls);
                }
                if !declarations.is_empty() {
                  return Ok(Some(Some(Symbol {
                    name: export_name.as_str().into(),
                    is_default: export_name == "default",
                    declarations,
                  })));
                }
              }
            }
          }
        }

        debug_assert!(false, "should not reach here");

        Ok(Some(None))
      }
      Module::Npm(_) | Module::Node(_) | Module::External(_) => Ok(None),
    }
  }

  fn collect_diagnostics_for_symbols(&self, nodes: &[Symbol]) {
    if let Some(diagnostics) = &self.diagnostics {
      let mut diagnostics = diagnostics.borrow_mut();
      diagnostics.analyze_doc_nodes(nodes);
    }
  }

  fn parse_with_reexports_inner(
    &self,
    specifier: &ModuleSpecifier,
    mut visited: HashSet<ModuleSpecifier>,
  ) -> Result<Document, DocError> {
    if !visited.insert(specifier.clone()) {
      return Ok(Default::default()); // circular
    }
    let module = resolve_deno_graph_module(self.graph, specifier)?;

    match module {
      Module::Js(_) | Module::Json(_) | Module::Wasm(_) => {
        let module_info = self.get_module_info(module.specifier())?;
        let mut document = self
          .get_symbols_for_module_info(module_info)?
          .unwrap_or_default();
        let exports = module_info.exports(&self.root_symbol);
        for (export_name, export) in exports.resolved {
          let export = export.as_resolved_export();
          let export_symbol = export.module.symbol(export.symbol_id).unwrap();
          let definitions = self
            .root_symbol
            .go_to_definitions(export.module, export_symbol)
            .collect::<Vec<_>>();

          if let Some(first_def) = definitions.first() {
            use deno_graph::symbols::DefinitionKind;
            match first_def.kind {
              DefinitionKind::ExportStar(file_dep) => {
                debug_assert_eq!(definitions.len(), 1);
                let def_location = definition_location(first_def);
                let specifier = self.resolve_dependency(
                  &file_dep.specifier,
                  first_def.module.specifier(),
                )?;
                let doc_nodes = self
                  .parse_with_reexports_inner(specifier, visited.clone())?;
                // Use the JSDoc from the export statement itself if present,
                // otherwise hoist module doc from the re-exported module.
                let js_doc = (|| {
                  if let ModuleInfoRef::Esm(esm) = export.module {
                    // Find the full export statement range, since the
                    // definition range may start at `*` rather than `export`.
                    for item in esm.source().program_ref().body() {
                      if let ModuleItemRef::ModuleDecl(decl) = item {
                        let decl_range = decl.range();
                        if decl_range.contains(first_def.range())
                          && let Some(js_doc) =
                            js_doc_for_range(esm, &decl_range)
                        {
                          if !js_doc.is_empty() {
                            return js_doc;
                          }
                          break;
                        }
                      }
                    }
                  }

                  // hoist any module doc to be the exported namespaces module doc
                  doc_nodes.module_doc.clone()
                })();
                let ns_def = NamespaceDef {
                  elements: doc_nodes
                    .symbols
                    .into_iter()
                    .map(|mut node| {
                      let decl = &mut node.declarations[0];
                      let target = decl.location.clone();
                      decl.def =
                        DeclarationDef::Reference(ReferenceDef { target });
                      decl.location = def_location.clone();

                      Arc::new(node)
                    })
                    .collect(),
                };
                let ns_symbol = Symbol::namespace(
                  export_name.into_boxed_str(),
                  false,
                  def_location,
                  DeclarationKind::Export,
                  js_doc,
                  ns_def,
                );
                document.symbols.push(ns_symbol);
              }
              DefinitionKind::Definition => {}
            }
          }
        }

        Ok(document)
      }
      Module::Npm(_) | Module::Node(_) | Module::External(_) => {
        Ok(Default::default())
      }
    }
  }

  fn get_imports_for_module_info(
    &self,
    module_info: &EsModuleInfo,
  ) -> Result<Vec<Import>, DocError> {
    let parsed_source = module_info.source();
    let referrer = module_info.specifier();
    let mut imports = vec![];

    for node in parsed_source.program_ref().body() {
      if let ModuleItemRef::ModuleDecl(ModuleDecl::Import(import_decl)) = node
        && let Some(js_doc) =
          js_doc_for_range(module_info, &import_decl.range())
      {
        for specifier in &import_decl.specifiers {
          use deno_ast::swc::ast::ImportSpecifier::*;

          let (imported_name, original_name, src) = match specifier {
            Named(named_specifier) => (
              named_specifier.local.sym.to_string(),
              named_specifier
                .imported
                .as_ref()
                .map(module_export_name_value)
                .or_else(|| Some(named_specifier.local.sym.to_string())),
              &import_decl.src.value,
            ),
            Default(default_specifier) => (
              default_specifier.local.sym.to_string(),
              Some("default".to_string()),
              &import_decl.src.value,
            ),
            Namespace(namespace_specifier) => (
              namespace_specifier.local.sym.to_string(),
              None,
              &import_decl.src.value,
            ),
          };

          let resolved_specifier =
            self.resolve_dependency(&src.to_string_lossy(), referrer)?;

          imports.push(Import {
            imported_name: imported_name.into_boxed_str(),
            js_doc: js_doc.clone(),
            src: resolved_specifier.to_string(),
            original_name,
          });
        }
      }
    }

    Ok(imports)
  }

  fn get_doc_for_expando_property(
    &self,
    module_info: &EsModuleInfo,
    expando_property: ExpandoPropertyRef,
  ) -> Option<Declaration> {
    let location =
      get_location(module_info, expando_property.prop_name_range().start());
    let js_doc =
      js_doc_for_range(module_info, &expando_property.inner().range())?;
    let init = expando_property.assignment();

    Some(decl_from_expr(module_info, init, location, js_doc))
  }

  fn get_doc_for_var_declarator_ident(
    &self,
    module_info: &EsModuleInfo,
    var_decl: &VarDecl,
    var_declarator: &VarDeclarator,
    ident: &Ident,
    full_range: &SourceRange,
  ) -> Option<Declaration> {
    let full_range = if ident.start() != var_declarator.start() {
      Cow::Owned(ident.range())
    } else {
      Cow::Borrowed(full_range)
    };
    let js_doc = js_doc_for_range(module_info, &full_range)?;

    if let Some(init) = &var_declarator.init
      && matches!(
        &**init,
        deno_ast::swc::ast::Expr::Class(_)
          | deno_ast::swc::ast::Expr::Fn(_)
          | deno_ast::swc::ast::Expr::Arrow(_)
      )
    {
      let location = get_location(module_info, ident.start());
      return Some(decl_from_expr(module_info, init, location, js_doc));
    }

    // todo(dsherret): it's not ideal to call this function over
    // and over for the same var declarator when there are a lot
    // of idents
    super::variable::get_docs_for_var_declarator(
      module_info,
      var_decl,
      var_declarator,
    )
    .into_iter()
    .find(|(name, _)| name.as_str() == &*ident.sym)
    .map(|(_name, var_def)| {
      let location = get_location(module_info, ident.start());
      Declaration::variable(location, DeclarationKind::Declare, js_doc, var_def)
    })
  }

  fn get_doc_for_class_decl(
    &self,
    module_info: &EsModuleInfo,
    class_decl: &ClassDecl,
    full_range: &SourceRange,
  ) -> Option<Declaration> {
    let jsdoc_range = match class_decl.class.decorators.first() {
      Some(decorator) if decorator.start() < full_range.start => {
        Cow::Owned(SourceRange::new(decorator.start(), full_range.end))
      }
      _ => Cow::Borrowed(full_range),
    };
    let js_doc = js_doc_for_range(module_info, &jsdoc_range)?;
    // declared classes cannot have decorators, so we ignore that return
    let (class_def, _) =
      super::class::get_doc_for_class_decl(module_info, class_decl);
    let location = get_location(module_info, full_range.start);
    Some(Declaration::class(
      location,
      DeclarationKind::Declare,
      js_doc,
      class_def,
    ))
  }

  fn get_decl_for_fn_decl(
    &self,
    module_info: &EsModuleInfo,
    fn_decl: &FnDecl,
    full_range: &SourceRange,
  ) -> Option<Declaration> {
    let js_doc = js_doc_for_range(module_info, full_range)?;
    let function_def =
      super::function::get_doc_for_fn_decl(module_info, fn_decl);
    let location = get_location(module_info, full_range.start);
    Some(Declaration::function(
      location,
      DeclarationKind::Declare,
      js_doc,
      function_def,
    ))
  }

  fn get_decl_for_interface_decl(
    &self,
    module_info: &EsModuleInfo,
    ts_interface_decl: &TsInterfaceDecl,
    full_range: &SourceRange,
  ) -> Option<Declaration> {
    let js_doc = js_doc_for_range(module_info, full_range)?;
    let interface_def = super::interface::get_doc_for_ts_interface_decl(
      module_info,
      ts_interface_decl,
      None,
    );
    let location = get_location(module_info, full_range.start);
    Some(Declaration::interface(
      location,
      DeclarationKind::Declare,
      js_doc,
      interface_def,
    ))
  }

  fn get_decl_for_type_alias(
    &self,
    module_info: &EsModuleInfo,
    ts_type_alias: &TsTypeAliasDecl,
    full_range: &SourceRange,
  ) -> Option<Declaration> {
    let js_doc = js_doc_for_range(module_info, full_range)?;
    let type_alias_def = super::type_alias::get_doc_for_ts_type_alias_decl(
      module_info,
      ts_type_alias,
    );
    let location = get_location(module_info, full_range.start);
    Some(Declaration::type_alias(
      location,
      DeclarationKind::Declare,
      js_doc,
      type_alias_def,
    ))
  }

  fn get_decl_for_enum(
    &self,
    module_info: &EsModuleInfo,
    ts_enum: &TsEnumDecl,
    full_range: &SourceRange,
  ) -> Option<Declaration> {
    let js_doc = js_doc_for_range(module_info, full_range)?;
    let enum_def =
      super::r#enum::get_doc_for_ts_enum_decl(module_info, ts_enum);
    let location = get_location(module_info, full_range.start);
    Some(Declaration::r#enum(
      location,
      DeclarationKind::Declare,
      js_doc,
      enum_def,
    ))
  }

  fn get_decl_for_ts_namespace(
    &self,
    module_info: &EsModuleInfo,
    ns_symbol: &GraphSymbol,
    ts_module: &TsModuleDecl,
    full_range: &SourceRange,
  ) -> Option<Declaration> {
    fn symbol_in_ancestors(
      id: UniqueSymbolId,
      parent: &GraphSymbol,
      module_info: &EsModuleInfo,
    ) -> bool {
      let mut current_symbol = Some(parent);
      while let Some(symbol) = current_symbol.take() {
        if symbol.unique_id() == id {
          return true;
        }
        current_symbol = symbol.parent_id().and_then(|s| module_info.symbol(s));
      }
      false
    }

    let first_ns_decl = ns_symbol
      .decls()
      .iter()
      .filter_map(|d| {
        d.maybe_node().and_then(|n| match n {
          SymbolNodeRef::ExportDecl(
            ExportDecl {
              decl: Decl::TsModule(n),
              ..
            },
            _,
          ) => Some(&**n),
          SymbolNodeRef::TsNamespace(n) => Some(n),
          _ => None,
        })
      })
      .next()
      .unwrap();
    if first_ns_decl.range() != ts_module.range() {
      return None; // we already analyzed this module
    }

    let _namespace_name = match &ts_module.id {
      TsModuleName::Ident(ident) => ident.sym.to_string(),
      TsModuleName::Str(str_) => str_.value.to_string_lossy().into_owned(),
    };
    let mut elements = Vec::new();
    let mut handled_symbols = HashSet::new();

    for (export_name, export_symbol_id) in ns_symbol.exports() {
      handled_symbols.insert(UniqueSymbolId::new(
        module_info.module_id(),
        *export_symbol_id,
      ));
      let export_symbol = module_info.symbol(*export_symbol_id).unwrap();
      let definitions = self
        .root_symbol
        .go_to_definitions(ModuleInfoRef::Esm(module_info), export_symbol);
      let original_range = &export_symbol.decls().first().unwrap().range;

      let mut declarations = vec![];
      for definition in definitions {
        let definition_id = definition.symbol.unique_id();
        if symbol_in_ancestors(definition_id, ns_symbol, module_info) {
          continue;
        }

        handled_symbols.insert(definition_id);

        let mut decls = self.decls_for_maybe_node(
          export_name,
          definition.module,
          definition.symbol,
          definition.symbol_decl.maybe_node(),
          Some(module_info),
          module_info.specifier(),
          Some(definition.symbol_decl),
          Some(original_range),
        );
        for decl in &mut decls {
          decl.declaration_kind = DeclarationKind::Export;
        }
        declarations.extend(decls);
      }
      if !declarations.is_empty() {
        elements.push(Arc::new(Symbol {
          name: export_name.as_str().into(),
          is_default: export_name == "default",
          declarations,
        }));
      }
    }

    let is_ambient = elements.is_empty() && !module_has_import(module_info);
    for child_id in ns_symbol.child_ids() {
      let unique_id = UniqueSymbolId::new(module_info.module_id(), child_id);
      if !handled_symbols.insert(unique_id) {
        continue; // already handled
      }
      if is_ambient
        || self.private
        || self.visibility.has_non_exported_public(&unique_id)
      {
        let child_symbol = module_info.symbol(child_id).unwrap();
        if let Some(symbol) = self.get_private_symbol_for_ast_symbol(
          ModuleInfoRef::Esm(module_info),
          child_symbol,
        ) {
          elements.push(Arc::new(symbol));
        }
      }
    }

    let js_doc = js_doc_for_range(module_info, full_range)?;
    let location = get_location(module_info, full_range.start);
    Some(Declaration::namespace(
      location,
      DeclarationKind::Declare,
      js_doc,
      NamespaceDef { elements },
    ))
  }

  fn get_private_symbol_for_ast_symbol(
    &self,
    module_info: ModuleInfoRef,
    child_symbol: &GraphSymbol,
  ) -> Option<Symbol> {
    debug_assert!(
      self
        .visibility
        .get_root_exported_deps(&child_symbol.unique_id())
        .is_none()
    );
    let mut declarations = Vec::with_capacity(child_symbol.decls().len());
    let mut name: Option<Box<str>> = None;
    for decl in child_symbol.decls() {
      let mut maybe_decls = self.decls_for_maybe_node(
        "",
        module_info,
        child_symbol,
        decl.maybe_node(),
        module_info.esm(),
        module_info.specifier(),
        None,
        None,
      );
      let is_declared = decl
        .maybe_node()
        .map(|node| self.get_declare_for_symbol_node(node))
        .unwrap_or(false);
      let declaration_kind = if is_declared {
        DeclarationKind::Declare
      } else {
        DeclarationKind::Private
      };
      for doc in &mut maybe_decls {
        doc.declaration_kind = declaration_kind;
      }
      // TODO: unsure about this
      if name.is_none() {
        // Get the name from the child symbol
        name = child_symbol
          .decls()
          .first()
          .and_then(|d| d.maybe_node())
          .and_then(|n| {
            use deno_graph::symbols::SymbolNodeRef;
            match n {
              SymbolNodeRef::ClassDecl(n) => Some(n.ident.sym.to_string()),
              SymbolNodeRef::FnDecl(n) => Some(n.ident.sym.to_string()),
              SymbolNodeRef::TsEnum(n) => Some(n.id.sym.to_string()),
              SymbolNodeRef::TsInterface(n) => Some(n.id.sym.to_string()),
              SymbolNodeRef::TsTypeAlias(n) => Some(n.id.sym.to_string()),
              SymbolNodeRef::TsNamespace(n) => match &n.id {
                TsModuleName::Ident(ident) => Some(ident.sym.to_string()),
                TsModuleName::Str(s) => {
                  Some(s.value.to_string_lossy().into_owned())
                }
              },
              SymbolNodeRef::Var(_, _, ident) => Some(ident.sym.to_string()),
              SymbolNodeRef::ExportDefaultDecl(_) => {
                Some("default".to_string())
              }
              SymbolNodeRef::ExportDefaultExpr(_) => {
                Some("default".to_string())
              }
              _ => None,
            }
          })
          .map(|s| s.into_boxed_str());
      }
      declarations.extend(maybe_decls);
    }
    if declarations.is_empty() {
      return None;
    }
    let name = name.unwrap_or_else(|| "".into());
    let is_default = &*name == "default";
    Some(Symbol {
      name,
      is_default,
      declarations,
    })
  }

  fn get_doc_for_export_default_decl(
    &self,
    module_info: &EsModuleInfo,
    export_default_decl: &ExportDefaultDecl,
  ) -> Option<Declaration> {
    let js_doc = js_doc_for_range(module_info, &export_default_decl.range())?;
    let location = get_location(module_info, export_default_decl.start());
    let doc_node = match &export_default_decl.decl {
      DefaultDecl::Class(class_expr) => {
        let default_name = class_expr
          .ident
          .as_ref()
          .map(|ident| ident.sym.to_string().into_boxed_str());
        let (class_def, decorator_js_doc) = crate::class::class_to_class_def(
          module_info,
          &class_expr.class,
          default_name,
        );
        let js_doc = if js_doc.is_empty() {
          decorator_js_doc
        } else {
          js_doc
        };
        Declaration::class(location, DeclarationKind::Export, js_doc, class_def)
      }
      DefaultDecl::Fn(fn_expr) => {
        let default_name =
          fn_expr.ident.as_ref().map(|ident| ident.sym.to_string());
        let function_def = crate::function::function_to_function_def(
          module_info,
          &fn_expr.function,
          default_name,
        );
        Declaration::function(
          location,
          DeclarationKind::Export,
          js_doc,
          function_def,
        )
      }
      DefaultDecl::TsInterfaceDecl(interface_decl) => {
        let default_name = interface_decl.id.sym.to_string();
        let interface_def = crate::interface::get_doc_for_ts_interface_decl(
          module_info,
          interface_decl,
          Some(default_name),
        );
        Declaration::interface(
          location,
          DeclarationKind::Export,
          js_doc,
          interface_def,
        )
      }
    };

    Some(doc_node)
  }

  fn get_decl_for_export_default_expr(
    &self,
    module_info: &EsModuleInfo,
    export_expr: &ExportDefaultExpr,
  ) -> Option<Declaration> {
    let js_doc = js_doc_for_range(module_info, &export_expr.range())?;
    let location = get_location(module_info, export_expr.start());
    if let deno_ast::swc::ast::Expr::Arrow(arrow_expr) = &*export_expr.expr {
      let function_def =
        crate::function::arrow_to_function_def(module_info, arrow_expr);
      Some(Declaration::function(
        location,
        DeclarationKind::Export,
        js_doc,
        function_def,
      ))
    } else {
      Some(Declaration::variable(
        location,
        DeclarationKind::Export,
        js_doc,
        VariableDef {
          kind: VarDeclKind::Var,
          ts_type: super::ts_type::infer_ts_type_from_expr(
            module_info,
            export_expr.expr.as_ref(),
            true,
          ),
        },
      ))
    }
  }

  fn get_decl_for_reference(
    &self,
    reference_module_info: &EsModuleInfo,
    parsed_module_info: &EsModuleInfo,
    reference_range: &SourceRange,
    target_range: &SourceRange,
  ) -> Option<Declaration> {
    if let Some(js_doc) =
      js_doc_for_range(reference_module_info, &reference_range.range())
    {
      let location =
        get_location(reference_module_info, reference_range.start());
      let target = get_location(parsed_module_info, target_range.start());
      Some(Declaration::reference(
        location,
        js_doc,
        ReferenceDef { target },
      ))
    } else {
      None
    }
  }

  fn get_symbols_for_module_info(
    &self,
    module_info: ModuleInfoRef,
  ) -> Result<Option<Document>, DocError> {
    match module_info {
      ModuleInfoRef::Json(module) => Ok(parse_json_module_symbol(
        module.specifier(),
        module.text_info().text_str(),
      )),
      ModuleInfoRef::Esm(module_info) => {
        if let Some(mut document) =
          self.get_document_for_module_info_body(module_info)
        {
          document.imports = self.get_imports_for_module_info(module_info)?;

          Ok(Some(document))
        } else {
          Ok(None)
        }
      }
    }
  }

  fn get_document_for_module_info_body(
    &self,
    module_info: &EsModuleInfo,
  ) -> Option<Document> {
    let mut symbols = Vec::new();
    // check to see if there is a module level JSDoc for the source file
    let module_doc =
      if let Some(module_js_doc) = module_js_doc_for_source(module_info) {
        if let Some((js_doc, _range)) = module_js_doc {
          js_doc
        } else {
          return None;
        }
      } else {
        Default::default()
      };

    let mut handled_symbols = HashSet::new();
    let exports = module_info.exports(&self.root_symbol);
    for (export_name, export) in &exports.resolved {
      let export = export.as_resolved_export();
      handled_symbols.insert(UniqueSymbolId::new(
        export.module.module_id(),
        export.symbol_id,
      ));
      let export_symbol = export.module.symbol(export.symbol_id).unwrap();
      let definitions = self
        .root_symbol
        .go_to_definitions(export.module, export_symbol);
      let original_range = &export_symbol.decls().first().unwrap().range;

      let mut declarations = vec![];

      for definition in definitions {
        handled_symbols.insert(definition.symbol.unique_id());
        let maybe_docs = self.decls_for_maybe_node(
          export_name,
          definition.module,
          definition.symbol,
          definition.symbol_decl.maybe_node(),
          export.module.esm(),
          export.module.specifier(),
          Some(definition.symbol_decl),
          Some(original_range),
        );
        for mut decl in maybe_docs {
          decl.declaration_kind = DeclarationKind::Export;
          declarations.push(decl);
        }
      }

      if !declarations.is_empty() {
        symbols.push(Symbol {
          name: export_name.as_str().into(),
          is_default: export_name == "default",
          declarations,
        });
      }
    }

    let is_ambient =
      exports.resolved.is_empty() && !module_has_import(module_info);
    for child_id in module_info.module_symbol().child_ids() {
      let unique_id = UniqueSymbolId::new(module_info.module_id(), child_id);
      if !handled_symbols.insert(unique_id) {
        continue; // already handled
      }
      if is_ambient
        || self.private
        || self.visibility.has_non_exported_public(&unique_id)
      {
        let child_symbol = module_info.symbol(child_id).unwrap();
        if let Some(symbol) = self.get_private_symbol_for_ast_symbol(
          ModuleInfoRef::Esm(module_info),
          child_symbol,
        ) {
          symbols.push(symbol);
        }
      }
    }

    Some(Document {
      module_doc,
      imports: vec![],
      symbols,
    })
  }

  #[allow(clippy::too_many_arguments)]
  fn decls_for_maybe_node(
    &self,
    export_name: &str,
    module_info: ModuleInfoRef,
    symbol: &GraphSymbol,
    maybe_node: Option<SymbolNodeRef<'_>>,
    original_module_source: Option<&EsModuleInfo>,
    original_specifier: &ModuleSpecifier,
    decl: Option<&SymbolDecl>,
    original_range: Option<&SourceRange>,
  ) -> Vec<Declaration> {
    let mut decls = Vec::with_capacity(2);
    match module_info {
      ModuleInfoRef::Json(module_info) => {
        if let Some(document) = parse_json_module_symbol(
          module_info.specifier(),
          module_info.text_info().text_str(),
        ) {
          for symbol in document.symbols {
            decls.extend(symbol.declarations);
          }
        }
        return decls;
      }
      ModuleInfoRef::Esm(module_info) => {
        let maybe_doc = if let Some(node) = maybe_node {
          if module_info.specifier() == original_specifier {
            self.get_decl_for_symbol_node_ref(module_info, symbol, node)
          } else if let Some(decl) = decl {
            self.get_decl_for_reference(
              original_module_source.unwrap(),
              module_info,
              original_range.unwrap(),
              &decl.range,
            )
          } else {
            None
          }
        } else {
          None
        };
        if let Some(doc) = maybe_doc {
          decls.push(doc);

          self.check_private_type_in_public_diagnostic(
            ModuleInfoRef::Esm(module_info),
            symbol,
          );

          if let Some(node) = maybe_node
            && node.is_function()
          {
            // find any expando properties for this function symbol
            if let Some(expando_namespace) = self
              .maybe_expando_property_namespace_doc(
                export_name,
                &decls[0],
                ModuleInfoRef::Esm(module_info),
                symbol,
              )
            {
              decls.push(expando_namespace);
            }
          }
        }
      }
    }

    decls
  }

  fn maybe_expando_property_namespace_doc(
    &self,
    func_name: &str,
    _func_doc: &Declaration,
    module_info: ModuleInfoRef,
    symbol: &GraphSymbol,
  ) -> Option<Declaration> {
    let expando_properties = symbol.exports().iter().flat_map(|(name, id)| {
      let symbol = module_info.symbol(*id).unwrap();
      symbol
        .decls()
        .iter()
        .filter_map(move |n| match n.maybe_node() {
          Some(SymbolNodeRef::ExpandoProperty(n)) => Some((name, n)),
          _ => None,
        })
    });
    let elements = expando_properties
      .flat_map(|(name, n)| {
        self
          .decls_for_maybe_node(
            name,
            module_info,
            symbol,
            Some(SymbolNodeRef::ExpandoProperty(n)),
            module_info.esm(),
            module_info.specifier(),
            None,
            None,
          )
          .into_iter()
          .map(|mut decl| {
            decl.declaration_kind = DeclarationKind::Declare;

            Arc::new(Symbol {
              name: name.as_str().into(),
              is_default: false,
              declarations: vec![decl],
            })
          })
          .collect::<Vec<_>>()
      })
      .collect::<Vec<_>>();
    if elements.is_empty() {
      return None;
    }
    Some(Declaration::namespace(
      elements[0].declarations[0].location.clone(),
      DeclarationKind::Declare,
      // give this a JS doc to prevent a missing JS doc diagnostic
      JsDoc {
        doc: Some(
          format!("Additional properties on the `{}` function.", func_name)
            .into_boxed_str(),
        ),
        tags: Box::new([]),
      },
      NamespaceDef { elements },
    ))
  }

  fn check_private_type_in_public_diagnostic(
    &self,
    doc_module_info: ModuleInfoRef<'_>,
    doc_symbol: &GraphSymbol,
  ) {
    let Some(diagnostics) = &self.diagnostics else {
      return;
    };
    if doc_module_info.specifier().scheme() != "file" {
      return; // don't report diagnostics on remote modules
    }
    let doc_symbol_id = doc_symbol.unique_id();
    let Some(deps_by_member) =
      self.visibility.get_root_exported_deps(&doc_symbol_id)
    else {
      return;
    };
    if deps_by_member.is_empty() {
      return; // avoid borrow_mut if not necessary
    }

    let mut diagnostics = diagnostics.borrow_mut();
    for decl_with_deps in deps_by_member.iter() {
      if decl_with_deps.had_ignorable_tag {
        continue; // ignore
      }

      let decl_symbol = doc_module_info
        .symbol(decl_with_deps.symbol_id.symbol_id)
        .unwrap();
      let Some(decl_name) =
        doc_module_info.fully_qualified_symbol_name(decl_symbol)
      else {
        continue;
      };

      for dep in &decl_with_deps.deps {
        let dep_module =
          self.root_symbol.module_from_id(dep.module_id).unwrap();
        let dep_symbol = dep_module.symbol(dep.symbol_id).unwrap();
        diagnostics.add_private_type_in_public(
          doc_module_info,
          &decl_name,
          decl_with_deps.decl_range,
          doc_symbol_id,
          dep_module,
          dep_symbol,
        );
      }
    }
  }

  fn get_decl_for_symbol_node_ref(
    &self,
    module_info: &EsModuleInfo,
    symbol: &GraphSymbol,
    node: SymbolNodeRef<'_>,
  ) -> Option<Declaration> {
    match node {
      SymbolNodeRef::ClassDecl(n) => {
        self.get_doc_for_class_decl(module_info, n, &n.class.range())
      }
      SymbolNodeRef::ExpandoProperty(n) => {
        self.get_doc_for_expando_property(module_info, n)
      }
      SymbolNodeRef::ExportDefaultDecl(n) => {
        self.get_doc_for_export_default_decl(module_info, n)
      }
      SymbolNodeRef::ExportDefaultExpr(n) => {
        self.get_decl_for_export_default_expr(module_info, n)
      }
      SymbolNodeRef::FnDecl(n) => {
        self.get_decl_for_fn_decl(module_info, n, &n.function.range())
      }
      SymbolNodeRef::TsEnum(n) => {
        self.get_decl_for_enum(module_info, n, &n.range())
      }
      SymbolNodeRef::TsInterface(n) => {
        self.get_decl_for_interface_decl(module_info, n, &n.range())
      }
      SymbolNodeRef::TsNamespace(n) => {
        self.get_decl_for_ts_namespace(module_info, symbol, n, &n.range())
      }
      SymbolNodeRef::TsTypeAlias(n) => {
        self.get_decl_for_type_alias(module_info, n, &n.range())
      }
      SymbolNodeRef::Var(parent_decl, n, ident) => self
        .get_doc_for_var_declarator_ident(
          module_info,
          parent_decl,
          n,
          ident,
          &parent_decl.range(),
        ),
      SymbolNodeRef::UsingVar(_, _, _) => {
        // makes no sense for using declarations to be in the public API
        None
      }
      SymbolNodeRef::ExportDecl(export_decl, inner) => match inner {
        ExportDeclRef::Class(n) => {
          self.get_doc_for_class_decl(module_info, n, &export_decl.range())
        }
        ExportDeclRef::Fn(n) => {
          self.get_decl_for_fn_decl(module_info, n, &export_decl.range())
        }
        ExportDeclRef::TsEnum(n) => {
          self.get_decl_for_enum(module_info, n, &export_decl.range())
        }
        ExportDeclRef::TsModule(n) => self.get_decl_for_ts_namespace(
          module_info,
          symbol,
          n,
          &export_decl.range(),
        ),
        ExportDeclRef::TsInterface(n) => {
          self.get_decl_for_interface_decl(module_info, n, &export_decl.range())
        }
        ExportDeclRef::TsTypeAlias(n) => {
          self.get_decl_for_type_alias(module_info, n, &export_decl.range())
        }
        ExportDeclRef::Var(var_decl, var_declarator, ident) => self
          .get_doc_for_var_declarator_ident(
            module_info,
            var_decl,
            var_declarator,
            ident,
            &export_decl.range(),
          ),
      },
      SymbolNodeRef::Module(_)
      | SymbolNodeRef::AutoAccessor(_)
      | SymbolNodeRef::ClassMethod(_)
      | SymbolNodeRef::ClassParamProp(_)
      | SymbolNodeRef::ClassProp(_)
      | SymbolNodeRef::Constructor(_)
      | SymbolNodeRef::TsIndexSignature(_)
      | SymbolNodeRef::TsCallSignatureDecl(_)
      | SymbolNodeRef::TsConstructSignatureDecl(_)
      | SymbolNodeRef::TsPropertySignature(_)
      | SymbolNodeRef::TsGetterSignature(_)
      | SymbolNodeRef::TsSetterSignature(_)
      | SymbolNodeRef::TsMethodSignature(_) => {
        //debug_assert!(false, "should not reach here");
        None
      }
    }
  }

  fn get_declare_for_symbol_node(&self, node: SymbolNodeRef) -> bool {
    match node {
      SymbolNodeRef::ClassDecl(n) => n.declare,
      SymbolNodeRef::ExportDecl(n, _) => self.get_declare_for_ast_decl(&n.decl),
      SymbolNodeRef::ExportDefaultDecl(_) => false,
      SymbolNodeRef::ExportDefaultExpr(_) => false,
      SymbolNodeRef::FnDecl(n) => n.declare,
      SymbolNodeRef::TsEnum(n) => n.declare,
      SymbolNodeRef::TsInterface(n) => n.declare,
      SymbolNodeRef::TsNamespace(n) => n.declare,
      SymbolNodeRef::TsTypeAlias(n) => n.declare,
      SymbolNodeRef::Var(n, _, _) => n.declare,
      SymbolNodeRef::UsingVar(_, _, _) => false,
      SymbolNodeRef::Module(_)
      | SymbolNodeRef::AutoAccessor(_)
      | SymbolNodeRef::ClassMethod(_)
      | SymbolNodeRef::ClassProp(_)
      | SymbolNodeRef::ClassParamProp(_)
      | SymbolNodeRef::ExpandoProperty(_)
      | SymbolNodeRef::Constructor(_)
      | SymbolNodeRef::TsIndexSignature(_)
      | SymbolNodeRef::TsCallSignatureDecl(_)
      | SymbolNodeRef::TsConstructSignatureDecl(_)
      | SymbolNodeRef::TsPropertySignature(_)
      | SymbolNodeRef::TsGetterSignature(_)
      | SymbolNodeRef::TsSetterSignature(_)
      | SymbolNodeRef::TsMethodSignature(_) => {
        debug_assert!(false, "should not reach here");
        false
      }
    }
  }

  fn get_declare_for_ast_decl(&self, decl: &Decl) -> bool {
    match decl {
      Decl::Class(class_decl) => class_decl.declare,
      Decl::Fn(fn_decl) => fn_decl.declare,
      Decl::TsEnum(ts_enum_decl) => ts_enum_decl.declare,
      Decl::TsInterface(ts_interface_decl) => ts_interface_decl.declare,
      Decl::TsModule(ts_module_decl) => ts_module_decl.declare,
      Decl::TsTypeAlias(ts_type_alias_decl) => ts_type_alias_decl.declare,
      Decl::Var(var_decl) => var_decl.declare,
      Decl::Using(_) => false,
    }
  }

  fn resolve_dependency(
    &self,
    specifier: &str,
    referrer: &ModuleSpecifier,
  ) -> Result<&ModuleSpecifier, DocError> {
    self
      .graph
      .resolve_dependency(specifier, referrer, /* prefer_types */ true)
      .ok_or_else(|| {
        DocError::Resolve(format!(
          "Failed resolving '{}' from '{}'.",
          specifier, referrer
        ))
      })
  }
}

fn decl_from_expr(
  module_info: &EsModuleInfo,
  expr: &deno_ast::swc::ast::Expr,
  location: Location,
  js_doc: JsDoc,
) -> Declaration {
  match expr {
    deno_ast::swc::ast::Expr::Class(class_expr) => {
      let def_name = class_expr
        .ident
        .as_ref()
        .map(|id| id.sym.to_string().into_boxed_str());
      let (class_def, decorator_js_doc) = crate::class::class_to_class_def(
        module_info,
        &class_expr.class,
        def_name,
      );
      let js_doc = if js_doc.is_empty() {
        decorator_js_doc
      } else {
        js_doc
      };
      Declaration::class(location, DeclarationKind::Declare, js_doc, class_def)
    }
    deno_ast::swc::ast::Expr::Fn(fn_expr) => {
      let def_name = fn_expr.ident.as_ref().map(|id| id.sym.to_string());
      let function_def = crate::function::function_to_function_def(
        module_info,
        &fn_expr.function,
        def_name,
      );
      Declaration::function(
        location,
        DeclarationKind::Declare,
        js_doc,
        function_def,
      )
    }
    deno_ast::swc::ast::Expr::Arrow(arrow_expr) => {
      let function_def =
        crate::function::arrow_to_function_def(module_info, arrow_expr);
      Declaration::function(
        location,
        DeclarationKind::Declare,
        js_doc,
        function_def,
      )
    }
    _ => {
      let ts_type =
        infer_simple_ts_type_from_init(module_info, Some(expr), true);
      Declaration::variable(
        location,
        DeclarationKind::Declare,
        js_doc,
        VariableDef {
          ts_type,
          kind: VarDeclKind::Const,
        },
      )
    }
  }
}

fn parse_json_module_symbol(
  specifier: &ModuleSpecifier,
  source: &str,
) -> Option<Document> {
  if let Ok(value) = serde_json::from_str(source) {
    Some(Document {
      module_doc: Default::default(),
      imports: vec![],
      symbols: vec![Symbol::variable(
        "default".into(),
        true,
        Location {
          filename: specifier.to_string().into_boxed_str(),
          col: 0,
          line: 0,
          byte_index: 0,
        },
        DeclarationKind::Export,
        JsDoc::default(),
        VariableDef {
          kind: VarDeclKind::Var,
          ts_type: Some(parse_json_module_type(&value)),
        },
      )],
    })
  } else {
    // no doc nodes
    None
  }
}

fn parse_json_module_type(value: &serde_json::Value) -> TsTypeDef {
  match value {
    serde_json::Value::Null => TsTypeDef::keyword("null"),
    serde_json::Value::Bool(value) => TsTypeDef::bool_value(*value),
    serde_json::Value::String(value) => {
      TsTypeDef::string_value(value.to_string())
    }
    serde_json::Value::Number(value) => match value.as_f64() {
      Some(value) => TsTypeDef::number_value(value),
      None => TsTypeDef::keyword("number"),
    },
    serde_json::Value::Array(_) => TsTypeDef {
      repr: "unknown[]".to_string(),
      kind: TsTypeDefKind::Array(Box::new(TsTypeDef::keyword("unknown"))),
    },
    serde_json::Value::Object(obj) => TsTypeDef {
      repr: "".to_string(),
      kind: TsTypeDefKind::TypeLiteral(TsTypeLiteralDef {
        properties: obj
          .iter()
          .map(|(key, value)| PropertyDef {
            name: key.to_string(),
            js_doc: Default::default(),
            ts_type: Some(parse_json_module_type(value)),
            params: Vec::new(),
            readonly: false,
            computed: false,
            optional: false,
            type_params: Box::new([]),
            location: Default::default(),
          })
          .collect(),
        ..Default::default()
      }),
    },
  }
}

fn module_has_import(module_info: &EsModuleInfo) -> bool {
  module_info.source().program_ref().body().any(|m| {
    matches!(
      m,
      ModuleItemRef::ModuleDecl(
        ModuleDecl::Import(_) | ModuleDecl::TsImportEquals(_)
      )
    )
  })
}

fn definition_location(
  definition: &deno_graph::symbols::Definition,
) -> Location {
  get_text_info_location(
    definition.module.specifier().as_str(),
    definition.module.text_info(),
    definition.range().start,
  )
}
