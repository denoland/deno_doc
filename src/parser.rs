// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use deno_ast::oxc::ast::ast::BindingIdentifier;
use deno_ast::oxc::ast::ast::BindingIdentifier as Ident;
use deno_ast::oxc::ast::ast::BindingPattern;
use deno_ast::oxc::ast::ast::ClassElement;
use deno_ast::oxc::ast::ast::IdentifierReference;
use deno_ast::oxc::ast::ast::MethodDefinitionKind;
use deno_ast::oxc::ast::ast::ModuleExportName;
use deno_ast::oxc::ast::ast::PropertyKey;
use deno_ast::oxc::ast::ast::TSInterfaceDeclaration;
use deno_ast::oxc::ast::ast::TSSignature;
use deno_ast::oxc::ast::ast::TSTypeName;
use deno_ast::oxc::ast::ast::Class;
use deno_ast::oxc::ast::ast::Declaration as OxcDeclaration;
use deno_ast::oxc::ast::ast::ExportDefaultDeclaration;
use deno_ast::oxc::ast::ast::ExportDefaultDeclarationKind;
use deno_ast::oxc::ast::ast::ExportNamedDeclaration;
use deno_ast::oxc::ast::ast::Expression;
use deno_ast::oxc::ast::ast::Function as FnDecl;
use deno_ast::oxc::ast::ast::ImportDeclarationSpecifier;
use deno_ast::oxc::ast::ast::Statement;
use deno_ast::oxc::ast::ast::TSEnumDeclaration as TsEnumDecl;
use deno_ast::oxc::ast::ast::TSInterfaceDeclaration as TsInterfaceDecl;
use deno_ast::oxc::ast::ast::TSModuleDeclaration as TsModuleDecl;
use deno_ast::oxc::ast::ast::TSModuleDeclarationName;
use deno_ast::oxc::ast::ast::TSTypeAliasDeclaration as TsTypeAliasDecl;
use deno_ast::oxc::ast::ast::VariableDeclaration as VarDecl;
use deno_ast::oxc::ast::ast::VariableDeclarator as VarDeclarator;
use deno_ast::oxc::span::GetSpan;
use deno_ast::oxc::span::Span;
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
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::error::Error;
use std::fmt;
use std::rc::Rc;
use std::sync::Arc;

use crate::Location;
use crate::diagnostics::DiagnosticsCollector;
use crate::diagnostics::DocDiagnostic;
use crate::interface::expr_to_name;
use crate::interface::property_key_name;
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
use crate::ts_type::Id;
use crate::ts_type::binding_identifier_to_id;
use crate::ts_type::identifier_reference_to_id;
use crate::ts_type::infer_simple_ts_type_from_init;
use crate::util::graph::resolve_deno_graph_module;
use crate::util::swc::get_location;
use crate::util::swc::get_text_info_location;
use crate::util::swc::js_doc_for_range;
use crate::util::swc::module_export_name_value;
use crate::util::swc::module_js_doc_for_source;
use crate::util::symbol::get_module_info;
use crate::util::types::MethodKind;
use crate::util::types::VarDeclKind;
use crate::variable::VariableDef;
use crate::visibility::SymbolVisibility;
use crate::{Declaration, Symbol};

type SourceRange = Span;

trait SpanCompat {
  fn contains(self, other: Span) -> bool;
}

impl SpanCompat for Span {
  fn contains(self, other: Span) -> bool {
    self.contains_inclusive(other)
  }
}

trait GetSpanCompat {
  fn range(&self) -> Span;
  fn start(&self) -> u32;
}

impl<T: GetSpan> GetSpanCompat for T {
  fn range(&self) -> Span {
    self.span()
  }

  fn start(&self) -> u32 {
    self.span().start
  }
}

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
    allocator: &'a deno_ast::oxc::allocator::Allocator,
    specifiers: &'a [ModuleSpecifier],
    options: DocParserOptions,
  ) -> Result<Self, anyhow::Error> {
    let root_symbol = Rc::new(deno_graph::symbols::RootSymbol::new(
      graph, parser, allocator,
    ));
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
    symbols: &mut Vec<Arc<Symbol>>,
    name_path: &[String],
    all_locations: &mut HashSet<Location>,
  ) -> Result<(), DocError> {
    let mut attempted_targets = HashSet::new();
    let mut i = 0;
    'outer: while i < symbols.len() {
      let name = symbols[i].name.to_string();

      for decl in &mut Arc::make_mut(&mut symbols[i]).declarations {
        match &mut decl.def {
          DeclarationDef::Namespace(namespace_def) => {
            let mut new_name_path = Vec::with_capacity(name_path.len() + 1);
            new_name_path.extend_from_slice(name_path);
            new_name_path.push(name.clone());

            self.resolve_references_for_nodes(
              specifier,
              &mut namespace_def.elements,
              &new_name_path,
              all_locations,
            )?;
          }
          DeclarationDef::Reference(reference_def) => {
            let mut new_name_path = Vec::with_capacity(name_path.len() + 1);
            new_name_path.extend_from_slice(name_path);
            new_name_path.push(name.clone());

            if !all_locations.contains(&reference_def.target)
              && attempted_targets.insert(reference_def.target.clone())
              && let Some(new_symbol) = self.resolve_dangling_reference(
                specifier,
                reference_def,
                new_name_path,
                false,
                &mut HashSet::new(),
              )?
            {
              if let Some(new_symbol) = new_symbol {
                symbols[i] = Arc::new(new_symbol);
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
    visited: &mut HashSet<(ModuleSpecifier, Vec<String>, bool)>,
  ) -> Result<Option<Option<Symbol>>, DocError> {
    if !visited.insert((specifier.clone(), name_path.clone(), star)) {
      return Ok(None);
    }

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
                visited,
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
                    visited,
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

  fn collect_diagnostics_for_symbols(&self, nodes: &[Arc<Symbol>]) {
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
                    for item in esm.statements() {
                      let decl_range = item.span();
                      if decl_range.contains(*first_def.range())
                        && let Some(js_doc) = js_doc_for_range(esm, decl_range)
                      {
                        if !js_doc.is_empty() {
                          return js_doc;
                        }
                        break;
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
                      let sym = Arc::make_mut(&mut node);
                      let decl = &mut sym.declarations[0];
                      // A nested `export * as` produces a synthetic namespace
                      // with no concrete symbol to point at. Turning it into a
                      // reference makes it dangle (there is nothing to resolve
                      // to) and it gets dropped, so keep it as a nested
                      // namespace; its own elements are references to concrete
                      // symbols and resolve normally.
                      if !matches!(decl.def, DeclarationDef::Namespace(_)) {
                        let target = decl.location.clone();
                        decl.def =
                          DeclarationDef::Reference(ReferenceDef { target });
                        decl.location = def_location.clone();
                      }

                      node
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
                document.symbols.push(Arc::new(ns_symbol));
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

  /// Resolves a heritage clause type name (`implements Foo`,
  /// `implements ns.Foo`) to the interface declarations it refers to,
  /// following imports and re-exports.
  fn resolve_heritage_interfaces<'b>(
    &'b self,
    module_info: &'b EsModuleInfo,
    type_name: &TSTypeName,
  ) -> Vec<(ModuleInfoRef<'b>, &'b TSInterfaceDeclaration<'b>)> {
    // splits `a.b.c` into the root identifier and the member path
    fn split_type_name<'e>(
      type_name: &'e TSTypeName<'e>,
      parts: &mut Vec<String>,
    ) -> Option<&'e IdentifierReference<'e>> {
      match type_name {
        TSTypeName::IdentifierReference(ident) => Some(ident),
        TSTypeName::QualifiedName(qualified_name) => {
          let root = split_type_name(&qualified_name.left, parts)?;
          parts.push(qualified_name.right.name.to_string());
          Some(root)
        }
        TSTypeName::ThisExpression(_) => None,
      }
    }

    let mut parts = Vec::new();
    let Some(root_ident) = split_type_name(type_name, &mut parts) else {
      return Vec::new();
    };
    let root_id = identifier_reference_to_id(module_info, root_ident);
    let Some(symbol) = module_info.symbol_from_swc(&root_id) else {
      return Vec::new();
    };

    let definitions: Vec<_> = if parts.is_empty() {
      self
        .root_symbol
        .go_to_definitions(ModuleInfoRef::Esm(module_info), symbol)
        .collect()
    } else {
      self
        .root_symbol
        .resolve_symbol_dep(
          ModuleInfoRef::Esm(module_info),
          &deno_graph::symbols::SymbolNodeDep::QualifiedId(root_id, parts),
        )
        .into_iter()
        .filter_map(|entry| match entry {
          deno_graph::symbols::ResolvedSymbolDepEntry::Path(path) => {
            Some(path.into_definitions())
          }
          deno_graph::symbols::ResolvedSymbolDepEntry::ImportType(_) => None,
        })
        .flatten()
        .collect()
    };

    definitions
      .into_iter()
      .filter_map(|definition| {
        let interface_decl = match definition.symbol_decl.maybe_node()? {
          SymbolNodeRef::TsInterface(n) => n,
          SymbolNodeRef::ExportDecl(_, ExportDeclRef::TsInterface(n)) => n,
          SymbolNodeRef::ExportDefaultDecl(n) => match &n.declaration {
            ExportDefaultDeclarationKind::TSInterfaceDeclaration(n) => &**n,
            _ => return None,
          },
          _ => return None,
        };
        Some((definition.module, interface_decl))
      })
      .collect()
  }

  /// Fills in missing JSDocs of instance members of a class from the members
  /// of the interfaces the class implements (following `extends` between the
  /// interfaces), mirroring how TypeScript surfaces those docs.
  fn apply_heritage_member_docs(
    &self,
    module_info: &EsModuleInfo,
    class: &Class,
    class_def: &mut crate::class::ClassDef,
  ) {
    if class.implements.is_empty() {
      return;
    }

    // `implements` only constrains the instance side of a class, so static
    // members never inherit from it.
    let needs_docs = class_def
      .methods
      .iter()
      .any(|method| !method.is_static && method.js_doc.is_empty())
      || class_def
        .properties
        .iter()
        .any(|property| !property.is_static && property.js_doc.is_empty());
    if !needs_docs {
      return;
    }

    /// Copies a JSDoc for inheritance onto a class member. `optional` and
    /// `default` on `@param` tags describe the interface's signature, which
    /// the class's own signature may not share, so they are not carried
    /// over — the class parameters provide their own.
    fn inherited_js_doc(js_doc: &JsDoc) -> JsDoc {
      let mut js_doc = js_doc.clone();
      for tag in js_doc.tags.iter_mut() {
        if let crate::js_doc::JsDocTag::Param {
          optional, default, ..
        } = tag
        {
          *optional = false;
          *default = None;
        }
      }
      js_doc
    }

    let mut method_docs = HashMap::<(String, MethodKind), JsDoc>::new();
    let mut property_docs = HashMap::<String, JsDoc>::new();
    let mut visited = HashSet::new();

    // Walk each `implements` clause depth-first through its `extends` chain
    // before moving on to the next clause, so that the first interface in
    // declaration order wins and a derived interface's member beats its
    // base's.
    for type_ref in class.implements.iter() {
      let mut stack =
        self.resolve_heritage_interfaces(module_info, &type_ref.expression);
      stack.reverse();

      while let Some((interface_module, interface_decl)) = stack.pop() {
        if !visited.insert((interface_module.module_id(), interface_decl.span))
        {
          continue;
        }
        let Some(interface_module_info) = interface_module.esm() else {
          continue;
        };

        for member in &interface_decl.body.body {
          let (key, kind) = match member {
            TSSignature::TSMethodSignature(sig) => {
              (&sig.key, Some(MethodKind::from(sig.kind)))
            }
            TSSignature::TSPropertySignature(sig) => (&sig.key, None),
            TSSignature::TSCallSignatureDeclaration(_)
            | TSSignature::TSConstructSignatureDeclaration(_)
            | TSSignature::TSIndexSignature(_) => continue,
          };
          let Some(js_doc) =
            js_doc_for_range(interface_module_info, member.span())
          else {
            continue;
          };
          if js_doc.is_empty() {
            continue;
          }
          let name = property_key_name(key);
          match kind {
            Some(kind) => {
              method_docs.entry((name, kind)).or_insert(js_doc);
            }
            None => {
              property_docs.entry(name).or_insert(js_doc);
            }
          }
        }

        let mut parents = interface_decl
          .extends
          .iter()
          .flat_map(|heritage| {
            self.resolve_heritage_interfaces(
              interface_module_info,
              &heritage.type_name,
            )
          })
          .collect::<Vec<_>>();
        parents.reverse();
        stack.extend(parents);
      }
    }

    for method in class_def.methods.iter_mut() {
      if method.is_static || !method.js_doc.is_empty() {
        continue;
      }
      // an accessor or a method can also implement an interface property
      // signature (e.g. a property of function type)
      let inherited = method_docs
        .get(&(method.name.to_string(), method.kind))
        .or_else(|| property_docs.get(&*method.name));
      if let Some(js_doc) = inherited {
        method.js_doc = inherited_js_doc(js_doc);
      }
    }
    for property in class_def.properties.iter_mut() {
      if property.is_static || !property.js_doc.is_empty() {
        continue;
      }
      // a field can also implement an interface method signature (e.g. an
      // arrow-function member) or an accessor signature
      let inherited = property_docs.get(&*property.name).or_else(|| {
        [MethodKind::Method, MethodKind::Getter, MethodKind::Setter]
          .iter()
          .find_map(|kind| method_docs.get(&(property.name.to_string(), *kind)))
      });
      if let Some(js_doc) = inherited {
        property.js_doc = inherited_js_doc(js_doc);
      }
    }
  }

  fn get_imports_for_module_info(
    &self,
    module_info: &EsModuleInfo,
  ) -> Result<Vec<Import>, DocError> {
    let referrer = module_info.specifier();
    let mut imports = vec![];

    for stmt in module_info.statements() {
      if let Statement::ImportDeclaration(import_decl) = stmt
        && let Some(js_doc) = js_doc_for_range(module_info, import_decl.span)
        && let Some(specifiers) = &import_decl.specifiers
      {
        for specifier in specifiers {
          let (imported_name, original_name, src) = match specifier {
            ImportDeclarationSpecifier::ImportSpecifier(named_specifier) => (
              named_specifier.local.name.to_string(),
              Some(module_export_name_value(&named_specifier.imported)),
              &import_decl.source.value,
            ),
            ImportDeclarationSpecifier::ImportDefaultSpecifier(
              default_specifier,
            ) => (
              default_specifier.local.name.to_string(),
              Some("default".to_string()),
              &import_decl.source.value,
            ),
            ImportDeclarationSpecifier::ImportNamespaceSpecifier(
              namespace_specifier,
            ) => (
              namespace_specifier.local.name.to_string(),
              None,
              &import_decl.source.value,
            ),
          };

          let resolved_specifier =
            self.resolve_dependency(src.as_ref(), referrer)?;

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
      get_location(module_info, expando_property.prop_name_span().start);
    let js_doc = js_doc_for_range(module_info, expando_property.inner().span)?;
    let init = expando_property.assignment();

    Some(decl_from_expr(self, module_info, init, location, js_doc))
  }

  fn get_doc_for_var_declarator_ident(
    &self,
    module_info: &EsModuleInfo,
    var_decl: &VarDecl,
    var_declarator: &VarDeclarator,
    ident: &Ident,
    full_range: SourceRange,
  ) -> Option<Declaration> {
    let full_range = if ident.span.start != var_declarator.span.start {
      ident.span
    } else {
      full_range
    };
    let js_doc = js_doc_for_range(module_info, full_range)?;

    if let Some(init) = &var_declarator.init
      && matches!(
        init,
        Expression::ClassExpression(_)
          | Expression::FunctionExpression(_)
          | Expression::ArrowFunctionExpression(_)
      )
    {
      let location = get_location(module_info, ident.span.start);
      return Some(decl_from_expr(self, module_info, init, location, js_doc));
    }

    if let Some(init) = &var_declarator.init
      && let Some(decl) =
        self.decl_from_init_alias(module_info, init, ident, &js_doc)
    {
      return Some(decl);
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
    .find(|(name, _)| name.as_str() == &*ident.name)
    .map(|(_name, var_def)| {
      let location = get_location(module_info, ident.span.start);
      Declaration::variable(location, DeclarationKind::Declare, js_doc, var_def)
    })
  }

  fn get_doc_for_class_decl(
    &self,
    module_info: &EsModuleInfo,
    class_decl: &Class,
    full_range: SourceRange,
  ) -> Option<Declaration> {
    let jsdoc_range = match class_decl.decorators.first() {
      Some(decorator) if decorator.span.start < full_range.start => {
        SourceRange::new(decorator.span.start, full_range.end)
      }
      _ => full_range,
    };
    let js_doc = js_doc_for_range(module_info, jsdoc_range)?;
    // declared classes cannot have decorators, so we ignore that return
    let (mut class_def, _) =
      super::class::get_doc_for_class_decl(module_info, class_decl);
    self.apply_heritage_member_docs(
      module_info,
      &class_decl.class,
      &mut class_def,
    );
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
    full_range: SourceRange,
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
    full_range: SourceRange,
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
    full_range: SourceRange,
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
    full_range: SourceRange,
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
    full_range: SourceRange,
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
          SymbolNodeRef::ExportDecl(export_decl, _) => {
            match &export_decl.declaration {
              Some(OxcDeclaration::TSModuleDeclaration(n)) => Some(&**n),
              _ => None,
            }
          }
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
      TSModuleDeclarationName::Identifier(ident) => ident.name.to_string(),
      TSModuleDeclarationName::StringLiteral(str_) => str_.value.to_string(),
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
              SymbolNodeRef::ClassDecl(n) => {
                n.id.as_ref().map(|id| id.name.to_string())
              }
              SymbolNodeRef::FnDecl(n) => {
                n.id.as_ref().map(|id| id.name.to_string())
              }
              SymbolNodeRef::TsEnum(n) => Some(n.id.name.to_string()),
              SymbolNodeRef::TsInterface(n) => Some(n.id.name.to_string()),
              SymbolNodeRef::TsTypeAlias(n) => Some(n.id.name.to_string()),
              SymbolNodeRef::TsNamespace(n) => match &n.id {
                TSModuleDeclarationName::Identifier(ident) => {
                  Some(ident.name.to_string())
                }
                TSModuleDeclarationName::StringLiteral(s) => {
                  Some(s.value.to_string())
                }
              },
              SymbolNodeRef::Var(_, _, ident) => Some(ident.name.to_string()),
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
    export_default_decl: &ExportDefaultDeclaration,
  ) -> Option<Declaration> {
    let js_doc = js_doc_for_range(module_info, export_default_decl.span)?;
    let location = get_location(module_info, export_default_decl.span.start);
    let doc_node = match &export_default_decl.declaration {
      ExportDefaultDeclarationKind::ClassDeclaration(class) => {
        let default_name = class
          .id
          .as_ref()
          .map(|ident| ident.name.to_string().into_boxed_str());
        let (mut class_def, decorator_js_doc) =
          crate::class::class_to_class_def(module_info, class, default_name);
        self.apply_heritage_member_docs(module_info, class, &mut class_def);
        let js_doc = if js_doc.is_empty() {
          decorator_js_doc
        } else {
          js_doc
        };
        Declaration::class(location, DeclarationKind::Export, js_doc, class_def)
      }
      ExportDefaultDeclarationKind::FunctionDeclaration(function) => {
        let default_name =
          function.id.as_ref().map(|ident| ident.name.to_string());
        let function_def = crate::function::function_to_function_def(
          module_info,
          function,
          default_name,
        );
        Declaration::function(
          location,
          DeclarationKind::Export,
          js_doc,
          function_def,
        )
      }
      ExportDefaultDeclarationKind::TSInterfaceDeclaration(interface_decl) => {
        let default_name = interface_decl.id.name.to_string();
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
      _ => {
        return self.get_decl_for_export_default_expr(
          module_info,
          &export_default_decl.declaration,
          js_doc,
          location,
        );
      }
    };

    Some(doc_node)
  }

  fn get_decl_for_export_default_expr(
    &self,
    module_info: &EsModuleInfo,
    export_expr: &ExportDefaultDeclarationKind,
    js_doc: JsDoc,
    location: Location,
  ) -> Option<Declaration> {
    if let ExportDefaultDeclarationKind::ArrowFunctionExpression(arrow_expr) =
      export_expr
    {
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
            export_expr.as_expression()?,
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
      js_doc_for_range(reference_module_info, reference_range.range())
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

    // deno_graph's export map only holds a single symbol per exported name,
    // so a name exported multiple times (e.g. `export { foo as Bar }` next to
    // `export interface Bar`, which merge in TypeScript) loses all but one of
    // its meanings. Collect the shadowed local targets per name so their
    // declarations can be merged back in.
    let shadowed_export_targets = collect_shadowed_export_targets(module_info);

    // Similarly, a name exported by multiple `export *` modules only resolves
    // to the first module (e.g. a builder function coming from one star
    // export and a same-named interface from an `export type *`). Collect the
    // exports of every star module so the other modules' declarations can be
    // merged back in.
    let re_export_all_exports = ModuleInfoRef::Esm(module_info)
      .re_export_all_nodes()
      .map(|nodes| {
        nodes
          .filter_map(|node| {
            let specifier = self
              .resolve_dependency(
                &node.src.value.to_string_lossy(),
                module_info.specifier(),
              )
              .ok()?;
            let module = self.get_module_info(specifier).ok()?;
            Some(module.exports(&self.root_symbol))
          })
          .collect::<Vec<_>>()
      })
      .unwrap_or_default();

    for (export_name, resolved_export) in &exports.resolved {
      let export = resolved_export.as_resolved_export();
      handled_symbols.insert(UniqueSymbolId::new(
        export.module.module_id(),
        export.symbol_id,
      ));
      let export_symbol = export.module.symbol(export.symbol_id).unwrap();
      let definitions = self
        .root_symbol
        .go_to_definitions(export.module, export_symbol);
      let original_range = &export_symbol.decls().first().unwrap().range;

      let mut declarations: Vec<Declaration> = vec![];
      let mut seen_declarations = HashSet::new();

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
          if seen_declarations
            .insert((decl.location.clone(), decl.def.to_kind()))
          {
            declarations.push(decl);
          }
        }
      }

      // Merge in the declarations of shadowed same-module exports of this
      // name.
      if let Some(targets) = shadowed_export_targets.get(export_name.as_str()) {
        for target in targets {
          let symbol = match target {
            ShadowedExportTarget::SwcId(id) => module_info.symbol_from_swc(id),
            ShadowedExportTarget::Range(range) => module_info
              .module_symbol()
              .child_ids()
              .filter_map(|id| module_info.symbol(id))
              .find(|symbol| {
                symbol.decls().iter().any(|decl| decl.range == *range)
              }),
          };
          let Some(symbol) = symbol else {
            continue;
          };
          let Some(target_original_range) =
            symbol.decls().first().map(|decl| &decl.range)
          else {
            continue;
          };
          for definition in self
            .root_symbol
            .go_to_definitions(ModuleInfoRef::Esm(module_info), symbol)
          {
            handled_symbols.insert(definition.symbol.unique_id());
            let maybe_docs = self.decls_for_maybe_node(
              export_name,
              definition.module,
              definition.symbol,
              definition.symbol_decl.maybe_node(),
              Some(module_info),
              module_info.specifier(),
              Some(definition.symbol_decl),
              Some(target_original_range),
            );
            for mut decl in maybe_docs {
              decl.declaration_kind = DeclarationKind::Export;
              if seen_declarations
                .insert((decl.location.clone(), decl.def.to_kind()))
              {
                declarations.push(decl);
              }
            }
          }
        }
      }

      // Merge in the declarations of this name from the other `export *`
      // modules.
      if matches!(
        resolved_export,
        deno_graph::symbols::ResolvedExportOrReExportAllPath::ReExportAllPath(
          _
        )
      ) {
        for star_exports in &re_export_all_exports {
          let Some(star_export) = star_exports.resolved.get(export_name) else {
            continue;
          };
          let star_export = star_export.as_resolved_export();
          if !handled_symbols.insert(UniqueSymbolId::new(
            star_export.module.module_id(),
            star_export.symbol_id,
          )) {
            continue;
          }
          let star_export_symbol =
            star_export.module.symbol(star_export.symbol_id).unwrap();
          let Some(star_original_range) =
            star_export_symbol.decls().first().map(|decl| &decl.range)
          else {
            continue;
          };
          for definition in self
            .root_symbol
            .go_to_definitions(star_export.module, star_export_symbol)
          {
            handled_symbols.insert(definition.symbol.unique_id());
            let maybe_docs = self.decls_for_maybe_node(
              export_name,
              definition.module,
              definition.symbol,
              definition.symbol_decl.maybe_node(),
              star_export.module.esm(),
              star_export.module.specifier(),
              Some(definition.symbol_decl),
              Some(star_original_range),
            );
            for mut decl in maybe_docs {
              decl.declaration_kind = DeclarationKind::Export;
              if seen_declarations
                .insert((decl.location.clone(), decl.def.to_kind()))
              {
                declarations.push(decl);
              }
            }
          }
        }
      }

      if !declarations.is_empty() {
        symbols.push(Arc::new(Symbol {
          name: export_name.as_str().into(),
          is_default: export_name == "default",
          declarations,
        }));
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
          symbols.push(Arc::new(symbol));
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
            decls.extend(symbol.declarations.clone());
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

      if decl_with_deps.is_alias_reexport {
        // a type alias like `export type A = B` is just another name for the
        // referenced type, which is effectively re-exported through the alias
        continue;
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
        self.get_doc_for_class_decl(module_info, n, n.span)
      }
      SymbolNodeRef::ExpandoProperty(n) => {
        self.get_doc_for_expando_property(module_info, n)
      }
      SymbolNodeRef::ExportDefaultDecl(n) => {
        self.get_doc_for_export_default_decl(module_info, n)
      }
      SymbolNodeRef::ExportDefaultExpr(n) => {
        let (js_doc, location) = self
          .export_default_expr_context(module_info, n)
          .unwrap_or_else(|| {
            (
              js_doc_for_range(module_info, n.span()).unwrap_or_default(),
              get_location(module_info, n.span().start),
            )
          });
        self.get_decl_for_export_default_expr(module_info, n, js_doc, location)
      }
      SymbolNodeRef::FnDecl(n) => {
        self.get_decl_for_fn_decl(module_info, n, n.span)
      }
      SymbolNodeRef::TsEnum(n) => {
        self.get_decl_for_enum(module_info, n, n.range())
      }
      SymbolNodeRef::TsInterface(n) => {
        self.get_decl_for_interface_decl(module_info, n, n.range())
      }
      SymbolNodeRef::TsNamespace(n) => {
        self.get_decl_for_ts_namespace(module_info, symbol, n, n.range())
      }
      SymbolNodeRef::TsTypeAlias(n) => {
        self.get_decl_for_type_alias(module_info, n, n.range())
      }
      SymbolNodeRef::Var(parent_decl, n, ident) => self
        .get_doc_for_var_declarator_ident(
          module_info,
          parent_decl,
          n,
          ident,
          parent_decl.range(),
        ),
      SymbolNodeRef::UsingVar(_, _, _) => {
        // makes no sense for using declarations to be in the public API
        None
      }
      SymbolNodeRef::ExportDecl(export_decl, inner) => match inner {
        ExportDeclRef::Class(n) => {
          self.get_doc_for_class_decl(module_info, n, export_decl.range())
        }
        ExportDeclRef::Fn(n) => {
          self.get_decl_for_fn_decl(module_info, n, export_decl.range())
        }
        ExportDeclRef::TsEnum(n) => {
          self.get_decl_for_enum(module_info, n, export_decl.range())
        }
        ExportDeclRef::TsModule(n) => self.get_decl_for_ts_namespace(
          module_info,
          symbol,
          n,
          export_decl.range(),
        ),
        ExportDeclRef::TsInterface(n) => {
          self.get_decl_for_interface_decl(module_info, n, export_decl.range())
        }
        ExportDeclRef::TsTypeAlias(n) => {
          self.get_decl_for_type_alias(module_info, n, export_decl.range())
        }
        ExportDeclRef::Var(var_decl, var_declarator, ident) => self
          .get_doc_for_var_declarator_ident(
            module_info,
            var_decl,
            var_declarator,
            ident,
            export_decl.range(),
          ),
        ExportDeclRef::TsImportEquals(_) => None,
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
      SymbolNodeRef::ExportDecl(n, _) => {
        self.get_declare_for_export_decl_declaration(n)
      }
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

  fn export_default_expr_context(
    &self,
    module_info: &EsModuleInfo,
    export_expr: &ExportDefaultDeclarationKind,
  ) -> Option<(JsDoc, Location)> {
    let expr_span = export_expr.span();
    for stmt in module_info.statements() {
      let Statement::ExportDefaultDeclaration(export_default) = stmt else {
        continue;
      };
      if export_default.declaration.span() == expr_span {
        return Some((
          js_doc_for_range(module_info, export_default.span)?,
          get_location(module_info, export_default.span.start),
        ));
      }
    }
    None
  }

  fn get_declare_for_export_decl_declaration(
    &self,
    export_decl: &ExportNamedDeclaration,
  ) -> bool {
    match &export_decl.declaration {
      Some(OxcDeclaration::ClassDeclaration(class_decl)) => class_decl.declare,
      Some(OxcDeclaration::FunctionDeclaration(fn_decl)) => fn_decl.declare,
      Some(OxcDeclaration::TSEnumDeclaration(ts_enum_decl)) => {
        ts_enum_decl.declare
      }
      Some(OxcDeclaration::TSInterfaceDeclaration(ts_interface_decl)) => {
        ts_interface_decl.declare
      }
      Some(OxcDeclaration::TSModuleDeclaration(ts_module_decl)) => {
        ts_module_decl.declare
      }
      Some(OxcDeclaration::TSTypeAliasDeclaration(ts_type_alias_decl)) => {
        ts_type_alias_decl.declare
      }
      Some(OxcDeclaration::VariableDeclaration(var_decl)) => var_decl.declare,
      _ => false,
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

enum ShadowedExportTarget {
  /// A local declaration or an aliased export targeting a local symbol.
  SwcId(Id),
  /// An aliased re-export (`export { foo as Bar } from "..."`), identified
  /// by the range of its export specifier.
  Range(SourceRange),
}

/// Collects, per exported name, the local targets of all module-level exports
/// of that name.
///
/// deno_graph's export map only keeps a single symbol per name, so when a
/// name is exported more than once (e.g. an aliased export next to a
/// declaration with the exported name, which merge in TypeScript), all but
/// one of the meanings are lost. The returned targets allow recovering them.
/// Only names with more than one export are returned.
fn collect_shadowed_export_targets(
  module_info: &EsModuleInfo,
) -> IndexMap<String, Vec<ShadowedExportTarget>> {
  let mut targets: IndexMap<String, Vec<ShadowedExportTarget>> =
    IndexMap::new();

  for stmt in &module_info.program().body {
    match stmt {
      Statement::ExportDeclaration(export_decl) => {
        let mut add_ident = |ident: &BindingIdentifier| {
          targets
            .entry(ident.name.to_string())
            .or_default()
            .push(ShadowedExportTarget::SwcId(binding_identifier_to_id(ident)));
        };
        match &export_decl.declaration {
          OxcDeclaration::ClassDeclaration(class_decl) => {
            if let Some(ident) = &class_decl.id {
              add_ident(ident);
            }
          }
          OxcDeclaration::FunctionDeclaration(fn_decl) => {
            if let Some(ident) = &fn_decl.id {
              add_ident(ident);
            }
          }
          OxcDeclaration::TSInterfaceDeclaration(interface_decl) => {
            add_ident(&interface_decl.id)
          }
          OxcDeclaration::TSTypeAliasDeclaration(type_alias) => {
            add_ident(&type_alias.id)
          }
          OxcDeclaration::TSEnumDeclaration(enum_decl) => {
            add_ident(&enum_decl.id)
          }
          OxcDeclaration::TSNamespaceDeclaration(ts_module_decl) => {
            add_ident(&ts_module_decl.id)
          }
          OxcDeclaration::VariableDeclaration(var_decl) => {
            for declarator in &var_decl.declarations {
              if let BindingPattern::BindingIdentifier(ident) = &declarator.id {
                add_ident(ident);
              }
            }
          }
          OxcDeclaration::TSExternalModuleDeclaration(_)
          | OxcDeclaration::TSGlobalDeclaration(_)
          | OxcDeclaration::TSImportEqualsDeclaration(_) => {}
        }
      }
      Statement::ExportNamedDeclaration(named_export) => {
        for specifier in &named_export.specifiers {
          let export_name = module_export_name_value(&specifier.exported);
          let ModuleExportName::IdentifierReference(orig) = &specifier.local
          else {
            continue;
          };
          targets
            .entry(export_name)
            .or_default()
            .push(ShadowedExportTarget::SwcId(identifier_reference_to_id(
              module_info,
              orig,
            )));
        }
      }
      Statement::ExportFromDeclaration(named_export) => {
        for specifier in &named_export.specifiers {
          let export_name = module_export_name_value(&specifier.exported);
          targets
            .entry(export_name)
            .or_default()
            .push(ShadowedExportTarget::Range(specifier.span));
        }
      }
      _ => {}
    }
  }

  // A name with a single export can't have been shadowed.
  targets.retain(|_, targets| targets.len() > 1);

  targets
}

impl DocParser<'_> {
  /// Resolves a variable initializer that aliases another declaration in the
  /// same module — `const Foo = FooClass` or `const baz = Foo.bar` — to a
  /// declaration of the aliased kind, so the alias documents as the class or
  /// function it refers to instead of as an opaque variable.
  fn decl_from_init_alias(
    &self,
    module_info: &EsModuleInfo,
    init: &Expression,
    ident: &BindingIdentifier,
    js_doc: &JsDoc,
  ) -> Option<Declaration> {
    /// The alias's own JSDoc wins; an undocumented alias inherits the
    /// documentation of the declaration it points at.
    fn js_doc_or_target(
      module_info: &EsModuleInfo,
      js_doc: &JsDoc,
      target_range: SourceRange,
    ) -> JsDoc {
      if js_doc.is_empty() {
        js_doc_for_range(module_info, target_range).unwrap_or_default()
      } else {
        js_doc.clone()
      }
    }

    let location = get_location(module_info, ident.span.start);

    match init {
      Expression::Identifier(init_ident) => {
        let symbol = module_info
          .symbol_from_swc(&identifier_reference_to_id(module_info, init_ident))?;
        for decl in symbol.decls() {
          let Some(node) = decl.maybe_node() else {
            continue;
          };
          let node = match node {
            SymbolNodeRef::ExportDecl(_, export_decl) => match export_decl {
              ExportDeclRef::Class(class_decl) => {
                SymbolNodeRef::ClassDecl(class_decl)
              }
              ExportDeclRef::Fn(fn_decl) => SymbolNodeRef::FnDecl(fn_decl),
              _ => continue,
            },
            node => node,
          };
          match node {
            SymbolNodeRef::ClassDecl(class_decl) => {
              let (mut class_def, decorator_js_doc) =
                // no `def_name`: the alias has a name of its own, and
                // `def_name` would override it everywhere the symbol is
                // rendered
                crate::class::class_to_class_def(module_info, class_decl, None);
              self.apply_heritage_member_docs(
                module_info,
                class_decl,
                &mut class_def,
              );
              let js_doc = if js_doc.is_empty() && decorator_js_doc.is_empty() {
                js_doc_or_target(module_info, js_doc, class_decl.span)
              } else if js_doc.is_empty() {
                decorator_js_doc
              } else {
                js_doc.clone()
              };
              return Some(Declaration::class(
                location,
                DeclarationKind::Declare,
                js_doc,
                class_def,
              ));
            }
            SymbolNodeRef::FnDecl(fn_decl) => {
              let function_def = crate::function::function_to_function_def(
                module_info,
                fn_decl,
                None,
              );
              let js_doc = js_doc_or_target(module_info, js_doc, fn_decl.span);
              return Some(Declaration::function(
                location,
                DeclarationKind::Declare,
                js_doc,
                function_def,
              ));
            }
            _ => continue,
          }
        }
        None
      }
      Expression::StaticMemberExpression(member) => {
        // `const baz = Foo.bar` where `bar` is a static method of a class
        let Expression::Identifier(obj_ident) = &member.object else {
          return None;
        };
        let prop_name = member.property.name.as_str();
        let symbol = module_info
          .symbol_from_swc(&identifier_reference_to_id(module_info, obj_ident))?;
        for decl in symbol.decls() {
          let Some(node) = decl.maybe_node() else {
            continue;
          };
          let class = match node {
            SymbolNodeRef::ClassDecl(class_decl) => class_decl,
            SymbolNodeRef::ExportDecl(_, ExportDeclRef::Class(class_decl)) => {
              class_decl
            }
            _ => continue,
          };
          for class_member in &class.body.body {
            let ClassElement::MethodDefinition(method) = class_member else {
              continue;
            };
            if !method.r#static || method.kind != MethodDefinitionKind::Method {
              continue;
            }
            let name_matches = match &method.key {
              PropertyKey::StaticIdentifier(key) => key.name == prop_name,
              PropertyKey::StringLiteral(key) => key.value == prop_name,
              _ => false,
            };
            if !name_matches {
              continue;
            }
            let function_def = crate::function::function_to_function_def(
              module_info,
              &method.value,
              None,
            );
            let js_doc = js_doc_or_target(module_info, js_doc, method.span);
            return Some(Declaration::function(
              location,
              DeclarationKind::Declare,
              js_doc,
              function_def,
            ));
          }
        }
        None
      }
      _ => None,
    }
  }
}

fn decl_from_expr(
  parser: &DocParser,
  module_info: &EsModuleInfo,
  expr: &Expression,
  location: Location,
  js_doc: JsDoc,
) -> Declaration {
  match expr {
    Expression::ClassExpression(class_expr) => {
      let def_name = class_expr
        .id
        .as_ref()
        .map(|id| id.name.to_string().into_boxed_str());
      let (mut class_def, decorator_js_doc) =
        crate::class::class_to_class_def(module_info, class_expr, def_name);
      parser.apply_heritage_member_docs(module_info, class_expr, &mut class_def);
      let js_doc = if js_doc.is_empty() {
        decorator_js_doc
      } else {
        js_doc
      };
      Declaration::class(location, DeclarationKind::Declare, js_doc, class_def)
    }
    Expression::FunctionExpression(fn_expr) => {
      let def_name = fn_expr.id.as_ref().map(|id| id.name.to_string());
      let function_def = crate::function::function_to_function_def(
        module_info,
        fn_expr,
        def_name,
      );
      Declaration::function(
        location,
        DeclarationKind::Declare,
        js_doc,
        function_def,
      )
    }
    Expression::ArrowFunctionExpression(arrow_expr) => {
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
      symbols: vec![Arc::new(Symbol::variable(
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
      ))],
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
  module_info.statements().iter().any(|m| {
    matches!(
      m,
      Statement::ImportDeclaration(_) | Statement::TSImportEqualsDeclaration(_)
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
