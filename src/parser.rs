// Copyright 2020-2022 the Deno authors. All rights reserved. MIT license.

use crate::diagnostics::DiagnosticsCollector;
use crate::diagnostics::DocDiagnostic;
use crate::js_doc::JsDoc;
use crate::node;
use crate::node::DeclarationKind;
use crate::node::DocNode;
use crate::node::ModuleDoc;
use crate::node::NamespaceDef;
use crate::swc_util::get_location;
use crate::swc_util::get_text_info_location;
use crate::swc_util::js_doc_for_range;
use crate::swc_util::module_export_name_value;
use crate::swc_util::module_js_doc_for_source;
use crate::symbol_util::symbol_has_ignorable_js_doc_tag;
use crate::ts_type::LiteralPropertyDef;
use crate::ts_type::TsTypeDef;
use crate::ts_type::TsTypeDefKind;
use crate::ts_type::TsTypeLiteralDef;
use crate::variable::VariableDef;
use crate::DocNodeKind;
use crate::ImportDef;
use crate::Location;
use crate::ReexportKind;

use deno_ast::swc::ast::ClassDecl;
use deno_ast::swc::ast::Decl;
use deno_ast::swc::ast::DefaultDecl;
use deno_ast::swc::ast::ExportDecl;
use deno_ast::swc::ast::ExportDefaultDecl;
use deno_ast::swc::ast::ExportDefaultExpr;
use deno_ast::swc::ast::ExportSpecifier;
use deno_ast::swc::ast::FnDecl;
use deno_ast::swc::ast::Ident;
use deno_ast::swc::ast::ImportSpecifier;
use deno_ast::swc::ast::ModuleDecl;
use deno_ast::swc::ast::ModuleItem;
use deno_ast::swc::ast::TsEnumDecl;
use deno_ast::swc::ast::TsInterfaceDecl;
use deno_ast::swc::ast::TsModuleDecl;
use deno_ast::swc::ast::TsModuleName;
use deno_ast::swc::ast::TsTypeAliasDecl;
use deno_ast::swc::ast::VarDecl;
use deno_ast::swc::ast::VarDeclKind;
use deno_ast::swc::ast::VarDeclarator;
use deno_ast::ParsedSource;
use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;
use deno_graph::symbols::DefinitionPath;
use deno_graph::symbols::EsmModuleInfo;
use deno_graph::symbols::ExportDeclRef;
use deno_graph::symbols::ModuleInfoRef;
use deno_graph::symbols::ResolvedSymbolDepEntry;
use deno_graph::symbols::Symbol;
use deno_graph::symbols::SymbolDecl;
use deno_graph::symbols::SymbolId;
use deno_graph::symbols::SymbolNodeDep;
use deno_graph::symbols::SymbolNodeRef;
use deno_graph::symbols::UniqueSymbolId;
use deno_graph::CapturingModuleParser;
use deno_graph::Module;
use deno_graph::ModuleGraph;
use deno_graph::ModuleSpecifier;
use indexmap::IndexMap;
use indexmap::IndexSet;

use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum DocError {
  Resolve(String),
  #[allow(dead_code)]
  Io(std::io::Error),
  Parse(deno_ast::Diagnostic),
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

impl From<deno_ast::Diagnostic> for DocError {
  fn from(error: deno_ast::Diagnostic) -> DocError {
    DocError::Parse(error)
  }
}

#[derive(Debug, Clone)]
enum ImportKind {
  Namespace(String),
  Named(String, Option<String>),
}

#[derive(Debug, Clone)]
struct Import {
  src: String,
  kind: ImportKind,
}

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
  root_symbol: deno_graph::symbols::RootSymbol<'a>,
  visibility: SymbolVisibility,
  diagnostics: Option<RefCell<DiagnosticsCollector>>,
}

impl<'a> DocParser<'a> {
  pub fn new(
    graph: &'a ModuleGraph,
    parser: CapturingModuleParser<'a>,
    options: DocParserOptions,
  ) -> Result<Self, anyhow::Error> {
    let root_symbol = deno_graph::symbols::RootSymbol::new(graph, parser);
    let visibility = SymbolVisibility::build(graph, &root_symbol)?;

    Ok(DocParser {
      graph,
      private: options.private,
      root_symbol,
      visibility,
      diagnostics: if options.diagnostics {
        Some(Default::default())
      } else {
        None
      },
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

  /// Parses a module into a list of exported items,
  /// as well as a list of reexported items which need to be fetched from other modules.
  pub fn parse_module(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Result<ModuleDoc, DocError> {
    let module_info = self.get_module_info(specifier)?;
    let definitions = self.get_doc_nodes_for_module_info(module_info)?;
    self.collect_diagnostics_for_nodes(&definitions);
    let reexports = self.get_reexports_for_module(module_info);
    let module_doc = ModuleDoc {
      definitions,
      reexports,
    };
    Ok(module_doc)
  }

  fn get_module_info(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Result<ModuleInfoRef, DocError> {
    get_module_info(&self.root_symbol, specifier)
  }

  /// Fetches `file_name` and returns a list of exported items (no reexports).
  #[cfg(feature = "rust")]
  pub fn parse(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Result<Vec<DocNode>, DocError> {
    let module_info = self.get_module_info(specifier)?;
    let doc_nodes = self.get_doc_nodes_for_module_info(module_info)?;
    self.collect_diagnostics_for_nodes(&doc_nodes);
    Ok(doc_nodes)
  }

  /// Fetches `file_name`, parses it, and resolves its reexports.
  pub fn parse_with_reexports(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Result<Vec<DocNode>, DocError> {
    let doc_nodes =
      self.parse_with_reexports_inner(specifier, HashSet::new())?;
    self.collect_diagnostics_for_nodes(&doc_nodes);
    Ok(doc_nodes)
  }

  fn collect_diagnostics_for_nodes(&self, nodes: &[DocNode]) {
    if let Some(diagnostics) = &self.diagnostics {
      let mut diagnostics = diagnostics.borrow_mut();
      diagnostics.analyze_doc_nodes(nodes);
    }
  }

  fn parse_with_reexports_inner(
    &self,
    specifier: &ModuleSpecifier,
    mut visited: HashSet<ModuleSpecifier>,
  ) -> Result<Vec<DocNode>, DocError> {
    if !visited.insert(specifier.clone()) {
      return Ok(Vec::new()); // circular
    }
    let module = resolve_deno_graph_module(self.graph, specifier)?;

    match module {
      Module::Esm(_) | Module::Json(_) => {
        let module_info = self.get_module_info(module.specifier())?;
        let module_doc_nodes =
          self.get_doc_nodes_for_module_info(module_info)?;
        let mut flattened_docs = Vec::new();
        let exports = module_info.exports(&self.root_symbol);
        for (export_name, export) in exports.resolved {
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
                let specifier = self.resolve_dependency(
                  &file_dep.specifier,
                  first_def.module.specifier(),
                )?;
                let doc_nodes = self
                  .parse_with_reexports_inner(&specifier, visited.clone())?;
                // hoist any module doc to be the exported namespaces module doc
                let mut js_doc = JsDoc::default();
                for doc_node in &doc_nodes {
                  if matches!(doc_node.kind, DocNodeKind::ModuleDoc) {
                    js_doc = doc_node.js_doc.clone();
                  }
                }
                let ns_def = NamespaceDef {
                  elements: doc_nodes
                    .iter()
                    .filter(|dn| !matches!(dn.kind, DocNodeKind::ModuleDoc))
                    .cloned()
                    .collect(),
                };
                let ns_doc_node = DocNode::namespace(
                  export_name,
                  definition_location(first_def),
                  DeclarationKind::Export,
                  js_doc,
                  ns_def,
                );
                flattened_docs.push(ns_doc_node);
              }
              DefinitionKind::Definition => {
                if first_def.module.specifier() != module_info.specifier() {
                  for definition in definitions {
                    let decl = definition.symbol_decl;
                    let maybe_doc = self.doc_for_maybe_node(
                      definition.module,
                      definition.symbol,
                      decl.maybe_node(),
                    );
                    if let Some(mut doc_node) = maybe_doc {
                      doc_node.name = export_name.clone();
                      doc_node.declaration_kind = DeclarationKind::Export;

                      flattened_docs.push(doc_node);
                    }
                  }
                }
              }
            }
          }
        }

        flattened_docs.extend(module_doc_nodes);
        Ok(flattened_docs)
      }
      Module::Npm(_) | Module::Node(_) | Module::External(_) => Ok(vec![]),
    }
  }

  fn get_doc_nodes_for_module_imports(
    &self,
    module_info: &EsmModuleInfo,
  ) -> Result<Vec<DocNode>, DocError> {
    let parsed_source = module_info.source();
    let referrer = module_info.specifier();
    let mut imports = vec![];

    for node in &parsed_source.module().body {
      if let deno_ast::swc::ast::ModuleItem::ModuleDecl(ModuleDecl::Import(
        import_decl,
      )) = node
      {
        if let Some(js_doc) =
          js_doc_for_range(parsed_source, &import_decl.range())
        {
          let location = get_location(parsed_source, import_decl.start());
          for specifier in &import_decl.specifiers {
            use deno_ast::swc::ast::ImportSpecifier::*;

            let (name, maybe_imported_name, src) = match specifier {
              Named(named_specifier) => (
                named_specifier.local.sym.to_string(),
                named_specifier
                  .imported
                  .as_ref()
                  .map(module_export_name_value)
                  .or_else(|| Some(named_specifier.local.sym.to_string())),
                import_decl.src.value.to_string(),
              ),
              Default(default_specifier) => (
                default_specifier.local.sym.to_string(),
                Some("default".to_string()),
                import_decl.src.value.to_string(),
              ),
              Namespace(namespace_specifier) => (
                namespace_specifier.local.sym.to_string(),
                None,
                import_decl.src.value.to_string(),
              ),
            };

            let resolved_specifier = self.resolve_dependency(&src, referrer)?;
            let import_def = ImportDef {
              src: resolved_specifier.to_string(),
              imported: maybe_imported_name,
            };

            let doc_node = DocNode::import(
              name,
              location.clone(),
              js_doc.clone(),
              import_def,
            );

            imports.push(doc_node);
          }
        }
      }
    }

    Ok(imports)
  }

  fn get_doc_for_var_declarator_ident(
    &self,
    module_info: &EsmModuleInfo,
    var_decl: &VarDecl,
    var_declarator: &VarDeclarator,
    ident: &Ident,
    full_range: &SourceRange,
  ) -> Option<DocNode> {
    let full_range = if ident.start() != var_declarator.start() {
      Cow::Owned(ident.range())
    } else {
      Cow::Borrowed(full_range)
    };
    let js_doc = js_doc_for_range(module_info.source(), &full_range)?;
    // todo(dsherret): it's not ideal to call this function over
    // and over for the same var declarator when there are a lot
    // of idents
    super::variable::get_docs_for_var_declarator(
      module_info,
      var_decl,
      var_declarator,
    )
    .into_iter()
    .find(|(name, _, _)| name.as_str() == &*ident.sym)
    .map(|(name, var_def, _)| {
      let location = get_location(module_info.source(), ident.start());
      DocNode::variable(
        name,
        location,
        DeclarationKind::Declare,
        js_doc.clone(),
        var_def,
      )
    })
  }

  fn get_doc_for_class_decl(
    &self,
    parsed_source: &ParsedSource,
    class_decl: &ClassDecl,
    full_range: &SourceRange,
  ) -> Option<DocNode> {
    let jsdoc_range = match class_decl.class.decorators.first() {
      Some(decorator) if decorator.start() < full_range.start => {
        Cow::Owned(SourceRange::new(decorator.start(), full_range.end))
      }
      _ => Cow::Borrowed(full_range),
    };
    let js_doc = js_doc_for_range(parsed_source, &jsdoc_range)?;
    // declared classes cannot have decorators, so we ignore that return
    let (name, class_def, _) =
      super::class::get_doc_for_class_decl(parsed_source, class_decl);
    let location = get_location(parsed_source, full_range.start);
    Some(DocNode::class(
      name,
      location,
      DeclarationKind::Declare,
      js_doc,
      class_def,
    ))
  }

  fn get_doc_for_fn_decl(
    &self,
    parsed_source: &ParsedSource,
    fn_decl: &FnDecl,
    full_range: &SourceRange,
  ) -> Option<DocNode> {
    let js_doc = js_doc_for_range(parsed_source, full_range)?;
    let (name, function_def) =
      super::function::get_doc_for_fn_decl(parsed_source, fn_decl);
    let location = get_location(parsed_source, full_range.start);
    Some(DocNode::function(
      name,
      location,
      DeclarationKind::Declare,
      js_doc,
      function_def,
    ))
  }

  fn get_doc_for_interface_decl(
    &self,
    parsed_source: &ParsedSource,
    ts_interface_decl: &TsInterfaceDecl,
    full_range: &SourceRange,
  ) -> Option<DocNode> {
    let js_doc = js_doc_for_range(parsed_source, full_range)?;
    let (name, interface_def) = super::interface::get_doc_for_ts_interface_decl(
      parsed_source,
      ts_interface_decl,
    );
    let location = get_location(parsed_source, full_range.start);
    Some(DocNode::interface(
      name,
      location,
      DeclarationKind::Declare,
      js_doc,
      interface_def,
    ))
  }

  fn get_docs_for_type_alias(
    &self,
    parsed_source: &ParsedSource,
    ts_type_alias: &TsTypeAliasDecl,
    full_range: &SourceRange,
  ) -> Option<DocNode> {
    let js_doc = js_doc_for_range(parsed_source, full_range)?;
    let (name, type_alias_def) =
      super::type_alias::get_doc_for_ts_type_alias_decl(
        parsed_source,
        ts_type_alias,
      );
    let location = get_location(parsed_source, full_range.start);
    Some(DocNode::type_alias(
      name,
      location,
      DeclarationKind::Declare,
      js_doc,
      type_alias_def,
    ))
  }

  fn get_doc_for_enum(
    &self,
    parsed_source: &ParsedSource,
    ts_enum: &TsEnumDecl,
    full_range: &SourceRange,
  ) -> Option<DocNode> {
    let js_doc = js_doc_for_range(parsed_source, full_range)?;
    let (name, enum_def) =
      super::r#enum::get_doc_for_ts_enum_decl(parsed_source, ts_enum);
    let location = get_location(parsed_source, full_range.start);
    Some(DocNode::r#enum(
      name,
      location,
      DeclarationKind::Declare,
      js_doc,
      enum_def,
    ))
  }

  fn get_doc_for_ts_namespace(
    &self,
    module_info: &EsmModuleInfo,
    symbol: &Symbol,
    ts_module: &TsModuleDecl,
    full_range: &SourceRange,
  ) -> Option<DocNode> {
    let first_ns_decl = symbol
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

    let namespace_name = match &ts_module.id {
      TsModuleName::Ident(ident) => ident.sym.to_string(),
      TsModuleName::Str(str_) => str_.value.to_string(),
    };
    let mut elements = Vec::new();
    let mut handled_symbols = HashSet::new();

    for (export_name, export_symbol_id) in symbol.exports() {
      handled_symbols.insert(UniqueSymbolId::new(
        module_info.module_id(),
        *export_symbol_id,
      ));
      let export_symbol = module_info.symbol(*export_symbol_id).unwrap();
      let definitions = self
        .root_symbol
        .go_to_definitions(ModuleInfoRef::Esm(module_info), export_symbol);
      for definition in definitions {
        handled_symbols.insert(definition.symbol.unique_id());
        if definition.module.specifier() != module_info.specifier() {
          continue;
        }

        let maybe_doc = self.doc_for_maybe_node(
          definition.module,
          definition.symbol,
          definition.symbol_decl.maybe_node(),
        );
        if let Some(mut doc_node) = maybe_doc {
          doc_node.name = export_name.to_string();
          doc_node.declaration_kind = DeclarationKind::Export;

          elements.push(doc_node);
        }
      }
    }

    let is_ambient = elements.is_empty() && !module_has_import(module_info);
    for child_id in symbol.child_ids() {
      let unique_id = UniqueSymbolId::new(module_info.module_id(), child_id);
      if !handled_symbols.insert(unique_id) {
        continue; // already handled
      }
      if is_ambient
        || self.private
        || self.visibility.has_non_exported_public(&unique_id)
      {
        let child_symbol = module_info.symbol(child_id).unwrap();
        elements.extend(self.get_private_doc_node_for_symbol(
          ModuleInfoRef::Esm(module_info),
          child_symbol,
        ));
      }
    }

    let js_doc = js_doc_for_range(module_info.source(), full_range)?;
    let location = get_location(module_info.source(), full_range.start);
    Some(DocNode::namespace(
      namespace_name,
      location,
      DeclarationKind::Declare,
      js_doc,
      NamespaceDef { elements },
    ))
  }

  fn get_private_doc_node_for_symbol(
    &self,
    module_info: ModuleInfoRef,
    child_symbol: &Symbol,
  ) -> Vec<DocNode> {
    debug_assert!(!self
      .visibility
      .root_exported_ids
      .contains_key(&child_symbol.unique_id()));
    let mut doc_nodes = Vec::with_capacity(child_symbol.decls().len());
    for decl in child_symbol.decls() {
      if let Some(mut doc_node) =
        self.doc_for_maybe_node(module_info, child_symbol, decl.maybe_node())
      {
        let is_declared = decl
          .maybe_node()
          .map(|node| self.get_declare_for_symbol_node(node))
          .unwrap_or(false);
        doc_node.declaration_kind = if is_declared {
          DeclarationKind::Declare
        } else {
          DeclarationKind::Private
        };
        doc_nodes.push(doc_node);
      }
    }
    doc_nodes
  }

  fn get_doc_for_export_default_decl(
    &self,
    parsed_source: &ParsedSource,
    export_default_decl: &ExportDefaultDecl,
  ) -> Option<DocNode> {
    let js_doc = js_doc_for_range(parsed_source, &export_default_decl.range())?;
    let location = get_location(parsed_source, export_default_decl.start());
    let name = "default".to_string();

    let doc_node = match &export_default_decl.decl {
      DefaultDecl::Class(class_expr) => {
        let (class_def, decorator_js_doc) =
          crate::class::class_to_class_def(parsed_source, &class_expr.class);
        let js_doc = if js_doc.is_empty() {
          decorator_js_doc
        } else {
          js_doc
        };
        DocNode::class(
          name,
          location,
          DeclarationKind::Export,
          js_doc,
          class_def,
        )
      }
      DefaultDecl::Fn(fn_expr) => {
        let function_def = crate::function::function_to_function_def(
          parsed_source,
          &fn_expr.function,
        );
        DocNode::function(
          name,
          location,
          DeclarationKind::Export,
          js_doc,
          function_def,
        )
      }
      DefaultDecl::TsInterfaceDecl(interface_decl) => {
        let (_, interface_def) =
          crate::interface::get_doc_for_ts_interface_decl(
            parsed_source,
            interface_decl,
          );
        DocNode::interface(
          name,
          location,
          DeclarationKind::Export,
          js_doc,
          interface_def,
        )
      }
    };

    Some(doc_node)
  }

  fn get_doc_for_export_default_expr(
    &self,
    parsed_source: &ParsedSource,
    export_expr: &ExportDefaultExpr,
  ) -> Option<DocNode> {
    if let Some(js_doc) = js_doc_for_range(parsed_source, &export_expr.range())
    {
      let location = get_location(parsed_source, export_expr.start());
      Some(DocNode::variable(
        String::from("default"),
        location,
        DeclarationKind::Export,
        js_doc,
        super::variable::VariableDef {
          kind: deno_ast::swc::ast::VarDeclKind::Var,
          ts_type: super::ts_type::infer_ts_type_from_expr(
            parsed_source,
            export_expr.expr.as_ref(),
            true,
          ),
        },
      ))
    } else {
      None
    }
  }

  fn get_imports_for_module_body(
    &self,
    module_body: &[deno_ast::swc::ast::ModuleItem],
  ) -> HashMap<String, Import> {
    let mut imports = HashMap::new();

    for node in module_body.iter() {
      if let ModuleItem::ModuleDecl(ModuleDecl::Import(import_decl)) = node {
        for specifier in &import_decl.specifiers {
          let import = match specifier {
            ImportSpecifier::Named(named_specifier) => Import {
              kind: ImportKind::Named(
                named_specifier.local.sym.to_string(),
                named_specifier
                  .imported
                  .as_ref()
                  .map(module_export_name_value),
              ),
              src: import_decl.src.value.to_string(),
            },
            ImportSpecifier::Default(default_specifier) => Import {
              kind: ImportKind::Named(
                default_specifier.local.sym.to_string(),
                Some("default".to_string()),
              ),
              src: import_decl.src.value.to_string(),
            },
            ImportSpecifier::Namespace(namespace_specifier) => Import {
              kind: ImportKind::Namespace(
                namespace_specifier.local.sym.to_string(),
              ),
              src: import_decl.src.value.to_string(),
            },
          };

          let name = match import.kind.clone() {
            ImportKind::Named(name, _) | ImportKind::Namespace(name) => name,
          };

          imports.insert(name, import);
        }
      }
    }

    imports
  }

  pub fn get_reexports_for_module(
    &self,
    module_info: ModuleInfoRef,
  ) -> Vec<node::Reexport> {
    let Some(module_info) = module_info.esm() else {
      return Vec::new();
    };
    let module_body = &module_info.source().module().body;

    let imports = self.get_imports_for_module_body(module_body);

    let mut reexports: Vec<node::Reexport> = vec![];

    if self.private {
      reexports.extend(imports.values().cloned().map(|import| node::Reexport {
        src: import.src,
        kind: match import.kind {
          ImportKind::Named(orig, exported) => {
            ReexportKind::Named(orig, exported)
          }
          ImportKind::Namespace(name) => ReexportKind::Namespace(name),
        },
      }))
    }

    for node in module_body.iter() {
      if let deno_ast::swc::ast::ModuleItem::ModuleDecl(module_decl) = node {
        let r = match module_decl {
          ModuleDecl::ExportNamed(named_export) => {
            if let Some(src) = &named_export.src {
              let src_str = src.value.to_string();
              named_export
                .specifiers
                .iter()
                .map(|export_specifier| match export_specifier {
                  ExportSpecifier::Namespace(ns_export) => node::Reexport {
                    kind: node::ReexportKind::Namespace(
                      module_export_name_value(&ns_export.name),
                    ),
                    src: src_str.to_string(),
                  },
                  ExportSpecifier::Default(specifier) => node::Reexport {
                    kind: node::ReexportKind::Named(
                      "default".to_string(),
                      Some(specifier.exported.sym.to_string()),
                    ),
                    src: src_str.to_string(),
                  },
                  ExportSpecifier::Named(named_export) => {
                    let export_name =
                      module_export_name_value(&named_export.orig);
                    let maybe_alias = named_export
                      .exported
                      .as_ref()
                      .map(module_export_name_value);
                    let kind =
                      node::ReexportKind::Named(export_name, maybe_alias);
                    node::Reexport {
                      kind,
                      src: src_str.to_string(),
                    }
                  }
                })
                .collect::<Vec<node::Reexport>>()
            } else {
              named_export
                .specifiers
                .iter()
                .filter_map(|specifier| {
                  if let ExportSpecifier::Named(specifier) = specifier {
                    if let Some(import) =
                      imports.get(&module_export_name_value(&specifier.orig))
                    {
                      // If it has the same name as the original import and private values are exported,
                      // don't export this again and document the same value twice.
                      if self.private && specifier.exported.is_none() {
                        return None;
                      }

                      let name = module_export_name_value(
                        specifier.exported.as_ref().unwrap_or(&specifier.orig),
                      );
                      Some(node::Reexport {
                        src: import.src.clone(),
                        kind: match &import.kind {
                          ImportKind::Named(orig, maybe_export) => {
                            ReexportKind::Named(
                              maybe_export
                                .clone()
                                .unwrap_or_else(|| orig.clone()),
                              Some(name),
                            )
                          }
                          ImportKind::Namespace(_) => {
                            ReexportKind::Namespace(name)
                          }
                        },
                      })
                    } else {
                      None
                    }
                  } else {
                    None
                  }
                })
                .collect()
            }
          }
          ModuleDecl::ExportAll(export_all) => {
            let reexport = node::Reexport {
              kind: node::ReexportKind::All,
              src: export_all.src.value.to_string(),
            };
            vec![reexport]
          }
          _ => vec![],
        };

        reexports.extend(r);
      }
    }

    reexports
  }

  fn get_doc_nodes_for_module_info(
    &self,
    module_info: ModuleInfoRef,
  ) -> Result<Vec<DocNode>, DocError> {
    match module_info {
      ModuleInfoRef::Json(module) => Ok(
        parse_json_module_doc_node(
          module.specifier(),
          module.text_info().text_str(),
        )
        .map(|n| vec![n])
        .unwrap_or_default(),
      ),
      ModuleInfoRef::Esm(module_info) => {
        let mut definitions =
          self.get_doc_nodes_for_module_info_body(module_info);
        definitions.extend(self.get_doc_nodes_for_module_imports(module_info)?);

        Ok(definitions)
      }
    }
  }

  fn get_doc_nodes_for_module_info_body(
    &self,
    module_info: &EsmModuleInfo,
  ) -> Vec<DocNode> {
    let mut doc_nodes = Vec::new();
    let parsed_source = module_info.source();
    // check to see if there is a module level JSDoc for the source file
    if let Some(module_js_doc) = module_js_doc_for_source(parsed_source) {
      if let Some((js_doc, range)) = module_js_doc {
        let doc_node =
          DocNode::module_doc(get_location(parsed_source, range.start), js_doc);
        doc_nodes.push(doc_node);
      } else {
        return vec![];
      }
    }

    let mut handled_symbols = HashSet::new();
    let exports = module_info.exports(&self.root_symbol);
    for (export_name, export) in &exports.resolved {
      handled_symbols.insert(UniqueSymbolId::new(
        export.module.module_id(),
        export.symbol_id,
      ));
      let export_symbol = export.module.symbol(export.symbol_id).unwrap();
      let definitions = self
        .root_symbol
        .go_to_definitions(export.module, export_symbol);
      for definition in definitions {
        if definition.module.specifier() != module_info.specifier() {
          continue;
        }
        handled_symbols.insert(definition.symbol.unique_id());
        let maybe_doc = self.doc_for_maybe_node(
          definition.module,
          definition.symbol,
          definition.symbol_decl.maybe_node(),
        );
        if let Some(mut doc_node) = maybe_doc {
          doc_node.name = export_name.clone();
          doc_node.declaration_kind = DeclarationKind::Export;

          doc_nodes.push(doc_node);
        }
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
        doc_nodes.extend(self.get_private_doc_node_for_symbol(
          ModuleInfoRef::Esm(module_info),
          child_symbol,
        ));
      }
    }

    doc_nodes
  }

  fn doc_for_maybe_node(
    &self,
    module_info: ModuleInfoRef,
    symbol: &Symbol,
    maybe_node: Option<SymbolNodeRef<'_>>,
  ) -> Option<DocNode> {
    let maybe_doc = match module_info {
      ModuleInfoRef::Json(module_info) => parse_json_module_doc_node(
        module_info.specifier(),
        module_info.text_info().text_str(),
      ),
      ModuleInfoRef::Esm(module_info) => {
        if let Some(node) = maybe_node {
          self.get_doc_for_symbol_node_ref(module_info, symbol, node)
        } else {
          None
        }
      }
    };

    self.check_private_type_in_public_diagnostic(
      &maybe_doc,
      module_info,
      symbol,
    );

    maybe_doc
  }

  fn check_private_type_in_public_diagnostic(
    &self,
    maybe_doc: &Option<DocNode>,
    module_info: ModuleInfoRef<'_>,
    symbol: &Symbol,
  ) {
    let Some(diagnostics) = &self.diagnostics else {
      return;
    };
    let Some(doc) = maybe_doc else {
      return;
    };

    let doc_id = symbol.unique_id();
    let Some(deps_by_member) = self.visibility.get_root_exported_deps(&doc_id)
    else {
      return;
    };
    if deps_by_member.is_empty() {
      return; // avoid borrow_mut if not necessary
    }

    let mut diagnostics = diagnostics.borrow_mut();
    for (maybe_member_id, deps) in deps_by_member.iter() {
      for dep in deps {
        let dep_module =
          self.root_symbol.get_module_from_id(dep.module_id).unwrap();
        let dep_symbol = dep_module.symbol(dep.symbol_id).unwrap();
        let member =
          maybe_member_id.and_then(|member_id| module_info.symbol(member_id));
        diagnostics.add_private_type_in_public(
          doc,
          doc_id,
          dep_module,
          dep_symbol,
          module_info,
          member,
        );
      }
    }
  }

  fn get_doc_for_symbol_node_ref(
    &self,
    module_info: &EsmModuleInfo,
    symbol: &Symbol,
    node: SymbolNodeRef<'_>,
  ) -> Option<DocNode> {
    let parsed_source = module_info.source();
    match node {
      SymbolNodeRef::ClassDecl(n) => {
        self.get_doc_for_class_decl(parsed_source, n, &n.class.range())
      }
      SymbolNodeRef::ExportDefaultDecl(n) => {
        self.get_doc_for_export_default_decl(parsed_source, n)
      }
      SymbolNodeRef::ExportDefaultExprLit(n, _) => {
        self.get_doc_for_export_default_expr(parsed_source, n)
      }
      SymbolNodeRef::FnDecl(n) => {
        self.get_doc_for_fn_decl(parsed_source, n, &n.function.range())
      }
      SymbolNodeRef::TsEnum(n) => {
        self.get_doc_for_enum(parsed_source, n, &n.range())
      }
      SymbolNodeRef::TsInterface(n) => {
        self.get_doc_for_interface_decl(parsed_source, n, &n.range())
      }
      SymbolNodeRef::TsNamespace(n) => {
        self.get_doc_for_ts_namespace(module_info, symbol, n, &n.range())
      }
      SymbolNodeRef::TsTypeAlias(n) => {
        self.get_docs_for_type_alias(parsed_source, n, &n.range())
      }
      SymbolNodeRef::Var(parent_decl, n, ident) => self
        .get_doc_for_var_declarator_ident(
          module_info,
          parent_decl,
          n,
          ident,
          &parent_decl.range(),
        ),
      SymbolNodeRef::ExportDecl(export_decl, inner) => match inner {
        ExportDeclRef::Class(n) => {
          self.get_doc_for_class_decl(parsed_source, n, &export_decl.range())
        }
        ExportDeclRef::Fn(n) => {
          self.get_doc_for_fn_decl(parsed_source, n, &export_decl.range())
        }
        ExportDeclRef::TsEnum(n) => {
          self.get_doc_for_enum(parsed_source, n, &export_decl.range())
        }
        ExportDeclRef::TsModule(n) => self.get_doc_for_ts_namespace(
          module_info,
          symbol,
          n,
          &export_decl.range(),
        ),
        ExportDeclRef::TsInterface(n) => self.get_doc_for_interface_decl(
          parsed_source,
          n,
          &export_decl.range(),
        ),
        ExportDeclRef::TsTypeAlias(n) => {
          self.get_docs_for_type_alias(parsed_source, n, &export_decl.range())
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
      | SymbolNodeRef::ClassProp(_)
      | SymbolNodeRef::Constructor(_)
      | SymbolNodeRef::TsIndexSignature(_)
      | SymbolNodeRef::TsCallSignatureDecl(_)
      | SymbolNodeRef::TsConstructSignatureDecl(_)
      | SymbolNodeRef::TsPropertySignature(_)
      | SymbolNodeRef::TsGetterSignature(_)
      | SymbolNodeRef::TsSetterSignature(_)
      | SymbolNodeRef::TsMethodSignature(_) => {
        debug_assert!(false, "should not reach here");
        None
      }
    }
  }

  fn get_declare_for_symbol_node(&self, node: SymbolNodeRef) -> bool {
    match node {
      SymbolNodeRef::ClassDecl(n) => n.declare,
      SymbolNodeRef::ExportDecl(n, _) => self.get_declare_for_decl(&n.decl),
      SymbolNodeRef::ExportDefaultDecl(_) => false,
      SymbolNodeRef::ExportDefaultExprLit(_, _) => false,
      SymbolNodeRef::FnDecl(n) => n.declare,
      SymbolNodeRef::TsEnum(n) => n.declare,
      SymbolNodeRef::TsInterface(n) => n.declare,
      SymbolNodeRef::TsNamespace(n) => n.declare,
      SymbolNodeRef::TsTypeAlias(n) => n.declare,
      SymbolNodeRef::Var(n, _, _) => n.declare,
      SymbolNodeRef::Module(_)
      | SymbolNodeRef::AutoAccessor(_)
      | SymbolNodeRef::ClassMethod(_)
      | SymbolNodeRef::ClassProp(_)
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

  fn get_declare_for_decl(&self, decl: &Decl) -> bool {
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
  ) -> Result<ModuleSpecifier, DocError> {
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

/// Resolve a deno_graph module redirecting to the types dependency if available.
fn resolve_deno_graph_module<'a>(
  graph: &'a ModuleGraph,
  specifier: &ModuleSpecifier,
) -> Result<&'a deno_graph::Module, DocError> {
  graph
    .try_get_prefer_types(specifier)
    .map_err(|err| DocError::Resolve(err.to_string()))?
    .ok_or_else(|| {
      DocError::Resolve(format!("Unable to load specifier: \"{}\"", specifier))
    })
}

fn get_module_info<'a>(
  root_symbol: &'a deno_graph::symbols::RootSymbol,
  specifier: &ModuleSpecifier,
) -> Result<ModuleInfoRef<'a>, DocError> {
  match root_symbol.get_module_from_specifier(specifier) {
    Some(symbol) => Ok(symbol),
    None => Err(DocError::Resolve(format!(
      "Could not find ES module in graph: {}",
      specifier
    ))),
  }
}

fn parse_json_module_doc_node(
  specifier: &ModuleSpecifier,
  source: &str,
) -> Option<DocNode> {
  if let Ok(value) = serde_json::from_str(source) {
    Some(DocNode {
      kind: DocNodeKind::Variable,
      name: "default".to_string(),
      location: Location {
        filename: specifier.to_string(),
        col: 0,
        line: 1,
      },
      declaration_kind: DeclarationKind::Export,
      variable_def: Some(VariableDef {
        kind: VarDeclKind::Var,
        ts_type: Some(parse_json_module_type(&value)),
      }),
      ..Default::default()
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
      kind: Some(TsTypeDefKind::Array),
      array: Some(Box::new(TsTypeDef::keyword("unknown"))),
      ..Default::default()
    },
    serde_json::Value::Object(obj) => TsTypeDef {
      repr: "".to_string(),
      kind: Some(TsTypeDefKind::TypeLiteral),
      type_literal: Some(TsTypeLiteralDef {
        properties: obj
          .iter()
          .map(|(key, value)| LiteralPropertyDef {
            name: key.to_string(),
            ts_type: Some(parse_json_module_type(value)),
            params: Vec::new(),
            readonly: false,
            computed: false,
            optional: false,
            type_params: Vec::new(),
          })
          .collect(),
        ..Default::default()
      }),
      ..Default::default()
    },
  }
}

fn module_has_import(module_info: &EsmModuleInfo) -> bool {
  module_info.source().module().body.iter().any(|m| {
    matches!(
      m,
      ModuleItem::ModuleDecl(
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

#[derive(Default)]
struct SymbolMembersWithDeps(
  IndexMap<Option<SymbolId>, IndexSet<UniqueSymbolId>>,
);

impl SymbolMembersWithDeps {
  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  fn add(&mut self, maybe_member_id: Option<SymbolId>, def_id: UniqueSymbolId) {
    self.0.entry(maybe_member_id).or_default().insert(def_id);
  }

  fn iter(
    &self,
  ) -> impl Iterator<Item = (&Option<SymbolId>, &IndexSet<UniqueSymbolId>)> {
    self.0.iter()
  }
}

struct SymbolVisibility {
  root_exported_ids: HashMap<UniqueSymbolId, SymbolMembersWithDeps>,
  /// Symbol identifiers that are not exported, but are referenced
  /// by exported symbols.
  non_exported_public_ids: HashSet<UniqueSymbolId>,
}

impl SymbolVisibility {
  pub fn build(
    graph: &ModuleGraph,
    root_symbol: &deno_graph::symbols::RootSymbol,
  ) -> Result<Self, DocError> {
    let mut root_exported_ids: HashMap<UniqueSymbolId, SymbolMembersWithDeps> =
      Default::default();

    // get a collection of all the symbols that are exports
    {
      let mut pending_symbols = Vec::new();
      for root in &graph.roots {
        let module = resolve_deno_graph_module(graph, root)?;
        let module_info = get_module_info(root_symbol, module.specifier())?;
        let exports = module_info.exports(root_symbol);
        for (_name, export) in &exports.resolved {
          let symbol = export.module.symbol(export.symbol_id).unwrap();
          let definitions =
            root_symbol.go_to_definitions(export.module, symbol);
          for definition in definitions {
            if root_exported_ids
              .insert(definition.symbol.unique_id(), Default::default())
              .is_none()
            {
              pending_symbols.push(definition);
            }
          }
        }
      }
      // analyze the pending symbols
      while let Some(definition) = pending_symbols.pop() {
        for (_name, export_id) in definition.symbol.exports() {
          let export_symbol = definition.module.symbol(*export_id).unwrap();
          let definitions =
            root_symbol.go_to_definitions(definition.module, export_symbol);
          for definition in definitions {
            if root_exported_ids
              .insert(definition.symbol.unique_id(), Default::default())
              .is_none()
            {
              pending_symbols.push(definition);
            }
          }
        }
      }
    }

    // now analyze for all the non exported types that are referenced by exported types
    // along with filling in any non-exported dependencies of root exported types
    let mut non_exported_public_ids = HashSet::new();
    let mut pending_symbol_ids =
      root_exported_ids.keys().copied().collect::<Vec<_>>();
    while let Some(original_id) = pending_symbol_ids.pop() {
      let module_info = root_symbol
        .get_module_from_id(original_id.module_id)
        .unwrap();
      let symbol = module_info.symbol(original_id.symbol_id).unwrap();
      // todo: don't populate implementation signatures and private members
      let deps_with_member_deps = symbol
        .decls()
        .iter()
        .filter(|d| !d.has_overloads())
        .flat_map(|decl| decl.deps().into_iter().map(move |dep| (decl, dep)))
        .map(|(decl, dep)| (None, decl, dep))
        .chain(symbol.members().iter().flat_map(|symbol_id| {
          let symbol = module_info.symbol(*symbol_id).unwrap();
          if symbol.is_private_member() {
            return Box::new(std::iter::empty())
              as Box<
                dyn Iterator<
                  Item = (Option<&Symbol>, &SymbolDecl, SymbolNodeDep),
                >,
              >;
          }
          Box::new(
            symbol
              .decls()
              .iter()
              .filter(|m| !m.has_overloads())
              .flat_map(move |decl| {
                decl
                  .deps()
                  .into_iter()
                  .map(move |dep| (Some(symbol), decl, dep))
              }),
          )
        }));
      // todo: inspect exports of symbols
      for (maybe_member, decl, dep) in deps_with_member_deps {
        let mut symbols = Vec::new();
        // split out the parts from the dependency and resolve just the first part
        // let (dep, parts) = dep.split_parts();
        let mut pending_entries =
          root_symbol.resolve_symbol_dep(module_info, symbol, &dep);
        while let Some(entry) = pending_entries.pop() {
          match entry {
            ResolvedSymbolDepEntry::Path(path) => {
              if symbol_has_ignorable_js_doc_tag(path.module(), path.symbol()) {
                symbols.clear();
                break; // stop searching
              }

              // only analyze declarations
              if path.symbol().is_decl() {
                symbols.push((path.module(), path.symbol()));
              }

              // queue the next parts
              match path {
                DefinitionPath::Path { next, .. } => {
                  pending_entries
                    .extend(next.into_iter().map(ResolvedSymbolDepEntry::Path));
                }
                DefinitionPath::Definition(_)
                | DefinitionPath::Unresolved(_) => {}
              }
            }
            ResolvedSymbolDepEntry::ImportType(_) => {
              // this is an import type with no property access, ignore it for now
            }
          }
        }

        let maybe_member_id = maybe_member.map(|m| m.symbol_id());
        for (_module, symbol) in symbols {
          let symbol_id = symbol.unique_id();
          if !root_exported_ids.contains_key(&symbol_id)
            && non_exported_public_ids.insert(symbol_id)
          {
            if let Some(dep_ids) = root_exported_ids.get_mut(&original_id) {
              dep_ids.add(maybe_member_id, symbol_id);
            }
            // examine the private types of this private type
            pending_symbol_ids.push(symbol_id);
          }
        }
      }
    }

    Ok(SymbolVisibility {
      root_exported_ids,
      non_exported_public_ids,
    })
  }

  pub fn get_root_exported_deps(
    &self,
    id: &UniqueSymbolId,
  ) -> Option<&SymbolMembersWithDeps> {
    self.root_exported_ids.get(id)
  }

  pub fn has_non_exported_public(&self, id: &UniqueSymbolId) -> bool {
    self.non_exported_public_ids.contains(id)
  }
}
