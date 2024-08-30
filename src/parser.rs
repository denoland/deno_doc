// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use crate::diagnostics::DiagnosticsCollector;
use crate::diagnostics::DocDiagnostic;
use crate::js_doc::JsDoc;
use crate::node;
use crate::node::DeclarationKind;
use crate::node::DocNode;
use crate::node::ModuleDoc;
use crate::node::NamespaceDef;
use crate::ts_type::infer_simple_ts_type_from_init;
use crate::ts_type::PropertyDef;
use crate::ts_type::TsTypeDef;
use crate::ts_type::TsTypeDefKind;
use crate::ts_type::TsTypeLiteralDef;
use crate::util::graph::resolve_deno_graph_module;
use crate::util::swc::get_location;
use crate::util::swc::get_text_info_location;
use crate::util::swc::js_doc_for_range;
use crate::util::swc::module_export_name_value;
use crate::util::swc::module_js_doc_for_source;
use crate::util::symbol::get_module_info;
use crate::variable::VariableDef;
use crate::visibility::SymbolVisibility;
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
use deno_ast::SourceRanged;
use deno_ast::SourceRangedForSpanned;
use deno_graph::symbols::EsModuleInfo;
use deno_graph::symbols::ExpandoPropertyRef;
use deno_graph::symbols::ExportDeclRef;
use deno_graph::symbols::ModuleInfoRef;
use deno_graph::symbols::Symbol;
use deno_graph::symbols::SymbolNodeRef;
use deno_graph::symbols::UniqueSymbolId;
use deno_graph::Module;
use deno_graph::ModuleGraph;
use deno_graph::ModuleParser;
use deno_graph::ModuleSpecifier;

use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::error::Error;
use std::fmt;
use std::rc::Rc;

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
  root_symbol: Rc<deno_graph::symbols::RootSymbol<'a>>,
  visibility: SymbolVisibility,
  diagnostics: Option<RefCell<DiagnosticsCollector<'a>>>,
}

impl<'a> DocParser<'a> {
  pub fn new(
    graph: &'a ModuleGraph,
    parser: &'a dyn ModuleParser,
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
      Module::Js(_) | Module::Json(_) => {
        let module_info = self.get_module_info(module.specifier())?;
        let module_doc_nodes =
          self.get_doc_nodes_for_module_info(module_info)?;
        let mut flattened_docs = Vec::new();
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
                let specifier = self.resolve_dependency(
                  &file_dep.specifier,
                  first_def.module.specifier(),
                )?;
                let doc_nodes = self
                  .parse_with_reexports_inner(specifier, visited.clone())?;
                // hoist any module doc to be the exported namespaces module doc
                let mut js_doc = JsDoc::default();
                for doc_node in &doc_nodes {
                  if matches!(doc_node.kind(), DocNodeKind::ModuleDoc) {
                    js_doc = doc_node.js_doc.clone();
                  }
                }
                let ns_def = NamespaceDef {
                  elements: doc_nodes
                    .into_iter()
                    .filter(|dn| !matches!(dn.kind(), DocNodeKind::ModuleDoc))
                    .map(Rc::new)
                    .collect(),
                };
                let ns_doc_node = DocNode::namespace(
                  export_name.into_boxed_str(),
                  false,
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
                    let maybe_docs = self.docs_for_maybe_node(
                      definition.module,
                      definition.symbol,
                      decl.maybe_node(),
                    );
                    for mut doc_node in maybe_docs {
                      doc_node.name = export_name.as_str().into();
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
    module_info: &EsModuleInfo,
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
              name.into_boxed_str(),
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

  fn get_doc_for_expando_property(
    &self,
    source: &ParsedSource,
    expando_property: ExpandoPropertyRef,
  ) -> Option<DocNode> {
    let location =
      get_location(source, expando_property.prop_name_range().start());
    let ts_type = infer_simple_ts_type_from_init(
      source,
      Some(expando_property.assignment()),
      /* is const */ true,
    );
    let js_doc = js_doc_for_range(source, &expando_property.inner().range())?;

    Some(DocNode::variable(
      expando_property.prop_name().to_string().into_boxed_str(),
      false,
      location,
      DeclarationKind::Declare,
      js_doc,
      VariableDef {
        ts_type,
        kind: VarDeclKind::Const,
      },
    ))
  }

  fn get_doc_for_var_declarator_ident(
    &self,
    module_info: &EsModuleInfo,
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
    .find(|(name, _)| name.as_str() == &*ident.sym)
    .map(|(name, var_def)| {
      let location = get_location(module_info.source(), ident.start());
      DocNode::variable(
        name.into_boxed_str(),
        false,
        location,
        DeclarationKind::Declare,
        js_doc,
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
      name.into_boxed_str(),
      false,
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
      name.into_boxed_str(),
      false,
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
      None,
    );
    let location = get_location(parsed_source, full_range.start);
    Some(DocNode::interface(
      name.into_boxed_str(),
      false,
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
      name.into_boxed_str(),
      false,
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
      name.into_boxed_str(),
      false,
      location,
      DeclarationKind::Declare,
      js_doc,
      enum_def,
    ))
  }

  fn get_doc_for_ts_namespace(
    &self,
    module_info: &EsModuleInfo,
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

        let maybe_docs = self.docs_for_maybe_node(
          definition.module,
          definition.symbol,
          definition.symbol_decl.maybe_node(),
        );
        for mut doc_node in maybe_docs {
          doc_node.name = export_name.as_str().into();
          doc_node.declaration_kind = DeclarationKind::Export;

          elements.push(Rc::new(doc_node));
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
        elements.extend(
          self
            .get_private_doc_node_for_symbol(
              ModuleInfoRef::Esm(module_info),
              child_symbol,
            )
            .into_iter()
            .map(Rc::new),
        );
      }
    }

    let js_doc = js_doc_for_range(module_info.source(), full_range)?;
    let location = get_location(module_info.source(), full_range.start);
    let is_default = namespace_name == "default";
    Some(DocNode::namespace(
      namespace_name.into_boxed_str(),
      is_default,
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
    debug_assert!(self
      .visibility
      .get_root_exported_deps(&child_symbol.unique_id())
      .is_none());
    let mut doc_nodes = Vec::with_capacity(child_symbol.decls().len());
    for decl in child_symbol.decls() {
      let maybe_docs =
        self.docs_for_maybe_node(module_info, child_symbol, decl.maybe_node());
      for mut doc_node in maybe_docs {
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
        let default_name = class_expr
          .ident
          .as_ref()
          .map(|ident| ident.sym.to_string().into_boxed_str());
        let (class_def, decorator_js_doc) = crate::class::class_to_class_def(
          parsed_source,
          &class_expr.class,
          default_name,
        );
        let js_doc = if js_doc.is_empty() {
          decorator_js_doc
        } else {
          js_doc
        };
        DocNode::class(
          name.into_boxed_str(),
          true,
          location,
          DeclarationKind::Export,
          js_doc,
          class_def,
        )
      }
      DefaultDecl::Fn(fn_expr) => {
        let default_name =
          fn_expr.ident.as_ref().map(|ident| ident.sym.to_string());
        let function_def = crate::function::function_to_function_def(
          parsed_source,
          &fn_expr.function,
          default_name,
        );
        DocNode::function(
          name.into_boxed_str(),
          true,
          location,
          DeclarationKind::Export,
          js_doc,
          function_def,
        )
      }
      DefaultDecl::TsInterfaceDecl(interface_decl) => {
        let default_name = interface_decl.id.sym.to_string();
        let (_, interface_def) =
          crate::interface::get_doc_for_ts_interface_decl(
            parsed_source,
            interface_decl,
            Some(default_name),
          );
        DocNode::interface(
          name.into_boxed_str(),
          true,
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
        "default".into(),
        true,
        location,
        DeclarationKind::Export,
        js_doc,
        VariableDef {
          kind: VarDeclKind::Var,
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
    module_body: &[ModuleItem],
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
    module_info: &EsModuleInfo,
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
      let export = export.as_resolved_export();
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
        let maybe_docs = self.docs_for_maybe_node(
          definition.module,
          definition.symbol,
          definition.symbol_decl.maybe_node(),
        );
        for mut doc_node in maybe_docs {
          doc_node.name = export_name.as_str().into();
          doc_node.is_default = Some(export_name == "default");
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

  fn docs_for_maybe_node(
    &self,
    module_info: ModuleInfoRef,
    symbol: &Symbol,
    maybe_node: Option<SymbolNodeRef<'_>>,
  ) -> Vec<DocNode> {
    let mut docs = Vec::with_capacity(2);
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

    if let Some(doc) = maybe_doc {
      docs.push(doc);

      self.check_private_type_in_public_diagnostic(module_info, symbol);

      if let Some(node) = maybe_node {
        if node.is_function() {
          // find any expando properties for this function symbol
          if let Some(expando_namespace) = self
            .maybe_expando_property_namespace_doc(&docs[0], module_info, symbol)
          {
            docs.push(expando_namespace);
          }
        }
      }
    }

    docs
  }

  fn maybe_expando_property_namespace_doc(
    &self,
    func_doc: &DocNode,
    module_info: ModuleInfoRef,
    symbol: &Symbol,
  ) -> Option<DocNode> {
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
        let mut docs = self.docs_for_maybe_node(
          module_info,
          symbol,
          Some(SymbolNodeRef::ExpandoProperty(n)),
        );
        for doc in &mut docs {
          doc.name = name.as_str().into();
          doc.declaration_kind = DeclarationKind::Declare;
        }
        docs
      })
      .map(Rc::new)
      .collect::<Vec<_>>();
    if elements.is_empty() {
      return None;
    }
    Some(DocNode::namespace(
      func_doc.name.clone(),
      false,
      elements[0].location.clone(),
      DeclarationKind::Declare,
      // give this a JS doc to prevent a missing JS doc diagnostic
      JsDoc {
        doc: Some(
          format!("Additional properties on the `{}` function.", func_doc.name)
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
    doc_symbol: &Symbol,
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

  fn get_doc_for_symbol_node_ref(
    &self,
    module_info: &EsModuleInfo,
    symbol: &Symbol,
    node: SymbolNodeRef<'_>,
  ) -> Option<DocNode> {
    let parsed_source = module_info.source();
    match node {
      SymbolNodeRef::ClassDecl(n) => {
        self.get_doc_for_class_decl(parsed_source, n, &n.class.range())
      }
      SymbolNodeRef::ExpandoProperty(n) => {
        self.get_doc_for_expando_property(parsed_source, n)
      }
      SymbolNodeRef::ExportDefaultDecl(n) => {
        self.get_doc_for_export_default_decl(parsed_source, n)
      }
      SymbolNodeRef::ExportDefaultExpr(n) => {
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
      SymbolNodeRef::UsingVar(_, _, _) => {
        // makes no sense for using declarations to be in the public API
        None
      }
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
      SymbolNodeRef::ExportDecl(n, _) => self.get_declare_for_decl(&n.decl),
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

fn parse_json_module_doc_node(
  specifier: &ModuleSpecifier,
  source: &str,
) -> Option<DocNode> {
  if let Ok(value) = serde_json::from_str(source) {
    Some(DocNode {
      name: "default".into(),
      is_default: Some(true),
      location: Location {
        filename: specifier.to_string().into_boxed_str(),
        col: 0,
        line: 1,
        byte_index: 0,
      },
      declaration_kind: DeclarationKind::Export,
      js_doc: JsDoc::default(),
      def: node::DocNodeDef::Variable {
        variable_def: VariableDef {
          kind: VarDeclKind::Var,
          ts_type: Some(parse_json_module_type(&value)),
        },
      },
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
      ..Default::default()
    },
  }
}

fn module_has_import(module_info: &EsModuleInfo) -> bool {
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
