// Copyright 2020-2022 the Deno authors. All rights reserved. MIT license.

use crate::js_doc::JsDoc;
use crate::namespace::NamespaceDef;
use crate::node;
use crate::node::DeclarationKind;
use crate::node::DocNode;
use crate::node::ModuleDoc;
use crate::swc_util::get_location;
use crate::swc_util::js_doc_for_range;
use crate::swc_util::module_export_name_value;
use crate::swc_util::module_js_doc_for_source;
use crate::DocNodeKind;
use crate::ImportDef;
use crate::Location;
use crate::ReexportKind;

use deno_ast::swc::ast::Decl;
use deno_ast::swc::ast::DefaultDecl;
use deno_ast::swc::ast::ExportSpecifier;
use deno_ast::swc::ast::Expr;
use deno_ast::swc::ast::ImportSpecifier;
use deno_ast::swc::ast::ModuleDecl;
use deno_ast::swc::ast::ModuleItem;
use deno_ast::swc::ast::Stmt;
use deno_ast::ParsedSource;
use deno_ast::SourceRangedForSpanned;
use deno_graph::CapturingModuleParser;
use deno_graph::ModuleGraph;
use deno_graph::ModuleParser;
use deno_graph::ModuleSpecifier;
use deno_graph::Resolved;

use std::collections::HashMap;
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

pub struct DocParser<'a> {
  pub graph: ModuleGraph,
  pub private: bool,
  pub parser: CapturingModuleParser<'a>,
}

impl<'a> DocParser<'a> {
  pub fn new(
    graph: ModuleGraph,
    private: bool,
    parser: CapturingModuleParser<'a>,
  ) -> Self {
    DocParser {
      graph,
      private,
      parser,
    }
  }

  /// Parses a module into a list of exported items,
  /// as well as a list of reexported items which need to be fetched from other modules.
  pub fn parse_module(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Result<ModuleDoc, DocError> {
    let parsed_source = self.get_parsed_source(specifier)?;
    let module = parsed_source.module();
    let mut definitions =
      self.get_doc_nodes_for_module_body(&parsed_source, module.body.clone());
    let import_doc_entries = self.get_doc_nodes_for_module_imports(
      &parsed_source,
      module.body.clone(),
      specifier,
    )?;
    definitions.extend(import_doc_entries);
    let reexports = self.get_reexports_for_module_body(&module.body);
    let module_doc = ModuleDoc {
      definitions,
      reexports,
    };
    Ok(module_doc)
  }

  fn get_parsed_source(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Result<ParsedSource, DocError> {
    let specifier = self.graph.resolve(specifier);
    let module = self.graph.get(&specifier).ok_or_else(|| {
      DocError::Resolve(format!(
        "Could not find module in graph: {}",
        specifier
      ))
    })?;
    let source = module.maybe_source.clone().ok_or_else(|| {
      DocError::Resolve(format!(
        "Could not find module source in graph: {}",
        specifier
      ))
    })?;
    self
      .parser
      .parse_module(&specifier, source, module.media_type)
      .map_err(DocError::Parse)
  }

  /// Fetches `file_name` and returns a list of exported items (no reexports).
  #[cfg(feature = "rust")]
  pub fn parse(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Result<Vec<DocNode>, DocError> {
    self.parse_module(specifier).map(|md| md.definitions)
  }

  fn flatten_reexports(
    &self,
    reexports: &[node::Reexport],
    referrer: &ModuleSpecifier,
  ) -> Result<Vec<DocNode>, DocError> {
    let mut by_src: HashMap<String, Vec<node::Reexport>> = HashMap::new();

    let mut processed_reexports: Vec<DocNode> = vec![];

    for reexport in reexports {
      if by_src.get(&reexport.src).is_none() {
        by_src.insert(reexport.src.to_string(), vec![]);
      }

      let bucket = by_src.get_mut(&reexport.src).unwrap();
      bucket.push(reexport.clone());
    }

    for specifier in by_src.keys() {
      let resolved_specifier = self
        .graph
        .resolve_dependency(specifier, referrer, true)
        .ok_or_else(|| DocError::Resolve(specifier.clone()))?;
      let doc_nodes = self.parse_with_reexports(resolved_specifier)?;
      let reexports_for_specifier = by_src.get(specifier).unwrap();

      for reexport in reexports_for_specifier {
        match &reexport.kind {
          node::ReexportKind::All => {
            processed_reexports.extend(doc_nodes.clone())
          }
          node::ReexportKind::Namespace(ns_name) => {
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
              ns_name.to_string(),
              Location {
                filename: specifier.to_string(),
                line: 1,
                col: 0,
              },
              DeclarationKind::Export,
              js_doc,
              ns_def,
            );
            processed_reexports.push(ns_doc_node);
          }
          node::ReexportKind::Named(ident, maybe_alias) => {
            // Try to find reexport.
            // NOTE: the reexport might actually be reexport from another
            // module; for now we're skipping nested reexports.
            let doc_nodes = doc_nodes
              .iter()
              .filter(|node| &node.name == ident)
              .collect::<Vec<_>>();

            for doc_node in doc_nodes {
              let doc_node = doc_node.clone();
              let doc_node = if let Some(alias) = maybe_alias {
                DocNode {
                  name: alias.to_string(),
                  ..doc_node
                }
              } else {
                doc_node
              };

              processed_reexports.push(doc_node);
            }
          }
        }
      }
    }

    Ok(processed_reexports)
  }

  /// Fetches `file_name`, parses it, and resolves its reexports.
  pub fn parse_with_reexports(
    &self,
    specifier: &ModuleSpecifier,
  ) -> Result<Vec<DocNode>, DocError> {
    let module = self
      .graph
      .try_get(specifier)
      .map_err(|err| DocError::Resolve(err.to_string()))?
      .ok_or_else(|| {
        DocError::Resolve(format!(
          "Unable to load specifier: \"{}\"",
          specifier
        ))
      })?;

    let module = if let Some((_, Resolved::Ok { specifier, .. })) =
      &module.maybe_types_dependency
    {
      self
        .graph
        .try_get(specifier)
        .map_err(|err| DocError::Resolve(err.to_string()))?
        .ok_or_else(|| {
          DocError::Resolve(format!(
            "Unable to load specifier: \"{}\"",
            specifier
          ))
        })?
    } else {
      module
    };

    let module_doc = self.parse_module(&module.specifier)?;

    let flattened_docs = if !module_doc.reexports.is_empty() {
      let mut flattened_reexports =
        self.flatten_reexports(&module_doc.reexports, &module.specifier)?;
      flattened_reexports.extend(module_doc.definitions);
      flattened_reexports
    } else {
      module_doc.definitions
    };

    Ok(flattened_docs)
  }

  fn get_doc_nodes_for_module_imports(
    &self,
    parsed_source: &ParsedSource,
    module_body: Vec<deno_ast::swc::ast::ModuleItem>,
    referrer: &ModuleSpecifier,
  ) -> Result<Vec<DocNode>, DocError> {
    let mut imports = vec![];

    for node in module_body.iter() {
      if let deno_ast::swc::ast::ModuleItem::ModuleDecl(ModuleDecl::Import(
        import_decl,
      )) = node
      {
        let js_doc = js_doc_for_range(parsed_source, &import_decl.range());
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

          let resolved_specifier = self
            .graph
            .resolve_dependency(&src, referrer, true)
            .ok_or_else(|| DocError::Resolve(src.clone()))?;
          let import_def = ImportDef {
            src: resolved_specifier.to_string(),
            imported: maybe_imported_name,
          };

          let doc_node =
            DocNode::import(name, location.clone(), js_doc.clone(), import_def);

          imports.push(doc_node);
        }
      }
    }

    Ok(imports)
  }

  pub fn get_doc_nodes_for_module_exports(
    &self,
    parsed_source: &ParsedSource,
    module_decl: &ModuleDecl,
  ) -> Vec<DocNode> {
    match module_decl {
      ModuleDecl::ExportDecl(export_decl) => {
        super::module::get_doc_node_for_export_decl(
          self,
          parsed_source,
          export_decl,
        )
      }
      ModuleDecl::ExportDefaultDecl(export_default_decl) => {
        let js_doc =
          js_doc_for_range(parsed_source, &export_default_decl.range());
        let location = get_location(parsed_source, export_default_decl.start());
        let name = "default".to_string();

        let doc_node = match &export_default_decl.decl {
          DefaultDecl::Class(class_expr) => {
            let (class_def, decorator_js_doc) =
              crate::class::class_to_class_def(
                parsed_source,
                &class_expr.class,
              );
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

        vec![doc_node]
      }
      _ => vec![],
    }
  }

  pub fn get_doc_node_for_decl(
    &self,
    parsed_source: &ParsedSource,
    decl: &Decl,
  ) -> Option<Vec<DocNode>> {
    match decl {
      Decl::Class(class_decl) => {
        // declared classes cannot have decorators, so we ignore that return
        let (name, class_def, _) =
          super::class::get_doc_for_class_decl(parsed_source, class_decl);
        let js_doc = js_doc_for_range(parsed_source, &class_decl.class.range());
        let location = get_location(parsed_source, class_decl.class.start());
        Some(vec![DocNode::class(
          name,
          location,
          DeclarationKind::Declare,
          js_doc,
          class_def,
        )])
      }
      Decl::Fn(fn_decl) => {
        let (name, function_def) =
          super::function::get_doc_for_fn_decl(parsed_source, fn_decl);
        let js_doc = js_doc_for_range(parsed_source, &fn_decl.function.range());
        let location = get_location(parsed_source, fn_decl.function.start());
        Some(vec![DocNode::function(
          name,
          location,
          DeclarationKind::Declare,
          js_doc,
          function_def,
        )])
      }
      Decl::Var(var_decl) => Some(
        super::variable::get_doc_for_var_decl(var_decl)
          .into_iter()
          .map(|(name, var_def)| {
            let js_doc = js_doc_for_range(parsed_source, &var_decl.range());
            let location = get_location(parsed_source, var_decl.start());
            DocNode::variable(
              name,
              location,
              DeclarationKind::Declare,
              js_doc,
              var_def,
            )
          })
          .collect(),
      ),
      Decl::TsInterface(ts_interface_decl) => {
        let (name, interface_def) =
          super::interface::get_doc_for_ts_interface_decl(
            parsed_source,
            ts_interface_decl,
          );
        let js_doc =
          js_doc_for_range(parsed_source, &ts_interface_decl.range());
        let location = get_location(parsed_source, ts_interface_decl.start());
        Some(vec![DocNode::interface(
          name,
          location,
          DeclarationKind::Declare,
          js_doc,
          interface_def,
        )])
      }
      Decl::TsTypeAlias(ts_type_alias) => {
        let (name, type_alias_def) =
          super::type_alias::get_doc_for_ts_type_alias_decl(
            parsed_source,
            ts_type_alias,
          );
        let js_doc = js_doc_for_range(parsed_source, &ts_type_alias.range());
        let location = get_location(parsed_source, ts_type_alias.start());
        Some(vec![DocNode::type_alias(
          name,
          location,
          DeclarationKind::Declare,
          js_doc,
          type_alias_def,
        )])
      }
      Decl::TsEnum(ts_enum) => {
        let (name, enum_def) =
          super::r#enum::get_doc_for_ts_enum_decl(parsed_source, ts_enum);
        let js_doc = js_doc_for_range(parsed_source, &ts_enum.range());
        let location = get_location(parsed_source, ts_enum.start());
        Some(vec![DocNode::r#enum(
          name,
          location,
          DeclarationKind::Declare,
          js_doc,
          enum_def,
        )])
      }
      Decl::TsModule(ts_module) => {
        let (name, namespace_def) = super::namespace::get_doc_for_ts_module(
          self,
          parsed_source,
          ts_module,
        );
        let js_doc = js_doc_for_range(parsed_source, &ts_module.range());
        let location = get_location(parsed_source, ts_module.start());
        Some(vec![DocNode::namespace(
          name,
          location,
          DeclarationKind::Declare,
          js_doc,
          namespace_def,
        )])
      }
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

  pub fn get_reexports_for_module_body(
    &self,
    module_body: &[deno_ast::swc::ast::ModuleItem],
  ) -> Vec<node::Reexport> {
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

  fn get_symbols_for_module_body(
    &self,
    parsed_source: &ParsedSource,
    module_body: &[deno_ast::swc::ast::ModuleItem],
  ) -> HashMap<String, DocNode> {
    let mut symbols = HashMap::new();

    for node in module_body.iter() {
      let doc_nodes = match node {
        ModuleItem::Stmt(Stmt::Decl(decl)) => {
          self.get_doc_node_for_decl(parsed_source, decl)
        }
        ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export_decl)) => {
          Some(super::module::get_doc_node_for_export_decl(
            self,
            parsed_source,
            export_decl,
          ))
        }
        _ => None,
      };

      if let Some(doc_nodes) = doc_nodes {
        for doc_node in doc_nodes {
          symbols.insert(doc_node.name.clone(), doc_node);
        }
      }
    }

    symbols
  }

  pub fn get_doc_nodes_for_module_body(
    &self,
    parsed_source: &ParsedSource,
    module_body: Vec<deno_ast::swc::ast::ModuleItem>,
  ) -> Vec<DocNode> {
    let symbols = self.get_symbols_for_module_body(parsed_source, &module_body);

    let mut doc_entries: Vec<DocNode> = Vec::new();
    let mut ambient_entries: Vec<DocNode> = Vec::new();

    let mut is_ambient = true;

    // check to see if there is a module level JSDoc for the source file
    if let Some((js_doc, range)) = module_js_doc_for_source(parsed_source) {
      let doc_node =
        DocNode::module_doc(get_location(parsed_source, range.start), js_doc);
      doc_entries.push(doc_node);
    }

    for node in module_body.iter() {
      match node {
        ModuleItem::Stmt(stmt) => {
          if let Stmt::Decl(decl) = stmt {
            if let Some(doc_nodes) =
              self.get_doc_node_for_decl(parsed_source, decl)
            {
              let is_declared = self.get_declare_for_decl(decl);
              for mut doc_node in doc_nodes {
                if self.private {
                  doc_node.declaration_kind = if is_declared {
                    DeclarationKind::Declare
                  } else {
                    DeclarationKind::Private
                  };
                  doc_entries.push(doc_node);
                } else if is_declared {
                  doc_node.declaration_kind = DeclarationKind::Declare;
                  ambient_entries.push(doc_node)
                }
              }
            }
          }
        }

        ModuleItem::ModuleDecl(module_decl) => {
          // If it has imports/exports, it isn't ambient.
          is_ambient = false;

          doc_entries.extend(
            self.get_doc_nodes_for_module_exports(parsed_source, module_decl),
          );

          match module_decl {
            ModuleDecl::ExportNamed(export_named) => {
              for specifier in &export_named.specifiers {
                match specifier {
                  ExportSpecifier::Named(named_specifier) => {
                    let symbol =
                      module_export_name_value(&named_specifier.orig);
                    if let Some(doc_node) = symbols.get(&symbol) {
                      let mut doc_node = doc_node.clone();
                      if let Some(exported) = &named_specifier.exported {
                        doc_node.name = module_export_name_value(exported)
                      }
                      doc_node.declaration_kind = DeclarationKind::Export;
                      doc_entries.push(doc_node)
                    }
                  }
                  // TODO(zhmushan)
                  ExportSpecifier::Default(_default_specifier) => {}
                  ExportSpecifier::Namespace(_namespace_specifier) => {}
                }
              }
            }
            ModuleDecl::ExportDefaultExpr(export_expr) => {
              if let Expr::Ident(ident) = export_expr.expr.as_ref() {
                if let Some(doc_node) = symbols.get(&ident.sym.to_string()) {
                  doc_entries.push(DocNode {
                    name: String::from("default"),
                    declaration_kind: DeclarationKind::Export,
                    ..doc_node.clone()
                  });
                }
              } else {
                let js_doc =
                  js_doc_for_range(parsed_source, &export_expr.range());
                let location = get_location(parsed_source, export_expr.start());
                doc_entries.push(DocNode::variable(
                  String::from("default"),
                  location,
                  DeclarationKind::Export,
                  js_doc,
                  super::variable::VariableDef {
                    kind: deno_ast::swc::ast::VarDeclKind::Var,
                    ts_type: super::ts_type::infer_ts_type_from_expr(
                      export_expr.expr.as_ref(),
                      true,
                    ),
                  },
                ));
              }
            }
            _ => {}
          }
        }
      }
    }

    if is_ambient {
      doc_entries.extend(ambient_entries);
    }

    doc_entries
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
    }
  }
}
