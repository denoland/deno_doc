// Copyright 2020-2023 the Deno authors. All rights reserved. MIT license.

use crate::node::DeclarationKind;
use crate::node::DocNode;
use crate::node::NamespaceDef;
use crate::swc_util::get_location;
use crate::variable::VariableDef;
use crate::DocNodeKind;
use crate::Location;

use deno_ast::swc::ast::Accessibility;
use deno_ast::swc::ast::DefaultDecl;
use deno_ast::SourceRange;
use deno_ast::SourceRangedForSpanned;
use deno_graph::type_tracer::EsmModuleSymbol;
use deno_graph::type_tracer::ExportDeclRef;
use deno_graph::type_tracer::SymbolId;
use deno_graph::type_tracer::SymbolNodeRef;
use deno_graph::type_tracer::UniqueSymbolId;

use std::collections::HashSet;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DocDiagnosticKind {
  MissingJsDoc,
  MissingTypeRef,
  PrivateTypeRef,
}

impl std::fmt::Display for DocDiagnosticKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      DocDiagnosticKind::MissingJsDoc => {
        f.write_str("Type is missing a JS documentation comment.")
      }
      DocDiagnosticKind::MissingTypeRef => f.write_str(
        "Missing explicit type. Add one to improve documentation and type checking performance.",
      ),
      DocDiagnosticKind::PrivateTypeRef => {
        f.write_str("Type is not exported, but referenced by an exported type.")
      }
    }
  }
}

#[derive(Debug, Clone)]
pub struct DocDiagnostic {
  pub location: Location,
  pub kind: DocDiagnosticKind,
}

#[derive(Default)]
pub struct DiagnosticsCollector {
  seen_private_types_in_public: HashSet<UniqueSymbolId>,
  seen_jsdoc_missing: HashSet<Location>,
  seen_missing_type_refs: HashSet<Location>,
  diagnostics: Vec<DocDiagnostic>,
}

impl DiagnosticsCollector {
  pub fn add_private_type_in_public(
    &mut self,
    module: &EsmModuleSymbol,
    symbol_id: SymbolId,
    node_ref: SymbolNodeRef,
  ) {
    let unique_id = UniqueSymbolId {
      module_id: module.module_id(),
      symbol_id,
    };
    if self.seen_private_types_in_public.insert(unique_id) {
      let ident_range = get_ident_range_of_node(node_ref);
      self.diagnostics.push(DocDiagnostic {
        location: get_location(module.source(), ident_range.start),
        kind: DocDiagnosticKind::PrivateTypeRef,
      })
    }
  }

  pub fn take_diagnostics(&mut self) -> Vec<DocDiagnostic> {
    let inner = std::mem::take(&mut self.diagnostics);
    *self = Default::default(); // reset
    inner
  }

  pub fn analyze_doc_nodes(&mut self, doc_nodes: &[DocNode]) {
    DiagnosticDocNodeVisitor { diagnostics: self }.visit_doc_nodes(doc_nodes)
  }

  fn add_missing_js_doc(&mut self, location: &Location) {
    if self.seen_jsdoc_missing.insert(location.clone()) {
      self.diagnostics.push(DocDiagnostic {
        location: location.clone(),
        kind: DocDiagnosticKind::MissingJsDoc,
      })
    }
  }

  fn add_missing_type_ref(&mut self, location: &Location) {
    if self.seen_missing_type_refs.insert(location.clone()) {
      self.diagnostics.push(DocDiagnostic {
        location: location.clone(),
        kind: DocDiagnosticKind::MissingTypeRef,
      })
    }
  }
}

struct DiagnosticDocNodeVisitor<'a> {
  diagnostics: &'a mut DiagnosticsCollector,
}

impl<'a> DiagnosticDocNodeVisitor<'a> {
  pub fn visit_doc_nodes(&mut self, doc_nodes: &[DocNode]) {
    let mut last_node: Option<&DocNode> = None;
    for doc_node in doc_nodes {
      if let Some(last_node) = last_node {
        if doc_node.name == last_node.name && last_node.function_def.is_some() {
          if let Some(current_fn) = &doc_node.function_def {
            if current_fn.has_body {
              continue; // it's an overload. Ignore it
            }
          }
        }
      }

      self.visit_doc_node(doc_node);

      last_node = Some(doc_node);
    }
  }

  fn visit_doc_node(&mut self, doc_node: &DocNode) {
    fn is_js_docable_kind(kind: &DocNodeKind) -> bool {
      match kind {
        DocNodeKind::Class
        | DocNodeKind::Enum
        | DocNodeKind::Function
        | DocNodeKind::Interface
        | DocNodeKind::Namespace
        | DocNodeKind::TypeAlias
        | DocNodeKind::Variable => true,
        DocNodeKind::Import | DocNodeKind::ModuleDoc => false,
      }
    }

    if doc_node.declaration_kind == DeclarationKind::Private {
      return; // skip, we don't do these diagnostics above private nodes
    }

    if doc_node.js_doc.doc.is_none() && is_js_docable_kind(&doc_node.kind) {
      self.diagnostics.add_missing_js_doc(&doc_node.location);
    }

    if let Some(def) = &doc_node.class_def {
      self.visit_class_def(def);
    }

    if let Some(def) = &doc_node.function_def {
      self.visit_function_def(doc_node, def);
    }

    if let Some(def) = &doc_node.interface_def {
      self.visit_interface_def(def);
    }

    if let Some(def) = &doc_node.namespace_def {
      self.visit_namespace_def(def);
    }

    if let Some(def) = &doc_node.variable_def {
      self.visit_variable_def(doc_node, def);
    }
  }

  fn visit_class_def(&mut self, def: &crate::class::ClassDef) {
    // ctors
    if def.constructors.len() == 1 {
      self.visit_class_ctor_def(&def.constructors[0]);
    } else if !def.constructors.is_empty() {
      // skip the first one
      let ctors = &def.constructors[1..];
      for ctor in ctors {
        self.visit_class_ctor_def(ctor);
      }
    }

    // properties
    for prop in &def.properties {
      if prop.accessibility == Some(Accessibility::Private) {
        continue; // don't do diagnostics for private types
      }
      if prop.js_doc.doc.is_none() {
        self.diagnostics.add_missing_js_doc(&prop.location);
      }
      if prop.ts_type.is_none() {
        self.diagnostics.add_missing_type_ref(&prop.location)
      }
    }

    // index signatures
    for sig in &def.index_signatures {
      if sig.js_doc.doc.is_none() {
        self.diagnostics.add_missing_js_doc(&sig.location);
      }
      if sig.ts_type.is_none() {
        self.diagnostics.add_missing_type_ref(&sig.location)
      }
    }

    // methods
    let mut last_name: Option<&str> = None;
    for method in &def.methods {
      if let Some(last_name) = last_name {
        if method.name == last_name && method.function_def.has_body {
          continue; // skip, it's the implementation signature
        }
      }
      if method.js_doc.doc.is_none() {
        self.diagnostics.add_missing_js_doc(&method.location);
      }
      if method.function_def.return_type.is_none() {
        self.diagnostics.add_missing_type_ref(&method.location)
      }

      last_name = Some(&method.name);
    }
  }

  fn visit_class_ctor_def(&mut self, ctor: &crate::class::ClassConstructorDef) {
    if ctor.js_doc.doc.is_none() {
      self.diagnostics.add_missing_js_doc(&ctor.location);
    }
  }

  fn visit_function_def(
    &mut self,
    parent: &DocNode,
    def: &crate::function::FunctionDef,
  ) {
    if parent.js_doc.doc.is_none() {
      self.diagnostics.add_missing_js_doc(&parent.location);
    }
    if def.return_type.is_none() {
      self.diagnostics.add_missing_type_ref(&parent.location)
    }
  }

  fn visit_interface_def(&mut self, def: &crate::interface::InterfaceDef) {
    // properties
    for prop in &def.properties {
      if prop.js_doc.doc.is_none() {
        self.diagnostics.add_missing_js_doc(&prop.location);
      }
      if prop.ts_type.is_none() {
        self.diagnostics.add_missing_type_ref(&prop.location)
      }
    }

    // index signatures
    for sig in &def.index_signatures {
      if sig.js_doc.doc.is_none() {
        self.diagnostics.add_missing_js_doc(&sig.location);
      }
      if sig.ts_type.is_none() {
        self.diagnostics.add_missing_type_ref(&sig.location)
      }
    }

    // methods
    for method in &def.methods {
      if method.js_doc.doc.is_none() {
        self.diagnostics.add_missing_js_doc(&method.location);
      }
      if method.return_type.is_none() {
        self.diagnostics.add_missing_type_ref(&method.location)
      }
    }
  }

  fn visit_namespace_def(&mut self, def: &NamespaceDef) {
    self.visit_doc_nodes(&def.elements);
  }

  fn visit_variable_def(&mut self, parent: &DocNode, def: &VariableDef) {
    if def.ts_type.is_none() {
      self.diagnostics.add_missing_type_ref(&parent.location);
    }
  }
}

fn get_ident_range_of_node(node_ref: SymbolNodeRef) -> SourceRange {
  match node_ref {
    SymbolNodeRef::ClassDecl(n) => n.ident.range(),
    SymbolNodeRef::ExportDecl(_, n) => match n {
      ExportDeclRef::Class(n) => n.ident.range(),
      ExportDeclRef::Fn(n) => n.ident.range(),
      ExportDeclRef::Var(_, _, ident) => ident.range(),
      ExportDeclRef::TsEnum(n) => n.id.range(),
      ExportDeclRef::TsInterface(n) => n.id.range(),
      ExportDeclRef::TsModule(n) => n.id.range(),
      ExportDeclRef::TsTypeAlias(n) => n.id.range(),
    },
    SymbolNodeRef::ExportDefaultDecl(n) => match &n.decl {
      DefaultDecl::Class(n) => match &n.ident {
        Some(n) => n.range(),
        None => SourceRange::new(n.start(), n.start() + "default".len()),
      },
      DefaultDecl::Fn(n) => match &n.ident {
        Some(n) => n.range(),
        None => SourceRange::new(n.start(), n.start() + "default".len()),
      },
      DefaultDecl::TsInterfaceDecl(n) => n.id.range(),
    },
    SymbolNodeRef::ExportDefaultExprLit(_, lit) => lit.range(),
    SymbolNodeRef::FnDecl(n) => n.ident.range(),
    SymbolNodeRef::TsEnum(n) => n.id.range(),
    SymbolNodeRef::TsInterface(n) => n.id.range(),
    SymbolNodeRef::TsNamespace(n) => n.id.range(),
    SymbolNodeRef::TsTypeAlias(n) => n.id.range(),
    SymbolNodeRef::Var(_, _, ident) => ident.range(),
  }
}
