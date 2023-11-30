// Copyright 2020-2023 the Deno authors. All rights reserved. MIT license.

use crate::js_doc::JsDoc;
use crate::node::DeclarationKind;
use crate::node::DocNode;
use crate::node::NamespaceDef;
use crate::ts_type::TsTypeDef;
use crate::util::swc::get_text_info_location;
use crate::util::swc::has_ignorable_js_doc_tag;
use crate::util::symbol::fully_qualified_symbol_name;
use crate::util::symbol::symbol_has_ignorable_js_doc_tag;
use crate::variable::VariableDef;
use crate::DocNodeKind;
use crate::Location;

use deno_ast::swc::ast::Accessibility;
use deno_ast::SourceRange;
use deno_graph::symbols::ModuleInfoRef;
use deno_graph::symbols::Symbol;
use deno_graph::symbols::UniqueSymbolId;

use std::borrow::Cow;
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DocDiagnosticKind {
  MissingJsDoc,
  MissingExplicitType,
  MissingReturnType,
  PrivateTypeRef {
    name: String,
    reference: String,
    /// The location of the reference.
    reference_location: Location,
  },
}

#[derive(Debug, Clone)]
pub struct DocDiagnostic {
  pub location: Location,
  pub kind: DocDiagnosticKind,
}

impl DocDiagnostic {
  pub fn message(&self) -> Cow<str> {
    match &self.kind {
      DocDiagnosticKind::MissingJsDoc => {
        Cow::Borrowed("Missing JSDoc comment.")
      }
      DocDiagnosticKind::MissingExplicitType => {
        Cow::Borrowed("Missing explicit type.")
      }
      DocDiagnosticKind::MissingReturnType => {
        Cow::Borrowed("Missing return type.")
      }
      DocDiagnosticKind::PrivateTypeRef {
        name,
        reference,
        reference_location,
      } => {
        let mut message =
          format!("Type '{}' references type '{}' ", name, reference);
        if reference_location.filename != self.location.filename {
          message.push_str(&format!(
            "({}:{}:{}) ",
            reference_location.filename,
            reference_location.line,
            reference_location.col + 1
          ));
        }
        message.push_str("which is not exported from a root module.");
        Cow::Owned(message)
      }
    }
  }
}

#[derive(Default)]
pub struct DiagnosticsCollector {
  seen_private_types_in_public: HashSet<(UniqueSymbolId, UniqueSymbolId)>,
  seen_jsdoc_missing: HashSet<Location>,
  seen_missing_type_refs: HashSet<Location>,
  diagnostics: Vec<DocDiagnostic>,
}

impl DiagnosticsCollector {
  pub fn add_private_type_in_public(
    &mut self,
    decl_module: ModuleInfoRef,
    decl_name: &str,
    decl_range: SourceRange,
    doc_symbol_id: UniqueSymbolId,
    referenced_module: ModuleInfoRef,
    referenced_symbol: &Symbol,
  ) {
    if !self.seen_private_types_in_public.insert((
      doc_symbol_id,
      UniqueSymbolId::new(
        referenced_module.module_id(),
        referenced_symbol.symbol_id(),
      ),
    )) {
      return;
    }
    if symbol_has_ignorable_js_doc_tag(referenced_module, referenced_symbol) {
      return; // ignore
    }
    let Some(reference) =
      fully_qualified_symbol_name(referenced_module, referenced_symbol)
    else {
      return;
    };

    self.diagnostics.push(DocDiagnostic {
      location: get_text_info_location(
        decl_module.specifier().as_str(),
        decl_module.text_info(),
        decl_range.start,
      ),
      kind: DocDiagnosticKind::PrivateTypeRef {
        name: decl_name.to_string(),
        reference: reference.to_string(),
        reference_location: referenced_symbol
          .decls()
          .iter()
          .next()
          .map(|d| {
            get_text_info_location(
              referenced_module.specifier().as_str(),
              referenced_module.text_info(),
              d.range.start,
            )
          })
          // should never happen, but just in case
          .unwrap_or_else(|| Location {
            filename: referenced_module.specifier().to_string(),
            line: 1,
            col: 1,
          }),
      },
    })
  }

  pub fn take_diagnostics(&mut self) -> Vec<DocDiagnostic> {
    let inner = std::mem::take(&mut self.diagnostics);
    *self = Default::default(); // reset
    inner
  }

  pub fn analyze_doc_nodes(&mut self, doc_nodes: &[DocNode]) {
    DiagnosticDocNodeVisitor { diagnostics: self }.visit_doc_nodes(doc_nodes)
  }

  fn check_missing_js_doc(&mut self, js_doc: &JsDoc, location: &Location) {
    if js_doc.doc.is_none()
      && !has_ignorable_js_doc_tag(js_doc)
      && self.seen_jsdoc_missing.insert(location.clone())
    {
      self.diagnostics.push(DocDiagnostic {
        location: location.clone(),
        kind: DocDiagnosticKind::MissingJsDoc,
      })
    }
  }

  fn check_missing_explicit_type(
    &mut self,
    ts_type: Option<&TsTypeDef>,
    js_doc: &JsDoc,
    location: &Location,
  ) {
    if ts_type.is_none()
      && !has_ignorable_js_doc_tag(js_doc)
      && self.seen_missing_type_refs.insert(location.clone())
    {
      self.diagnostics.push(DocDiagnostic {
        location: location.clone(),
        kind: DocDiagnosticKind::MissingExplicitType,
      })
    }
  }

  fn check_missing_return_type(
    &mut self,
    return_type: Option<&TsTypeDef>,
    js_doc: &JsDoc,
    location: &Location,
  ) {
    if return_type.is_none()
      && !has_ignorable_js_doc_tag(js_doc)
      && self.seen_missing_type_refs.insert(location.clone())
    {
      self.diagnostics.push(DocDiagnostic {
        location: location.clone(),
        kind: DocDiagnosticKind::MissingReturnType,
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
      if doc_node.location.filename.starts_with("http") {
        continue; // don't report diagnostics on a remote module
      }

      if let Some(last_node) = last_node {
        if doc_node.name == last_node.name && last_node.function_def.is_some() {
          if let Some(current_fn) = &doc_node.function_def {
            if current_fn.has_body {
              continue; // it's an overload. Ignore it
            }
          }
        }
      }

      if !has_ignorable_js_doc_tag(&doc_node.js_doc) {
        self.visit_doc_node(doc_node);
      }

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

    if is_js_docable_kind(&doc_node.kind) {
      self
        .diagnostics
        .check_missing_js_doc(&doc_node.js_doc, &doc_node.location);
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
      self
        .diagnostics
        .check_missing_js_doc(&prop.js_doc, &prop.location);
      self.diagnostics.check_missing_explicit_type(
        prop.ts_type.as_ref(),
        &prop.js_doc,
        &prop.location,
      )
    }

    // index signatures
    for sig in &def.index_signatures {
      self
        .diagnostics
        .check_missing_js_doc(&sig.js_doc, &sig.location);
      self.diagnostics.check_missing_explicit_type(
        sig.ts_type.as_ref(),
        &sig.js_doc,
        &sig.location,
      )
    }

    // methods
    let mut last_name: Option<&str> = None;
    for method in &def.methods {
      if let Some(last_name) = last_name {
        if method.name == last_name && method.function_def.has_body {
          continue; // skip, it's the implementation signature
        }
      }

      self
        .diagnostics
        .check_missing_js_doc(&method.js_doc, &method.location);
      self.diagnostics.check_missing_return_type(
        method.function_def.return_type.as_ref(),
        &method.js_doc,
        &method.location,
      );

      last_name = Some(&method.name);
    }
  }

  fn visit_class_ctor_def(&mut self, ctor: &crate::class::ClassConstructorDef) {
    self
      .diagnostics
      .check_missing_js_doc(&ctor.js_doc, &ctor.location);
  }

  fn visit_function_def(
    &mut self,
    parent: &DocNode,
    def: &crate::function::FunctionDef,
  ) {
    self
      .diagnostics
      .check_missing_js_doc(&parent.js_doc, &parent.location);
    self.diagnostics.check_missing_return_type(
      def.return_type.as_ref(),
      &parent.js_doc,
      &parent.location,
    );
  }

  fn visit_interface_def(&mut self, def: &crate::interface::InterfaceDef) {
    // properties
    for prop in &def.properties {
      self
        .diagnostics
        .check_missing_js_doc(&prop.js_doc, &prop.location);

      self.diagnostics.check_missing_explicit_type(
        prop.ts_type.as_ref(),
        &prop.js_doc,
        &prop.location,
      )
    }

    // index signatures
    for sig in &def.index_signatures {
      self
        .diagnostics
        .check_missing_js_doc(&sig.js_doc, &sig.location);
      self.diagnostics.check_missing_explicit_type(
        sig.ts_type.as_ref(),
        &sig.js_doc,
        &sig.location,
      );
    }

    // methods
    for method in &def.methods {
      self
        .diagnostics
        .check_missing_js_doc(&method.js_doc, &method.location);
      self.diagnostics.check_missing_return_type(
        method.return_type.as_ref(),
        &method.js_doc,
        &method.location,
      );
    }
  }

  fn visit_namespace_def(&mut self, def: &NamespaceDef) {
    self.visit_doc_nodes(&def.elements);
  }

  fn visit_variable_def(&mut self, parent: &DocNode, def: &VariableDef) {
    self.diagnostics.check_missing_explicit_type(
      def.ts_type.as_ref(),
      &parent.js_doc,
      &parent.location,
    );
  }
}
