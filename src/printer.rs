// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use crate::display::display_abstract;
use crate::display::display_async;
use crate::display::display_generator;
use crate::display::SliceDisplayer;
use crate::js_doc::JsDoc;
use crate::js_doc::JsDocTag;
use crate::node::DeclarationKind;
use crate::node::DocNode;
use crate::node::DocNodeDef;
use crate::Location;

use deno_terminal::colors;
use deno_terminal::colors::Style;

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result as FmtResult;

fn italic_cyan<'a, S: Display + 'a>(s: S) -> Style<Style<S>> {
  colors::italic(colors::cyan(s))
}

pub struct DocPrinter<'a> {
  doc_nodes: &'a [DocNode],
  use_color: bool,
  private: bool,
}

impl DocPrinter<'_> {
  pub fn new(
    doc_nodes: &[DocNode],
    use_color: bool,
    private: bool,
  ) -> DocPrinter {
    DocPrinter {
      doc_nodes,
      use_color,
      private,
    }
  }

  pub fn format(&self, w: &mut Formatter<'_>) -> FmtResult {
    self.format_with_indent(w, self.doc_nodes, 0)
  }

  fn format_with_indent(
    &self,
    w: &mut Formatter<'_>,
    doc_nodes: &[DocNode],
    indent: i64,
  ) -> FmtResult {
    colors::set_use_color(self.use_color);

    let mut sorted = Vec::from(doc_nodes);
    sorted.sort_unstable_by(|a, b| {
      let kind_cmp = self.kind_order(a).cmp(&self.kind_order(b));
      if kind_cmp == core::cmp::Ordering::Equal {
        a.name.cmp(&b.name)
      } else {
        kind_cmp
      }
    });

    for node in &sorted {
      let has_overloads = if matches!(node.def, DocNodeDef::Function { .. }) {
        sorted
          .iter()
          .filter(|n| {
            matches!(n.def, DocNodeDef::Function { .. }) && n.name == node.name
          })
          .count()
          > 1
      } else {
        false
      };

      if !has_overloads
        || node
          .function_def()
          .map(|def| !def.has_body)
          .unwrap_or(false)
      {
        write!(
          w,
          "{}",
          colors::italic_gray(&format!(
            "Defined in {}\n\n",
            get_location_string(&node.location),
          ))
        )?;
      }

      self.format_signature(w, node, indent, has_overloads)?;

      self.format_jsdoc(w, &node.js_doc, indent + 1)?;
      writeln!(w)?;

      match node.def {
        DocNodeDef::Class { .. } => self.format_class(w, node)?,
        DocNodeDef::Enum { .. } => self.format_enum(w, node)?,
        DocNodeDef::Interface { .. } => self.format_interface(w, node)?,
        DocNodeDef::Namespace { .. } => self.format_namespace(w, node)?,
        _ => {}
      }
    }

    if self.use_color {
      colors::set_use_color(false);
    }

    Ok(())
  }

  fn kind_order(&self, node: &DocNode) -> i64 {
    match node.def {
      DocNodeDef::ModuleDoc { .. } => 0,
      DocNodeDef::Function { .. } => 1,
      DocNodeDef::Variable { .. } => 2,
      DocNodeDef::Class { .. } => 3,
      DocNodeDef::Enum { .. } => 4,
      DocNodeDef::Interface { .. } => 5,
      DocNodeDef::TypeAlias { .. } => 6,
      DocNodeDef::Namespace { .. } => 7,
      DocNodeDef::Import { .. } => 8,
      DocNodeDef::Reference { .. } => 9,
    }
  }

  fn format_signature(
    &self,
    w: &mut Formatter<'_>,
    node: &DocNode,
    indent: i64,
    has_overloads: bool,
  ) -> FmtResult {
    match node.def {
      DocNodeDef::ModuleDoc { .. } => self.format_module_doc(w, node, indent),
      DocNodeDef::Function { .. } => {
        self.format_function_signature(w, node, indent, has_overloads)
      }
      DocNodeDef::Variable { .. } => {
        self.format_variable_signature(w, node, indent)
      }
      DocNodeDef::Class { .. } => self.format_class_signature(w, node, indent),
      DocNodeDef::Enum { .. } => self.format_enum_signature(w, node, indent),
      DocNodeDef::Interface { .. } => {
        self.format_interface_signature(w, node, indent)
      }
      DocNodeDef::TypeAlias { .. } => {
        self.format_type_alias_signature(w, node, indent)
      }
      DocNodeDef::Namespace { .. } => {
        self.format_namespace_signature(w, node, indent)
      }
      DocNodeDef::Import { .. } => Ok(()),
      DocNodeDef::Reference { .. } => {
        self.format_reference_signature(w, node, indent)
      }
    }
  }

  fn format_jsdoc(
    &self,
    w: &mut Formatter<'_>,
    js_doc: &JsDoc,
    indent: i64,
  ) -> FmtResult {
    if let Some(doc) = &js_doc.doc {
      for line in doc.lines() {
        writeln!(w, "{}{}", Indent(indent), colors::gray(line))?;
      }
    }
    if !js_doc.tags.is_empty() {
      writeln!(w)?;
    }
    for tag in js_doc.tags.iter() {
      self.format_jsdoc_tag(w, tag, indent)?;
    }
    Ok(())
  }

  fn format_jsdoc_tag_maybe_doc(
    &self,
    w: &mut Formatter<'_>,
    maybe_doc: &Option<Box<str>>,
    indent: i64,
  ) -> FmtResult {
    if let Some(doc) = maybe_doc {
      for line in doc.lines() {
        writeln!(w, "{}{}", Indent(indent + 2), colors::gray(line))?;
      }
      writeln!(w)
    } else {
      Ok(())
    }
  }

  fn format_jsdoc_tag_doc(
    &self,
    w: &mut Formatter<'_>,
    doc: &str,
    indent: i64,
  ) -> FmtResult {
    for line in doc.lines() {
      writeln!(w, "{}{}", Indent(indent + 2), colors::gray(line))?;
    }
    writeln!(w)
  }

  fn format_jsdoc_tag(
    &self,
    w: &mut Formatter<'_>,
    tag: &JsDocTag,
    indent: i64,
  ) -> FmtResult {
    match tag {
      JsDocTag::Callback { name, doc } => {
        writeln!(
          w,
          "{}@{} {}",
          Indent(indent),
          colors::magenta("callback"),
          colors::bold(name)
        )?;
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::Category { doc } => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("category"))?;
        self.format_jsdoc_tag_doc(w, doc, indent)
      }
      JsDocTag::Constructor => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("constructor"))
      }
      JsDocTag::Default { value, doc } => {
        writeln!(
          w,
          "{}@{} {{{}}}",
          Indent(indent),
          colors::magenta("default"),
          italic_cyan(value),
        )?;
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::Deprecated { doc } => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("deprecated"))?;
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::Enum { type_ref, doc } => {
        writeln!(
          w,
          "{}@{} {{{}}}",
          Indent(indent),
          colors::magenta("enum"),
          italic_cyan(type_ref),
        )?;
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::Example { doc } => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("example"))?;
        self.format_jsdoc_tag_doc(w, doc, indent)
      }
      JsDocTag::Experimental => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("experimental"))
      }
      JsDocTag::Extends { type_ref, doc } => {
        writeln!(
          w,
          "{}@{} {{{}}}",
          Indent(indent),
          colors::magenta("extends"),
          italic_cyan(type_ref)
        )?;
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::Ignore => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("ignore"))
      }
      JsDocTag::Internal => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("internal"))
      }
      JsDocTag::Module { name } => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("module"))?;
        self.format_jsdoc_tag_maybe_doc(w, name, indent)
      }
      JsDocTag::Param {
        name,
        type_ref,
        optional,
        default,
        doc,
      } => {
        write!(w, "{}@{}", Indent(indent), colors::magenta("param"))?;
        if let Some(type_ref) = type_ref {
          write!(w, " {{{}}}", italic_cyan(type_ref))?;
        }
        if *optional {
          write!(w, " [?]")?;
        } else if let Some(default) = default {
          write!(w, " [{}]", italic_cyan(default))?;
        }
        writeln!(w, " {}", colors::bold(name))?;
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::Public => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("public"))
      }
      JsDocTag::Private => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("private"))
      }
      JsDocTag::Property {
        name,
        type_ref,
        doc,
      } => {
        writeln!(
          w,
          "{}@{} {{{}}} {}",
          Indent(indent),
          colors::magenta("property"),
          italic_cyan(type_ref),
          colors::bold(name)
        )?;
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::Protected => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("protected"))
      }
      JsDocTag::ReadOnly => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("readonly"))
      }
      JsDocTag::Return { type_ref, doc } => {
        write!(w, "{}@{}", Indent(indent), colors::magenta("return"))?;
        if let Some(type_ref) = type_ref {
          writeln!(w, " {{{}}}", italic_cyan(type_ref))?;
        } else {
          writeln!(w)?;
        }
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::Tags { tags } => {
        writeln!(
          w,
          "{}@{} {}",
          Indent(indent),
          colors::magenta("tags"),
          tags.join(", "),
        )
      }
      JsDocTag::Template { name, doc } => {
        writeln!(
          w,
          "{}@{} {}",
          Indent(indent),
          colors::magenta("template"),
          colors::bold(name)
        )?;
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::This { type_ref, doc } => {
        writeln!(
          w,
          "{}@{} {{{}}}",
          Indent(indent),
          colors::magenta("this"),
          italic_cyan(type_ref)
        )?;
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::TypeDef {
        name,
        type_ref,
        doc,
      } => {
        writeln!(
          w,
          "{}@{} {{{}}} {}",
          Indent(indent),
          colors::magenta("typedef"),
          italic_cyan(type_ref),
          colors::bold(name)
        )?;
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::TypeRef { type_ref, doc } => {
        writeln!(
          w,
          "{}@{} {{{}}}",
          Indent(indent),
          colors::magenta("typeref"),
          italic_cyan(type_ref)
        )?;
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
      JsDocTag::Unsupported { value } => {
        let name = value.split_whitespace().next().unwrap_or("@");
        let value = &value[name.len()..];
        writeln!(
          w,
          "{}@{}{}",
          Indent(indent),
          colors::magenta(&name[1..]),
          value
        )
      }
      JsDocTag::See { doc } => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("see"))?;
        self.format_jsdoc_tag_doc(w, doc, indent)
      }
      JsDocTag::Since { doc } => {
        writeln!(w, "{}@{}", Indent(indent), colors::magenta("since"))?;
        self.format_jsdoc_tag_doc(w, doc, indent)
      }
      JsDocTag::Throws { type_ref, doc } => {
        write!(w, "{}@{}", Indent(indent), colors::magenta("return"))?;
        if let Some(type_ref) = type_ref {
          writeln!(w, " {{{}}}", italic_cyan(type_ref))?;
        } else {
          writeln!(w)?;
        }
        self.format_jsdoc_tag_maybe_doc(w, doc, indent)
      }
    }
  }

  fn format_class(&self, w: &mut Formatter<'_>, node: &DocNode) -> FmtResult {
    let class_def = node.class_def().unwrap();
    let has_overloads = class_def.constructors.len() > 1;
    for node in class_def.constructors.iter() {
      if !has_overloads || !node.has_body {
        writeln!(w, "{}{}", Indent(1), node,)?;
        self.format_jsdoc(w, &node.js_doc, 2)?;
      }
    }
    for node in class_def.properties.iter().filter(|node| {
      self.private
        || node
          .accessibility
          .unwrap_or(deno_ast::swc::ast::Accessibility::Public)
          != deno_ast::swc::ast::Accessibility::Private
    }) {
      for d in node.decorators.iter() {
        writeln!(w, "{}{}", Indent(1), d)?;
      }
      writeln!(w, "{}{}", Indent(1), node,)?;
      self.format_jsdoc(w, &node.js_doc, 2)?;
    }
    for index_sign_def in class_def.index_signatures.iter() {
      writeln!(w, "{}{}", Indent(1), index_sign_def)?;
    }
    for node in class_def.methods.iter().filter(|node| {
      self.private
        || node
          .accessibility
          .unwrap_or(deno_ast::swc::ast::Accessibility::Public)
          != deno_ast::swc::ast::Accessibility::Private
    }) {
      let has_overloads = class_def
        .methods
        .iter()
        .filter(|n| n.name == node.name)
        .count()
        > 1;
      if !has_overloads || !node.function_def.has_body {
        for d in node.function_def.decorators.iter() {
          writeln!(w, "{}{}", Indent(1), d)?;
        }
        writeln!(w, "{}{}", Indent(1), node,)?;
        self.format_jsdoc(w, &node.js_doc, 2)?;
      }
    }
    writeln!(w)
  }

  fn format_enum(&self, w: &mut Formatter<'_>, node: &DocNode) -> FmtResult {
    let enum_def = node.enum_def().unwrap();
    for member in &enum_def.members {
      writeln!(w, "{}{}", Indent(1), colors::bold(&member.name))?;
      self.format_jsdoc(w, &member.js_doc, 2)?;
    }
    writeln!(w)
  }

  fn format_interface(
    &self,
    w: &mut Formatter<'_>,
    node: &DocNode,
  ) -> FmtResult {
    let interface_def = node.interface_def().unwrap();

    for constructor in &interface_def.constructors {
      writeln!(w, "{}{}", Indent(1), constructor)?;
      self.format_jsdoc(w, &constructor.js_doc, 2)?;
    }
    for property_def in &interface_def.properties {
      writeln!(w, "{}{}", Indent(1), property_def)?;
      self.format_jsdoc(w, &property_def.js_doc, 2)?;
    }
    for method_def in &interface_def.methods {
      writeln!(w, "{}{}", Indent(1), method_def)?;
      self.format_jsdoc(w, &method_def.js_doc, 2)?;
    }
    for index_sign_def in &interface_def.index_signatures {
      writeln!(w, "{}{}", Indent(1), index_sign_def)?;
    }
    writeln!(w)
  }

  fn format_namespace(
    &self,
    w: &mut Formatter<'_>,
    node: &DocNode,
  ) -> FmtResult {
    let elements = &node.namespace_def().unwrap().elements;
    for node in elements {
      let has_overloads = if matches!(node.def, DocNodeDef::Function { .. }) {
        elements
          .iter()
          .filter(|n| {
            matches!(n.def, DocNodeDef::Function { .. }) && n.name == node.name
          })
          .count()
          > 1
      } else {
        false
      };
      self.format_signature(w, node, 1, has_overloads)?;
      self.format_jsdoc(w, &node.js_doc, 2)?;
    }
    writeln!(w)
  }

  fn format_class_signature(
    &self,
    w: &mut Formatter<'_>,
    node: &DocNode,
    indent: i64,
  ) -> FmtResult {
    let class_def = node.class_def().unwrap();
    for node in class_def.decorators.iter() {
      writeln!(w, "{}{}", Indent(indent), node)?;
    }
    write!(
      w,
      "{}{}{}{} {}",
      Indent(indent),
      fmt_visibility(node.declaration_kind),
      display_abstract(class_def.is_abstract),
      colors::magenta("class"),
      colors::bold(&node.name),
    )?;
    if !class_def.type_params.is_empty() {
      write!(
        w,
        "<{}>",
        SliceDisplayer::new(&class_def.type_params, ", ", false)
      )?;
    }

    if let Some(extends) = &class_def.extends {
      write!(w, " {} {}", colors::magenta("extends"), extends)?;
    }
    if !class_def.super_type_params.is_empty() {
      write!(
        w,
        "<{}>",
        SliceDisplayer::new(&class_def.super_type_params, ", ", false)
      )?;
    }

    if !class_def.implements.is_empty() {
      write!(
        w,
        " {} {}",
        colors::magenta("implements"),
        SliceDisplayer::new(&class_def.implements, ", ", false)
      )?;
    }

    writeln!(w)
  }

  fn format_enum_signature(
    &self,
    w: &mut Formatter<'_>,
    node: &DocNode,
    indent: i64,
  ) -> FmtResult {
    writeln!(
      w,
      "{}{}{} {}",
      Indent(indent),
      fmt_visibility(node.declaration_kind),
      colors::magenta("enum"),
      colors::bold(&node.name)
    )
  }

  fn format_function_signature(
    &self,
    w: &mut Formatter<'_>,
    node: &DocNode,
    indent: i64,
    has_overloads: bool,
  ) -> FmtResult {
    let function_def = node.function_def().unwrap();
    if !has_overloads || !function_def.has_body {
      write!(
        w,
        "{}{}{}{}{} {}",
        Indent(indent),
        fmt_visibility(node.declaration_kind),
        display_async(function_def.is_async),
        colors::magenta("function"),
        display_generator(function_def.is_generator),
        colors::bold(&node.name)
      )?;
      if !function_def.type_params.is_empty() {
        write!(
          w,
          "<{}>",
          SliceDisplayer::new(&function_def.type_params, ", ", false)
        )?;
      }
      write!(
        w,
        "({})",
        SliceDisplayer::new(&function_def.params, ", ", false)
      )?;
      if let Some(return_type) = &function_def.return_type {
        write!(w, ": {}", return_type)?;
      }
      writeln!(w)?;
    }
    Ok(())
  }

  fn format_interface_signature(
    &self,
    w: &mut Formatter<'_>,
    node: &DocNode,
    indent: i64,
  ) -> FmtResult {
    let interface_def = node.interface_def().unwrap();
    write!(
      w,
      "{}{}{} {}",
      Indent(indent),
      fmt_visibility(node.declaration_kind),
      colors::magenta("interface"),
      colors::bold(&node.name)
    )?;

    if !interface_def.type_params.is_empty() {
      write!(
        w,
        "<{}>",
        SliceDisplayer::new(&interface_def.type_params, ", ", false)
      )?;
    }

    if !interface_def.extends.is_empty() {
      write!(
        w,
        " {} {}",
        colors::magenta("extends"),
        SliceDisplayer::new(&interface_def.extends, ", ", false)
      )?;
    }

    writeln!(w)
  }

  fn format_module_doc(
    &self,
    _w: &mut Formatter<'_>,
    _node: &DocNode,
    _indent: i64,
  ) -> FmtResult {
    // currently we do not print out JSDoc in the printer, so there is nothing
    // to print.
    Ok(())
  }

  fn format_type_alias_signature(
    &self,
    w: &mut Formatter<'_>,
    node: &DocNode,
    indent: i64,
  ) -> FmtResult {
    let type_alias_def = node.type_alias_def().unwrap();
    write!(
      w,
      "{}{}{} {}",
      Indent(indent),
      fmt_visibility(node.declaration_kind),
      colors::magenta("type"),
      colors::bold(&node.name),
    )?;

    if !type_alias_def.type_params.is_empty() {
      write!(
        w,
        "<{}>",
        SliceDisplayer::new(&type_alias_def.type_params, ", ", false)
      )?;
    }

    writeln!(w, " = {}", type_alias_def.ts_type)
  }

  fn format_namespace_signature(
    &self,
    w: &mut Formatter<'_>,
    node: &DocNode,
    indent: i64,
  ) -> FmtResult {
    writeln!(
      w,
      "{}{}{} {}",
      Indent(indent),
      fmt_visibility(node.declaration_kind),
      colors::magenta("namespace"),
      colors::bold(&node.name)
    )
  }

  fn format_reference_signature(
    &self,
    w: &mut Formatter<'_>,
    node: &DocNode,
    indent: i64,
  ) -> FmtResult {
    let reference_def = node.reference_def().unwrap();

    writeln!(
      w,
      "{}{}{} {}: {}",
      Indent(indent),
      fmt_visibility(node.declaration_kind),
      colors::magenta("reference"),
      colors::bold(&node.name),
      colors::italic_gray(get_location_string(&reference_def.target)),
    )
  }

  fn format_variable_signature(
    &self,
    w: &mut Formatter<'_>,
    node: &DocNode,
    indent: i64,
  ) -> FmtResult {
    let variable_def = node.variable_def().unwrap();
    write!(
      w,
      "{}{}{} {}",
      Indent(indent),
      fmt_visibility(node.declaration_kind),
      colors::magenta(match variable_def.kind {
        deno_ast::swc::ast::VarDeclKind::Const => "const",
        deno_ast::swc::ast::VarDeclKind::Let => "let",
        deno_ast::swc::ast::VarDeclKind::Var => "var",
      }),
      colors::bold(&node.name),
    )?;
    if let Some(ts_type) = &variable_def.ts_type {
      write!(w, ": {}", ts_type)?;
    }
    writeln!(w)
  }
}

impl Display for DocPrinter<'_> {
  fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
    self.format(f)
  }
}

fn fmt_visibility(decl_kind: DeclarationKind) -> impl std::fmt::Display {
  colors::italic_gray(if decl_kind == DeclarationKind::Private {
    "private "
  } else {
    ""
  })
}

fn get_location_string(location: &Location) -> String {
  format!(
    "{}:{}:{}",
    location.filename,
    location.line,
    // todo(#150): for some reason the column is 0-indexed and the line
    // is 1-indexed. Display them both as 1-indexed so that vscode goes
    // to the correct column when clicking this.
    location.col + 1
  )
}

struct Indent(pub i64);

impl Display for Indent {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    for _ in 0..self.0 {
      write!(f, "  ")?;
    }
    Ok(())
  }
}
