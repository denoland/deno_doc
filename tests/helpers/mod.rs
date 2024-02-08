// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::path::Path;
use std::path::PathBuf;

mod test_builder;

use deno_graph::ModuleSpecifier;
use indexmap::IndexMap;
use serde::Deserialize;
use serde::Serialize;
pub use test_builder::*;

pub struct Spec {
  pub private: bool,
  pub files: Vec<SpecFile>,
  pub output_json_file: SpecFile,
  pub output_doc_file: SpecFile,
  pub diagnostics: String,
}

impl Spec {
  pub fn emit(&self) -> String {
    let mut text = String::new();
    if self.private {
      text.push_str("{ \"private\": true }\n");
    }
    for file in &self.files {
      text.push_str(&file.emit());
      text.push('\n');
    }
    if !self.diagnostics.is_empty() {
      text.push_str("# diagnostics\n");
      text.push_str(&self.diagnostics);
    }
    text.push_str(&self.output_doc_file.emit());
    text.push('\n');
    text.push_str(&self.output_json_file.emit());
    text
  }
}

#[derive(Debug, Serialize, Deserialize)]
struct SpecOptions {
  pub private: bool,
}

#[derive(Debug)]
pub struct SpecFile {
  pub specifier: String,
  pub text: String,
  pub headers: IndexMap<String, String>,
}

impl SpecFile {
  pub fn emit(&self) -> String {
    let mut text = format!("# {}\n", self.specifier);
    if !self.headers.is_empty() {
      text.push_str(&format!(
        "HEADERS: {}\n",
        serde_json::to_string(&self.headers).unwrap()
      ));
    }
    text.push_str(&self.text);
    text
  }

  pub fn url(&self) -> ModuleSpecifier {
    let specifier = &self.specifier;
    if !specifier.starts_with("http") && !specifier.starts_with("file") {
      ModuleSpecifier::parse(&format!("file:///{}", specifier)).unwrap()
    } else {
      ModuleSpecifier::parse(specifier).unwrap()
    }
  }
}

pub fn get_specs_in_dir(path: &Path) -> Vec<(PathBuf, Spec)> {
  let files = collect_files_in_dir_recursive(path);
  let files = if files
    .iter()
    .any(|file| file.path.to_string_lossy().to_lowercase().contains("_only"))
  {
    files
      .into_iter()
      .filter(|file| {
        file.path.to_string_lossy().to_lowercase().contains("_only")
      })
      .collect()
  } else {
    files
  };
  files
    .into_iter()
    .map(|file| (file.path, parse_spec(file.text)))
    .collect()
}

fn parse_spec(text: String) -> Spec {
  let mut files = Vec::new();
  let mut current_file = None;
  let mut options: Option<SpecOptions> = None;
  for (i, line) in text.split('\n').enumerate() {
    if i == 0 && line.starts_with('{') {
      options = Some(serde_json::from_str(line).unwrap());
      continue;
    }
    if let Some(specifier) = line.strip_prefix("# ") {
      if let Some(file) = current_file.take() {
        files.push(file);
      }
      current_file = Some(SpecFile {
        specifier: specifier.to_string(),
        text: String::new(),
        headers: Default::default(),
      });
    } else if let Some(headers) = line.strip_prefix("HEADERS: ") {
      current_file.as_mut().unwrap().headers =
        serde_json::from_str(headers).unwrap();
    } else {
      let current_file = current_file.as_mut().unwrap();
      if !current_file.text.is_empty() {
        current_file.text.push('\n');
      }
      current_file.text.push_str(line);
    }
  }
  files.push(current_file.unwrap());
  let output_json_file = files.remove(
    files
      .iter()
      .position(|f| f.specifier == "output.json")
      .unwrap(),
  );
  let output_doc_file = files.remove(
    files
      .iter()
      .position(|f| f.specifier == "output.txt")
      .unwrap(),
  );
  let diagnostics = take_text_file(&mut files, "diagnostics");
  Spec {
    private: options.map(|o| o.private).unwrap_or(false),
    files,
    output_json_file,
    output_doc_file,
    diagnostics,
  }
}

fn take_text_file(files: &mut Vec<SpecFile>, name: &str) -> String {
  if let Some(index) = files.iter().position(|f| f.specifier == name) {
    let file = files.remove(index);
    file.text
  } else {
    String::new()
  }
}

struct CollectedFile {
  pub path: PathBuf,
  pub text: String,
}

fn collect_files_in_dir_recursive(path: &Path) -> Vec<CollectedFile> {
  let mut result = Vec::new();

  for entry in path.read_dir().unwrap().flatten() {
    let entry_path = entry.path();
    if entry_path.is_file() {
      let text = std::fs::read_to_string(&entry_path).unwrap();
      result.push(CollectedFile {
        path: entry_path,
        text,
      });
    } else {
      result.extend(collect_files_in_dir_recursive(&entry_path));
    }
  }

  result
}
