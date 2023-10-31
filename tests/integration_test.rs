// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::path::PathBuf;

use deno_graph::source::Source;
use pretty_assertions::assert_eq;

use crate::helpers::get_specs_in_dir;
use crate::helpers::TestBuilder;

mod helpers;

#[tokio::test]
async fn test_doc_specs() {
  // run tests with `UPDATE=1` to automatically update the spec files
  for (test_file_path, spec) in
    get_specs_in_dir(&PathBuf::from("./tests/specs/"))
  {
    eprintln!("Running {}", test_file_path.display());
    let mut builder = TestBuilder::new();
    builder
      .with_loader(|loader| {
        for file in &spec.files {
          let source = Source::Module {
            specifier: file.url().to_string(),
            maybe_headers: Some(file.headers.clone().into_iter().collect()),
            content: file.text.clone(),
          };
          loader.add_source(file.url(), source);
        }
      })
      .set_private(spec.private);

    let result = builder.build().await;
    let update_var = std::env::var("UPDATE");
    let mut json_output =
      serde_json::to_string_pretty(&result.json_output).unwrap();
    json_output.push('\n');
    let diagnostics = result
      .diagnostics
      .iter()
      .map(|d| {
        format!(
          "{}:{}:{} {}",
          d.location.filename,
          d.location.line,
          d.location.col + 1,
          d.kind.to_string()
        )
      })
      .collect::<Vec<_>>();
    let spec = if update_var.as_ref().map(|v| v.as_str()) == Ok("1") {
      let mut spec = spec;
      spec.output_json_file.text = json_output.clone();
      spec.output_doc_file.text = result.text_output.clone();
      spec.diagnostics = diagnostics.clone();
      std::fs::write(&test_file_path, spec.emit()).unwrap();
      spec
    } else {
      spec
    };
    assert_eq!(
      result.text_output,
      spec.output_doc_file.text,
      "Should be same for doc output {}",
      test_file_path.display()
    );
    assert_eq!(
      json_output,
      spec.output_json_file.text,
      "Should be same for json output {}",
      test_file_path.display()
    );
    assert_eq!(
      diagnostics,
      spec.diagnostics,
      "Should be same for {}",
      test_file_path.display()
    );
  }
}
