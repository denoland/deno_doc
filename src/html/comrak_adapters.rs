// Copied and modified from https://github.com/kivikakk/comrak/blob/main/src/plugins/syntect.rs

//! Adapter for the Syntect syntax highlighter plugin.

use comrak::adapters::HeadingAdapter;
use comrak::adapters::HeadingMeta;
use comrak::nodes::Sourcepos;
use std::io::Write;
use std::sync::{Arc, Mutex};

#[derive(Default)]
pub struct HeadingToCAdapter {
  toc: Arc<Mutex<Vec<(u8, String, String)>>>,
  anchorizer: Arc<Mutex<comrak::html::Anchorizer>>,
}

impl HeadingToCAdapter {
  pub fn get_toc(&self) -> Vec<(u8, String, String)> {
    let lock = self.toc.lock().unwrap();
    lock.clone()
  }
}

impl HeadingAdapter for HeadingToCAdapter {
  fn enter(
    &self,
    output: &mut dyn Write,
    heading: &HeadingMeta,
    _sourcepos: Option<Sourcepos>,
  ) -> std::io::Result<()> {
    let mut anchorizer = self.anchorizer.lock().unwrap();

    let anchor = anchorizer.anchorize(heading.content.clone());
    writeln!(output, r#"<h{} id="{anchor}">"#, heading.level)?;

    let mut lock = self.toc.lock().unwrap();
    lock.push((heading.level, heading.content.clone(), anchor));

    Ok(())
  }

  fn exit(
    &self,
    output: &mut dyn Write,
    heading: &HeadingMeta,
  ) -> std::io::Result<()> {
    writeln!(output, "</h{}>", heading.level)?;
    Ok(())
  }
}
