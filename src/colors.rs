// Copyright 2018-2020 the Deno authors. All rights reserved. MIT license.
use std::fmt;
use std::io::Write;
use std::sync::atomic::{AtomicBool, Ordering};
use termcolor::Color::{Ansi256, Blue, Green, Magenta, Red};
use termcolor::{Ansi, ColorSpec, WriteColor};

lazy_static! {
  static ref USE_COLOR: AtomicBool = AtomicBool::new(false);
}

pub fn enable_color() {
  USE_COLOR.store(true, Ordering::Relaxed);
}

pub fn disable_color() {
  USE_COLOR.store(false, Ordering::Relaxed);
}

pub fn use_color() -> bool {
  USE_COLOR.load(Ordering::Relaxed)
}

fn style(s: &str, colorspec: ColorSpec) -> impl fmt::Display {
  if !use_color() {
    return String::from(s);
  }
  let mut v = Vec::new();
  let mut ansi_writer = Ansi::new(&mut v);
  ansi_writer.set_color(&colorspec).unwrap();
  ansi_writer.write_all(s.as_bytes()).unwrap();
  ansi_writer.reset().unwrap();
  String::from_utf8_lossy(&v).into_owned()
}

pub fn yellow(s: &str) -> impl fmt::Display {
  let mut style_spec = ColorSpec::new();
  style_spec.set_fg(Some(Ansi256(11)));
  style(&s, style_spec)
}

pub fn cyan(s: &str) -> impl fmt::Display {
  let mut style_spec = ColorSpec::new();
  style_spec.set_fg(Some(Ansi256(14)));
  style(&s, style_spec)
}

pub fn red(s: &str) -> impl fmt::Display {
  let mut style_spec = ColorSpec::new();
  style_spec.set_fg(Some(Red));
  style(&s, style_spec)
}

pub fn green(s: &str) -> impl fmt::Display {
  let mut style_spec = ColorSpec::new();
  style_spec.set_fg(Some(Green)).set_intense(true);
  style(&s, style_spec)
}

pub fn magenta(s: &str) -> impl fmt::Display {
  let mut style_spec = ColorSpec::new();
  style_spec.set_fg(Some(Magenta));
  style(&s, style_spec)
}

pub fn bold(s: &str) -> impl fmt::Display {
  let mut style_spec = ColorSpec::new();
  style_spec.set_bold(true);
  style(&s, style_spec)
}

pub fn gray(s: &str) -> impl fmt::Display {
  let mut style_spec = ColorSpec::new();
  style_spec.set_fg(Some(Ansi256(8)));
  style(&s, style_spec)
}

pub fn italic_gray(s: &str) -> impl fmt::Display {
  let mut style_spec = ColorSpec::new();
  style_spec.set_fg(Some(Ansi256(8))).set_italic(true);
  style(&s, style_spec)
}

pub fn intense_blue(s: &str) -> impl fmt::Display {
  let mut style_spec = ColorSpec::new();
  style_spec.set_fg(Some(Blue)).set_intense(true);
  style(&s, style_spec)
}
