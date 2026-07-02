// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use deno_terminal::colors;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;

pub(crate) struct SliceDisplayer<'a, T: Display>(&'a [T], &'a str, bool);

impl<'a, T: Display> SliceDisplayer<'a, T> {
  pub fn new(
    slice: &'a [T],
    separator: &'a str,
    trailing: bool,
  ) -> SliceDisplayer<'a, T> {
    SliceDisplayer(slice, separator, trailing)
  }
}

impl<T: Display> Display for SliceDisplayer<'_, T> {
  fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    if self.0.is_empty() {
      return Ok(());
    }

    write!(f, "{}", self.0[0])?;
    for v in &self.0[1..] {
      write!(f, "{}{}", self.1, v)?;
    }

    if self.2 {
      write!(f, "{}", self.1)?;
    }

    Ok(())
  }
}

pub(crate) fn display_computed(is_computed: bool, name: &str) -> impl Display {
  colors::bold(if is_computed {
    format!("[{}]", name)
  } else {
    name.to_string()
  })
}

pub(crate) fn display_optional(is_optional: bool) -> impl Display {
  colors::magenta(if is_optional { "?" } else { "" })
}

pub(crate) fn display_readonly(is_readonly: bool) -> impl Display {
  colors::magenta(if is_readonly { "readonly " } else { "" })
}

cfg_if! {
  if #[cfg(feature = "rust")] {
    use crate::util::types::Accessibility;
    use crate::util::types::MethodKind;

    pub(crate) fn display_abstract(is_abstract: bool) -> impl Display {
      colors::magenta(if is_abstract { "abstract " } else { "" })
    }

    pub(crate) fn display_accessibility(
      accessibility: Option<Accessibility>, show_public: bool
    ) -> impl Display {
      colors::magenta(
        match accessibility {
          None => "",
          Some(Accessibility::Public) => if show_public { "public " } else { "" },
          Some(Accessibility::Protected) => "protected ",
          Some(Accessibility::Private) => "private ",
        },
      )
    }

    pub(crate) fn display_async(is_async: bool) -> impl Display {
      colors::magenta(if is_async { "async " } else { "" })
    }

    pub(crate) fn display_generator(is_generator: bool) -> impl Display {
      colors::magenta(if is_generator { "*" } else { "" })
    }

    pub(crate) fn display_method(
      method: MethodKind,
    ) -> impl Display {
      colors::magenta(match method {
        MethodKind::Getter => "get ",
        MethodKind::Setter => "set ",
        _ => "",
      })
    }

    pub(crate) fn display_override(is_override: bool) -> impl Display {
      colors::magenta(if is_override { "override "} else { "" })
    }

    pub(crate) fn display_static(is_static: bool) -> impl Display {
      colors::magenta(if is_static { "static " } else { "" })
    }
  }
}
