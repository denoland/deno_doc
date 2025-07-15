use crate::{DocNode};
use crate::{DocNodeDef};
use crate::variable::VariableDef;

#[derive(Debug)]
pub struct NodeChange {
  name: String,
  changes: Vec<Change>,
}

#[derive(Debug)]
pub enum Change {
  Added {
    kind: &'static str,
    value: String,
  },
  Changed {
    kind: &'static str,
    old_value: String,
    new_value: String,
  },
  Deleted {
    kind: &'static str,
    value: String,
  },
  NodeChange(NodeChange),
}

pub fn diff(a: &[DocNode], b: &[DocNode]) -> Vec<Change> {
  let mut out = vec![];

  for a_node in a {
    if let Some(b_node) = b.iter().find(|b_node| b_node.get_name() == a_node.get_name()) {
      if let Some(node) = visit(&a_node, b_node) {
        out.push(Change::NodeChange(node));
      }
    } else {
      out.push(Change::Deleted {
        kind: "name", // node
        value: a_node.get_name().to_string()
      });
    }
  }
  for b_node in b {
    if !a.iter().any(|a_node| b_node.get_name() == a_node.get_name()) {
      out.push(Change::Added {
        kind: "name", // node
        value: b_node.get_name().to_string(),
      });
    }
  }

  out
}


fn visit(a: &DocNode, b: &DocNode) -> Option<NodeChange> {
  let mut changes: Vec<Change> = vec![];

  if a.declaration_kind != b.declaration_kind {
    changes.push(Change::Changed {
      kind: "declaration_kind",
      old_value: serde_json::to_string_pretty(&a.declaration_kind).unwrap(),
      new_value: serde_json::to_string_pretty(&b.declaration_kind).unwrap(),
    });
  }

  match &a.def {
    DocNodeDef::Variable { variable_def } => {
      if let Some(b) = b.variable_def() {
        visit_variable(&mut changes, variable_def, b);
      } else {
        changes.push(Change::Changed {
          kind: "definition_kind",
          old_value: serde_json::to_value(&a.def).unwrap().as_object().unwrap().get("kind").unwrap().as_str().unwrap().to_string(),
          new_value: serde_json::to_value(&b.def).unwrap().as_object().unwrap().get("kind").unwrap().as_str().unwrap().to_string(),
        });
        // TODO: show body change
      }
    }
    DocNodeDef::Function { .. } => {}
    DocNodeDef::Enum { .. } => {}
    DocNodeDef::Class { .. } => {}
    DocNodeDef::TypeAlias { .. } => {}
    DocNodeDef::Namespace { .. } => {}
    DocNodeDef::Interface { .. } => {}
    DocNodeDef::Import { .. } => {}
    DocNodeDef::ModuleDoc => {}
    DocNodeDef::Reference { .. } => {}
  }

  if changes.is_empty() {
    None
  } else {
    Some(NodeChange {
      name: a.get_name().to_string(),
      changes,
    })
  }
}


fn visit_variable(changes: &mut Vec<Change>, a: &VariableDef, b: &VariableDef) {
  if a.kind != b.kind {
    changes.push(Change::Changed {
      kind: "variable_kind",
      old_value: serde_json::to_string_pretty(&a.kind).unwrap(),
      new_value: serde_json::to_string_pretty(&b.kind).unwrap(),
    });
  }

  if a.ts_type != b.ts_type {
    match (&a.ts_type, &b.ts_type) {
      (Some(a), Some(b)) => {
        if a.repr != b.repr {
          changes.push(Change::Changed {
            kind: "ts_type",
            old_value: a.repr.to_string(),
            new_value: b.repr.to_string(),
          });
        }
      }
      (Some(a), None) => {
        changes.push(Change::Deleted {
          kind: "ts_type",
          value: a.repr.to_string(),
        });
      }
      (None, Some(b)) => {
        changes.push(Change::Added {
          kind: "ts_type",
          value: b.repr.to_string(),
        });
      }
      (None, None) => {}
    }
  }
}
