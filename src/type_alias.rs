// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.
use crate::js_doc::JsDoc;
use crate::js_doc::JsDocTag;
use crate::node::Location;
use crate::params::ParamDef;
use crate::params::ParamPatternDef;
use crate::ts_type::PropertyDef;
use crate::ts_type::TsFnOrConstructorDef;
use crate::ts_type::TsTypeDef;
use crate::ts_type::TsTypeDefKind;
use crate::ts_type_param::TsTypeParamDef;
use crate::ts_type_param::maybe_type_param_decl_to_type_param_defs;
use deno_graph::symbols::EsModuleInfo;
use serde::Deserialize;
use serde::Serialize;

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct TypeAliasDef {
  pub ts_type: TsTypeDef,
  #[serde(skip_serializing_if = "<[_]>::is_empty", default)]
  pub type_params: Box<[TsTypeParamDef]>,
}

pub fn get_doc_for_ts_type_alias_decl(
  module_info: &EsModuleInfo,
  type_alias_decl: &deno_ast::swc::ast::TsTypeAliasDecl,
) -> TypeAliasDef {
  let ts_type = TsTypeDef::new(module_info, &type_alias_decl.type_ann);
  let type_params = maybe_type_param_decl_to_type_param_defs(
    module_info,
    type_alias_decl.type_params.as_deref(),
  );

  TypeAliasDef {
    ts_type,
    type_params,
  }
}

/// A type alias declared in a JSDoc block via a `@typedef` or `@callback` tag,
/// as is commonly done in JavaScript files.
pub(crate) struct JsDocTypeAlias {
  pub name: Box<str>,
  pub js_doc: JsDoc,
  pub def: TypeAliasDef,
}

enum JsDocTypeAliasKind {
  TypeDef {
    ts_type: TsTypeDef,
    properties: Vec<PropertyDef>,
  },
  Callback {
    params: Vec<ParamDef>,
    return_type: Option<TsTypeDef>,
  },
}

struct JsDocTypeAliasBuilder {
  name: Box<str>,
  doc: Option<Box<str>>,
  kind: JsDocTypeAliasKind,
  /// `@param` and `@returns` tags of a callback, kept so their docs are not
  /// lost.
  tags: Vec<JsDocTag>,
}

/// Collects the type aliases declared via `@typedef` and `@callback` tags in
/// a JSDoc block. `@property` tags following a `@typedef`, and `@param` and
/// `@returns` tags following a `@callback`, describe the shape of that type,
/// while `@template` tags apply to every type declared in the block.
pub(crate) fn type_aliases_from_js_doc(
  js_doc: &JsDoc,
  location: &Location,
) -> Vec<JsDocTypeAlias> {
  if !js_doc.tags.iter().any(|tag| {
    matches!(tag, JsDocTag::TypeDef { .. } | JsDocTag::Callback { .. })
  }) {
    return vec![];
  }

  let mut type_params = vec![];
  let mut shared_tags = vec![];
  let mut builders: Vec<JsDocTypeAliasBuilder> = vec![];

  for tag in js_doc.tags.iter() {
    match tag {
      JsDocTag::TypeDef { name, ts_type, doc } => {
        builders.push(JsDocTypeAliasBuilder {
          name: name.clone(),
          doc: doc.clone(),
          kind: JsDocTypeAliasKind::TypeDef {
            ts_type: ts_type.clone(),
            properties: vec![],
          },
          tags: vec![],
        });
      }
      JsDocTag::Callback { name, doc } => {
        builders.push(JsDocTypeAliasBuilder {
          name: name.clone(),
          doc: doc.clone(),
          kind: JsDocTypeAliasKind::Callback {
            params: vec![],
            return_type: None,
          },
          tags: vec![],
        });
      }
      JsDocTag::Template { name, .. } => {
        type_params.push(TsTypeParamDef {
          name: name.to_string(),
          constraint: None,
          default: None,
        });
      }
      JsDocTag::Property {
        name,
        ts_type,
        optional,
        doc,
      } => {
        // nested properties (`@property {string} a.b`) are not supported
        if let Some(JsDocTypeAliasBuilder {
          kind: JsDocTypeAliasKind::TypeDef { properties, .. },
          ..
        }) = builders.last_mut()
          && !name.contains('.')
        {
          properties.push(PropertyDef {
            name: name.to_string(),
            js_doc: JsDoc {
              doc: doc.clone(),
              tags: Box::new([]),
            },
            location: location.clone(),
            params: vec![],
            readonly: false,
            computed: false,
            optional: *optional,
            ts_type: Some(ts_type.clone()),
            type_params: Box::new([]),
          });
        }
      }
      JsDocTag::Param {
        name,
        ts_type,
        optional,
        ..
      } => {
        if let Some(JsDocTypeAliasBuilder {
          kind: JsDocTypeAliasKind::Callback { params, .. },
          tags,
          ..
        }) = builders.last_mut()
        {
          tags.push(tag.clone());
          // nested params (`@param {string} options.a`) are not supported
          if name.contains('.') {
            continue;
          }
          params.push(ParamDef {
            pattern: ParamPatternDef::Identifier {
              name: name.to_string(),
              optional: *optional,
            },
            decorators: Box::new([]),
            ts_type: ts_type.clone(),
          });
        }
      }
      JsDocTag::Return { ts_type, .. } => {
        if let Some(JsDocTypeAliasBuilder {
          kind: JsDocTypeAliasKind::Callback { return_type, .. },
          tags,
          ..
        }) = builders.last_mut()
        {
          tags.push(tag.clone());
          *return_type = ts_type.clone();
        }
      }
      tag => shared_tags.push(tag.clone()),
    }
  }

  let type_params: Box<[TsTypeParamDef]> = type_params.into_boxed_slice();

  builders
    .into_iter()
    .map(|builder| {
      let ts_type = match builder.kind {
        JsDocTypeAliasKind::TypeDef {
          ts_type,
          properties,
        } => {
          if !properties.is_empty() && is_object_type(&ts_type) {
            TsTypeDef::object(vec![], properties)
          } else {
            ts_type
          }
        }
        JsDocTypeAliasKind::Callback {
          params,
          return_type,
        } => TsTypeDef {
          repr: String::new(),
          kind: TsTypeDefKind::FnOrConstructor(Box::new(
            TsFnOrConstructorDef {
              constructor: false,
              ts_type: return_type
                .unwrap_or_else(|| TsTypeDef::keyword("unknown")),
              params,
              type_params: Box::new([]),
            },
          )),
        },
      };

      let doc = match (&js_doc.doc, builder.doc) {
        (Some(doc), Some(tag_doc)) => {
          Some(format!("{doc}\n\n{tag_doc}").into())
        }
        (doc, tag_doc) => doc.clone().or(tag_doc),
      };

      JsDocTypeAlias {
        name: builder.name,
        js_doc: JsDoc {
          doc,
          tags: shared_tags.iter().cloned().chain(builder.tags).collect(),
        },
        def: TypeAliasDef {
          ts_type,
          type_params: type_params.clone(),
        },
      }
    })
    .collect()
}

/// Whether the type is `object` or `Object`, which in combination with
/// `@property` tags describes an object type.
fn is_object_type(ts_type: &TsTypeDef) -> bool {
  match &ts_type.kind {
    TsTypeDefKind::Keyword(keyword) => keyword == "object",
    TsTypeDefKind::TypeRef(type_ref) => {
      type_ref.type_name == "Object" && type_ref.type_params.is_none()
    }
    _ => false,
  }
}
