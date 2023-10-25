use crate::html::r#type::render_type_def;
use crate::html::util::*;

pub fn render_enum(doc_node: &crate::DocNode) -> String {
  let mut members = doc_node.enum_def.as_ref().unwrap().members.clone();

  members.sort_by(|a, b| a.name.cmp(&b.name));

  let items = members
    .into_iter()
    .map(|member| {
      let id =
        name_to_id("enum", &format!("{}_{}", &doc_node.name, &member.name));
      doc_entry(
        &id,
        &member.name,
        &member
          .init
          .as_ref()
          .map(|init| format!(" = {}", render_type_def(init)))
          .unwrap_or_default(),
      )
    })
    .collect::<String>();

  // TODO: examples

  format!("<div>{}</div>", section("Members", &items))
}
