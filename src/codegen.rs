//! code generator: emit VB6 from trashcan ASTs

use ast;

/// trait for all emittable types
pub trait Emit {
    fn emit(&self) -> String;
}

impl<'a> Emit for ast::Module<'a> {
    fn emit(&self) -> String {
        match self {
            &ast::Module::Normal(items) => items.iter()
                .fold(String::new(), |mut acc, ref item| {
                    acc.push_str(&item.emit()); acc
                }),
            &ast::Module::Class(items) => unimplemented!(),
        }
    }
}

impl<'a> Emit for ast::Item<'a> {
    fn emit(&self) -> String {
        match self {
            &ast::Item::Function { ref name, params, ref ret, body } =>
                emit_function(name, params, ret, body),
            &ast::Item::StructDef { ref name, members } =>
                emit_struct(name, members),
            &ast::Item::EnumDef { ref name, members } =>
                emit_enum(name, members),
        }
    }
}

fn emit_function(name: &ast::Ident, params: &[(ast::Ident, ast::Type)],
  ret: &Option<ast::Type>, body: &[ast::Statement]) -> String {
    format!("Public {type} {name} ({params:?}){ret}\n\t{body:?}\nEnd {type}",
      type = if ret.is_some() { "Function" } else { "Sub" },
      name = name.0,
      params = "params TBD",
      ret = match ret { &Some(ref t) => " As TBD", _ => "" },
      body = "body tbd")
}

fn emit_struct(name: &ast::Ident, members: &[(ast::Ident, ast::Type)])
  -> String {
    unimplemented!()
}

fn emit_enum(name: &ast::Ident, members: &[ast::Ident]) -> String {
    unimplemented!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn emit_item() {
        let s = ast::Item::Function {
            name: ast::Ident("do_whatever"),
            params: &[],
            ret: None,
            body: &[],
        }.emit();
        println!("{}", s);
    }

}
