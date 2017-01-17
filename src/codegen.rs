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
                emit_function(&name, params, ret, body),
            &ast::Item::StructDef { ref name, members } =>
                emit_struct(&name, members),
            &ast::Item::EnumDef { ref name, members } =>
                emit_enum(&name, members),
        }
    }
}

impl Emit for ast::AccessMode {
    fn emit(&self) -> String {
        match self {
            &ast::AccessMode::Private => String::from("Private"),
            &ast::AccessMode::Public => String::from("Public"),
        }
    }
}

impl Emit for ast::ParamMode {
    fn emit(&self) -> String {
        match self {
            &ast::ParamMode::ByVal => String::from("ByVal"),
            &ast::ParamMode::ByRef => String::from("ByRef"),
        }
    }
}

impl<'a> Emit for ast::Type<'a> {
    fn emit(&self) -> String {
        match self {
            &ast::Type::Boolean => String::from("Boolean"),
            &ast::Type::Byte => String::from("Byte"),
            &ast::Type::Integer => String::from("Integer"),
            &ast::Type::Long => String::from("Long"),
            &ast::Type::Single => String::from("Single"),
            &ast::Type::Double => String::from("Double"),
            &ast::Type::String => String::from("String"),
            &ast::Type::Currency => String::from("Currency"),
            &ast::Type::Date => String::from("Date"),
            &ast::Type::Variant => String::from("Variant"),
            &ast::Type::Object(ast::Ident(i)) => i.to_string(),
            &ast::Type::Struct(ast::Ident(i)) => i.to_string(),
            &ast::Type::Enum(ast::Ident(i)) => i.to_string(),
            &ast::Type::Array(t, _) => {
                let mut base = t.emit();
                base.push_str("()");
                base
            },
        }
    }
}

fn emit_function(
  name: &ast::Ident,
  params: &[(ast::ParamMode, ast::Ident, ast::Type)],
  ret: &Option<ast::Type>,
  body: &[ast::Statement])
  -> String {
    let param_spec = params.iter()
        .map(|&(ref m, ref i, ref t)| {
            format!("{} {} as {}", m.emit(), i.0, t.emit())
        })
        .collect::<Vec<_>>()
        .join(", ");
    format!("Public {type} {name} ({params}){ret}\n\t{body:?}\nEnd {type}",
      type = if ret.is_some() { "Function" } else { "Sub" },
      name = name.0,
      params = param_spec,
      ret = match ret {
          &Some(ref t) => {
              let mut s = String::from(" As ");
              s.push_str(&t.emit());
              s
          },
          _ => String::new(),
      },
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
            params: &[
                (ast::ParamMode::ByVal, ast::Ident("x"), ast::Type::Long),
                (ast::ParamMode::ByRef, ast::Ident("y"), ast::Type::Double),
                (ast::ParamMode::ByRef, ast::Ident("z"),
                    ast::Type::Array(&ast::Type::Double, ())),
            ],
            ret: Some(ast::Type::Struct(ast::Ident("MyType"))),
            body: &[],
        }.emit();
        println!("{}", s);
    }

}
