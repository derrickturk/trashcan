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
            &ast::Item::Function(ref mode, ref func) => emit_func(mode, func),
            &ast::Item::StructDef(ref mode, ref def) => emit_struct(mode, def),
            &ast::Item::EnumDef(ref mode, ref def) => emit_enum(mode, def),
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

fn emit_func(mode: &ast::AccessMode, func: &ast::Function) -> String {
    let param_spec = func.params.iter()
        .map(|&(ref m, ref i, ref t)| {
            format!("{} {} as {}", m.emit(), i.0, t.emit())
        })
        .collect::<Vec<_>>()
        .join(", ");
    format!("{access} {type} {name} ({params}){ret}\n\t{body:?}\nEnd {type}",
      access = mode.emit(),
      type = if func.ret.is_some() { "Function" } else { "Sub" },
      name = func.name.0,
      params = param_spec,
      ret = match func.ret {
          Some(ref t) => {
              let mut s = String::from(" As ");
              s.push_str(&t.emit());
              s
          },
          _ => String::new(),
      },
      body = "body tbd")
}

fn emit_struct(mode: &ast::AccessMode, def: &ast::StructDef) -> String {
    unimplemented!()
}

fn emit_enum(mode: &ast::AccessMode, def: &ast::EnumDef) -> String {
    unimplemented!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn emit_item() {
        let s = ast::Item::Function(
            ast::AccessMode::Private,
            &ast::Function {
                name: ast::Ident("do_whatever"),
                params: &[
                    (ast::ParamMode::ByVal, ast::Ident("x"), ast::Type::Long),
                    (ast::ParamMode::ByRef, ast::Ident("y"), ast::Type::Double),
                    (ast::ParamMode::ByRef, ast::Ident("z"),
                        ast::Type::Array(&ast::Type::Double, ())),
                ],
                ret: Some(ast::Type::Struct(ast::Ident("MyType"))),
                body: &[],
            }
        ).emit();
        println!("{}", s);
    }

}
