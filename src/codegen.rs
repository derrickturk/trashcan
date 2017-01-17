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

// TODO: handle multidimensional arrays properly
impl<'a> Emit for ast::FunctionParameter<'a> {
    fn emit(&self) -> String {
        match self.typ {
            &ast::Type::Array(ref inner, _) => ast::FunctionParameter {
                name: ast::Ident(&{
                    let mut s = String::from(self.name.0);
                    s.push_str("()");
                    s
                }),
                typ: inner,
                mode: self.mode,
            }.emit(),
            t => format!("{} {} as {}", self.mode.emit(), self.name.0,
              t.emit())
        }
    }
}

// TODO: handle multidimensional arrays properly
impl<'a> Emit for ast::VariableDeclaration<'a> {
    fn emit(&self) -> String {
        match self.typ {
            &ast::Type::Array(ref inner, dim) => ast::VariableDeclaration {
                name: ast::Ident(&{
                    let mut s = String::from(self.name.0);
                    match dim {
                        Some(n) => s.push_str(&format!("({})", n)),
                        None => s.push_str(""),
                    };
                    s
                }),
                typ: inner,
            }.emit(),
            t => format!("{} as {}", self.name.0, t.emit())
        }
    }
}

// TODO: handle multidimensional arrays properly
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
        .map(Emit::emit)
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
    let member_spec = def.members.iter()
        .map(Emit::emit)
        .collect::<Vec<_>>()
        .join("\n\t");
    format!("{access} Type {name}\n\t{members}\nEnd Type",
      access = mode.emit(),
      name = def.name.0,
      members = member_spec)
}

fn emit_enum(mode: &ast::AccessMode, def: &ast::EnumDef) -> String {
    unimplemented!()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn emit_fn() {
        let s = ast::Item::Function(
            ast::AccessMode::Private,
            &ast::Function {
                name: ast::Ident("do_whatever"),
                params: &[
                    ast::FunctionParameter {
                        name: ast::Ident("x"),
                        typ: &ast::Type::Long,
                        mode: ast::ParamMode::ByVal,
                    },
                    ast::FunctionParameter {
                        name: ast::Ident("y"),
                        typ: &ast::Type::Double,
                        mode: ast::ParamMode::ByRef,
                    },
                    ast::FunctionParameter {
                        name: ast::Ident("z"),
                        typ: &ast::Type::Array(&ast::Type::Double, None),
                        mode: ast::ParamMode::ByRef,
                    },
                ],
                ret: Some(ast::Type::Struct(ast::Ident("MyType"))),
                body: &[],
            }
        ).emit();
        println!("{}", s);
    }

    #[test]
    fn emit_st() {
        let s = ast::Item::StructDef(
            ast::AccessMode::Public,
            &ast::StructDef {
                name: ast::Ident("my_struct"),
                members: &[
                    ast::VariableDeclaration {
                        name: ast::Ident("my_arr"),
                        typ: &ast::Type::Array(&ast::Type::Double, Some(10)),
                    },
                    ast::VariableDeclaration {
                        name: ast::Ident("my_dbl"),
                        typ: &ast::Type::Double,
                    },
                ],
            },
        ).emit();
        println!("{}", s);
    }
}
