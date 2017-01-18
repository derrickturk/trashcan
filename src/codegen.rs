//! code generator: emit VB6 from trashcan ASTs

use ast;
use symtab;

const INDENT: &'static str = "    ";

// TODO: probably more like emit(&self, symtab: &..., indent: u32)

/// trait for all emittable types
pub trait Emit {
    fn emit(&self, indent: u32) -> String;
}

impl<'a> Emit for ast::Module<'a> {
    fn emit(&self, indent: u32) -> String {
        match self {
            &ast::Module::Normal(_, items) => items.iter()
                .fold(String::new(), |mut acc, ref item| {
                    acc.push_str(&item.emit(indent)); acc
                }),
            &ast::Module::Class(_, items) => unimplemented!(),
        }
    }
}

impl<'a> Emit for ast::Item<'a> {
    fn emit(&self, indent: u32) -> String {
        match self {
            &ast::Item::Function(ref mode, ref func) =>
                emit_func(mode, func, indent),
            &ast::Item::StructDef(ref mode, ref def) =>
                emit_struct(mode, def, indent),
            &ast::Item::EnumDef(ref mode, ref def) =>
                emit_enum(mode, def, indent),
        }
    }
}

// TODO: handle multidimensional arrays properly
impl<'a> Emit for ast::FunctionParameter<'a> {
    fn emit(&self, _indent: u32) -> String {
        match self.typ {
            &ast::Type::Array(ref inner, _) => ast::FunctionParameter {
                name: ast::Ident(&{
                    let mut s = String::from(self.name.0);
                    s.push_str("()");
                    s
                }),
                typ: inner,
                mode: self.mode,
            }.emit(0),
            t => format!("{} {} as {}", self.mode.emit(0), self.name.0,
              t.emit(0))
        }
    }
}

// TODO: handle multidimensional arrays properly
impl<'a> Emit for ast::VariableDeclaration<'a> {
    fn emit(&self, indent: u32) -> String {
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
            }.emit(indent),
            t => format!("{}{} as {}", emit_indent(indent),
              self.name.0, t.emit(0))
        }
    }
}

impl<'a> Emit for ast::Statement<'a> {
    fn emit(&self, indent: u32) -> String {
        match self {
            &ast::Statement::Declaration(decl, None) => {
                let mut s = emit_indent(indent);
                s.push_str("Dim ");
                s.push_str(&decl.emit(0));
                s
            },

            &ast::Statement::Declaration(decl, Some(expr)) => {
                let mut s = ast::Statement::Declaration(decl, None)
                    .emit(indent);
                s.push_str("\n");
                s.push_str(
                    &ast::Statement::Assignment(
                        &ast::Expression::Ident(decl.name), expr).emit(indent));
                s
            },

            // TODO: this will have to look up the type of the symbol identified
            //   by ident in the symbol table eventually (to decide whether
            //   to emit = or Set =)
            &ast::Statement::Assignment(place, expr) => {
                format!("{}{} = {}",
                        emit_indent(indent),
                        place.emit(0),
                        expr.emit(0))
            },

            &ast::Statement::FnCall(ident, args) => {
                format!("{}{} {}", emit_indent(indent), ident.0,
                  args.iter().map(|a| a.emit(0)).collect::<Vec<_>>().join(", "))
            },

            &ast::Statement::Conditional { cond, body, elsifs, els } => {
                let this_indent = emit_indent(indent);
                let mut s = this_indent.clone();
                s.push_str("If ");
                s.push_str(&cond.emit(0));
                s.push_str(" Then\n");
                for st in body {
                    s.push_str(&st.emit(indent + 1));
                    s.push_str("\n");
                }
                for &(cond, body) in elsifs {
                    s.push_str(&this_indent);
                    s.push_str("Else If ");
                    s.push_str(&cond.emit(0));
                    s.push_str(" Then\n");
                    for st in body {
                        s.push_str(&st.emit(indent + 1));
                        s.push_str("\n");
                    }
                }
                match els {
                    Some(body) => {
                        s.push_str(&this_indent);
                        s.push_str("Else\n");
                        for st in body {
                            s.push_str(&st.emit(indent + 1));
                            s.push_str("\n");
                        }
                    },
                    _ => {}
                };
                s.push_str(&this_indent);
                s.push_str("End If");
                s
            },

            &ast::Statement::WhileLoop { cond, body } => {
                let mut s = emit_indent(indent);
                s.push_str("Do While ");
                s.push_str(&cond.emit(0));
                s.push_str("\n");
                for st in body {
                    s.push_str(&st.emit(indent + 1));
                    s.push_str("\n");
                }
                s.push_str(&emit_indent(indent));
                s.push_str("Loop");
                s
            }
        }
    }
}

impl<'a> Emit for ast::Expression<'a> {
    fn emit(&self, indent: u32) -> String {
        match self {
            &ast::Expression::Literal(ref l) => l.emit(indent),
            &ast::Expression::Ident(ref i) =>
                format!("{}{}", emit_indent(indent), i.0),
            // TODO: probably need the symbol table here to choose VarPtr vs
            //   StrPtr vs AddressOf
            &ast::Expression::AddressOf(ref i) =>
                format!("{}VarPtr({})", emit_indent(indent), i.0),
            // TODO: probably need the symbol table here to choose () vs
            //   .Item() etc
            &ast::Expression::Index(place, i) =>
                format!("{}{}({})",
                        emit_indent(indent),
                        place.emit(0),
                        i.emit(0)),
            &ast::Expression::UnOpApply(op, e) =>
                format!("{}{}{}", emit_indent(indent), op.emit(0), e.emit(0)),
            &ast::Expression::BinOpApply(op, e1, e2) => format!("{}{}{}{}",
                emit_indent(indent), e1.emit(0), op.emit(0), e2.emit(0)),
            &ast::Expression::Grouped(e) =>
                format!("{}({})", emit_indent(indent), e.emit(0)),
            &ast::Expression::FnCall(ref i, args) =>
                format!("{}{}({})", emit_indent(indent), i.0,
                  args.iter().map(|a| a.emit(0)).collect::<Vec<_>>().join(", ")),
        }
    }
}

impl<'a> Emit for ast::Literal {
    fn emit(&self, indent: u32) -> String {
        let mut s = emit_indent(indent);
        match self {
            &ast::Literal::Bool(b) =>
                s.push_str(if b { "True" } else { "False" }),
            &ast::Literal::Int(i) => s.push_str(&i.to_string()),
            &ast::Literal::Float(f) => {
                s.push_str(&f.to_string());
                s.push_str("#");
            },
            &ast::Literal::Str(ref t) => {
                s.push_str(&escape_string(t));
            },
            _ => unimplemented!(),
        };
        s
    }
}

impl Emit for ast::UnOp {
    fn emit(&self, _indent: u32) -> String {
        match self {
            &ast::UnOp::Minus => String::from("-"),
            &ast::UnOp::LogNot => String::from("Not "),
            &ast::UnOp::BitNot => String::from("Not "),
        }
    }
}

/// Built-in binary operators
impl Emit for ast::BinOp {
    fn emit(&self, _indent: u32) -> String {
        match self {
            &ast::BinOp::Dot => String::from("."),
            &ast::BinOp::Add => String::from(" + "),
            &ast::BinOp::Sub => String::from(" - "),
            &ast::BinOp::Mul => String::from(" * "),
            &ast::BinOp::Div => String::from(" / "),
            &ast::BinOp::Pow => String::from("^"),
            &ast::BinOp::StrConcat => String::from(" & "),
            &ast::BinOp::Eq => String::from(" = "),
            &ast::BinOp::NotEq => String::from(" <> "),
            &ast::BinOp::Lt => String::from(" < "),
            &ast::BinOp::Gt => String::from(" > "),
            &ast::BinOp::LtEq => String::from(" <= "),
            &ast::BinOp::GtEq => String::from(" >= "),
            &ast::BinOp::LogAnd => String::from(" And "),
            &ast::BinOp::LogOr => String::from(" Or "),
            &ast::BinOp::BitAnd => String::from(" And "),
            &ast::BinOp::BitOr => String::from(" Or "),
            &ast::BinOp::BitXor => String::from(" Xor "),
        }
    }
}

impl Emit for ast::AccessMode {
    fn emit(&self, _indent: u32) -> String {
        match self {
            &ast::AccessMode::Private => String::from("Private"),
            &ast::AccessMode::Public => String::from("Public"),
        }
    }
}

impl Emit for ast::ParamMode {
    fn emit(&self, _indent: u32) -> String {
        match self {
            &ast::ParamMode::ByVal => String::from("ByVal"),
            &ast::ParamMode::ByRef => String::from("ByRef"),
        }
    }
}

// TODO: handle multidimensional arrays properly
impl<'a> Emit for ast::Type<'a> {
    fn emit(&self, _indent: u32) -> String {
        match self {
            &ast::Type::Boolean => String::from("Boolean"),
            &ast::Type::Byte => String::from("Byte"),
            &ast::Type::Integer => String::from("Integer"),
            &ast::Type::Long => String::from("Long"),
            &ast::Type::LongPtr => String::from("LongPtr"),
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
                let mut base = t.emit(_indent);
                base.push_str("()");
                base
            },
        }
    }
}

fn emit_indent(indent: u32) -> String {
    // use String::repeat once that's stabilized
    let mut s = String::with_capacity(indent as usize * INDENT.len());
    for _ in 0..indent {
        s.push_str(INDENT);
    }
    s
}

fn emit_func(mode: &ast::AccessMode, func: &ast::Function,
  indent: u32) -> String {
    let param_spec = func.params.iter()
        .map(|ref s| s.emit(0))
        .collect::<Vec<_>>()
        .join(", ");
    let body = func.body.iter()
        .map(|ref s| s.emit(indent + 1))
        .collect::<Vec<_>>()
        .join("\n");
    format!("{ind}{access} {type} {name} ({params}){ret}\n\
             {body}\n\
             {ind}End {type}",
      ind = emit_indent(indent),
      access = mode.emit(0),
      type = if func.ret.is_some() { "Function" } else { "Sub" },
      name = func.name.0,
      params = param_spec,
      ret = match func.ret {
          Some(ref t) => {
              let mut s = String::from(" As ");
              s.push_str(&t.emit(0));
              s
          },
          _ => String::new(),
      },
      body = body)
}

fn emit_struct(mode: &ast::AccessMode, def: &ast::StructDef,
  indent: u32) -> String {
    let member_spec = def.members.iter()
        .map(|ref m| m.emit(indent + 1))
        .collect::<Vec<_>>()
        .join("\n");
    format!("{ind}{access} Type {name}\n{members}\n{ind}End Type",
      ind = emit_indent(indent),
      access = mode.emit(0),
      name = def.name.0,
      members = member_spec)
}

fn emit_enum(mode: &ast::AccessMode, def: &ast::EnumDef,
  indent: u32) -> String {
    format!("{ind}{access} Enum {name}\n{members}\n{ind}End Enum",
      ind = emit_indent(indent),
      access = mode.emit(0),
      name = def.name.0,
      members = def.members.iter().map(|ref i| {
            let mut s = emit_indent(indent + 1);
            s.push_str(i.0);
            s
        })
        .collect::<Vec<_>>().join("\n"))
}

fn escape_string(s: &str) -> String {
    let s = s.replace("\"", "\"\"");
    let s = s.replace("\\n", "\" & vbCrLf & \"");
    let s = s.replace("\\t", "\" & vbTab & \"");
    format!("\"{}\"", s)
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
                body: &[
                    ast::Statement::Declaration(
                        &ast::VariableDeclaration {
                            name: ast::Ident("x"), typ: &ast::Type::Long
                        },
                        Some(&ast::Expression::Literal(
                            ast::Literal::Int(2349)
                        )),
                    ),
                    ast::Statement::Declaration(
                        &ast::VariableDeclaration {
                            name: ast::Ident("y"),
                            typ: &ast::Type::Array(&ast::Type::Double, Some(25))
                        },
                        None,
                    ),
                    ast::Statement::Assignment(
                        &ast::Expression::Ident(ast::Ident("x")),
                        &ast::Expression::Literal(
                            ast::Literal::Str(String::from("I ate\\ta lot of \
                              meat\\n...the other day"))
                        )),
                    ast::Statement::Assignment(
                        &ast::Expression::Index(
                            &ast::Expression::Ident(ast::Ident("y")),
                            &ast::Expression::Literal(ast::Literal::Int(45))
                        ),
                        &ast::Expression::Literal(ast::Literal::Float(123.45))
                    ),
                    ast::Statement::Conditional {
                        cond: &ast::Expression::BinOpApply(
                                  ast::BinOp::LtEq,
                                  &ast::Expression::Ident(ast::Ident("x")),
                                  &ast::Expression::Literal(
                                      ast::Literal::Int(20))),
                        body: &[
                            &ast::Statement::Declaration(
                                &ast::VariableDeclaration {
                                    name: ast::Ident("z"),
                                    typ: &ast::Type::Currency,
                                },
                                None
                            ),
                        ],
                        elsifs: &[],
                        els: Some(&[
                                  &ast::Statement::Assignment(
                                      &ast::Expression::Ident(ast::Ident("x")),
                                      &ast::Expression::Literal(
                                          ast::Literal::Int(9))),
                        ]),
                    },
                    ast::Statement::WhileLoop {
                        cond: &ast::Expression::BinOpApply(
                                  ast::BinOp::LtEq,
                                  &ast::Expression::Ident(ast::Ident("x")),
                                  &ast::Expression::Literal(
                                      ast::Literal::Int(20))),
                        body: &[
                            &ast::Statement::Declaration(
                                &ast::VariableDeclaration {
                                    name: ast::Ident("z"),
                                    typ: &ast::Type::Currency,
                                },
                                None
                            ),
                        ],
                    },
                ],
            }
        ).emit(1);
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
        ).emit(1);
        println!("{}", s);
    }

    #[test]
    fn emit_en() {
        let s = ast::Item::EnumDef(
            ast::AccessMode::Private,
            &ast::EnumDef {
                name: ast::Ident("my_enum"),
                members: &[
                    ast::Ident("FirstChoice"),
                    ast::Ident("SecondChoice"),
                    ast::Ident("ThirdChoice"),
                ],
            },
        ).emit(1);
        println!("{}", s);
    }
}
