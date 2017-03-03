//! code generator: emit VB6 from trashcan ASTs

use std::io;
use std::io::Write;

use ast::*;
use parser::SrcLoc;

const INDENT: u32 = 4;

// TODO: probably more like emit(&self, symtab: &..., indent: u32)

/// trait for all emittable types
pub trait Emit<Ctxt> {
    fn emit<W: Write>(&self, out: &mut W, ctxt: Ctxt, indent: u32)
      -> io::Result<()>;
}

mod module;
mod item;
mod stmt;
mod expr;
mod ty;
mod lit;
mod bits;

/*

// TODO: handle multidimensional arrays properly
impl<'a> Emit for FunctionParameter<'a> {
    fn emit(&self, _indent: u32) -> String {
        match self.typ {
            &Type::Array(ref inner, _) => FunctionParameter {
                name: Ident({
                    let mut s = String::from(self.name.0);
                    s.push_str("()");
                    s
                }),
                typ: inner,
                mode: self.mode,
                loc: self.loc.clone(),
            }.emit(0),
            t => format!("{} {} as {}", self.mode.emit(0), self.name.0,
              t.emit(0))
        }
    }
}

// TODO: handle multidimensional arrays properly
impl<'a> Emit for VariableDeclaration<'a> {
    fn emit(&self, indent: u32) -> String {
        match self.typ {
            &Type::Array(ref inner, dim) => VariableDeclaration {
                name: Ident({
                    let mut s = String::from(self.name.0);
                    match dim {
                        Some(n) => s.push_str(&format!("({})", n)),
                        None => s.push_str(""),
                    };
                    s
                }),
                typ: inner,
                loc: self.loc.clone(),
            }.emit(indent),
            t => format!("{}{} as {}", emit_indent(indent),
              self.name.0, t.emit(0))
        }
    }
}

impl<'a> Emit for Statement<'a> {
    fn emit(&self, indent: u32) -> String {
        match &self.kind {
            &StatementKind::Declaration(decl, None) => {
                let mut s = emit_indent(indent);
                s.push_str("Dim ");
                s.push_str(&decl.emit(0));
                s
            },

            &StatementKind::Declaration(decl, Some(expr)) => {
                let mut s = Statement {
                    kind: StatementKind::Declaration(decl, None),
                    loc: self.loc.clone(),
                }.emit(indent);
                s.push_str("\n");
                s.push_str(
                    &Statement {
                        kind: StatementKind::Assignment(
                                  &Expression::Ident(decl.name), expr),
                        loc: self.loc.clone(),
                    }.emit(indent));
                s
            },

            // TODO: this will have to look up the type of the symbol identified
            //   by ident in the symbol table eventually (to decide whether
            //   to emit = or Set =)
            &StatementKind::Assignment(place, expr) => {
                format!("{}{} = {}",
                        emit_indent(indent),
                        place.emit(0),
                        expr.emit(0))
            },

            &StatementKind::FnCall(ident, args) => {
                format!("{}{} {}", emit_indent(indent), ident.0,
                  args.iter().map(|a| a.emit(0)).collect::<Vec<_>>().join(", "))
            },

            &StatementKind::Conditional { cond, body, elsifs, els } => {
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

            &StatementKind::WhileLoop { cond, body } => {
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

impl<'a> Emit for Expression<'a> {
    fn emit(&self, indent: u32) -> String {
        match self {
            &Expression::Literal(ref l) => l.emit(indent),
            &Expression::Ident(ref i) =>
                format!("{}{}", emit_indent(indent), i.0),
            // TODO: probably need the symbol table here to choose VarPtr vs
            //   StrPtr vs AddressOf
            &Expression::AddressOf(ref i) =>
                format!("{}VarPtr({})", emit_indent(indent), i.0),
            // TODO: probably need the symbol table here to choose () vs
            //   .Item() etc
            &Expression::Index(place, i) =>
                format!("{}{}({})",
                        emit_indent(indent),
                        place.emit(0),
                        i.emit(0)),
            &Expression::UnOpApply(op, e) =>
                format!("{}{}{}", emit_indent(indent), op.emit(0), e.emit(0)),
            &Expression::BinOpApply(op, e1, e2) => format!("{}{}{}{}",
                emit_indent(indent), e1.emit(0), op.emit(0), e2.emit(0)),
            &Expression::Grouped(e) =>
                format!("{}({})", emit_indent(indent), e.emit(0)),
            &Expression::FnCall(ref i, args) =>
                format!("{}{}({})", emit_indent(indent), i.0,
                  args.iter().map(|a| a.emit(0)).collect::<Vec<_>>().join(", ")),
        }
    }
}

impl<'a> Emit for Literal {
    fn emit(&self, indent: u32) -> String {
        let mut s = emit_indent(indent);
        match self {
            &Literal::Bool(b) =>
                s.push_str(if b { "True" } else { "False" }),
            &Literal::Int(i) => s.push_str(&i.to_string()),
            &Literal::Float(f) => {
                s.push_str(&f.to_string());
                s.push_str("#");
            },
            &Literal::Str(ref t) => {
                s.push_str(&escape_string(t));
            },
            _ => unimplemented!(),
        };
        s
    }
}

impl Emit for UnOp {
    fn emit(&self, _indent: u32) -> String {
        match self {
            &UnOp::Minus => String::from("-"),
            &UnOp::LogNot => String::from("Not "),
            &UnOp::BitNot => String::from("Not "),
        }
    }
}

/// Built-in binary operators
impl Emit for BinOp {
    fn emit(&self, _indent: u32) -> String {
        match self {
            &BinOp::Dot => String::from("."),
            &BinOp::Add => String::from(" + "),
            &BinOp::Sub => String::from(" - "),
            &BinOp::Mul => String::from(" * "),
            &BinOp::Div => String::from(" / "),
            &BinOp::Pow => String::from("^"),
            &BinOp::StrConcat => String::from(" & "),
            &BinOp::Eq => String::from(" = "),
            &BinOp::NotEq => String::from(" <> "),
            &BinOp::Lt => String::from(" < "),
            &BinOp::Gt => String::from(" > "),
            &BinOp::LtEq => String::from(" <= "),
            &BinOp::GtEq => String::from(" >= "),
            &BinOp::LogAnd => String::from(" And "),
            &BinOp::LogOr => String::from(" Or "),
            &BinOp::BitAnd => String::from(" And "),
            &BinOp::BitOr => String::from(" Or "),
            &BinOp::BitXor => String::from(" Xor "),
        }
    }
}

impl Emit for AccessMode {
    fn emit(&self, _indent: u32) -> String {
        match self {
            &AccessMode::Private => String::from("Private"),
            &AccessMode::Public => String::from("Public"),
        }
    }
}

impl Emit for ParamMode {
    fn emit(&self, _indent: u32) -> String {
        match self {
            &ParamMode::ByVal => String::from("ByVal"),
            &ParamMode::ByRef => String::from("ByRef"),
        }
    }
}

// TODO: handle multidimensional arrays properly
impl<'a> Emit for Type<'a> {
    fn emit(&self, _indent: u32) -> String {
        match self {
            &Type::Boolean => String::from("Boolean"),
            &Type::Byte => String::from("Byte"),
            &Type::Integer => String::from("Integer"),
            &Type::Long => String::from("Long"),
            &Type::LongPtr => String::from("LongPtr"),
            &Type::Single => String::from("Single"),
            &Type::Double => String::from("Double"),
            &Type::String => String::from("String"),
            &Type::Currency => String::from("Currency"),
            &Type::Date => String::from("Date"),
            &Type::Variant => String::from("Variant"),
            &Type::Object(Ident(i)) => i.to_string(),
            &Type::Struct(Ident(i)) => i.to_string(),
            &Type::Enum(Ident(i)) => i.to_string(),
            &Type::Array(t, _) => {
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

fn emit_func(mode: AccessMode, func: &Function,
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

fn emit_struct(mode: AccessMode, def: &StructDef,
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

fn emit_enum(mode: AccessMode, def: &EnumDef,
  indent: u32) -> String {
    format!("{ind}{access} Enum {name}\n{members}\n{ind}End Enum",
      ind = emit_indent(indent),
      access = mode.emit(0),
      name = def.name.0,
      members = def.members.iter().map(|ref i| {
            let mut s = emit_indent(indent + 1);
            s.push_str(&i.0);
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

*/

/*
#[cfg(test)]
mod test {
    use super::*;
    use parser::SrcLoc;

    #[test]
    fn emit_fn() {
        let loc = SrcLoc {
            file: String::from("<test literal>"),
            line: 0,
            start: 0,
            len: 0,
        };

        let s = Item {
            kind: ItemKind::Function(&Function {
                name: Ident("do_whatever"),
                params: &[
                    FunctionParameter {
                        name: Ident("x"),
                        typ: &Type::Long,
                        mode: ParamMode::ByVal,
                        loc: loc.clone(),
                    },
                    FunctionParameter {
                        name: Ident("y"),
                        typ: &Type::Double,
                        mode: ParamMode::ByRef,
                        loc: loc.clone(),
                    },
                    FunctionParameter {
                        name: Ident("z"),
                        typ: &Type::Array(&Type::Double, None),
                        mode: ParamMode::ByRef,
                        loc: loc.clone(),
                    },
                ],
                ret: Some(Type::Struct(Ident("MyType"))),
                body: &[
                    Statement {
                        kind: StatementKind::Declaration(
                            &VariableDeclaration {
                                name: Ident("x"),
                                typ: &Type::Long,
                                loc: loc.clone(),
                            },
                            Some(&Expression::Literal(
                                Literal::Int(2349)
                            )),
                        ),
                        loc: loc.clone(),
                    },
                    Statement {
                        kind: StatementKind::Declaration(
                            &VariableDeclaration {
                                name: Ident("y"),
                                typ: &Type::Array(&Type::Double, Some(25)),
                                loc: loc.clone(),
                            },
                            None,
                        ),
                        loc: loc.clone(),
                    },
                    Statement {
                        kind: StatementKind::Assignment(
                            &Expression::Ident(Ident("x")),
                            &Expression::Literal(
                                Literal::Str(String::from("I ate\\ta lot of \
                                  meat\\n...the other day"))
                            )),
                            loc: loc.clone(),
                    },
                    Statement {
                        kind: StatementKind::Assignment(
                            &Expression::Index(
                                &Expression::Ident(Ident("y")),
                                &Expression::Literal(Literal::Int(45))
                            ),
                            &Expression::Literal(Literal::Float(123.45))
                        ),
                        loc: loc.clone(),
                    },
                    Statement {
                        kind: StatementKind::Conditional {
                            cond: &Expression::BinOpApply(
                                      BinOp::LtEq,
                                      &Expression::Ident(Ident("x")),
                                      &Expression::Literal(
                                          Literal::Int(20))),
                            body: &[
                                &Statement {
                                    kind: StatementKind::Declaration(
                                        &VariableDeclaration {
                                            name: Ident("z"),
                                            typ: &Type::Currency,
                                            loc: loc.clone(),
                                        },
                                        None
                                    ),
                                    loc: loc.clone(),
                                },
                            ],
                            elsifs: &[],
                            els: Some(&[
                                  &Statement {
                                      kind: StatementKind::Assignment(
                                          &Expression::Ident(Ident("x")),
                                          &Expression::Literal(
                                              Literal::Int(9))),
                                      loc: loc.clone(),
                                  },
                            ]),
                        },
                        loc: loc.clone(),
                    },
                    Statement {
                        kind: StatementKind::WhileLoop {
                            cond: &Expression::BinOpApply(
                                      BinOp::LtEq,
                                      &Expression::Ident(Ident("x")),
                                      &Expression::Literal(
                                          Literal::Int(20))),
                            body: &[
                                &Statement {
                                    kind: StatementKind::Declaration(
                                        &VariableDeclaration {
                                            name: Ident("z"),
                                            typ: &Type::Currency,
                                            loc: loc.clone(),
                                        },
                                        None
                                    ),
                                    loc: loc.clone(),
                                },
                            ],
                        },
                        loc: loc.clone(),
                    },
                ],
            }),
            access: AccessMode::Private,
            loc: loc.clone(),
        }.emit(1);
        println!("{}", s);
    }

    #[test]
    fn emit_st() {
        let s = Item {
            kind: ItemKind::StructDef(&StructDef {
                name: Ident("my_struct"),
                members: &[
                    VariableDeclaration {
                        name: Ident("my_arr"),
                        typ: &Type::Array(&Type::Double, Some(10)),
                        loc: parser::SrcLoc {
                            file: String::from("<test literal>"),
                            line: 0,
                            start: 0,
                            len: 0,
                        },
                    },
                    VariableDeclaration {
                        name: Ident("my_dbl"),
                        typ: &Type::Double,
                        loc: parser::SrcLoc {
                            file: String::from("<test literal>"),
                            line: 0,
                            start: 0,
                            len: 0,
                        },
                    },
                ],
            }),
            access: AccessMode::Public,
            loc: parser::SrcLoc {
                file: String::from("<test literal>"),
                line: 0,
                start: 0,
                len: 0,
            },
        }.emit(1);
        println!("{}", s);
    }

    #[test]
    fn emit_en() {
        let s = Item {
            kind: ItemKind::EnumDef(&EnumDef {
                name: Ident("my_enum"),
                members: &[
                    Ident("FirstChoice"),
                    Ident("SecondChoice"),
                    Ident("ThirdChoice"),
                ],
            }),
            access: AccessMode::Private,
            loc: parser::SrcLoc {
                file: String::from("<test literal>"),
                line: 0,
                start: 0,
                len: 0,
            },
        }.emit(1);
        println!("{}", s);
    }
}
*/
