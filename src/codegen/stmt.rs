//! code generation for trashcan statements

use std::io;
use std::io::Write;

use ast::*;
use super::*;
use super::expr::*;
use super::ty::*;
use super::bits::*;

impl<'a> Emit<&'a FunDef> for Stmt {
    fn emit<W: Write>(&self, out: &mut W, ctxt: &'a FunDef, indent: u32)
      -> io::Result<()> {
        match self.data {
            StmtKind::ExprStmt(ref e) => {
                e.emit(out, ExprPos::Stmt, indent)
            },

            StmtKind::VarDecl(ref decls) => {
                for decl in decls {
                    emit_decl(out, decl, indent)?;
                }
                Ok(())
            },

            StmtKind::Assign(ref place, ref op, ref expr) => {
                place.emit(out, ExprPos::Expr, indent)?;
                write!(out, " {:?} ", op)?;
                expr.emit(out, ExprPos::Expr, 0)?;
                out.write_all(b"\n")
            },

            StmtKind::Return(ref expr) => {
                let fnsub = match ctxt.ret {
                    None => "Sub",
                    Some(_) => "Function",
                };

                match expr {
                    &Some(ref e) => {
                        write!(out, "{:in$}", "",
                          in = (indent * INDENT) as usize)?;
                        match ctxt.ret.as_ref().unwrap().is_object() {
                            Some(true) => out.write_all(b"Set ")?,
                            Some(false) => {},
                            None => {
                                // TODO: invoke "type inference" on RHS here
                            }
                        }
                        ctxt.name.emit(out, (), 0)?;
                        out.write_all(b" = ")?;
                        e.emit(out, ExprPos::Expr, 0)?;
                        out.write_all(b"\n")?;
                    },
                    &None => {}
                }

                // TODO: don't need this if last stmt in fundef
                write!(out, "{:in$}Exit {}\n", "", fnsub,
                  in = (indent * INDENT) as usize)
            },

            StmtKind::Print(ref expr) => {
                write!(out, "{:in$}Debug.Print ", "",
                  in = (indent * INDENT) as usize)?;
                expr.emit(out, ExprPos::Expr, 0)?;
                out.write_all(b"\n")
            },

            StmtKind::IfStmt { ref cond, ref body, ref elsifs, ref els } => {
                write!(out, "{:in$}If ", "", in = (indent * INDENT) as usize)?;
                cond.emit(out, ExprPos::Expr, 0)?;
                out.write_all(b" Then\n")?;
                for stmt in body {
                    stmt.emit(out, ctxt, indent + 1)?;
                }

                for &(ref cond, ref body) in elsifs {
                    write!(out, "{:in$}ElseIf ", "",
                      in = (indent * INDENT) as usize)?;
                    cond.emit(out, ExprPos::Expr, 0)?;
                    out.write_all(b" Then\n")?;
                    for stmt in body {
                        stmt.emit(out, ctxt, indent + 1)?;
                    }
                }

                if let &Some(ref body) = els {
                    write!(out, "{:in$}Else\n", "",
                      in = (indent * INDENT) as usize)?;
                    for stmt in body {
                        stmt.emit(out, ctxt, indent + 1)?;
                    }
                }

                write!(out, "{:in$}End If\n", "",
                  in = (indent * INDENT) as usize)
            },

            ref other => {
                write!(out, "{:in$}{:?}\n", "", other,
                  in = (indent * INDENT) as usize)
            }
        }
    }
}

fn emit_decl<W: Write>(out: &mut W, decl: &(Ident, Type, Option<Expr>),
  indent: u32) -> io::Result<()> {
    write!(out, "{:in$}{:?}\n", "", decl, in = (indent * INDENT) as usize)
}
