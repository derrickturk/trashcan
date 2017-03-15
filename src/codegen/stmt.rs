//! code generation for trashcan statements

use std::io;
use std::io::Write;

use ast::*;
use analysis;
use analysis::SymbolTable;
use analysis::ExprCtxt;
use super::*;
use super::expr::*;
use super::ty::*;
use super::bits::*;

#[macro_use] use super::super::parser;

impl<'a> Emit<&'a (&'a FunDef, ExprCtxt)> for Stmt {
    fn emit<W: Write>(&self, out: &mut W, symtab: &SymbolTable,
      ctxt: &'a (&'a FunDef, ExprCtxt), indent: u32) -> io::Result<()> {
        match self.data {
            StmtKind::ExprStmt(ref e) => {
                e.emit(out, symtab, ExprPos::Stmt, indent)?;
                out.write_all(b"\n")
            },

            StmtKind::VarDecl(ref decls) => {
                for decl in decls {
                    emit_decl(out, decl, symtab, ctxt, indent)?;
                }
                Ok(())
            },

            StmtKind::Assign(ref place, ref op, ref expr) => {
                write!(out, "{:in$}", "",
                  in = (indent * INDENT) as usize)?;

                let place_ty = analysis::type_of(place, symtab, &ctxt.1)
                    .expect("dumpster fire: untypeable expression \
                      in codegen");
                match place_ty.is_object() {
                    Some(true) => out.write_all(b"Set ")?,
                    Some(false) => {},
                    None => {
                        let expr_ty = analysis::type_of(expr, symtab, &ctxt.1)
                            .expect("dumpster fire: untypeable expression \
                              in codegen");
                        match expr_ty.is_object() {
                            Some(true) => out.write_all(b"Set ")?,
                            _ => {},
                        }
                    }
                };

                place.emit(out, symtab, ExprPos::Expr, 0)?;
                out.write_all(b" = ")?;
                match op {
                    &AssignOp::Assign => {},
                    _ => {
                        place.emit(out, symtab, ExprPos::Expr, 0)?;
                        op.emit(out, symtab, (), 0)?;
                    }
                }
                expr.emit(out, symtab, ExprPos::Expr, 0)?;
                out.write_all(b"\n")
            },

            StmtKind::Return(ref expr) => {
                let fnsub = match ctxt.0.ret {
                    Type::Void => "Sub",
                    _ => "Function",
                };

                match expr {
                    &Some(ref e) => {
                        write!(out, "{:in$}", "",
                          in = (indent * INDENT) as usize)?;
                        match ctxt.0.ret.is_object() {
                            Some(true) => out.write_all(b"Set ")?,
                            Some(false) => {},
                            None => {
                                let ret_ty = analysis::type_of(
                                    e, symtab, &ctxt.1)
                                    .expect("dumpster fire: \
                                      untypeable expression in codegen");
                                match ret_ty.is_object() {
                                    Some(true) => out.write_all(b"Set ")?,
                                    _ => {},
                                };
                            },
                        }
                        ctxt.0.name.emit(out, symtab, (), 0)?;
                        out.write_all(b" = ")?;
                        e.emit(out, symtab, ExprPos::Expr, 0)?;
                        out.write_all(b"\n")?;
                    },
                    &None => {}
                }

                // if we're the last statement in the function body,
                //   we don't need an "Exit Function"; we use a ptr cast here
                //   because we really will be a reference to
                //   ctxt.0.body.last().unwrap()
                if self as *const _ != ctxt.0.body.last().unwrap() as *const _ {
                    write!(out, "{:in$}Exit {}\n", "", fnsub,
                      in = (indent * INDENT) as usize)?;
                }

                Ok(())
            },

            StmtKind::Print(ref expr) => {
                write!(out, "{:in$}Debug.Print ", "",
                  in = (indent * INDENT) as usize)?;
                expr.emit(out, symtab, ExprPos::Expr, 0)?;
                out.write_all(b"\n")
            },

            StmtKind::IfStmt { ref cond, ref body, ref elsifs, ref els } => {
                write!(out, "{:in$}If ", "", in = (indent * INDENT) as usize)?;
                cond.emit(out, symtab, ExprPos::Expr, 0)?;
                out.write_all(b" Then\n")?;
                for stmt in body {
                    stmt.emit(out, symtab, ctxt, indent + 1)?;
                }

                for &(ref cond, ref body) in elsifs {
                    write!(out, "{:in$}ElseIf ", "",
                      in = (indent * INDENT) as usize)?;
                    cond.emit(out, symtab, ExprPos::Expr, 0)?;
                    out.write_all(b" Then\n")?;
                    for stmt in body {
                        stmt.emit(out, symtab, ctxt, indent + 1)?;
                    }
                }

                if let &Some(ref body) = els {
                    write!(out, "{:in$}Else\n", "",
                      in = (indent * INDENT) as usize)?;
                    for stmt in body {
                        stmt.emit(out, symtab, ctxt, indent + 1)?;
                    }
                }

                write!(out, "{:in$}End If\n", "",
                  in = (indent * INDENT) as usize)
            },

            StmtKind::WhileLoop { ref cond, ref body } => {
                write!(out, "{:in$}Do While ", "",
                  in = (indent * INDENT) as usize)?;
                cond.emit(out, symtab, ExprPos::Expr, 0)?;
                out.write_all(b"\n")?;

                for stmt in body {
                    stmt.emit(out, symtab, ctxt, indent + 1)?;
                }

                write!(out, "{:in$}Loop\n", "",
                  in = (indent * INDENT) as usize)
            },

            StmtKind::ForLoop { ref var, ref spec, ref body } => {
                let vardecl = Stmt {
                    data: StmtKind::VarDecl(
                              vec![(var.0.clone(), var.1.clone(), None)]),
                    loc: empty_loc!(),
                };
                vardecl.emit(out, symtab, ctxt, indent)?;

                write!(out, "{:in$}For ", "",
                  in = (indent * INDENT) as usize)?;

                match *spec {
                    ForSpec::Range(ref from, ref to, ref by) => {
                        var.0.emit(out, symtab, (), 0)?;
                        out.write_all(b" = ")?;
                        from.emit(out, symtab, ExprPos::Expr, 0)?;
                        out.write_all(b" To ")?;
                        to.emit(out, symtab, ExprPos::Expr, 0)?;
                        match *by {
                            Some(ref step) => {
                                out.write_all(b" Step ")?;
                                step.emit(out, symtab, ExprPos::Expr, 0)?;
                            },
                            None => {},
                        }
                        out.write_all(b"\n");
                    },

                    ForSpec::Each(ref expr) => {
                        out.write_all(b"Each ")?;
                        var.0.emit(out, symtab, (), 0)?;
                        out.write_all(b" In ")?;
                        expr.emit(out, symtab, ExprPos::Expr, 0)?;
                        out.write_all(b"\n")?;
                    },
                }

                for stmt in body {
                    stmt.emit(out, symtab, ctxt, indent + 1)?;
                }

                write!(out, "{:in$}Next ", "",
                  in = (indent * INDENT) as usize)?;
                var.0.emit(out, symtab, (), 0)?;
                out.write_all(b"\n")
            }

            ref other => {
                write!(out, "{:in$}{:?}\n", "", other,
                  in = (indent * INDENT) as usize)
            }
        }
    }
}

fn emit_decl<'a, W: Write>(out: &mut W, decl: &(Ident, Type, Option<Expr>),
  symtab: &SymbolTable, ctxt: &'a (&'a FunDef, ExprCtxt), indent: u32)
  -> io::Result<()> {
    write!(out, "{:in$}Dim ", "", in = (indent * INDENT) as usize)?;
    decl.0.emit(out, symtab, (), 0)?;
    decl.1.emit(out, symtab, TypePos::Decl, 0)?;
    out.write_all(b"\n")?;
    
    match decl.2 {
        Some(ref init) => {
            let assign_stmt = Stmt {
                data: StmtKind::Assign(
                          Expr {
                              data: ExprKind::Name(Path(None, decl.0.clone())),
                              loc: empty_loc!(),
                          },
                          AssignOp::Assign,
                          init.clone()
                      ),
                      loc: empty_loc!(),
            };
            assign_stmt.emit(out, symtab, ctxt, indent)?;
        },

        None => {},
    }
    
    Ok(())
}
