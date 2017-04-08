//! code generation for trashcan statements

use std::io;
use std::io::Write;

use ast::*;
use parser::SrcLoc;
use analysis::SymbolTable;
use super::*;
use super::expr::*;
use super::ty::*;

impl<'a> Emit<&'a FunDef> for Stmt {
    fn emit<W: Write>(&self, out: &mut W, symtab: &SymbolTable,
      ctxt: &'a FunDef, indent: u32) -> io::Result<()> {
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

                let place_ty = place.ty.as_ref()
                    .expect("dumpster fire: untyped expression \
                      in codegen");
                match place_ty.is_object() {
                    Some(true) => out.write_all(b"Set ")?,
                    Some(false) => {},
                    None => {
                        let expr_ty = expr.ty.as_ref()
                            .expect("dumpster fire: untyped expression \
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
                let fnsub = match ctxt.ret {
                    Type::Void => "Sub",
                    _ => "Function",
                };

                match expr {
                    &Some(ref e) => {
                        write!(out, "{:in$}", "",
                          in = (indent * INDENT) as usize)?;
                        match ctxt.ret.is_object() {
                            Some(true) => out.write_all(b"Set ")?,
                            Some(false) => {},
                            None => {
                                let ret_ty = e.ty.as_ref()
                                    .expect("dumpster fire: \
                                      untyped expression in codegen");
                                match ret_ty.is_object() {
                                    Some(true) => out.write_all(b"Set ")?,
                                    _ => {},
                                };
                            },
                        }
                        ctxt.name.emit(out, symtab, (), 0)?;
                        out.write_all(b" = ")?;
                        e.emit(out, symtab, ExprPos::Expr, 0)?;
                        out.write_all(b"\n")?;
                    },
                    &None => {}
                }

                // if we're the last statement in the function body,
                //   we don't need an "Exit Function"; we use a ptr cast here
                //   because we really will be a reference to
                //   ctxt.body.last().unwrap()
                if self as *const _ != ctxt.body.last().unwrap() as *const _ {
                    write!(out, "{:in$}Exit {}\n", "", fnsub,
                      in = (indent * INDENT) as usize)?;
                }

                Ok(())
            },

            StmtKind::Alloc(ref expr, ref extents) => {
                write!(out, "{:in$}ReDim ", "",
                  in = (indent * INDENT) as usize)?;
                expr.emit(out, symtab, ExprPos::Expr, 0)?;
                emit_alloc_extents(out, extents, symtab, 0)?;
                out.write_all(b"\n")
            },

            StmtKind::ReAlloc(ref expr, preserved, ref extents) => {
                write!(out, "{:in$}ReDim Preserve ", "",
                  in = (indent * INDENT) as usize)?;
                expr.emit(out, symtab, ExprPos::Expr, 0)?;
                emit_realloc_extents(out, expr, (preserved, extents),
                  symtab, 0)?;
                out.write_all(b"\n")
            },

            StmtKind::DeAlloc(ref expr) => {
                write!(out, "{:in$}Erase ", "",
                  in = (indent * INDENT) as usize)?;
                expr.emit(out, symtab, ExprPos::Expr, 0)?;
                out.write_all(b"\n")
            },

            StmtKind::Print(ref exprs) => {
                write!(out, "{:in$}Debug.Print ", "",
                  in = (indent * INDENT) as usize)?;
                for (i, expr) in exprs.iter().enumerate() {
                    if i != 0 {
                        // TODO: is this what we want or & & &
                        //   probably punt until we have "as str"
                        out.write_all(b"; ")?;
                    }
                    expr.emit(out, symtab, ExprPos::Expr, 0)?;
                }
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
                    loc: SrcLoc::empty(),
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
                        out.write_all(b"\n")?;
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
            },

            StmtKind::ForAlong { .. } => {
                panic!("dumpster fire: raw ForAlong in codegen");
            },
        }
    }
}

fn emit_decl<'a, W: Write>(out: &mut W, decl: &(Ident, Type, Option<Expr>),
  symtab: &SymbolTable, ctxt: &'a FunDef, indent: u32)
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
                              ty: Some(decl.1.clone()),
                              loc: SrcLoc::empty(),
                          },
                          AssignOp::Assign,
                          init.clone()
                      ),
                      loc: SrcLoc::empty(),
            };
            assign_stmt.emit(out, symtab, ctxt, indent)?;
        },

        None => {},
    }
    
    Ok(())
}

fn emit_alloc_extents<W: Write>(out: &mut W, extents: &Vec<AllocExtent>,
  symtab: &SymbolTable, indent: u32) -> io::Result<()> {
    write!(out, "{:in$}(", "", in = (indent * INDENT) as usize)?;
    for (i, extent) in extents.iter().enumerate() {
        if i != 0 {
            out.write_all(b", ")?;
        }

        let (lb, ub) = match *extent {
            AllocExtent::Range(ref lb, ref ub) => (lb, ub),
            AllocExtent::Along(_) =>
                panic!("dumpster fire: raw along expr in codegen"),
        };

        match *lb {
            None => {
                out.write_all(b"0 To ")?;
                match ub.data {
                    ExprKind::Lit(Literal::Int32(n)) => {
                        write!(out, "{}", n - 1)?;
                    },

                    _ => {
                        ub.emit(out, symtab, ExprPos::Expr, 0)?;
                        out.write_all(b" - 1")?;
                    },
                };
            },

            Some(ref lb) => {
                lb.emit(out, symtab, ExprPos::Expr, 0)?;
                out.write_all(b" To ")?;
                ub.emit(out, symtab, ExprPos::Expr, 0)?;
            },
        };
    }
    out.write_all(b")")
}

fn emit_realloc_extents<W: Write>(out: &mut W, array_expr: &Expr,
  extents: (usize, &AllocExtent), symtab: &SymbolTable, indent: u32)
  -> io::Result<()> {
    let (preserved, lb, ub) = match extents {
        (preserved, &AllocExtent::Range(ref lb, ref ub)) => (preserved, lb, ub),
        (_, &AllocExtent::Along(_)) =>
            panic!("dumpster fire: raw along expr in codegen"),
    };

    write!(out, "{:in$}(", "", in = (indent * INDENT) as usize)?;

    for dim in 1..(preserved + 1) {
        out.write_all(b"LBound(")?;
        array_expr.emit(out, symtab, ExprPos::Expr, 0)?;
        write!(out, ", {}) To UBound(", dim)?;
        array_expr.emit(out, symtab, ExprPos::Expr, 0)?;
        write!(out, ", {}), ", dim)?;
    }

    match *lb {
        None => {
            out.write_all(b"0 To ")?;
            match ub.data {
                ExprKind::Lit(Literal::Int32(n)) => {
                    write!(out, "{}", n - 1)?;
                },

                _ => {
                    ub.emit(out, symtab, ExprPos::Expr, 0)?;
                    out.write_all(b" - 1")?;
                },
            };
        },

        Some(ref lb) => {
            lb.emit(out, symtab, ExprPos::Expr, 0)?;
            out.write_all(b" To ")?;
            ub.emit(out, symtab, ExprPos::Expr, 0)?;
        },
    };

    out.write_all(b")")
}
