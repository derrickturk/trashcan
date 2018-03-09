//! code generation for trashcan statements

use std::io;
use std::io::Write;

use ast::*;
use super::*;

#[derive(Copy, Clone, Debug)]
pub enum ExprPos {
    /// used as expression
    Expr,
    /// used as statement (i.e. fn call or member invoke with no/discarded
    /// return value)
    Stmt,
}

impl Emit<ExprPos> for Expr {
    fn emit<W: Write>(&self, out: &mut W, symtab: &SymbolTable,
      ctxt: ExprPos, indent: u32) -> io::Result<()> {
        match self.data {
            ExprKind::Lit(ref literal) => literal.emit(out, symtab, (), indent),

            ExprKind::Name(ref path) => {
                path.emit(out, symtab, (), indent)
            },

            ExprKind::Index(ref expr, ref indices) => {
                expr.emit(out, symtab, ExprPos::Expr, indent)?;
                out.write_all(b"(")?;
                for (i, index) in indices.iter().enumerate() {
                    if i != 0 {
                        out.write_all(b", ")?;
                    }
                    index.emit(out, symtab, ExprPos::Expr, 0)?;
                }
                out.write_all(b")")
            },

            ExprKind::Call(ref path, ref args, ref optargs) => {
                path.emit(out, symtab, (), indent)?;

                match ctxt {
                    ExprPos::Expr => out.write_all(b"(")?,
                    ExprPos::Stmt => out.write_all(b" ")?,
                };

                for (i, arg) in args.iter().enumerate() {
                    if i != 0 { out.write_all(b", ")?; }
                    arg.emit(out, symtab, ExprPos::Expr, 0)?;
                }

                if !args.is_empty() && !optargs.is_empty() {
                    out.write_all(b", ")?; 
                }

                for (i, &(ref name, ref arg)) in optargs.iter().enumerate() {
                    if i != 0 { out.write_all(b", ")?; }
                    name.emit(out, symtab, (), 0)?;
                    out.write_all(b" := ")?;
                    arg.emit(out, symtab, ExprPos::Expr, 0)?;
                }

                match ctxt {
                    ExprPos::Expr => out.write_all(b")")?,
                    ExprPos::Stmt => {},
                };

                Ok(())
            },

            ExprKind::Member(ref expr, ref member) => {
                expr.emit(out, symtab, ctxt, indent)?;
                out.write_all(b".")?;
                member.emit(out, symtab, (), 0)
            },

            ExprKind::MemberInvoke(ref expr, ref member, ref args) => {
                expr.emit(out, symtab, ctxt, indent)?;
                out.write_all(b".")?;
                member.emit(out, symtab, (), 0)?;

                match ctxt {
                    ExprPos::Expr => out.write_all(b"(")?,
                    ExprPos::Stmt => out.write_all(b" ")?,
                };

                for (i, arg) in args.iter().enumerate() {
                    if i != 0 { out.write_all(b", ")?; }
                    arg.emit(out, symtab, ExprPos::Expr, 0)?;
                }

                match ctxt {
                    ExprPos::Expr => out.write_all(b")")?,
                    ExprPos::Stmt => {},
                };

                Ok(())
            },

            ExprKind::UnOpApp(ref expr, ref op) => {
                write!(out, "{:in$}", "", in = (indent * INDENT) as usize)?;
                match *op {
                    UnOp::Negate => {
                        out.write_all(b"-")?;
                        expr.emit(out, symtab, ctxt, 0)?;
                    },

                    UnOp::BitNot | UnOp::LogNot => {
                        out.write_all(b"Not ")?;
                        expr.emit(out, symtab, ctxt, 0)?;
                    },

                    UnOp::AddressOf => {
                        let expr_ty = expr.ty.as_ref()
                            .expect("dumpster fire: untyped expression \
                                    in codegen");
                        match *expr_ty {
                            Type::Obj | Type::Object(_) =>
                                out.write_all(b"ObjPtr(")?,
                            _ => out.write_all(b"VarPtr(")?,
                        };
                        expr.emit(out, symtab, ctxt, 0)?;
                        out.write_all(b")")?;
                    },
                };
                Ok(())
            },

            ExprKind::BinOpApp(ref lhs, ref rhs, ref op) => {
                // no infix "IsNot" in VB6; convert to Not (... Is ...)
                let op = match *op {
                    BinOp::NotIdentEq => {
                        write!(out, "{:in$}Not (", "",
                          in = (indent * INDENT) as usize)?;
                        BinOp::IdentEq
                    },

                    // wacky special case: if we have a division, and both
                    //   operands are definitely of integral type, we need
                    //   to emit a '\' integer-divide op
                    // TODO: should we handle this with an AST node and a
                    //   rewriter?

                    BinOp::Div => {
                        write!(out, "{:in$}(", "",
                          in = (indent * INDENT) as usize)?;

                        let lhs_ty = lhs.ty.as_ref()
                            .expect("dumpster fire: untyped expression \
                                    in codegen");
                        let rhs_ty = rhs.ty.as_ref()
                            .expect("dumpster fire: untyped expression \
                                    in codegen");

                        if lhs_ty.is_integral() && rhs_ty.is_integral() {
                            lhs.emit(out, symtab, ctxt, 0)?;
                            out.write_all(b" \\ ")?;
                            rhs.emit(out, symtab, ctxt, 0)?;
                            return out.write_all(b")")
                        } else {
                            BinOp::Div // use normal-case code
                        }
                    },

                    op => {
                        write!(out, "{:in$}(", "",
                          in = (indent * INDENT) as usize)?;
                        op
                    },
                };

                lhs.emit(out, symtab, ctxt, 0)?;
                op.emit(out, symtab, (), 0)?;
                rhs.emit(out, symtab, ctxt, 0)?;
                out.write_all(b")")
            },

            ExprKind::CondExpr { .. } => {
                panic!("dumpster fire: raw CondExpr in codegen");
            },

            ExprKind::ExtentExpr(ref expr, kind, dim) => {
                let expr_ty = expr.ty.as_ref()
                    .expect("dumpster fire: untyped expression \
                            in codegen");

                let emit_dim = match *expr_ty {
                    Type::Array(_, ref bounds) => bounds.dims() != 1,
                    Type::VarArgsArray => false,
                    // TODO: maybe allow variants here (checked at runtime)?
                    _ => panic!("dumpster fire: non-array expression \
                      in extent expr"),
                };

                write!(out, "{:in$}", "", in = (indent * INDENT) as usize)?;

                match kind {
                    ExtentKind::First => {
                        out.write_all(b"LBound(")?;
                        expr.emit(out, symtab, ctxt, 0)?;
                        if emit_dim {
                            write!(out, ", {})", dim + 1)
                        } else {
                            out.write_all(b")")
                        }
                    },

                    ExtentKind::Last => {
                        out.write_all(b"UBound(")?;
                        expr.emit(out, symtab, ctxt, 0)?;
                        if emit_dim {
                            write!(out, ", {})", dim + 1)
                        } else {
                            out.write_all(b")")
                        }
                    },

                    // TODO: would it be cleaner to handle this in a rewriter?
                    ExtentKind::Length => {
                        out.write_all(b"(UBound(")?;
                        expr.emit(out, symtab, ctxt, 0)?;
                        if emit_dim {
                            write!(out, ", {}) - LBound(", dim + 1)?;
                        } else {
                            out.write_all(b") - LBound(")?;
                        }
                        expr.emit(out, symtab, ctxt, 0)?;
                        if emit_dim {
                            write!(out, ", {}) + 1)", dim + 1)
                        } else {
                            out.write_all(b") + 1)")
                        }
                    },
                }
            },

            ExprKind::Cast(ref expr, ref ty) => {
                let cast_op = match *ty {
                    Type::UInt8 => "CByte",
                    Type::Int16 => "CInt",
                    Type::Int32 => "CLng",
                    Type::IntPtr => "CLngPtr",
                    Type::Float32 => "CSng",
                    Type::Float64 => "CDbl",
                    Type::String => "CStr",
                    Type::Currency => "CCur",
                    Type::Date => "CDate",
                    Type::Variant => "CVar",
                    ref ty => panic!("dumpster fire: bad (or untransformed) \
                      cast to {} in codegen", ty)
                };

                write!(out, "{:in$}{}(", "", cast_op,
                  in = (indent * INDENT) as usize)?;
                expr.emit(out, symtab, ctxt, 0)?;
                out.write_all(b")")
            },

            ExprKind::VbExpr(ref bytes) => {
                write!(out, "{:in$}", "", in = (indent * INDENT) as usize)?;
                out.write_all(bytes)
            },
        }
    }
}
