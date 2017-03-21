//! code generation for trashcan statements

use std::io;
use std::io::Write;

use ast::*;
use parser::SrcLoc;
use super::*;

use analysis;
use analysis::ExprCtxt;

#[derive(Copy, Clone, Debug)]
pub enum ExprPos {
    /// used as expression
    Expr,
    /// used as statement (i.e. fn call or member invoke with no/discarded
    /// return value)
    Stmt,
}

impl<'a> Emit<(ExprPos, &'a ExprCtxt)> for Expr {
    fn emit<W: Write>(&self, out: &mut W, symtab: &SymbolTable,
      ctxt: (ExprPos, &'a ExprCtxt), indent: u32) -> io::Result<()> {
        match self.data {
            ExprKind::Lit(ref literal) => literal.emit(out, symtab, (), indent),

            ExprKind::Name(ref path) => {
                path.emit(out, symtab, (), indent)
            },

            ExprKind::Index(ref expr, ref indices) => {
                expr.emit(out, symtab, (ExprPos::Expr, ctxt.1), indent)?;
                out.write_all(b"(")?;
                for (i, index) in indices.iter().enumerate() {
                    if i != 0 {
                        out.write_all(b", ")?;
                    }
                    index.emit(out, symtab, (ExprPos::Expr, ctxt.1), 0)?;
                }
                out.write_all(b")")
            },

            ExprKind::Call(ref path, ref args) => {
                let pathexpr = Expr {
                    data: ExprKind::Name(path.clone()),
                    loc: SrcLoc::empty(),
                };
                pathexpr.emit(out, symtab, ctxt, indent)?;

                match ctxt.0 {
                    ExprPos::Expr => out.write_all(b"(")?,
                    ExprPos::Stmt => out.write_all(b" ")?,
                };

                for (i, arg) in args.iter().enumerate() {
                    if i != 0 { out.write_all(b", ")?; }
                    arg.emit(out, symtab, (ExprPos::Expr, ctxt.1), 0)?;
                }

                match ctxt.0 {
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

                match ctxt.0 {
                    ExprPos::Expr => out.write_all(b"(")?,
                    ExprPos::Stmt => out.write_all(b" ")?,
                };

                for (i, arg) in args.iter().enumerate() {
                    if i != 0 { out.write_all(b", ")?; }
                    arg.emit(out, symtab, (ExprPos::Expr, ctxt.1), 0)?;
                }

                match ctxt.0 {
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
                        let expr_ty = analysis::type_of(expr, symtab, ctxt.1)
                            .expect("dumpster fire: untypeable expression \
                                    in codegen");
                        match expr_ty {
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

            // TODO: be more clever with parens
            ExprKind::BinOpApp(ref lhs, ref rhs, ref op) => {
                // no infix "IsNot" in VB6; convert to Not (... Is ...)
                let op = match *op {
                    BinOp::NotIdentEq => {
                        write!(out, "{:in$}Not (", "",
                          in = (indent * INDENT) as usize)?;
                        BinOp::IdentEq
                    },

                    op => {
                        write!(out, "{:in$}(", "",
                          in = (indent * INDENT) as usize)?;
                        op
                    },

                    // TODO: emit integer division '\' for integral operands
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
                // TODO: we could typecheck and drop the ,1 in the 1-D array
                //   case (it'd make the output slightly prettier)

                let expr_ty = analysis::type_of(expr, symtab, ctxt.1)
                    .expect("dumpster fire: untypeable expression \
                            in codegen");

                let emit_dim = match expr_ty {
                    Type::Array(_, ref bounds) => bounds.dims() != 1,
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
