//! code generation for trashcan statements

use std::io;
use std::io::Write;

use ast::*;
use super::*;
use super::bits::*;
use super::ty::*;

#[derive(Copy, Clone, Debug)]
pub enum ExprPos {
    /// used as expression
    Expr,
    /// used as statement (i.e. fn call or member invoke with no/discarded
    /// return value)
    Stmt,
}

impl<'a> Emit<ExprPos> for Expr {
    fn emit<W: Write>(&self, out: &mut W, symtab: &SymbolTable,
      ctxt: ExprPos, indent: u32) -> io::Result<()> {
        match self.data {
            ExprKind::Lit(ref literal) => literal.emit(out, symtab, (), indent),

            ExprKind::Name(ref path) => {
                path.emit(out, symtab, (), indent)
            },

            ExprKind::Call(ref path, ref args) => {
                let pathexpr = Expr {
                    data: ExprKind::Name(path.clone()),
                    loc: empty_loc!(),
                };
                pathexpr.emit(out, symtab, ctxt, indent)?;

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
                        out.write_all(b"-");
                        expr.emit(out, symtab, ctxt, 0)?;
                    },

                    UnOp::BitNot | UnOp::LogNot => {
                        out.write_all(b"Not ");
                        expr.emit(out, symtab, ctxt, 0)?;
                    },

                    UnOp::AddressOf => {
                        // TODO: this will change for fn or object types
                        out.write_all(b"VarPtr(")?;
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
                };

                lhs.emit(out, symtab, ctxt, 0)?;
                op.emit(out, symtab, (), 0)?;
                rhs.emit(out, symtab, ctxt, 0)?;
                out.write_all(b")")
            },

            ExprKind::VbExpr(ref bytes) => {
                write!(out, "{:in$}", "", in = (indent * INDENT) as usize)?;
                out.write_all(bytes)
            },

            ref e => {
                // TODO: unimplemented cases
                write!(out, "{:in$}{:?}", "", e,
                  in = (indent * INDENT) as usize)
            }
        }
    }
}
