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
    fn emit<W: Write>(&self, out: &mut W, ctxt: ExprPos, indent: u32)
      -> io::Result<()> {
        match self.data {
            ExprKind::Lit(ref literal) => literal.emit(out, (), indent),

            ExprKind::Name(ref path) => {
                write!(out, "{:in$}", "", in = (indent * INDENT) as usize)?;
                for (i, nm) in path.0.iter().enumerate() {
                    if i != 0 { out.write_all(b".")?; }
                    nm.emit(out, (), 0)?;
                }
                Ok(())
            },

            ExprKind::Call(ref path, ref args) => {
                let pathexpr = Expr {
                    data: ExprKind::Name(path.clone()),
                    loc: empty_loc!(),
                };
                pathexpr.emit(out, ctxt, indent)?;

                match ctxt {
                    ExprPos::Expr => out.write_all(b"(")?,
                    ExprPos::Stmt => out.write_all(b" ")?,
                };

                for (i, arg) in args.iter().enumerate() {
                    if i != 0 { out.write_all(b", ")?; }
                    arg.emit(out, ExprPos::Expr, 0)?;
                }

                match ctxt {
                    ExprPos::Expr => out.write_all(b")")?,
                    ExprPos::Stmt => {},
                };

                Ok(())
            }

            ExprKind::VbExpr(ref bytes) => {
                write!(out, "{:in$}", "", in = (indent * INDENT) as usize)?;
                out.write_all(bytes)
            },

            ref e => {
                write!(out, "{:in$}{:?}", "", e,
                  in = (indent * INDENT) as usize)
            }
        }
    }
}
