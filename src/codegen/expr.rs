//! code generation for trashcan statements

use std::io;
use std::io::Write;

use ast::*;
use super::*;
use super::bits::*;
use super::ty::*;

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
