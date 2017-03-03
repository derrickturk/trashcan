//! code generation for trashcan statements

use std::io;
use std::io::Write;

use ast::*;
use super::*;
use super::bits::*;
use super::ty::*;

impl<'a> Emit<&'a FunDef> for Stmt {
    fn emit<W: Write>(&self, out: &mut W, ctxt: &'a FunDef, indent: u32)
      -> io::Result<()> {
        match self.data {
            StmtKind::ExprStmt(ref e) => {
                write!(out, "{:in$}{:?}\n", "", e,
                  in = (indent * INDENT) as usize)
            },

            StmtKind::VarDecl(ref decls) => {
                for decl in decls {
                    emit_decl(out, decl, indent)?;
                }
                Ok(())
            },

            StmtKind::Assign(ref place, ref op, ref expr) => {
                write!(out, "{:in$}{:?} {:?} {:?}\n", "", place, op, expr,
                  in = (indent * INDENT) as usize)
            },

            StmtKind::Return(ref expr) => {
                match expr {
                    &Some(ref e) => {
                        // TODO: Set required here if object type
                        ctxt.name.emit(out, (), indent)?;
                        out.write_all(b" = ")?;
                        write!(out, "{:?}\n", e)?;
                    },
                    &None => {}
                }
                // TODO: don't need this if last stmt in fundef
                write!(out, "{:in$}Exit Function\n", "",
                  in = (indent * INDENT) as usize)
            },

            StmtKind::Print(ref expr) => {
                write!(out, "{:in$}Debug.Print {:?}\n", "", expr,
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
