//! code generator: emit VB6 from trashcan ASTs

use std::io;
use std::io::Write;

use ast::*;
use parser::SrcLoc;
use analysis::SymbolTable;

use visit::ASTVisitor;

const INDENT: u32 = 4;

/// trait for all emittable types
pub trait Emit<Ctxt> {
    fn emit<W: Write>(&self, out: &mut W, symtab: &SymbolTable,
      ctxt: Ctxt, indent: u32) -> io::Result<()>;
}

/* SOON

struct EmitVisitor<'a, W: Write + 'a> {
    indent: u32,
    symtab: &'a SymbolTable,
    out: &'a mut W,
}

impl<'a> ASTVisitor for EmitVisitor<'a> {
    fn visit_module(&mut self, m: &Module) {
    }
}

*/

mod module;
mod item;
mod stmt;
mod expr;
mod ty;
mod lit;
mod bits;
