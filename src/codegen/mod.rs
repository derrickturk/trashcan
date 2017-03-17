//! code generator: emit VB6 from trashcan ASTs

use std::io;
use std::io::Write;

use analysis::SymbolTable;

const INDENT: u32 = 4;

/// trait for all emittable types
pub trait Emit<Ctxt> {
    fn emit<W: Write>(&self, out: &mut W, symtab: &SymbolTable,
      ctxt: Ctxt, indent: u32) -> io::Result<()>;
}

mod module;
mod item;
mod stmt;
mod expr;
mod ty;
mod lit;
mod bits;
