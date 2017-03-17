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

/*

struct EmitVisitor<'a, W: Write + 'a> {
    indent: u32,
    symtab: &'a SymbolTable,
    out: Option<W>,
}

impl<'a> EmitVisitor<'a> {
    fn new(symtab: &'a SymbolTable) -> Self {
        Self {
            indent: 0,
            symtab: symtab,
            out: None,
            error: io::Result<()>,
        }
    }
}

macro_rules! vtry {
    ($e:expr) => {
        match $e {
            Ok(res) => res,
            Err(e) => {
                self.error = Err(e);
                return;
            }
        }
    }
}

impl<'a> ASTVisitor for EmitVisitor<'a> {
    fn visit_module(&mut self, m: &Module) {
        let file = m.filename();
        let self.out = vtry!(File::create(&file));
        vtry!(m.emit(&mut file, &symtab, (), 0));
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
