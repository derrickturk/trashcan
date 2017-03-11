//! trashcan's deferred-type resolution pass

use ast::*;
use visit;
use visit::ASTVisitorMut;
use super::*;

struct Resolver<'a> {
    symtab: &'a mut SymbolTable,
}

impl<'a> ASTVisitorMut for Resolver<'a> {
    fn visit_ident(&mut self, i: &mut Ident, module: Option<&Ident>,
      function: Option<&Ident>, typename: Option<&Ident>) {
        println!("ident: {}", i);
    }
}

pub fn resolve_deferred(dumpster: &mut Dumpster, symtab: &mut SymbolTable)
  -> AnalysisResult<()> {
    let mut resolver = Resolver { symtab: symtab };
    resolver.visit_dumpster(dumpster);
    Ok(())
}
