//! trashcan's deferred-type resolution pass

use ast::*;
use visit;
use visit::ASTVisitorMut;
use visit::NameCtxt;
use super::*;

struct Resolver<'a> {
    symtab: &'a mut SymbolTable,
}

impl<'a> ASTVisitorMut for Resolver<'a> {
    fn visit_path(&mut self, p: &mut Path, ctxt: NameCtxt) {
        println!("path: {} as {:?}", p, ctxt);
        self.walk_path(p, ctxt);
    }

    fn visit_ident(&mut self, i: &mut Ident, ctxt: NameCtxt) {
        println!("ident: {}{} as {:?}", i,
                 i.1.as_ref().map(|_| " <gensym>").unwrap_or(""), ctxt);
    }

    fn visit_type(&mut self, t: &mut Type, module: &Ident) {
        println!("type: {}", t);
    }

    fn visit_literal(&mut self, l: &mut Literal, module: &Ident,
      function: &Ident) {
        println!("literal: {:?}", l);
    }
}

pub fn resolve_deferred(dumpster: &mut Dumpster, symtab: &mut SymbolTable)
  -> AnalysisResult<()> {
    let mut resolver = Resolver { symtab: symtab };
    resolver.visit_dumpster(dumpster);
    Ok(())
}
