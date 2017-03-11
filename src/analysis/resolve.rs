//! trashcan's deferred-type resolution pass

use ast::*;
use parser::SrcLoc;
use visit;
use visit::ASTVisitorMut;
use visit::NameCtxt;
use super::*;

struct Resolver<'a> {
    symtab: &'a mut SymbolTable,
    errors: Vec<AnalysisError>,
}

impl<'a> ASTVisitorMut for Resolver<'a> {
    /*
    // TODO: this is fucking ugly
    fn visit_type(&mut self, t: &mut Type, module: &Ident, loc: &SrcLoc) {
        println!("checking {}", t);
        let new_t = match *t {
            Type::Deferred(ref path) => {
                let module = match *path {
                    Path(Some(ref path_module), _) => &path_module.0,
                    Path(None, _) => &module.0,
                };

                if let Some(symtab) = self.symtab.get(module) {
                    if let Some(sym) = symtab.get(&(path.1).0) {
                        match *sym {
                            Symbol::Struct { .. } => Type::Struct(path.clone()),
                            _ => {
                                self.errors.push(AnalysisError {
                                    kind: AnalysisErrorKind::TypeError,
                                    regarding: Some(format!(
                                            "{} does not name a type", path)),
                                    loc: loc.clone(),
                                });
                                Type::Void
                            }
                        }
                    } else {
                        self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::NotDefined,
                            regarding: Some(format!("{}", path)),
                            loc: loc.clone(),
                        });
                        Type::Void
                    }
                } else {
                    self.errors.push(AnalysisError {
                        kind: AnalysisErrorKind::NotDefined,
                        regarding: Some(format!("{}", path)),
                        loc: loc.clone(),
                    });
                    Type::Void
                }
            },

            ref t => t.clone(),
        };

        *t = new_t;
    }
    */

    fn visit_path(&mut self, p: &mut Path, ctxt: NameCtxt, loc: &SrcLoc) {
        println!("path: {} @ {:?}", p, ctxt);
        self.walk_path(p, ctxt, loc);
    }

    fn visit_ident(&mut self, i: &mut Ident, ctxt: NameCtxt, loc: &SrcLoc) {
        println!("ident: {} @ {:?}", i, ctxt);
    }
}

pub fn resolve_deferred(dumpster: &mut Dumpster, symtab: &mut SymbolTable)
  -> AnalysisResult<()> {
    let mut resolver = Resolver { symtab: symtab, errors: vec![] };
    resolver.visit_dumpster(dumpster);
    for e in resolver.errors {
        return Err(e);
    }
    Ok(())
}
