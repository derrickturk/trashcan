//! trashcan's AST rewrite rules, used to implement various language features

use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};

use super::*;
use ast::*;
use visit::NameCtxt;
use fold::ASTFolder;

static mut GENSYM_ID: AtomicUsize = ATOMIC_USIZE_INIT;

pub fn gensym(orig: Option<Ident>) -> Ident {
    let num = unsafe { GENSYM_ID.fetch_add(1, Ordering::Relaxed) };
    Ident(format!("ø{}", num), orig.map(|i| i.0))
}

/// use to subsitute value-naming idents within a given scope
///   (not types, parameters, or functions)
pub struct ScopedSubstitutionFolder<'a> {
    pub orig: &'a Ident,
    pub replace: &'a Ident,
    pub module: &'a Ident,
    pub function: Option<&'a Ident>,
}

impl<'a> ASTFolder for ScopedSubstitutionFolder<'a> {
    // TODO: should we hook fold_path instead?

    fn fold_ident(&mut self, ident: Ident, ctxt: NameCtxt, loc: &SrcLoc)
      -> Ident {
        match ctxt {
            NameCtxt::Value(module, function, _) => {
                if module == self.module && function == self.function
                  && ident == *self.orig {
                    self.replace.clone()
                } else {
                    ident
                }
            },

            _ => ident,
        }
    }
}
