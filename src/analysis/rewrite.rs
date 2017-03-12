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
///   (not types, parameters, or functions);
/// it's easier if this thing owns copies of the identifiers
pub struct ScopedSubstitutionFolder {
    pub orig: Ident,
    pub replace: Ident,
    pub module: Ident,
    pub function: Option<Ident>,
    pub defns: bool,
}

impl ASTFolder for ScopedSubstitutionFolder {
    // TODO: should we hook fold_path instead?

    fn fold_ident(&mut self, ident: Ident, ctxt: NameCtxt, loc: &SrcLoc)
      -> Ident {
        let (module, function) = match ctxt {
            NameCtxt::Value(m, f, _) => (m, f),
            NameCtxt::DefValue(m, f, _) if self.defns => (m, f),
            NameCtxt::DefParam(m, f, _, _) if self.defns => (m, Some(f)),
            _ => return ident,
        };

        if module == &self.module && function == self.function.as_ref()
          && ident == self.orig {
            self.replace.clone()
        } else {
            ident
        }
    }
}
