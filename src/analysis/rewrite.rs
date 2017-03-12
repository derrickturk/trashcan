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

// TODO: some impl fns that make this easier to construct

/// use to subsitute idents within a given scope
/// it's easier if this thing owns copies of the identifiers
pub struct ScopedSubstitutionFolder {
    pub orig: Ident,
    pub replace: Ident,
    pub module: Ident,
    pub function: Option<Ident>,
    pub defns: bool,
    pub values: bool,
    pub fns: bool,
    pub types: bool,
}

impl ASTFolder for ScopedSubstitutionFolder {
    // TODO: should we hook fold_path instead?

    fn fold_ident(&mut self, ident: Ident, ctxt: NameCtxt, loc: &SrcLoc)
      -> Ident {
        let (module, function) = match ctxt {
            NameCtxt::Value(m, f, _) if self.values =>
                (m, f),
            NameCtxt::DefValue(m, f, _) if self.defns && self.values =>
                (m, f),
            NameCtxt::DefParam(m, f, _, _) if self.defns && self.values =>
                (m, Some(f)),

            NameCtxt::Function(m, _) if self.fns =>
                (m, None),
            NameCtxt::DefFunction(m) if self.defns && self.fns =>
                (m, None),

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
