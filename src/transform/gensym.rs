//! trashcan's gensym and substitution tools

use std::sync::atomic::{AtomicUsize, Ordering};

use ast::*;
use parser::SrcLoc;

use visit::NameCtxt;

use fold;
use fold::ASTFolder;

static mut GENSYM_ID: AtomicUsize = AtomicUsize::new(0);

pub fn gensym(orig: Option<Ident>) -> Ident {
    let num = unsafe { GENSYM_ID.fetch_add(1, Ordering::Relaxed) };
    Ident(format!("ø{}", num), orig.map(|i| i.1.unwrap_or(i.0)))
}

// TODO: some impl fns that make this easier to construct

/// use to subsitute idents within a given scope
/// it's easier if this thing owns copies of the identifiers
pub struct ScopedSubstitutionFolder {
    pub orig: Ident,
    pub replace: Ident,
    pub module: Option<Ident>,
    pub function: Option<Ident>,
    pub defns: bool,
    pub values: bool,
    pub fns: bool,
    pub types: bool,
    pub members: bool,
    pub modules: bool,
}

impl ASTFolder for ScopedSubstitutionFolder {
    // TODO: should we hook fold_path instead?

    fn fold_ident(&mut self, ident: Ident, ctxt: NameCtxt, _loc: &SrcLoc)
      -> Ident {
        let (module, function) = match ctxt {
            NameCtxt::Value(m, f, _) if self.values =>
                (Some(m), f),
            NameCtxt::DefValue(m, f, _, _) if self.defns && self.values =>
                (Some(m), f),
            NameCtxt::DefParam(m, f, _, _) if self.defns && self.values =>
                (Some(m), Some(f)),
            NameCtxt::DefConstant(m, _, _) if self.defns && self.values =>
                (Some(m), None),

            NameCtxt::Function(m, _) if self.fns =>
                (Some(m), None),
            NameCtxt::DefFunction(m) if self.defns && self.fns =>
                (Some(m), None),

            NameCtxt::Type(m, _) if self.types =>
                (Some(m), None),
            NameCtxt::DefType(m) if self.defns && self.types =>
                (Some(m), None),

            // members can be referenced (from + arbitrary).exprs;
            //   it also doesn't matter if other types have members with the
            //   same names (we don't do any sort of inference), so to avoid
            //   needing a full symbol table and typecheck to do member renames
            //   we just replace all members of every type everywhere with a
            //   matching name
            NameCtxt::Member(_, _, _) if self.members =>
                (None, None),
            NameCtxt::DefMember(_, _, _) if self.defns && self.members =>
                (None, None),

            NameCtxt::Module if self.modules =>
                (None, None),
            NameCtxt::DefModule if self.defns && self.modules =>
                (None, None),

            _ => return ident,
        };

        if module == self.module.as_ref() && function == self.function.as_ref()
          && ident == self.orig {
            self.replace.clone()
        } else {
            ident
        }
    }
}

/// use to substitute single-ident name-exprs within a given scope
/// it's easier if this thing owns copies of everything
pub struct ScopedExprSubstitutionFolder {
    pub orig: Ident,
    pub replace: Expr,
    pub module: Ident,
    pub function: Option<Ident>,
}

impl ASTFolder for ScopedExprSubstitutionFolder {
    fn fold_expr(&mut self, expr: Expr, module: &Ident,
      function: Option<&Ident>) -> Expr {
        let expr = fold::noop_fold_expr(self, expr, module, function);

        if self.module != *module || self.function.as_ref() != function {
            return expr;
        }

        let Expr { data, ty, loc } = expr;

        let data = match data {
            ExprKind::Name(Path(None, ref ident)) if *ident == self.orig =>
                self.replace.clone().data,

            e => e,
        };

        Expr {
            data,
            ty,
            loc,
        }
    }
}
