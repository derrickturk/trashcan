//! trashcan's AST rewrite rules, used to implement various language features

use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};

use super::*;
use ast::*;

static mut GENSYM_ID: AtomicUsize = ATOMIC_USIZE_INIT;

pub fn gensym() -> Ident {
    let num = unsafe { GENSYM_ID.fetch_add(1, Ordering::Relaxed) };
    Ident(format!("ø{}", num))
}

/// a trait for AST items into which gensyms can be substituted
///   rules:
///     * we can only substitute for 'bare' idents (not::paths)
///     * we never substitute for funparams
pub trait Substitute {
    fn substitute(self, orig: &Ident, replace: &Ident) -> Self;
}

impl Substitute for Dumpster {
    fn substitute(self, orig: &Ident, replace: &Ident) -> Self {
        Dumpster {
            modules: self.modules.into_iter().
                map(|m| m.substitute(orig, replace)).collect()
        }
    }
}

impl Substitute for Module {
    fn substitute(self, orig: &Ident, replace: &Ident) -> Self {
        Module {
            name: self.name.substitute(orig, replace),
            data: match self.data {
                ModuleKind::Normal(items) => ModuleKind::Normal(
                    items.into_iter().map(|i| i.substitute(orig, replace))
                      .collect()
                ),
            },
            loc: self.loc
        }
    }
}

impl Substitute for NormalItem {
    fn substitute(self, orig: &Ident, replace: &Ident) -> Self {
        match self {
            NormalItem::Function(def) => NormalItem::Function(
                def.substitute(orig, replace))
        }
    }
}

impl Substitute for FunDef {
    fn substitute(self, orig: &Ident, replace: &Ident) -> Self {
        FunDef {
            name: self.name.substitute(orig, replace),
            access: self.access,
            params: self.params,
            ret: self.ret,
            body: self.body.into_iter().map(|s| s.substitute(orig, replace))
                .collect(),
            loc: self.loc,
        }
    }
}

impl Substitute for Stmt {
    fn substitute(self, orig: &Ident, replace: &Ident) -> Self {
        let data = match self.data {
            StmtKind::ExprStmt(e) =>
                StmtKind::ExprStmt(e.substitute(orig, replace)),

            StmtKind::VarDecl(vars) =>
                StmtKind::VarDecl(vars.into_iter().map(|(var, ty, init)| {
                    (
                        var.substitute(orig, replace),
                        ty,
                        init.map(|e| e.substitute(orig, replace))
                    )
                }).collect()),

            StmtKind::Assign(e1, op, e2) =>
                StmtKind::Assign(e1.substitute(orig, replace),
                  op, e2.substitute(orig, replace)),

            StmtKind::Return(e) =>
                StmtKind::Return(e.map(|e| e.substitute(orig, replace))),

            StmtKind::Print(e) =>
                StmtKind::Print(e.substitute(orig, replace)),

            StmtKind::IfStmt { cond, body, elsifs, els } =>
                StmtKind::IfStmt {
                    cond: cond.substitute(orig, replace),
                    body: body.into_iter().map(|s| s.substitute(orig, replace))
                        .collect(),
                    elsifs: elsifs.into_iter().map(|(cond, body)| {
                        (
                            cond.substitute(orig, replace),
                            body.into_iter()
                              .map(|s| s.substitute(orig, replace)).collect()
                        )
                    }).collect(),
                    els: els.map(|body| body.into_iter().map(
                                 |s| s.substitute(orig, replace)).collect()),
                },

            StmtKind::WhileLoop { cond, body } =>
                StmtKind::WhileLoop {
                    cond: cond.substitute(orig, replace),
                    body: body.into_iter().map(|s| s.substitute(orig, replace))
                        .collect(),
                },

            StmtKind::ForLoop { var: (var, ty), spec, body } =>
                StmtKind::ForLoop {
                    var: (var.substitute(orig, replace), ty),
                    spec: match spec {
                        ForSpec::Range(begin, end, step) =>
                            ForSpec::Range(
                                begin.substitute(orig, replace),
                                end.substitute(orig, replace),
                                step.map(|e| e.substitute(orig, replace)),
                            ),
                        ForSpec::Each(e) =>
                            ForSpec::Each(e.substitute(orig, replace)),
                    },
                    body: body.into_iter().map(|s| s.substitute(orig, replace))
                        .collect(),
                },
        };

        Stmt {
            data: data,
            loc: self.loc,
        }
    }
}

impl Substitute for Expr {
    fn substitute(self, orig: &Ident, replace: &Ident) -> Self {
        self
    }
}

impl Substitute for Ident {
    fn substitute(self, orig: &Ident, replace: &Ident) -> Self {
        if self.0 == orig.0 {
            replace.clone()
        } else {
            self
        }
    }
}