//! trashcan's AST transforms, used to implement language features

use super::*;
use ast::*;

/// replace for loop iteration variables with gensyms for pseudo-block scoping
pub fn for_loop_var_gensym(dumpster: Dumpster) -> Dumpster {
    Dumpster {
        modules: dumpster.modules.into_iter().map(|m| {
            Module {
                name: m.name,
                data: match m.data {
                    ModuleKind::Normal(items) =>
                        ModuleKind::Normal(items.into_iter().map(|i| {
                            match i {
                                NormalItem::Function(def) =>
                                    NormalItem::Function(FunDef {
                                        name: def.name,
                                        access: def.access,
                                        params: def.params,
                                        ret: def.ret,
                                        body: for_loop_var_gensym_stmts(def.body),
                                        loc: def.loc,
                                    }),
                            }
                        }).collect()),
                },
                loc: m.loc,
            }
        }).collect()
    }
}

/// replace logical-op expressions (and conditions) with short-circuiting
/// equivalents
pub fn short_circuit_logicals(dumpster: Dumpster) -> Dumpster {
    Dumpster {
        modules: dumpster.modules.into_iter().map(|m| {
            m
        }).collect()
    }
}

fn for_loop_var_gensym_stmts(stmts: Vec<Stmt>) -> Vec<Stmt> {
    stmts.into_iter().map(|s| {
        let sub_var = match s.data {
            StmtKind::ForLoop { ref var, .. } => Some(var.0.clone()),
            _ => None,
        };

        if let Some(var) = sub_var {
            let newvar = gensym();
            s.substitute(&var, &newvar)
        } else {
            s
        }
    }).collect()
}
