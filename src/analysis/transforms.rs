//! trashcan's AST transforms, used to implement language features

use super::*;
use ast::*;

/// replace for loop iteration variables with gensyms for pseudo-block scoping
pub fn for_loop_var_gensym(dumpster: Dumpster) -> Dumpster {
    Dumpster {
        modules: dumpster.modules.into_iter().map(|m| {
            m
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
