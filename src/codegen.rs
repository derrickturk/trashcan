//! code generator: emit VB6 from trashcan ASTs

use ast;

/// trait for all emittable types
pub trait Emit {
    fn emit(&self) -> String;
}

impl<'a> Emit for ast::Module<'a> {
    fn emit(&self) -> String {
        String::new()
    }
}
