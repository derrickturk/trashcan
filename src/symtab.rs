//! trashcan's symbol table; our target language has a single namespace,
//!   so public identifiers are unique at the Dumpster level regardless of
//!   type. private identifiers become Module::Ident (they must be unique
//!   at the module level). global scope is "".

// the symbol table type will be something like
//   Map<ident, Map<ident, (type, ...)>> where the outer map key is the scope
//   (possibly gensymmed for private scopes) and the inner is the symbol name

use std::collections::HashMap;

use ast;

pub struct SymbolTable<'a>(HashMap<String, HashMap<String, Symbol<'a>>>);

/// a symbol definition
pub enum Symbol<'a> {
    Constant(&'a ast::Type<'a>),
    Variable(&'a ast::Type<'a>),
    Function(&'a [&'a ast::Type<'a>], Option<&'a ast::Type<'a>>),
    Module,
    ClassModule
}
