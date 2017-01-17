//! trashcan's symbol table; our target language has a single namespace,
//!   so public identifiers are unique at the Dumpster level regardless of
//!   type. we gensym the names of private scopes...?

// the symbol table type will be something like
//   Map<ident, Map<ident, (type, ...)>> where the outer map key is the scope
//   (possibly gensymmed for private scopes) and the inner is the symbol name
