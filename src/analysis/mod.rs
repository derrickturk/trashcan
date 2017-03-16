//! trashcan's code analysis pipeline, including symbol table and type
//!   checking as well as various pre-codegen rewrite rules

// the symbol table type will be something like
//   Map<ident, Map<ident, (type, ...)>> where the outer map key is the scope
//   (possibly gensymmed for private scopes) and the inner is the symbol name

use parser::SrcLoc;
use ast;

#[derive(Clone, Debug)]
pub struct AnalysisError {
    kind: AnalysisErrorKind,
    regarding: Option<String>,
    loc: SrcLoc,
}

#[derive(Copy, Clone, Debug)]
pub enum AnalysisErrorKind {
    DuplicateSymbol,
    NotDefined,
    SymbolAccess,
    TypeError,
    InvalidStmt,
    InvalidExpr,
    FnCallError,
}

pub type AnalysisResult<T> = Result<T, AnalysisError>;

pub mod symtab;
pub use self::symtab::*;
pub mod typecheck;
pub use self::typecheck::*;
