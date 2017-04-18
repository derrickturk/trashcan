//! trashcan's code analysis pipeline, including symbol table and type
//!   checking as well as various pre-codegen rewrite rules

use std::fmt;

use parser::SrcLoc;

#[derive(Clone, Debug)]
pub struct AnalysisError {
    kind: AnalysisErrorKind,
    regarding: Option<String>,
    loc: SrcLoc,
}

impl fmt::Display for AnalysisError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            AnalysisErrorKind::DuplicateSymbol =>
                write!(f, "duplicate symbol")?,
            AnalysisErrorKind::NotDefined =>
                write!(f, "undefined item")?,
            AnalysisErrorKind::SymbolAccess =>
                write!(f, "access error")?,
            AnalysisErrorKind::TypeError =>
                write!(f, "type error")?,
            AnalysisErrorKind::InvalidStmt =>
                write!(f, "invalid statement")?,
            AnalysisErrorKind::InvalidExpr =>
                write!(f, "invalid expression")?,
            AnalysisErrorKind::FnCallError =>
                write!(f, "invalid fn call")?,
            AnalysisErrorKind::PrivateInPublic =>
                write!(f, "private item in public interface")?,
            AnalysisErrorKind::RecursiveType =>
                write!(f, "invalid recursive type")?,
        };

        if let Some(ref msg) = self.regarding {
            write!(f, ": {}", msg)?;
        }

        write!(f, " @ {}", self.loc)
    }
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
    PrivateInPublic,
    RecursiveType,
}

pub type AnalysisResult<T> = Result<T, AnalysisError>;
pub type AnalysisResultMany<T> = Result<T, Vec<AnalysisError>>;

pub mod symtab;
pub use self::symtab::*;
pub mod typecheck;
pub use self::typecheck::*;
