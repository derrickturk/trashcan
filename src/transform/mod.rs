//! trashcan's AST transforms, used to implement language features

mod gensym;

mod renames;
pub use self::renames::*;

mod rewrites;
pub use self::rewrites::*;
