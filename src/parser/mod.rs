//! trashcan's parser and affiliated types

use ast::*;

#[derive(Copy, Clone, Debug)]
pub enum ParseError {
    ExpectedByte(u8),
    ExpectedNotByte(u8),
    ExpectedWhiteSpace,
    ExpectedDigit,
    ExpectedAsciiLetter,
    ExpectedInSet,
    ExpectedKeyword(&'static [u8]),
    ExpectedTypename,
    ExpectedIdent,
    ExpectedExpr,
    ExpectedDimSpecifier,
    ExpectedForSpecifier,
    ExpectedDefaultArgument,
    NoAltMatch,
    LookAhead,
    InvalidLiteral,
    InvalidEscape,
    KeywordAsIdent(&'static [u8]),
    InvalidArrayDim,
    InvalidTrailingContent,
}

// outer Result: Err is unrecoverable (stop parsing and return to toplevel)
// inner Result: Err is recoverable
pub type ParseResult<'a, R> = Result<
    (&'a [u8], Result<R, ParseError>),
    (&'a [u8], ParseError)
>;

macro_rules! expect_parse {
    ($e:expr => $p:pat) => {
        match $e {
            Ok((rest, Ok($p))) => { assert_eq!(rest.len(), 0) },
            Ok((_, Ok(r))) => { panic!("parse result: {:?}", r) },
            Ok((_, Err(e))) => { panic!("parse error: {:?}", e) },
            Err((_, e)) => { panic!("unrecoverable parse error: {:?}", e) },
        }
    }
}

macro_rules! expect_parse_err {
    ($e:expr => $p:pat) => {
        match $e {
            Ok((_, Err($p))) => { },
            Ok((_, Ok(r))) => { panic!("parse result: {:?}", r) },
            Ok((_, Err(e))) => { panic!("parse error: {:?}", e) },
            Err((_, e)) => { panic!("unrecoverable parse error: {:?}", e) },
        }
    }
}

macro_rules! expect_parse_cut {
    ($e:expr => $p:pat) => {
        match $e {
            Err((_, $p)) => { },
            Ok((_, Ok(r))) => { panic!("parse result: {:?}", r) },
            Ok((_, Err(e))) => { panic!("parse error: {:?}", e) },
            Err((_, e)) => { panic!("unrecoverable parse error: {:?}", e) },
        }
    }
}

#[macro_use]
mod bits;
mod lit;
mod op;
mod ident;
mod expr;
mod stmt;
mod item;
mod srcloc;
pub use self::srcloc::SrcLoc;

pub fn parse_dumpster(file: &str, src: &[u8]) -> Result<Dumpster, ParseError> {
    let map = srcloc::map_source(file, src);
    unimplemented!()
}
