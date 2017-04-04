//! trashcan's parser and affiliated types

use ast::*;

#[derive(Copy, Clone, Debug)]
pub enum ParseErrorKind {
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
    ExpectedItem,
    ExpectedModule,
    NoAltMatch,
    LookAhead,
    InvalidLiteral,
    InvalidEscape,
    KeywordAsIdent(&'static [u8]),
    InvalidArrayDim,
    InvalidTrailingContent,
}

// the internal parser result type, used to allow cuts back to the
//   toplevel parser on "unrecoverable" errors
// outer Result: Err is unrecoverable (stop parsing and return to toplevel)
// inner Result: Err is recoverable
type CutParseResult<'a, R> = Result<
    (&'a [u8], Result<R, ParseErrorKind>),
    (&'a [u8], ParseErrorKind)
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
mod module;
mod srcloc;
pub use self::srcloc::SrcLoc;

// the public error/result types for parsing

#[derive(Clone, Debug)]
pub struct ParseError {
    kind: ParseErrorKind,
    loc: SrcLoc,
}

pub type ParseResult<T> = Result<T, ParseError>;

pub fn parse_dumpster(file: &str, src: &[u8]) -> ParseResult<Dumpster> {
    let map = srcloc::map_source(file, src);
    map.translate_errors(module::dumpster(map.src()))
        .map(|d| map.rebase_srclocs(d))
}
