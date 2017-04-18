//! trashcan's parser and affiliated types

use std::fmt;
use std::str;

use ast::*;

#[derive(Copy, Clone, Debug)]
pub enum ParseErrorKind {
    ExpectedByte(u8),
    ExpectedNotByte(u8),
    ExpectedWhiteSpace,
    ExpectedDigit,
    ExpectedAsciiLetter,
    ExpectedInSet(Option<&'static str>),
    ExpectedKeyword(&'static [u8]),
    ExpectedLiteral,
    ExpectedTypename,
    ExpectedIdent,
    ExpectedExpr,
    ExpectedDimSpecifier,
    ExpectedForSpecifier,
    ExpectedOptParams,
    ExpectedDefaultArgument,
    ExpectedModule,
    NoAltMatch,
    LookAhead,
    InvalidLiteral,
    InvalidEscape,
    KeywordAsIdent(&'static [u8]),
    InvalidArrayDim,
    InvalidTrailingContent,
}

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ParseErrorKind::ExpectedByte(byte) =>
                write!(f, "expected '{}'", byte as char),
            ParseErrorKind::ExpectedNotByte(byte) =>
                write!(f, "unexpected '{}'", byte as char),
            ParseErrorKind::ExpectedWhiteSpace =>
                write!(f, "expected whitespace"),
            ParseErrorKind::ExpectedDigit =>
                write!(f, "expected digit"),
            ParseErrorKind::ExpectedAsciiLetter =>
                write!(f, "expected ASCII letter"),
            ParseErrorKind::ExpectedInSet(ref desc) =>
                write!(f, "expected {}", desc.unwrap_or("set member")),
            ParseErrorKind::ExpectedKeyword(ref kw) =>
                write!(f, "expected \"{}\"",
                  unsafe { str::from_utf8_unchecked(kw) }),
            ParseErrorKind::ExpectedLiteral =>
                write!(f, "expected literal"),
            ParseErrorKind::ExpectedTypename =>
                write!(f, "expected type name"),
            ParseErrorKind::ExpectedIdent =>
                write!(f, "expected identifier"),
            ParseErrorKind::ExpectedExpr =>
                write!(f, "expected expression"),
            ParseErrorKind::ExpectedDimSpecifier =>
                write!(f, "expected dimension specifier"),
            ParseErrorKind::ExpectedForSpecifier =>
                write!(f, "expected for-loop specifier"),
            ParseErrorKind::ExpectedOptParams =>
                write!(f, "expected optional parameters"),
            ParseErrorKind::ExpectedDefaultArgument =>
                write!(f, "expected default argument"),
            ParseErrorKind::ExpectedModule
                => write!(f, "expected module definition"),
            ParseErrorKind::NoAltMatch
                => write!(f, "all alternatives failed"), // dumpster fire?
            ParseErrorKind::LookAhead
                => write!(f, "look-ahead error"), // dumpster fire?
            ParseErrorKind::InvalidLiteral
                => write!(f, "invalid literal value"),
            ParseErrorKind::InvalidEscape
                => write!(f, "invalid escape character"),
            ParseErrorKind::KeywordAsIdent(ref kw)
                => write!(f, "keyword \"{}\" used as identifier",
                     unsafe { str::from_utf8_unchecked(kw) }),
            ParseErrorKind::InvalidArrayDim
                => write!(f, "invalid array dimension"),
            ParseErrorKind::InvalidTrailingContent
                => write!(f, "invalid trailing content"),
        }
    }
}

// the internal parser result type, used to allow cuts back to the
//   toplevel parser on "unrecoverable" errors
// outer Result: Err is unrecoverable (stop parsing and return to toplevel)
// inner Result: Err is recoverable
type CutParseResult<'a, R> = Result<
    (&'a [u8], Result<R, ParseErrorKind>),
    (&'a [u8], ParseErrorKind)
>;

#[allow(unreachable_patterns)]
macro_rules! expect_parse {
    ($e:expr => $p:pat) => {
        #[allow(unreachable_patterns)]
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
        #[allow(unreachable_patterns)]
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
        #[allow(unreachable_patterns)]
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

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} @ {}", self.kind, self.loc)
    }
}

pub type ParseResult<T> = Result<T, ParseError>;

pub fn parse_dumpster(file: &str, src: &[u8]) -> ParseResult<Dumpster> {
    let map = srcloc::map_source(file, src);
    map.translate_errors(module::dumpster(map.src()))
        .map(|d| map.rebase_srclocs(d))
}
