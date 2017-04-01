//! trashcan's parser and affiliated types

use ast::*;

#[derive(Copy, Clone, Debug)]
pub enum ParseError {
    ExpectedChar(u8),
    ExpectedWhiteSpace,
    ExpectedDigit,
    ExpectedKeyword(&'static [u8]),
    NoAltMatch,
    InvalidLiteral,
    InvalidEscape,
    KeywordAsIdent,
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
