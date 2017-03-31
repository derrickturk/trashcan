//! trashcan's parser and affiliated types

use ast::*;

#[derive(Copy, Clone, Debug)]
pub enum ParseError {
    ExpectedWhiteSpace,
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
            Ok((rest, Ok(r))) => { panic!("parse result: {:?}", r) },
            Ok((rest, Err(e))) => { panic!("parse error: {:?}", e) },
            Err((rest, e)) => { panic!("unrecoverable parse error: {:?}", e) },
        }
    }
}

macro_rules! expect_parse_err {
    ($e:expr => $p:pat) => {
        match $e {
            Ok((rest, Err($p))) => { },
            Ok((rest, Ok(r))) => { panic!("parse result: {:?}", r) },
            Ok((rest, Err(e))) => { panic!("parse error: {:?}", e) },
            Err((rest, e)) => { panic!("unrecoverable parse error: {:?}", e) },
        }
    }
}

macro_rules! expect_parse_cut {
    ($e:expr => $p:pat) => {
        match $e {
            Err((rest, $p)) => { },
            Ok((rest, Ok(r))) => { panic!("parse result: {:?}", r) },
            Ok((rest, Err(e))) => { panic!("parse error: {:?}", e) },
            Err((rest, e)) => { panic!("unrecoverable parse error: {:?}", e) },
        }
    }
}

#[macro_use]
mod bits;

mod lit;
