//! trashcan's sub-parsers for literals

use std::str;

use super::{ParseError, ParseResult};
#[macro_use]
use super::bits::*;

use ast::*;

fn literal_null(input: &[u8]) -> ParseResult<Literal> {
    alt!(input,
        keyword(input, b"nullptr") => |_| Literal::NullPtr
      ; keyword(input, b"nullvar") => |_| Literal::NullVar
      ; keyword(input, b"emptyvar") => |_| Literal::EmptyVar
    )
}

fn literal_bool(input: &[u8]) -> ParseResult<Literal> {
    alt!(input,
        keyword(input, b"true") => |_| Literal::Bool(true)
      ; keyword(input, b"false") => |_| Literal::Bool(false)
    )
}

fn literal_int(input: &[u8]) -> ParseResult<Literal> {
    let (i, _) = opt(input, multispace)?;
    let (i, num) = require!(digits(i));
    let (i, tag) = require!(opt!(alt!(i,
        keyword_immediate(i, b"u8")
    )));

    let num = unsafe { str::from_utf8_unchecked(num) };
    let parsed = match tag {
        None => num.parse::<i32>().map(Literal::Int32),
        Some(b"u8") => num.parse::<u8>().map(Literal::UInt8),
        Some(b"i16") => num.parse::<i16>().map(Literal::Int16),
        Some(b"i32") => num.parse::<i32>().map(Literal::Int32),
        Some(b"isize") => num.parse::<i64>().map(Literal::IntPtr),
        _ => panic!("dumpster fire: bad tag in int literal"),
    };

    match parsed {
        Ok(lit) => Ok((i, Ok(lit))),
        // if we have numbers and a tag but fail the numeric parse,
        //   that's an unrecoverable error
        Err(_) => Err((input, ParseError::InvalidLiteral)),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_nulls() {
        expect_parse!(literal_null(b"nullptr") => Literal::NullPtr);
        expect_parse!(literal_null(b" nullvar") => Literal::NullVar);
        expect_parse!(literal_null(b"\nemptyvar") => Literal::EmptyVar);
    }

    #[test]
    fn parse_bools() {
        expect_parse!(literal_bool(b"   true") => Literal::Bool(true));
        expect_parse!(literal_bool(b"false") => Literal::Bool(false));
        expect_parse_err!(literal_bool(b"fake") => _);
    }

    #[test]
    fn parse_ints() {
        expect_parse!(literal_int(b"721") => Literal::Int32(721));
        expect_parse!(literal_int(b"123u8") => Literal::UInt8(123u8));
        expect_parse_err!(literal_int(b"alskf") => ParseError::ExpectedDigit);
        expect_parse_cut!(literal_int(b"123456789u8") =>
          ParseError::InvalidLiteral);
    }
}
