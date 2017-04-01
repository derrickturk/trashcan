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
      ; keyword_immediate(i, b"i16")
      ; keyword_immediate(i, b"i32")
      ; keyword_immediate(i, b"isize")
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

fn literal_float(input: &[u8]) -> ParseResult<Literal> {
    let (i, _) = opt(input, multispace)?;
    let (i, whole) = require!(digits(i));
    // mandatory decimal point
    let (i, _) = require!(byte(i, b'.'));
    let (i, frac) = require!(opt(i, digits));
    let (i, tag) = require!(opt!(alt!(i,
        keyword_immediate(i, b"f32")
      ; keyword_immediate(i, b"f64")
    )));

    let mut num = String::from(unsafe { str::from_utf8_unchecked(whole) });
    match frac {
        None => { },
        Some(frac) => {
            num.push_str(".");
            num.push_str(unsafe { str::from_utf8_unchecked(frac) });
        }
    };

    let parsed = match tag {
        None => num.parse::<f64>().map(Literal::Float64),
        Some(b"f32") => num.parse::<f32>().map(Literal::Float32),
        Some(b"f64") => num.parse::<f64>().map(Literal::Float64),
        _ => panic!("dumpster fire: bad tag in float literal"),
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

    #[test]
    fn parse_floats() {
        expect_parse!(literal_float(b"124.5") => Literal::Float64(124.5));
        expect_parse!(literal_float(b"1234.56f32") => Literal::Float32(1234.56f32));
        expect_parse_err!(literal_float(b"x.12") => ParseError::ExpectedDigit);
        /*
        expect_parse_cut!(literal_float(b"9999999999999999999999999999.0f32") =>
          ParseError::InvalidLiteral);
        */
    }
}
