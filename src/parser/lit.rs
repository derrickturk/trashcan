//! trashcan's sub-parsers for literals

use std::str;

use super::{ParseErrorKind, CutParseResult};
#[macro_use]
use super::bits::*;

use ast::*;

pub fn literal(input: &[u8]) -> CutParseResult<Literal> {
    alt!(input,
        literal_null(input)
      ; literal_bool(input)
      ; literal_currency(input)
      ; literal_float(input)
      ; literal_int(input)
      ; literal_string(input)
  //  TODO: "wacky" literal types
  //  ; literal_date));
    )
}

fn literal_null(input: &[u8]) -> CutParseResult<Literal> {
    alt!(input,
        keyword(input, b"nullptr") => |_| Literal::NullPtr
      ; keyword(input, b"nullvar") => |_| Literal::NullVar
      ; keyword(input, b"emptyvar") => |_| Literal::EmptyVar
    )
}

fn literal_bool(input: &[u8]) -> CutParseResult<Literal> {
    alt!(input,
        keyword(input, b"true") => |_| Literal::Bool(true)
      ; keyword(input, b"false") => |_| Literal::Bool(false)
    )
}

fn literal_int(input: &[u8]) -> CutParseResult<Literal> {
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
        Ok(lit) => ok!(i, lit),
        // if we have numbers and a tag but fail the numeric parse,
        //   that's an unrecoverable error
        Err(_) => cut!(input, ParseErrorKind::InvalidLiteral),
    }
}

fn literal_float(input: &[u8]) -> CutParseResult<Literal> {
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
        Ok(lit) => ok!(i, lit),
        // if we have numbers and a tag but fail the numeric parse,
        //   that's an unrecoverable error
        Err(_) => cut!(input, ParseErrorKind::InvalidLiteral),
    }
}

fn literal_currency(input: &[u8]) -> CutParseResult<Literal> {
    let (i, _) = opt(input, multispace)?;

    let (i, whole) = require!(digits(i));

    let (i, frac) = require!(opt!(chain!(i,
        |i| byte(i, b'.') =>
        digits
    )));

    let (i, _) = require!(keyword_immediate(i, b"currency"));

    let whole = unsafe { str::from_utf8_unchecked(whole) }.parse::<i64>();
    let frac = match frac {
        None => Ok(0i16),
        Some(frac) => {
            unsafe { str::from_utf8_unchecked(frac) }.parse::<i16>()
        },
    };

    let (whole, frac) = match (whole, frac) {
        (Err(_), _) => return cut!(input, ParseErrorKind::InvalidLiteral),
        (_, Err(_)) => return cut!(input, ParseErrorKind::InvalidLiteral),
        (Ok(whole), Ok(frac)) => (whole, frac),
    };

    ok!(i, make_currency(whole, frac))
}

fn literal_string(input: &[u8]) -> CutParseResult<Literal> {
    let (i, _) = opt(input, multispace)?;
    let (i, _) = require!(byte(i, b'"'));
    let (i, escaped) = require!(escaped_string(i));
    let (i, _) = require!(byte(i, b'"'));

    match String::from_utf8(escaped) {
        Ok(s) => ok!(i, Literal::String(s)),
        Err(_) => cut!(input, ParseErrorKind::InvalidLiteral)
    }
}

#[inline]
fn make_currency(whole: i64, frac: i16) -> Literal {
    let frac_digits = (frac as f32).log10().ceil() as i16;
    let frac_scalar = f32::powf(10.0, (4 - frac_digits) as f32);
    Literal::Currency(whole * 10000 + (frac as f32 * frac_scalar) as i64)
}

fn escaped_string(input: &[u8]) -> CutParseResult<Vec<u8>> {
    let mut s = Vec::new();
    let mut bytes_consumed = 0;
    let mut bytes = input.iter();
    while let Some(c) = bytes.next() {
        if *c == b'"' {
            break;
        }

        if *c == b'\\' {
            match bytes.next() {
                Some(&b'n') => s.push(b'\n'),
                Some(&b't') => s.push(b'\t'),
                Some(&b'"') => s.push(b'"'),
                // TODO: more escapes here
                _ => return cut!(&input[bytes_consumed..],
                  ParseErrorKind::InvalidEscape)
            }
            bytes_consumed += 2;
            continue;
        }

        // TODO: it'd be nice to allow rust style multiline strings
        //   (or maybe C-style adjacent-literal concatenation)
        // first option needs peek here; second just needs a change to the
        // literal_string production

        bytes_consumed += 1;
        s.push(*c);
    }

    ok!(&input[bytes_consumed..], s)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_literals() {
        expect_parse!(literal(b"nullptr") => Literal::NullPtr);
        expect_parse!(literal(b"123.45") => Literal::Float64(123.45));
        expect_parse!(literal(b"\n123i16") => Literal::Int16(123i16));
        expect_parse!(literal(b"\t123.45currency") =>
          Literal::Currency(1234500i64));
        expect_parse!(literal(b"\"hello\tworld\"") => Literal::String(_));
        expect_parse_err!(literal(b"alskf") => ParseErrorKind::NoAltMatch);
        expect_parse_cut!(literal(b"  123456789u8") =>
          ParseErrorKind::InvalidLiteral);
    }

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
        expect_parse_err!(literal_int(b"alskf") =>
          ParseErrorKind::ExpectedDigit);
        expect_parse_cut!(literal_int(b"123456789u8") =>
          ParseErrorKind::InvalidLiteral);
    }

    #[test]
    fn parse_floats() {
        expect_parse!(literal_float(b"124.5") => Literal::Float64(124.5));
        expect_parse!(literal_float(b"1234.56f32") => Literal::Float32(1234.56f32));
        expect_parse_err!(literal_float(b"x.12") => ParseErrorKind::ExpectedDigit);
        /*
        expect_parse_cut!(literal_float(b"9999999999999999999999999999.0f32") =>
          ParseErrorKind::InvalidLiteral);
        */
    }

    #[test]
    fn parse_currency() {
        expect_parse!(literal_currency(b" 12345.67currency") =>
          Literal::Currency(123456700i64));
        expect_parse_cut!(
            literal_currency(b"999.9999999999999999999999999999currency") => _);
    }

    #[test]
    fn parse_string() {
        expect_parse!(literal_string(b"\"hello world\\nor whatever\"") =>
          Literal::String(_));
        expect_parse_err!(literal_string(b"\"unclosed") => _);
        expect_parse_cut!(literal_string(b"   \"invalid \\x escape\"") => _);
    }
}
