//! trashcan's sub-parsers for identifiers, paths, and typenames

use std::str;

use super::{ParseErrorKind, CutParseResult};
#[macro_use]
use super::bits::*;

use ast::*;

pub const IDENT_CONT_CHARS: &'static [u8] =
    b"ABCDEFGHIJKLMNOPQRSTUVWXYZ\
      abcdefghijklmnopqrstuvwxyz\
      0123456789\
      _";

pub const KEYWORDS: [&'static [u8]; 38] = [
    b"let",
    b"as",
    b"print",
    b"return",
    b"for",
    b"while",
    b"pub",
    b"mod",
    b"fn",
    b"class",
    b"struct",
    b"enum",
    b"new",
    b"alloc",
    b"realloc",
    b"dealloc",
    b"first_index",
    b"last_index",
    b"array_length",
    b"along",
    b"nullptr",
    b"emptyvar",
    b"nullvar",
    b"bool",
    b"true",
    b"false",
    b"u8",
    b"i16",
    b"i32",
    b"i64",
    b"isize",
    b"f32",
    b"f64",
    b"str",
    b"currency",
    b"date",
    b"var",
    b"obj",
];

pub fn path(input: &[u8]) -> CutParseResult<Path> {
    let (i, module) = require!(opt(input, path_module));
    let (i, item) = require!(ident(i));
    ok!(i, Path(module, item))
}

#[inline]
fn path_module(input: &[u8]) -> CutParseResult<Ident> {
    let (i, _) = opt(input, multispace)?;
    let (i, module) = require!(ident(i));
    let (i, _) = require!(keyword(i, b"::"));
    ok!(i, module)
}

pub fn ident(input: &[u8]) -> CutParseResult<Ident> {
    let (i, loc) = require!(pos(input));
    let (i, _) = opt(input, multispace)?;
    let (i, first) = require!(ascii_letters(i));
    let (i, rest) = require!(opt!(bytes_in(i, IDENT_CONT_CHARS)));

    let mut id = String::from(unsafe { str::from_utf8_unchecked(first) });
    if let Some(rest) = rest {
        id.push_str(unsafe { str::from_utf8_unchecked(rest) })
    }

    if let Some(kw) = KEYWORDS.iter().filter(|k| **k == id.as_bytes()).next() {
        cut!(input, ParseErrorKind::KeywordAsIdent(kw))
    } else {
        ok!(i, Ident(id, None))
    }
}

pub fn typename(input: &[u8]) -> CutParseResult<Type> {
    let (i, _) = opt(input, multispace)?;
    let (i, base) = require!(alt!(i,
        keyword_immediate(i, b"bool") => |_| Type::Bool
      ; keyword_immediate(i, b"u8") => |_| Type::UInt8
      ; keyword_immediate(i, b"i16") => |_| Type::Int16
      ; keyword_immediate(i, b"i32") => |_| Type::Int32
      ; keyword_immediate(i, b"isize") => |_| Type::IntPtr
      ; keyword_immediate(i, b"f32") => |_| Type::Float32
      ; keyword_immediate(i, b"f64") => |_| Type::Float64
      ; keyword_immediate(i, b"str") => |_| Type::String
      ; keyword_immediate(i, b"currency") => |_| Type::Currency
      ; keyword_immediate(i, b"date") => |_| Type::Date
      ; keyword_immediate(i, b"var") => |_| Type::Variant
      ; keyword_immediate(i, b"obj") => |_| Type::Obj
      ; path(i) => |p| Type::Deferred(p)
    ));

    let (i, spec) = require!(opt(i, array_spec));
    match spec {
        None => ok!(i, base),
        Some(spec) => ok!(i, Type::Array(Box::new(base), spec)),
    }
}

fn array_spec(input: &[u8]) -> CutParseResult<ArrayBounds> {
    let (i, _) = opt(input, multispace)?;
    let (i, _) = require!(byte(i, b'['));
    // everything past here should cut: we know we're in an array bound
    let (i, bounds) = require_or_cut!(alt!(i,
        array_static_bounds(i)
      ; array_dynamic_bounds(i)
    ));
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b']'));
    ok!(i, bounds)
}

fn array_dynamic_bounds(input: &[u8]) -> CutParseResult<ArrayBounds> {
    let (i, commas) = require!(many(input,
      |i| chain!(i,
          |i| opt(i, multispace) =>
          |i| byte(i, b',')
      )));

    ok!(i, ArrayBounds::Dynamic(commas.len() + 1))
}

fn array_static_bounds(input: &[u8]) -> CutParseResult<ArrayBounds> {
    let (i, bounds) = require!(delimited_at_least_one(input,
      array_dim,
      |i| chain!(i,
          |i| opt(i, multispace) =>
          |i| byte(i, b',')
      )));
    ok!(i, ArrayBounds::Static(bounds))
}

fn array_dim(input: &[u8]) -> CutParseResult<(i32, i32)> {
    let (i, _) = opt(input, multispace)?;
    let (i, first) = require!(digits(i));

    let (i, end) = require!(opt!(chain!(i,
        |i| opt(i, multispace) =>
        |i| byte(i, b':') =>
        |i| opt(i, multispace) =>
        |i| cut_if_err!(digits(i)) // we should cut! if we dont see a digit here
    )));

    match make_range(first, end) {
        Ok(dim) => ok!(i, dim),
        Err(_) => err!(input, ParseErrorKind::InvalidArrayDim),
    }
}

fn make_range(first: &[u8], end: Option<&[u8]>)
  -> Result<(i32, i32), <i32 as str::FromStr>::Err> {
    let first = unsafe { str::from_utf8_unchecked(first).parse()? };
    match end {
        None => Ok((0, first - 1)),
        Some (end) => {
            let end = unsafe { str::from_utf8_unchecked(end).parse()? };
            Ok((first, end))
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_paths() {
        expect_parse!(path(b"  abc :: some_guy") =>
          Path(Some(Ident(_, None)), Ident(_, None)));
        expect_parse!(path(b"some_guy") =>
          Path(None, Ident(_, None)));
        expect_parse_err!(path(b"some:::wrong_thing") =>
          ParseErrorKind::ExpectedAsciiLetter);
    }

    #[test]
    fn parse_idents() {
        expect_parse!(ident(b"abc_123") => Ident(_, None));
        expect_parse_err!(ident(b"  __abc_123") =>
          ParseErrorKind::ExpectedAsciiLetter);
        expect_parse_cut!(ident(b"  for") =>
          ParseErrorKind::KeywordAsIdent(b"for"));
    }

    #[test]
    fn parse_typenames() {
        expect_parse!(typename(b"  i32") => Type::Int32);
        expect_parse!(typename(b"some::ty") => Type::Deferred(_));
        expect_parse!(typename(b"something") => Type::Deferred(_));
        expect_parse!(typename(b"i32[]") =>
          Type::Array(_, ArrayBounds::Dynamic(1)));
        expect_parse!(typename(b"f64[1:10, 17:34]") =>
          Type::Array(_, ArrayBounds::Static(_)));
        expect_parse!(typename(b"something::else[,,,]") =>
          Type::Array(_, ArrayBounds::Dynamic(_)));

        expect_parse_err!(typename(b"__cant_be_ident") =>
          ParseErrorKind::NoAltMatch);
        expect_parse_cut!(typename(b"bad::array[bobby]") => _);
        expect_parse_cut!(typename(b"some::for") =>
          ParseErrorKind::KeywordAsIdent(b"for"));
    }

    #[test]
    fn parse_array_bounds() {
        expect_parse!(array_static_bounds(b"10") => _);
        expect_parse!(array_static_bounds(b" 0:17") => _);
        expect_parse!(array_static_bounds(b" 0:17,9") => _);
        expect_parse!(array_static_bounds(b"10, 10, 10") => _);
        expect_parse_cut!(array_static_bounds(b"17, 99:potato") =>
          ParseErrorKind::ExpectedDigit);

        expect_parse!(array_dynamic_bounds(b"") => ArrayBounds::Dynamic(1));
        expect_parse!(array_dynamic_bounds(b" , , ") => ArrayBounds::Dynamic(3));

        expect_parse!(array_spec(b"[10]") => ArrayBounds::Static(_));
        expect_parse!(array_spec(b" [ 10, 17: 99 ]") => ArrayBounds::Static(_));
        expect_parse!(array_spec(b" []") => ArrayBounds::Dynamic(1));
        expect_parse!(array_spec(b" [ ,\t,\n,\n\n ]") =>
          ArrayBounds::Dynamic(4));
        expect_parse_err!(array_spec(b"bobby") =>
          ParseErrorKind::ExpectedByte(b'['));
        expect_parse_cut!(array_spec(b"[ big bad bobby ]") => _);
    }

    #[test]
    fn parse_array_dim() {
        expect_parse!(array_dim(b"123") => (0, 122));
        expect_parse!(array_dim(b"17 : 32") => (17, 32));
        expect_parse_err!(array_dim(b"  poatato:23") =>
          ParseErrorKind::ExpectedDigit);
        expect_parse_cut!(array_dim(b"  17:potato") =>
          ParseErrorKind::ExpectedDigit);
    }
}
