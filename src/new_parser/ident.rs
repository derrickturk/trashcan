//! trashcan's sub-parsers for identifiers, paths, and typenames

use std::str;

use super::{ParseError, ParseResult};
#[macro_use]
use super::bits::*;

use ast::*;

pub const IDENT_CONT_CHARS: &'static [u8] =
    b"ABCDEFGHIJKLMNOPQRSTUVWXYZ\
      abcdefghijklmnopqrstuvwxyz\
      0123456789\
      _";

pub const KEYWORDS: [&'static str; 38] = [
    "let",
    "as",
    "print",
    "return",
    "for",
    "while",
    "pub",
    "mod",
    "fn",
    "class",
    "struct",
    "enum",
    "new",
    "alloc",
    "realloc",
    "dealloc",
    "first_index",
    "last_index",
    "array_length",
    "along",
    "nullptr",
    "emptyvar",
    "nullvar",
    "bool",
    "true",
    "false",
    "u8",
    "i16",
    "i32",
    "i64",
    "isize",
    "f32",
    "f64",
    "str",
    "currency",
    "date",
    "var",
    "obj",
];

pub fn path(input: &[u8]) -> ParseResult<Path> {
    let (i, module) = require!(opt(input, path_module));
    let (i, item) = require!(ident(i));
    ok!(i, Path(module, item))
}

#[inline]
fn path_module(input: &[u8]) -> ParseResult<Ident> {
    let (i, _) = opt(input, multispace)?;
    let (i, module) = require!(ident(i));
    let (i, _) = require!(keyword(i, b"::"));
    ok!(i, module)
}

pub fn ident(input: &[u8]) -> ParseResult<Ident> {
    let (i, _) = opt(input, multispace)?;
    let (i, first) = require!(ascii_letters(i));
    let (i, rest) = require!(opt!(bytes_in(i, IDENT_CONT_CHARS)));

    let mut id = String::from(unsafe { str::from_utf8_unchecked(first) });
    if let Some(rest) = rest {
        id.push_str(unsafe { str::from_utf8_unchecked(rest) })
    }

    if KEYWORDS.contains(&id.as_str()) {
        cut!(input, ParseError::KeywordAsIdent)
    } else {
        ok!(i, Ident(id, None))
    }
}

/*

named!(pub typename<Type>, complete!(do_parse!(
        opt!(call!(nom::multispace)) >>
  base: alt_complete!(
            tag!("bool") => { |_| Type::Bool }
          | tag!("u8") => { |_| Type::UInt8 }
          | tag!("i16") => { |_| Type::Int16 }
          | tag!("i32") => { |_| Type::Int32 }
          | tag!("isize") => { |_| Type::IntPtr }
          | tag!("f32") => { |_| Type::Float32 }
          | tag!("f64") => { |_| Type::Float64 }
          | tag!("str") => { |_| Type::String }
          | tag!("currency") => { |_| Type::Currency }
          | tag!("date") => { |_| Type::Date }
          | tag!("var") => { |_| Type::Variant }
          | tag!("obj") => { |_| Type::Obj }
          | path => { |p| Type::Deferred(p) }
        ) >>
  spec: opt!(array_spec) >>
        (match spec {
            Some(spec) => Type::Array(Box::new(base), spec),
            None => base
        })
)));

named!(array_spec<ArrayBounds>, complete!(do_parse!(
        opt!(call!(nom::multispace)) >>
        char!('[') >>
  dims: alt_complete!(
            array_static_bounds
          | array_dynamic_bounds
        ) >>
        opt!(call!(nom::multispace)) >>
        char!(']') >>
        (dims)
)));

named!(array_dynamic_bounds<ArrayBounds>, complete!(map!(
        many0!(ws!(char!(','))),
        |vec: Vec<_>| ArrayBounds::Dynamic(vec.len() + 1)
)));

named!(array_static_bounds<ArrayBounds>, complete!(map!(
        separated_nonempty_list!(ws!(char!(',')), array_dim),
        ArrayBounds::Static
)));

*/

fn array_dim(input: &[u8]) -> ParseResult<(i32, i32)> {
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
        Err(_) => err!(input, ParseError::InvalidArrayDim),
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
          ParseError::ExpectedAsciiLetter);
    }

    #[test]
    fn parse_idents() {
        expect_parse!(ident(b"abc_123") => Ident(_, None));
        expect_parse_err!(ident(b"  __abc_123") =>
          ParseError::ExpectedAsciiLetter);
        expect_parse_cut!(ident(b"  for") => ParseError::KeywordAsIdent);
    }

    #[test]
    fn parse_array_dim() {
        expect_parse!(array_dim(b"123") => (0, 122));
        expect_parse!(array_dim(b"17 : 32") => (17, 32));
        expect_parse_err!(array_dim(b"  poatato:23") =>
          ParseError::ExpectedDigit);
        expect_parse_cut!(array_dim(b"  17:potato") =>
          ParseError::ExpectedDigit);
    }
}
