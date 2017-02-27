//! trashcan's sub-parsers for identifiers, paths, and typenames

use nom::{self, IResult, ErrorKind};

use ast::*;
use super::*;

pub const IDENT_CONT_CHARS: &'static str =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ\
     abcdefghijklmnopqrstuvwxyz\
     0123456789\
     _";

pub const KEYWORDS: [&'static str; 23] = [
    "let",
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
    "bool",
    "u8",
    "i16",
    "i32",
    "i64",
    "f32",
    "f64",
    "str",
    "currency",
    "date",
    "var",
    "obj",
];

named!(pub path<Path>, complete!(map!(
    separated_nonempty_list!(ws!(tag!("::")), ident), Path)));

named!(maybe_ident<Ident>, complete!(map!(do_parse!(
        opt!(call!(nom::multispace)) >>
 first: call!(nom::alpha) >>
  rest: opt!(is_a_s!(IDENT_CONT_CHARS)) >>
        (first, rest)
), |(first, rest)| {
    unsafe { // we know characters are valid utf8
        let mut s = String::from(str::from_utf8_unchecked(first));
        if let Some(rest) = rest {
            s.push_str(str::from_utf8_unchecked(rest));
        }
        Ident(s)
    }
})));

pub fn ident(input: &[u8]) -> IResult<&[u8], Ident> {
    let res = maybe_ident(input);
    match res {
        IResult::Done(rest, Ident(name)) =>
            if KEYWORDS.contains(&name.as_str()) {
                IResult::Error(
                    ErrorKind::Custom(CustomErrors::KeywordAsIdent as u32))
            } else {
                IResult::Done(rest, Ident(name))
            },
        err => err,
    }
}

named!(pub typename<MaybeType>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    alt_complete!(
        map!(tag!("bool"), |_| MaybeType::Known(Type::Bool))
      | map!(tag!("u8"), |_| MaybeType::Known(Type::UInt8))
      | map!(tag!("i16"), |_| MaybeType::Known(Type::Int16))
      | map!(tag!("i32"), |_| MaybeType::Known(Type::Int32))
      | map!(tag!("isize"), |_| MaybeType::Known(Type::IntPtr))
      | map!(tag!("f32"), |_| MaybeType::Known(Type::Float32))
      | map!(tag!("f64"), |_| MaybeType::Known(Type::Float64))
      | map!(tag!("str"), |_| MaybeType::Known(Type::String))
      | map!(tag!("currency"), |_| MaybeType::Known(Type::Currency))
      | map!(tag!("date"), |_| MaybeType::Known(Type::Date))
      | map!(tag!("var"), |_| MaybeType::Known(Type::Variant))
      | map!(tag!("obj"), |_| MaybeType::Known(Type::Obj))
      | map!(ident, |i| MaybeType::Deferred(i))
    )
)));
