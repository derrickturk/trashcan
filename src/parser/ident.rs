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

// TODO urgent: arrays
named!(pub typename<Type>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    alt_complete!(
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
      | ident => { |i| Type::Deferred(i) }
    )
)));
