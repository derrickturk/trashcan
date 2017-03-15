//! trashcan's sub-parsers for identifiers, paths, and typenames

use nom::{self, IResult, ErrorKind};

use ast::*;
use super::*;

pub const IDENT_CONT_CHARS: &'static str =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ\
     abcdefghijklmnopqrstuvwxyz\
     0123456789\
     _";

pub const KEYWORDS: [&'static str; 33] = [
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
    "new",
    "alloc",
    "realloc",
    "dealloc",
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

named!(pub path<Path>, complete!(do_parse!(
   module:  opt!(complete!(do_parse!(
                module: ident >>
                        opt!(call!(nom::multispace)) >>
                        tag!("::") >>
                        (module)
            ))) >>
      item: ident >>
            (Path(module, item))
)));

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
        Ident(s, None)
    }
})));

pub fn ident(input: &[u8]) -> IResult<&[u8], Ident> {
    let res = maybe_ident(input);
    match res {
        IResult::Done(rest, Ident(name, _)) =>
            if KEYWORDS.contains(&name.as_str()) {
                IResult::Error(
                    ErrorKind::Custom(CustomErrors::KeywordAsIdent as u32))
            } else {
                IResult::Done(rest, Ident(name, None))
            },
        err => err,
    }
}

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

named!(array_dim<(i32, i32)>, map_res!(complete!(do_parse!(
        opt!(call!(nom::multispace)) >>
 first: call!(nom::digit) >>
        opt!(call!(nom::multispace)) >>
   end: opt!(do_parse!(
                char!(':') >>
                opt!(call!(nom::multispace)) >>
           end: call!(nom::digit) >>
                (end)
        )) >>
        (first, end)
)), |(first, end)| make_range(first, end)));

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
