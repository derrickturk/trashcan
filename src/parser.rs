//! trashcan's parser and affiliated types

use std::str;

use nom;

use ast::*;

#[derive(Clone)]
pub struct SrcLoc {
    pub file: String,
    pub line: u32,
    pub start: u32,
    pub len: u32,
}

const IDENT_CONT_CHARS: &'static str =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ\
     abcdefghijklmnopqrstuvwxyz\
     0123456789\
     _";

/*

named!(dumpster(&[u8]) -> Dumpster, map!(
    many1!(module),
    |mods| Dumpster { modules: mods }
));

// no idea WTF this is blowing up on
named!(module(&[u8]) -> Module, ws!(do_parse!(
            tag!("mod") >>
      name: ident >>
  contents: delimited!(char!('{'), tag!("meat"), char!('}')) >>
            unimplemented!()
            )));

// named!(ident(&[u8]) -> Ident, 

*/

named!(ident(&[u8]) -> Ident, map!(do_parse!(
 first: call!(nom::alpha) >>
  rest: is_a_s!(IDENT_CONT_CHARS) >>
        (first, rest)
), |(first, rest)| {
    unsafe { // we know characters are valid utf8
        let mut s = String::from(str::from_utf8_unchecked(first));
        s.push_str(str::from_utf8_unchecked(rest));
        Ident(s)
    }
}));

enum MaybeType {
    Known(Type),
    Deferred(Ident),
}

named!(typename<MaybeType>, alt_complete!(
    map!(tag!("bool"), |_| MaybeType::Known(Type::Bool))
  | map!(tag!("i8"), |_| MaybeType::Known(Type::Int8))
  | map!(tag!("i16"), |_| MaybeType::Known(Type::Int16))
  | map!(tag!("i32"), |_| MaybeType::Known(Type::Int32))
  | map!(tag!("isize"), |_| MaybeType::Known(Type::IntPtr))
  | map!(tag!("f32"), |_| MaybeType::Known(Type::Float32))
  | map!(tag!("f64"), |_| MaybeType::Known(Type::Float64))
  | map!(tag!("str"), |_| MaybeType::Known(Type::String))
  | map!(tag!("currency"), |_| MaybeType::Known(Type::Currency))
  | map!(tag!("date"), |_| MaybeType::Known(Type::Date))
  | map!(tag!("var"), |_| MaybeType::Known(Type::Variant))
  | map!(ident, |i| MaybeType::Deferred(i))
));

named!(assign_op<AssignOp>, alt_complete!(
    map!(char!('='), |_| AssignOp::Assign)
  | map!(tag!("+="), |_| AssignOp::AddAssign)
  | map!(tag!("-="), |_| AssignOp::SubAssign)
  | map!(tag!("*="), |_| AssignOp::MulAssign)
  | map!(tag!("/="), |_| AssignOp::DivAssign)
  | map!(tag!("%="), |_| AssignOp::ModAssign)
  | map!(tag!("^="), |_| AssignOp::PowAssign)
  | map!(tag!("@="), |_| AssignOp::StrCatAssign)
  | map!(tag!("&="), |_| AssignOp::BitAndAssign)
  | map!(tag!("|="), |_| AssignOp::BitOrAssign)
  | map!(tag!("&&="), |_| AssignOp::LogAndAssign)
  | map!(tag!("||="), |_| AssignOp::LogOrAssign)
));

named!(un_op<UnOp>, map!(one_of!("-~!"), |c| match c {
    '-' => UnOp::Negate,
    '~' => UnOp::BitNot,
    '!' => UnOp::LogNot,
    _ => panic!("internal parser error")
}));

named!(bin_op<BinOp>, alt_complete!(
    map!(tag!("=="), |_| BinOp::Eq)
  | map!(tag!("!="), |_| BinOp::NotEq)
  | map!(tag!("<="), |_| BinOp::LtEq)
  | map!(tag!(">="), |_| BinOp::GtEq)
  | map!(tag!("&&"), |_| BinOp::LogAnd)
  | map!(tag!("||"), |_| BinOp::LogOr)
  | map!(one_of!("+-*/%^@<>&|"), |c| match c {
        '+' => BinOp::Add,
        '-' => BinOp::Sub,
        '*' => BinOp::Mul,
        '/' => BinOp::Div,
        '%' => BinOp::Mod,
        '^' => BinOp::Pow,
        '@' => BinOp::StrCat,
        '<' => BinOp::Lt,
        '>' => BinOp::Gt,
        '&' => BinOp::BitAnd,
        '|' => BinOp::BitOr,
        _ => panic!("internal parser error")
    })
));

#[cfg(test)]
mod test {
    use super::*;
    use nom::IResult;

    #[test]
    fn parse_assign_ops() {
        if let IResult::Done(_, AssignOp::Assign) = assign_op("=".as_bytes()) {
        } else {
            panic!("didn't parse AssignOp::Assign");
        }

        if let IResult::Done(_, AssignOp::BitAndAssign) = assign_op("&=".as_bytes()) {
        } else {
            panic!("didn't parse AssignOp::BitAndAssign");
        }

        assert!(assign_op("xx=".as_bytes()).is_err());
    }

    #[test]
    fn parse_un_ops() {
        if let IResult::Done(_, UnOp::Negate) = un_op("-".as_bytes()) {
        } else {
            panic!("didn't parse UnOp::Negate");
        }

        assert!(un_op("x".as_bytes()).is_err());
    }

    #[test]
    fn parse_bin_ops() {
        if let IResult::Done(_, BinOp::Add) = bin_op("+".as_bytes()) {
        } else {
            panic!("didn't parse BinOp::Add");
        }

        if let IResult::Done(_, BinOp::BitAnd) = bin_op("&".as_bytes()) {
        } else {
            panic!("didn't parse BinOp::BitAnd");
        }

        if let IResult::Done(_, BinOp::LogAnd) = bin_op("&&".as_bytes()) {
        } else {
            panic!("didn't parse BinOp::LogAnd");
        }

        assert!(bin_op("xx".as_bytes()).is_err());
    }

    #[test]
    fn parse_ident() {
        let res = ident("_abcdef".as_bytes());
        assert!(res.is_err());

        match ident("a_23".as_bytes()) {
            IResult::Done(_, Ident(s)) => assert!(s == "a_23"),
            _ => panic!("couldn't parse ident")
        }

        let res = ident("big bad sally".as_bytes());
        assert!(res.is_err());
    }

    #[test]
    fn parse_type() {
        let res = typename("_abcdef".as_bytes());
        assert!(res.is_err());

        match typename("boogaloo".as_bytes()) {
            IResult::Done(_, MaybeType::Deferred(Ident(s))) => {
                assert!(s == "boogaloo")
            },
            _ => panic!("couldn't parse deferred-ident type")
        }

        match typename("i32".as_bytes()) {
            IResult::Done(_, MaybeType::Known(Int32)) => { },
            _ => panic!("couldn't parse i32")
        }
    }
}
