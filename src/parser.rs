//! trashcan's parser and affiliated types

use std::str;

use nom;

use ast::*;

#[derive(Clone, Debug)]
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

named!(path<Path>, complete!(map!(
    separated_nonempty_list!(ws!(char!('.')), ident), Path)));

named!(ident<Ident>, complete!(map!(do_parse!(
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

enum MaybeType {
    Known(Type),
    Deferred(Ident),
}

named!(typename<MaybeType>, complete!(preceded!(
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
      | map!(ident, |i| MaybeType::Deferred(i))
    )
)));

named!(assign_op<AssignOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    alt_complete!(
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
    )
)));

named!(un_op<UnOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    map!(one_of!("-~!"), |c| match c {
        '-' => UnOp::Negate,
        '~' => UnOp::BitNot,
        '!' => UnOp::LogNot,
        _ => panic!("internal parser error")
    })
)));

named!(bin_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    alt_complete!(
        map!(tag!("=="), |_| BinOp::Eq)
      | map!(tag!("!="), |_| BinOp::NotEq)
      | map!(tag!("<="), |_| BinOp::LtEq)
      | map!(tag!(">="), |_| BinOp::GtEq)
      | map!(tag!("&&"), |_| BinOp::LogAnd)
      | map!(tag!("||"), |_| BinOp::LogOr)
      | map!(one_of!("+-*/%^@<>&|."), |c| match c {
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
            '.' => BinOp::MemInvoke,
            _ => panic!("internal parser error")
        })
    )
)));

named!(literal<Literal>, alt_complete!(
    literal_bool
  | literal_float // try this before int
  | literal_int
//  | literal_string
//  | literal_currency
//  | literal_date));
));

named!(literal_bool<Literal>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    alt!(
        map!(tag!("true"), |_| Literal::Bool(true))
      | map!(tag!("false"), |_| Literal::Bool(false))
    )
)));

named!(literal_int<Literal>, complete!(map_res!(do_parse!(
         opt!(call!(nom::multispace)) >>
    num: call!(nom::digit) >>
    tag: opt!(complete!(alt!(
            tag!("u8")
          | tag!("i16")
          | tag!("i32")
          | tag!("isize")
         ))) >>
    (num, tag)), |(num, tag): (&[u8], Option<&[u8]>)| {
        let num = unsafe { str::from_utf8_unchecked(num) };
        let tag = tag.map(|t| unsafe { str::from_utf8_unchecked(t) });
        match tag {
            Some("u8") => num.parse::<u8>().map(Literal::UInt8),
            Some("i16") => num.parse::<i16>().map(Literal::Int16),
            Some("i32") => num.parse::<i32>().map(Literal::Int32),
            Some("isize") => num.parse::<i64>().map(Literal::IntPtr),
            // default i32
            None => num.parse::<i32>().map(Literal::Int32),
            _ => panic!("internal parser error")
        }
    })));

named!(literal_float<Literal>, complete!(map_res!(do_parse!(
         opt!(call!(nom::multispace)) >>
  whole: call!(nom::digit) >>
         char!('.') >> // mandatory decimal point
   frac: opt!(complete!(call!(nom::digit))) >>
    tag: opt!(complete!(alt!(
            tag!("f32")
          | tag!("f64")
         ))) >>
    (whole, frac, tag)), |(w, f, tag): (&[u8], Option<&[u8]>, Option<&[u8]>)| {
        let num = unsafe {
            let mut s = String::from(str::from_utf8_unchecked(w));
            match f {
                Some(frac) => {
                    s.push_str(".");
                    s.push_str(str::from_utf8_unchecked(frac));
                }
                None => {}
            }
            s
        };
        let tag = tag.map(|t| unsafe { str::from_utf8_unchecked(t) });
        match tag {
            Some("f32") => num.parse::<f32>().map(Literal::Float32),
            Some("f64") => num.parse::<f64>().map(Literal::Float64),
            // default f64
            None => num.parse::<f64>().map(Literal::Float64),
            _ => panic!("internal parser error")
        }
    })));

#[cfg(test)]
mod test {
    use super::*;
    use nom::IResult;

    #[test]
    fn parse_literal_ints() {
        if let IResult::Done(_, Literal::UInt8(17u8)) = literal("17u8".as_bytes()) {
        } else {
            panic!("didn't parse literal 17u8");
        }

        match literal("12345".as_bytes()) {
            IResult::Done(_, Literal::Int32(12345)) => { },
            res => panic!("didn't parse literal 12345: {:?}", res)
        }

        match literal("12345u8".as_bytes()) {
            IResult::Done(_, _) => panic!("parsed 12345u8"),
            _ => { }
        }
    }

    #[test]
    fn parse_literal_floats() {
        match literal("1.".as_bytes()) {
            IResult::Done(_, Literal::Float64(1.0)) => { },
            res => panic!("didn't parse literal 1.: {:?}", res),
        }

        match literal("1.3f32".as_bytes()) {
            IResult::Done(_, Literal::Float32(1.3f32)) => { },
            _ => panic!("didn't parse literal 1.3f32")
        }
    }

    #[test]
    fn parse_literal() {
        if let IResult::Done(_, Literal::Bool(true)) = literal("true".as_bytes()) {
        } else {
            panic!("didn't parse literal true");
        }

        let res = literal("not!good".as_bytes());
        assert!(res.is_err());
    }

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
            IResult::Done(_, Ident(s)) => assert_eq!(s, "a_23"),
            _ => panic!("couldn't parse ident")
        }

        match ident("this".as_bytes()) {
            IResult::Done(_, Ident(s)) => assert_eq!(s, "this"),
            _ => panic!("couldn't parse 'this' as ident")
        }
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

    #[test]
    fn parse_path() {
        let res = path("_this::wont::even::start".as_bytes());
        assert!(res.is_err());

        match path("ident".as_bytes()) {
            IResult::Done(_, Path(vec)) => assert_eq!(vec.len(), 1),
            _ => panic!("didn't parse single-ident path")
        }

        match path("an_ident".as_bytes()) {
            IResult::Done(_, Path(vec)) => assert_eq!(vec.len(), 1),
            _ => panic!("didn't parse single-ident path")
        }

        match path("some_module.an_ident".as_bytes()) {
            IResult::Done(_, Path(vec)) => assert_eq!(vec.len(), 2),
            _ => panic!("didn't parse two-ident path")
        }

        match path("some_module.some.\nother .  thing".as_bytes()) {
            IResult::Done(_, Path(vec)) => {
                for &Ident(ref s) in &vec {
                    println!("ident: {}", s);
                }
                assert_eq!(vec.len(), 4);
            }
            _ => panic!("didn't parse messy path")
        }
    }
}
