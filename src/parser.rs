//! trashcan's parser and affiliated types

use std::str;

use nom::{self, IResult, ErrorKind};

use ast::*;

#[derive(Clone, Debug)]
pub struct SrcLoc {
    pub file: String,
    pub line: u32,
    pub start: u32,
    pub len: u32,
}

// for now...
macro_rules! empty_loc {
    () => {
        SrcLoc {
            file: String::new(),
            line: 0,
            start: 0,
            len: 0,
        }
    }
}

const IDENT_CONT_CHARS: &'static str =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ\
     abcdefghijklmnopqrstuvwxyz\
     0123456789\
     _";

const KEYWORDS: [&'static str; 23] = [
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

enum CustomErrors {
    KeywordAsIdent,
    InvalidEscape,
}

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

*/

// we have to handle left recursion very carefully
//   when parsing expressions (see https://en.wikipedia.org/wiki/Left_recursion)
//   and an example at https://github.com/Geal/nom/blob/master/tests/arithmetic_ast.rs

// the "rest" (recursive part) of a recursive expr
enum RecExprRest {
    BinOpExpr(BinOp, Expr),
    CondExpr(Expr, Expr),
}

// the "rest" (recursive part) of a "unitary" recursive expr
enum UnitaryRecExprRest {
    Indexed(Expr),
    Member(Ident),
    MemberInvoke(Ident, Vec<Expr>),
    // FunCall(Vec<Expr>),
}

// official table of intended operator precedence!
//   shamelessly stolen from C
// 1 : arrays[], fncalls(), (parens), weird(mbr).invokes
// 2 : !unaryops
// 3 : ^
// 4 : * / %
// 5 : + - @
// 6 : > < >= <=
// 7 : == !=
// 8 : &
// 9 : |
// 10: &&
// 11: ||
// 12: x ? y : z

// pull a nonrecursive expr, and maybe a recursive rest
named!(expr<Expr>, complete!(map!(do_parse!(
    first: call!(logorexpr) >>
     rest: opt!(call!(condexpr)) >>
           (first, rest)),
   |(first, rest)| {
       match rest {
           None => first,

           Some(RecExprRest::BinOpExpr(op, e)) => Expr {
               data: ExprKind::BinOpApp(Box::new(first), Box::new(e), op),
               loc: empty_loc!(),
           },

           Some(RecExprRest::CondExpr(ifexpr, elseexpr)) => Expr {
               data: ExprKind::CondExpr {
                         cond: Box::new(first),
                         if_expr: Box::new(ifexpr),
                         else_expr: Box::new(elseexpr),
                     },
               loc: empty_loc!(),
           },
       }
})));

// non-unitary exprs, in decreasing precedence order...

fn fold_bin_exprs(first: Expr, rest: Vec<(BinOp, Expr)>) -> Expr {
    rest.into_iter().fold(first, |sofar, (op, e)| {
        Expr {
            data: ExprKind::BinOpApp(Box::new(sofar), Box::new(e), op),
            loc: empty_loc!(),
        }
    })
}

named!(powexpr<Expr>, complete!(do_parse!(
    first: unitary_op_expr >>
     rest: many0!(tuple!(pow_op, unitary_op_expr)) >>
           (fold_bin_exprs(first, rest))
)));

named!(muldivexpr<Expr>, complete!(do_parse!(
    first: powexpr >>
     rest: many0!(tuple!(muldiv_op, powexpr)) >>
           (fold_bin_exprs(first, rest))
)));

named!(addsubexpr<Expr>, complete!(do_parse!(
    first: muldivexpr >>
     rest: many0!(tuple!(addsub_op, muldivexpr)) >>
           (fold_bin_exprs(first, rest))
)));

named!(cmpexpr<Expr>, complete!(do_parse!(
    first: addsubexpr >>
     rest: many0!(tuple!(cmp_op, addsubexpr)) >>
           (fold_bin_exprs(first, rest))
)));

named!(eqexpr<Expr>, complete!(do_parse!(
    first: cmpexpr >>
     rest: many0!(tuple!(eq_op, cmpexpr)) >>
           (fold_bin_exprs(first, rest))
)));

named!(bitandexpr<Expr>, complete!(do_parse!(
    first: eqexpr >>
     rest: many0!(tuple!(bitand_op, eqexpr)) >>
           (fold_bin_exprs(first, rest))
)));

named!(bitorexpr<Expr>, complete!(do_parse!(
    first: bitandexpr >>
     rest: many0!(tuple!(bitor_op, bitandexpr)) >>
           (fold_bin_exprs(first, rest))
)));

named!(logandexpr<Expr>, complete!(do_parse!(
    first: bitorexpr >>
     rest: many0!(tuple!(logand_op, bitorexpr)) >>
           (fold_bin_exprs(first, rest))
)));

named!(logorexpr<Expr>, complete!(do_parse!(
    first: logandexpr >>
     rest: many0!(tuple!(logor_op, logandexpr)) >>
           (fold_bin_exprs(first, rest))
)));

named!(condexpr<RecExprRest>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
            char!('?') >>
    ifexpr: call!(expr) >>
            opt!(call!(nom::multispace)) >>
            char!(':') >>
            opt!(call!(nom::multispace)) >>
  elseexpr: call!(expr) >>
            (RecExprRest::CondExpr(ifexpr, elseexpr))
)));

// "unitary" exprs, possibly preceded by unary operators
// this alt_complete is arguably backwards
named!(unitary_op_expr<Expr>, alt_complete!(
    unitary_expr
  | tuple!(un_op, unitary_op_expr) => { |(op, e)| Expr {
        data: ExprKind::UnOpApp(Box::new(e), op),
        loc: empty_loc!(),
    }}
));

// "unitary" exprs (bind to unary ops for precedence)
// pull a nonrecursive expr, and maybe a recursive rest
named!(unitary_expr<Expr>, complete!(map!(do_parse!(
    first: call!(nonrec_unitary_expr) >>
     rest: opt!(alt_complete!(
               indexed
             | memberinvoke
             | member
           )) >>
           (first, rest)),
   |(first, rest)| {
       match rest {
           None => first,

           Some(UnitaryRecExprRest::Indexed(e)) => Expr {
               data: ExprKind::Index(Box::new(first), Box::new(e)),
               loc: empty_loc!(),
           },

           Some(UnitaryRecExprRest::Member(i)) => Expr {
               data: ExprKind::Member(Box::new(first), i),
               loc: empty_loc!(),
           },

           Some(UnitaryRecExprRest::MemberInvoke(i, args)) => Expr {
               data: ExprKind::MemberInvoke(Box::new(first), i, args),
               loc: empty_loc!(),
           },
       }
})));

// a non left-recursive unitary expr
named!(nonrec_unitary_expr<Expr>, alt_complete!(
    // if we ever allow indirect fncalls this will become left-recursive
    fncall

  | path => { |p| Expr {
        data: ExprKind::Name(p),
        loc: empty_loc!(),
    }}

  | literal => { |lit| Expr {
        data: ExprKind::Lit(lit),
        loc: empty_loc!(),
    }}

  | grouped
));

named!(fncall<Expr>, complete!(do_parse!(
    name: call!(path) >>
          opt!(call!(nom::multispace)) >>
          char!('(') >>
    args: separated_list!(ws!(char!(',')), expr) >>
          char!(')') >>
          (Expr {
              data: ExprKind::Call(name, args),
              loc: empty_loc!(),
          })
)));

// an expr grouped in parentheses, to force precedence
named!(grouped<Expr>, complete!(do_parse!(
       opt!(call!(nom::multispace)) >>
       char!('(') >>
    e: expr >>
       opt!(call!(nom::multispace)) >>
       char!(')') >>
       (e)
)));

// various possible recursive "rests" of unitary exprs

named!(indexed<UnitaryRecExprRest>, complete!(do_parse!(
        opt!(call!(nom::multispace)) >>
        char!('[') >>
 index: call!(expr) >>
        opt!(call!(nom::multispace)) >>
        char!(']') >>
        (UnitaryRecExprRest::Indexed(index))
)));

named!(member<UnitaryRecExprRest>, complete!(do_parse!(
        opt!(call!(nom::multispace)) >>
        char!('.') >>
 name:  call!(ident) >>
        (UnitaryRecExprRest::Member(name))
)));

named!(memberinvoke<UnitaryRecExprRest>, complete!(do_parse!(
        opt!(call!(nom::multispace)) >>
        char!('.') >>
 name:  call!(ident) >>
        char!('(') >>
  args: separated_list!(ws!(char!(',')), expr) >>
        char!(')') >>
        (UnitaryRecExprRest::MemberInvoke(name, args))
)));

named!(path<Path>, complete!(map!(
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
      | map!(tag!("obj"), |_| MaybeType::Known(Type::Obj))
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
        '&' => UnOp::AddressOf,
        _ => panic!("internal parser error")
    })
)));

named!(pow_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    map!(char!('^'), |_| BinOp::Pow)
)));

named!(muldiv_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    map!(one_of!("*/%"), |c| match c {
        '*' => BinOp::Mul,
        '/' => BinOp::Div,
        '%' => BinOp::Mod,
        _ => panic!("internal parser error")
    })
)));

named!(addsub_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    map!(one_of!("+-@"), |c| match c {
        '+' => BinOp::Add,
        '-' => BinOp::Sub,
        '@' => BinOp::StrCat,
        _ => panic!("internal parser error")
    })
)));

named!(cmp_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    alt_complete!(
        map!(tag!("<="), |_| BinOp::LtEq)
      | map!(tag!(">="), |_| BinOp::GtEq)
      | map!(one_of!("<>"), |c| match c {
            '<' => BinOp::Lt,
            '>' => BinOp::Gt,
            _ => panic!("internal parser error")
        })
    )
)));

named!(eq_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    alt_complete!(
        map!(tag!("=="), |_| BinOp::Eq)
      | map!(tag!("!="), |_| BinOp::NotEq)
    )
)));

named!(bitand_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    map!(char!('&'), |_| BinOp::BitAnd)
)));

named!(bitor_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    map!(char!('|'), |_| BinOp::BitOr)
)));

named!(logand_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    map!(tag!("&&"), |_| BinOp::LogAnd)
)));

named!(logor_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    map!(tag!("||"), |_| BinOp::LogOr)
)));

named!(literal<Literal>, alt_complete!(
    literal_bool
  | literal_float // try this before int
  | literal_int
  | literal_string
//  TODO: "wacky" literal types
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

named!(literal_string<Literal>, map_res!(complete!(preceded!(
    opt!(call!(nom::multispace)),
    delimited!(
        char!('"'),
        escaped_string,
        char!('"')
    )
)), |bytes| {
    String::from_utf8(bytes).map(Literal::String)
}));

fn escaped_string(input: &[u8]) -> nom::IResult<&[u8], Vec<u8>> {
    let mut s = Vec::new();
    let mut bytes = input.iter();
    while let Some(c) = bytes.next() {
        if *c == b'"' {
            break;
        }

        if *c == b'\\' {
            match bytes.next() {
                Some(&b'n') => s.push(b'\n'),
                Some(&b't') => s.push(b'\t'),
                // TODO: more escapes here
                _ => return IResult::Error(
                    ErrorKind::Custom(CustomErrors::InvalidEscape as u32))
            }
        }

        // TODO: it'd be nice to allow rust style multiline strings
        //   (or maybe C-style adjacent-literal concatenation)
        // first option needs peek here; second just needs a change to the
        // literal_string production

        s.push(*c);
    }

    IResult::Done(&input[s.len()..], s)
}

// TODO: do we need to pre-emptively tag idents that conflict with VB keywords?
//   forbid them?
//   prepend some goofy ©high-ASCII char?
// answer: yes, in codegen (not here)

#[cfg(test)]
mod test {
    use super::*;
    use nom::{self, IResult, ErrorKind};

    #[test]
    fn parse_literal_strs() {
        let s = "it is only fitting that\ttabs are embedded here.\n";
        let lit_s = format!("\"{}\"", s);

        if let IResult::Done(_, Literal::String(l)) = literal(lit_s.as_bytes()) {
            assert_eq!(s, l);
        } else {
            panic!("failed to parse literal string.");
        }

        let res = literal(b"\"an invalid \\xescape sequence\\\"");
        assert!(res.is_err());
    }

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
        if let IResult::Done(_, BinOp::Add) = addsub_op("+".as_bytes()) {
        } else {
            panic!("didn't parse BinOp::Add");
        }

        if let IResult::Done(_, BinOp::BitAnd) = bitand_op("&".as_bytes()) {
        } else {
            panic!("didn't parse BinOp::BitAnd");
        }

        if let IResult::Done(_, BinOp::LogAnd) = logand_op("&&".as_bytes()) {
        } else {
            panic!("didn't parse BinOp::LogAnd");
        }
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

        match ident(b"for") {
            IResult::Error(ErrorKind::Custom(e))
                if e == CustomErrors::KeywordAsIdent as u32 => { },
            res => panic!("didn't fail keyword-as-ident: {:?}, res")
        }

        match ident("fortuna".as_bytes()) {
            IResult::Done(_, Ident(s)) => assert_eq!(s, "fortuna"),
            _ => panic!("couldn't parse ident")
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

        match path("some_module::an_ident".as_bytes()) {
            IResult::Done(_, Path(vec)) => assert_eq!(vec.len(), 2),
            _ => panic!("didn't parse two-ident path")
        }

        match path("some_module::some::\nother ::  thing".as_bytes()) {
            IResult::Done(_, Path(vec)) => {
                for &Ident(ref s) in &vec {
                    println!("ident: {}", s);
                }
                assert_eq!(vec.len(), 4);
            }
            _ => panic!("didn't parse messy path")
        }
    }

    #[test]
    fn parse_expr() {
        let e = b"32.5";
        match expr(e) {
            IResult::Done(_, Expr { data: ExprKind::Lit(Literal::Float64(32.5)), loc: _ }) => { },
            res => panic!("didn't parse literal expr: {:?}", res)
        }

        let e = b"some::modules::array[23]";
        match expr(e) {
            IResult::Done(_, Expr { data: ExprKind::Index(e1, e2), loc: _ }) => {
                match *e1 {
                    Expr { data: ExprKind::Name(p), loc: _ } => { },
                    res => panic!("indexing expr: didn't parse e1 as path: {:?}", res)
                }

                match *e2 {
                    Expr { data: ExprKind::Lit(Literal::Int32(23)), loc: _ } => { },
                    res => panic!("indexing expr: didn't parse e2 as literal: {:?}", res)
                }
            },
            res => panic!("didn't parse indexing expr: {:?}", res)
        }

        let e = b"some::modules::array[some.other.array[23]]";
        assert!(expr(e).is_done());

        let e = b"some::fun(1, 2, x[2], other())";
        assert!(expr(e).is_done());

        let e = b"x ? f(23) : y[17]";
        assert!(expr(e).is_done());

        let e = b"!f(2) ? f(~23) : y[17]";
        assert!(expr(e).is_done());

        let e = b"!(f(2) ? f(~23) : y[17])";
        assert!(expr(e).is_done());

        let e = b"2 ^ 3";
        assert!(expr(e).is_done());

        let e = b"(2 + 3 * 7 && f(9) | ~x[17]) @ \"bob\"";
        assert!(expr(e).is_done());

        let e = b"f(17).x + some_mod::f(23).foo(99)";
        assert!(expr(e).is_done());
    }
}
