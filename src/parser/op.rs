//! trashcan's sub-parsers for operators

use nom::{self, IResult, ErrorKind};

use ast::*;
use super::*;

named!(pub assign_op<AssignOp>, complete!(preceded!(
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

named!(pub pow_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    map!(char!('^'), |_| BinOp::Pow)
)));
named!(pub un_op<UnOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    map!(one_of!("-~!"), |c| match c {
        '-' => UnOp::Negate,
        '~' => UnOp::BitNot,
        '!' => UnOp::LogNot,
        '&' => UnOp::AddressOf,
        _ => panic!("internal parser error")
    })
)));

named!(pub muldiv_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    map!(one_of!("*/%"), |c| match c {
        '*' => BinOp::Mul,
        '/' => BinOp::Div,
        '%' => BinOp::Mod,
        _ => panic!("internal parser error")
    })
)));

named!(pub addsub_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    map!(one_of!("+-@"), |c| match c {
        '+' => BinOp::Add,
        '-' => BinOp::Sub,
        '@' => BinOp::StrCat,
        _ => panic!("internal parser error")
    })
)));

named!(pub cmp_op<BinOp>, complete!(preceded!(
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

named!(pub eq_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    alt_complete!(
        map!(tag!("=="), |_| BinOp::Eq)
      | map!(tag!("!="), |_| BinOp::NotEq)
    )
)));

named!(pub bitand_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    map!(char!('&'), |_| BinOp::BitAnd)
)));

named!(pub bitor_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    map!(char!('|'), |_| BinOp::BitOr)
)));

named!(pub logand_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    map!(tag!("&&"), |_| BinOp::LogAnd)
)));

named!(pub logor_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    map!(tag!("||"), |_| BinOp::LogOr)
)));
