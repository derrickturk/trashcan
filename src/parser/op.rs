//! trashcan's sub-parsers for operators

use nom;

use ast::*;

named!(pub assign_op<AssignOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    alt_complete!(
        char!('=') => { |_| AssignOp::Assign }
      | tag!("+=") => { |_| AssignOp::AddAssign }
      | tag!("-=") => { |_| AssignOp::SubAssign }
      | tag!("*=") => { |_| AssignOp::MulAssign }
      | tag!("/=") => { |_| AssignOp::DivAssign }
      | tag!("%=") => { |_| AssignOp::ModAssign }
      | tag!("^=") => { |_| AssignOp::PowAssign }
      | tag!("@=") => { |_| AssignOp::StrCatAssign }
      | tag!("&=") => { |_| AssignOp::BitAndAssign }
      | tag!("|=") => { |_| AssignOp::BitOrAssign }
      | tag!("&&=") => { |_| AssignOp::LogAndAssign }
      | tag!("||=") => { |_| AssignOp::LogOrAssign }
    )
)));

named!(pub pow_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    map!(char!('^'), |_| BinOp::Pow)
)));
named!(pub un_op<UnOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    map!(one_of!("-~!&"), |c| match c {
        '-' => UnOp::Negate,
        '~' => UnOp::BitNot,
        '!' => UnOp::LogNot,
        '&' => UnOp::AddressOf,
        _ => panic!("dumpster fire: bad unary operator")
    })
)));

named!(pub muldiv_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    map!(one_of!("*/%"), |c| match c {
        '*' => BinOp::Mul,
        '/' => BinOp::Div,
        '%' => BinOp::Mod,
        _ => panic!("dumpster fire: bad binary operator")
    })
)));

named!(pub addsub_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    map!(one_of!("+-@"), |c| match c {
        '+' => BinOp::Add,
        '-' => BinOp::Sub,
        '@' => BinOp::StrCat,
        _ => panic!("dumpster fire: bad binary operator")
    })
)));

named!(pub cmp_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    alt_complete!(
        tag!("<=") => { |_| BinOp::LtEq }
      | tag!(">=") => { |_| BinOp::GtEq }
      | one_of!("<>") => { |c| match c {
            '<' => BinOp::Lt,
            '>' => BinOp::Gt,
            _ => panic!("dumpster fire: bad binary operator")
        }}
    )
)));

named!(pub eq_op<BinOp>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    alt_complete!(
        tag!("===") => { |_| BinOp::IdentEq }
      | tag!("!==") => { |_| BinOp::NotIdentEq }
      | tag!("==") => { |_| BinOp::Eq }
      | tag!("!=") => { |_| BinOp::NotEq }
    )
)));

named!(pub bitand_op<BinOp>, complete!(do_parse!(
    opt!(call!(nom::multispace)) >>
    char!('&') >>
    // avoid conflict with parse of && (logical and)
    //   as & & (bitwise-and address-of)
    not!(peek!(char!('&'))) >>
    (BinOp::BitAnd)
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
