//! trashcan's sub-parsers for expressions

use nom;

use ast::*;
use super::op::*;
use super::lit::*;
use super::ident::*;

use std::str;

// we have to handle left recursion very carefully
//   when parsing expressions (see https://en.wikipedia.org/wiki/Left_recursion)
//   and an example at https://github.com/Geal/nom/blob/master/tests/arithmetic_ast.rs

// the "rest" (recursive part) of a recursive expr
enum RecExprRest {
    CondExpr(Expr, Expr),
    Cast(Type),
}

// the "rest" (recursive part) of a "unitary" recursive expr
enum UnitaryRecExprRest {
    Indexed(Vec<Expr>),
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
named!(pub expr<Expr>, complete!(map!(do_parse!(
    first: call!(logorexpr) >>
     rest: opt!(alt_complete!(
               condexpr
             | castexpr
           )) >>
           (first, rest)),
   |(first, rest)| {
       match rest {
           None => first,

           Some(RecExprRest::CondExpr(ifexpr, elseexpr)) => Expr {
               data: ExprKind::CondExpr {
                         cond: Box::new(first),
                         if_expr: Box::new(ifexpr),
                         else_expr: Box::new(elseexpr),
                     },
               loc: empty_loc!(),
           },

           Some(RecExprRest::Cast(ty)) => Expr {
               data: ExprKind::Cast(Box::new(first), ty),
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

named!(castexpr<RecExprRest>, complete!(do_parse!(
        call!(nom::multispace) >>
        tag!("as") >>
        call!(nom::multispace) >>
   ty:  typename >>
        (RecExprRest::Cast(ty))
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

fn fold_unitary_exprs(first: Expr, rest: Vec<UnitaryRecExprRest>) -> Expr {
    rest.into_iter().fold(first, |sofar, rest| {
        match rest {
            UnitaryRecExprRest::Indexed(indices) => Expr {
                data: ExprKind::Index(Box::new(sofar), indices),
                loc: empty_loc!(),
            },

            UnitaryRecExprRest::Member(i) => Expr {
                data: ExprKind::Member(Box::new(sofar), i),
                loc: empty_loc!(),
            },

            UnitaryRecExprRest::MemberInvoke(i, args) => Expr {
                data: ExprKind::MemberInvoke(Box::new(sofar), i, args),
                loc: empty_loc!(),
            },
        }
    })
}

// "unitary" exprs (bind to unary ops for precedence)
// pull a nonrecursive expr, and maybe a recursive rest
named!(unitary_expr<Expr>, complete!(do_parse!(
    first: call!(nonrec_unitary_expr) >>
     rest: many0!(alt_complete!(
               indexed
             | memberinvoke
             | member
           )) >>
           (fold_unitary_exprs(first, rest))
)));

// a non left-recursive unitary expr
named!(nonrec_unitary_expr<Expr>, alt_complete!(
    // if we ever allow indirect fncalls this will become left-recursive
    fncall

  | extent_expr // n.b. this MUST be above path

  | path => { |p| Expr {
        data: ExprKind::Name(p),
        loc: empty_loc!(),
    }}

  | literal => { |lit| Expr {
        data: ExprKind::Lit(lit),
        loc: empty_loc!(),
    }}

  | grouped

  | vbexpr => { |e| Expr {
      data: ExprKind::VbExpr(e),
      loc: empty_loc!(),
    }}
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

// an extents expression e.g. first_index<0>(arr)
named!(extent_expr<Expr>, complete!(map_res!(do_parse!(
            opt!(call!(nom::multispace)) >>
      kind: alt_complete!(
                tag!("first_index") => { |_| ExtentKind::First }
              | tag!("last_index") => { |_| ExtentKind::Last }
            ) >>
            opt!(call!(nom::multispace)) >>
            char!('<') >>
            opt!(call!(nom::multispace)) >>
       dim: call!(nom::digit) >>
            opt!(call!(nom::multispace)) >>
            char!('>') >>
            opt!(call!(nom::multispace)) >>
            char!('(') >>
       arr: expr >>
            opt!(call!(nom::multispace)) >>
            char!(')') >>
            (arr, kind, dim)
), |(arr, kind, dim): (Expr, ExtentKind, &[u8])|
            -> Result<Expr, <usize as str::FromStr>::Err> {
    let dim = unsafe { str::from_utf8_unchecked(dim) };
    Ok(Expr {
        data: ExprKind::ExtentExpr(Box::new(arr), kind, dim.parse::<usize>()?),
        loc: empty_loc!(),
    })
})));

// a passthrough VB expression
// TODO: this needs work to pass through location,
//   and handle escaping ` inside vb stmts
named!(vbexpr<Vec<u8>>, complete!(do_parse!(
        opt!(call!(nom::multispace)) >>
        char!('`') >>
    vb: is_not!("`") >>
        char!('`') >>
        (vb.iter().cloned().collect())
)));

// various possible recursive "rests" of unitary exprs

named!(indexed<UnitaryRecExprRest>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
            char!('[') >>
   indices: separated_nonempty_list!(ws!(char!(',')), expr) >>
            opt!(call!(nom::multispace)) >>
            char!(']') >>
            (UnitaryRecExprRest::Indexed(indices))
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
