//! trashcan's sub-parsers for statements

use nom::{self, IResult, ErrorKind};

use ast::*;
use super::*;
use super::expr::*;

named!(pub stmt<Stmt>, alt_complete!(
    decl
  | ret
  | print
  | ifstmt
  | whileloop
  | assignment
  | vbstmt => { |s| { Stmt {
        data: StmtKind::VbStmt(s),
        loc: empty_loc!(),
    }}}
  | terminated!(expr, terminator) => { |e: Expr| {
        let loc = e.loc.clone();
        Stmt {
          data: StmtKind::ExprStmt(e),
          loc: loc,
        }
    }}
));

named!(decl<Stmt>, complete!(do_parse!(
        opt!(call!(nom::multispace)) >>
        tag!("let") >>
        call!(nom::multispace) >>
 decls: separated_list!(ws!(char!(',')), vardecl) >>
        terminator >>
        (Stmt {
            data: StmtKind::VarDecl(decls),
            loc: empty_loc!()
        })
)));

named!(vardecl<(Ident, Type, Option<Expr>)>, complete!(do_parse!(
  name: ident >>
        opt!(call!(nom::multispace)) >>
        char!(':') >>
    ty: typename >>
  init: varinit >>
        (name, ty, init)
)));

named!(varinit<Option<Expr>>, complete!(opt!(do_parse!(
    opt!(call!(nom::multispace)) >>
    char!('=') >>
 e: expr >>
    (e)
))));

named!(assignment<Stmt>, complete!(do_parse!(
    e1: expr >>
    op: assign_op >>
    e2: expr >>
        terminator >>
        (Stmt {
            data: StmtKind::Assign(e1, op, e2),
            loc: empty_loc!()
        })
)));

named!(ret<Stmt>, complete!(do_parse!(
    opt!(call!(nom::multispace)) >>
    tag!("return") >>
 e: opt!(preceded!(call!(nom::multispace), expr)) >>
    terminator >>
    (Stmt {
        data: StmtKind::Return(e),
        loc: empty_loc!(),
    })
)));

named!(ifstmt<Stmt>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
            tag!("if") >>
            call!(nom::multispace) >>
      cond: expr >>
            opt!(call!(nom::multispace)) >>
            char!('{') >>
      body: many0!(stmt) >>
            opt!(call!(nom::multispace)) >>
            char!('}') >>
    elsifs: many0!(elsif) >>
       els: opt!(els) >>
            (Stmt {
                data: StmtKind::IfStmt {
                    cond: cond,
                    body: body,
                    elsifs: elsifs,
                    els: els,
                },
                loc: empty_loc!(),
            })
)));

named!(elsif<(Expr, Vec<Stmt>)>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
            tag!("else") >>
            call!(nom::multispace) >>
            tag!("if") >>
            call!(nom::multispace) >>
      cond: expr >>
            opt!(call!(nom::multispace)) >>
            char!('{') >>
      body: many0!(stmt) >>
            opt!(call!(nom::multispace)) >>
            char!('}') >>
            (cond, body)
)));

named!(els<Vec<Stmt>>, complete!(do_parse!(
        opt!(call!(nom::multispace)) >>
        tag!("else") >>
        opt!(call!(nom::multispace)) >>
        char!('{') >>
  body: many0!(stmt) >>
        opt!(call!(nom::multispace)) >>
        char!('}') >>
        (body)
)));

named!(whileloop<Stmt>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
            tag!("while") >>
            call!(nom::multispace) >>
      cond: expr >>
            opt!(call!(nom::multispace)) >>
            char!('{') >>
      body: many0!(stmt) >>
            opt!(call!(nom::multispace)) >>
            char!('}') >>
            (Stmt {
                data: StmtKind::WhileLoop {
                    cond: cond,
                    body: body,
                },
                loc: empty_loc!(),
            })
)));

named!(print<Stmt>, complete!(do_parse!(
    opt!(call!(nom::multispace)) >>
    tag!("print") >>
 e: preceded!(call!(nom::multispace), expr) >>
    terminator >>
    (Stmt {
        data: StmtKind::Print(e),
        loc: empty_loc!(),
    })
)));

// TODO: this needs work to pass through location,
//   and handle escaping ` inside vb stmts
named!(vbstmt<Vec<u8>>, complete!(do_parse!(
        opt!(call!(nom::multispace)) >>
        char!('`') >>
    vb: is_not!("`") >>
        char!('`') >>
        terminator >>
        (vb.iter().cloned().collect())
)));

named!(terminator<char>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    char!(';')
)));
