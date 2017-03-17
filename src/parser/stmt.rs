//! trashcan's sub-parsers for statements

use nom::{self, IResult, ErrorKind};

use ast::*;
use super::*;
use super::expr::*;

named!(pub stmt<Stmt>, alt_complete!(
    decl
  | ret
  | print
  | alloc
  | realloc
  | dealloc
  | ifstmt
  | whileloop
  | forloop
  | assignment
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
 decls: separated_list!(ws!(char!(',')), vardeclinit) >>
        terminator >>
        (Stmt {
            data: StmtKind::VarDecl(decls),
            loc: empty_loc!()
        })
)));

named!(vardeclinit<(Ident, Type, Option<Expr>)>, complete!(do_parse!(
          decl: vardecl >>
          init: opt!(varinit) >>
                (decl.0, decl.1, init)
)));

named!(vardecl<(Ident, Type)>, complete!(do_parse!(
  name: ident >>
        opt!(call!(nom::multispace)) >>
        char!(':') >>
    ty: typename >>
        (name, ty)
)));

named!(varinit<Expr>, complete!(do_parse!(
    opt!(call!(nom::multispace)) >>
    char!('=') >>
 e: expr >>
    (e)
)));

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

named!(forloop<Stmt>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
            tag!("for") >>
            call!(nom::multispace) >>
       var: forvardecl >>
      spec: alt_complete!(
                for_range => { |(from, to, step)|
                    ForSpec::Range(from, to, step) }
              | for_each => { ForSpec::Each }
            ) >>
            opt!(call!(nom::multispace)) >>
            char!('{') >>
      body: many0!(stmt) >>
            opt!(call!(nom::multispace)) >>
            char!('}') >>
            (Stmt {
                data: StmtKind::ForLoop {
                    var: var,
                    spec: spec,
                    body: body,
                },
                loc: empty_loc!(),
            })
)));

named!(forvardecl<(Ident, Type, ParamMode)>, complete!(do_parse!(
  name: ident >>
        opt!(call!(nom::multispace)) >>
        char!(':') >>
byref:  opt!(preceded!(
            opt!(nom::multispace),
            char!('&'))) >>
    ty: typename >>
        (name, ty, byref.map(|_| ParamMode::ByRef).unwrap_or(ParamMode::ByVal))
)));

named!(for_range<(Expr, Expr, Option<Expr>)>, complete!(do_parse!(
        opt!(call!(nom::multispace)) >>
        char!('=') >>
 first: expr >>
        opt!(call!(nom::multispace)) >>
        char!(':') >>
  last: expr >>
  step: opt!(do_parse!(
                opt!(call!(nom::multispace)) >>
                char!(':') >>
           step: expr >>
                (step)
        )) >>
        (first, last, step)
)));

named!(for_each<Expr>, complete!(do_parse!(
        call!(nom::multispace) >>
        tag!("in") >>
        call!(nom::multispace) >>
     e: expr >>
        (e)
)));

// a note on the syntax:
// yeah, the thing <- alloc [...] "placement" syntax sucks, but:
//   1) it avoided an Extremely Vexing Parse (alloc arr[...]; looks
//     like `alloc indexing-expr` not `alloc name-expr bounds`)
//   2) I dunno maybe we'll use it for something else later?

named!(alloc<Stmt>, complete!(do_parse!(
     array: expr >>
            opt!(call!(nom::multispace)) >>
            tag!("<-") >>
            opt!(call!(nom::multispace)) >>
            tag!("alloc") >>
   extents: alloc_extents >>
            terminator >>
            (Stmt {
                data: StmtKind::Alloc(array, extents),
                loc: empty_loc!(),
            })
)));

named!(realloc<Stmt>, complete!(do_parse!(
     array: expr >>
            opt!(call!(nom::multispace)) >>
            tag!("<-") >>
            opt!(call!(nom::multispace)) >>
            tag!("realloc") >>
   extents: realloc_extents >>
            terminator >>
            (Stmt {
                data: StmtKind::ReAlloc(array, extents),
                loc: empty_loc!(),
            })
)));

named!(dim_extent<(Option<Expr>, Expr)>, complete!(do_parse!(
    lb: opt!(complete!(terminated!(
            expr,
            preceded!(
                opt!(call!(nom::multispace)),
                char!(':'))
        ))) >>
    ub: expr >>
        (lb, ub)
)));

named!(alloc_extents<AllocExtents>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
            char!('[') >>
   extents: alt_complete!(
                separated_nonempty_list!(ws!(char!(',')), dim_extent) => {
                    |extents| AllocExtents::Range(extents)
                }

              | preceded!(
                  terminated!(tag!("along"), call!(nom::multispace)),
                  expr) => {
                      |e| AllocExtents::Along(e)
                  }
            ) >>
            opt!(call!(nom::multispace)) >>
            char!(']') >>
            (extents)
)));

named!(realloc_extents<ReAllocExtents>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
            char!('[') >>
  extents:  alt_complete!(
                do_parse!(
                   predims: many0!(ws!(char!(','))) >>
                    extent: dim_extent >>
                            (predims, extent)
                ) => {
                    |(predims, extent): (Vec<_>, _)|
                        ReAllocExtents::Range(predims.len(), extent)
                }

              | preceded!(
                  terminated!(tag!("along"), call!(nom::multispace)),
                  expr) => {
                      |e| ReAllocExtents::Along(e)
                  }
            ) >>
            opt!(call!(nom::multispace)) >>
            char!(']') >>
            (extents)
)));

named!(dealloc<Stmt>, complete!(do_parse!(
    opt!(call!(nom::multispace)) >>
    tag!("dealloc") >>
 e: preceded!(call!(nom::multispace), expr) >>
    terminator >>
    (Stmt {
        data: StmtKind::DeAlloc(e),
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

named!(terminator<char>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    char!(';')
)));
