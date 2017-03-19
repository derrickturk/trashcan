//! trashcan's sub-parsers for statements

use nom;

use ast::*;
use super::SrcLoc;
use super::expr::*;
use super::op::*;
use super::ident::*;

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
  | foralong
  | assignment
  | terminated!(expr, terminator) => { |e: Expr| {
        let loc = e.loc.clone();
        Stmt {
          data: StmtKind::ExprStmt(e),
          loc,
        }
    }}
));

named!(decl<Stmt>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
 start_pos: call!(super::pos) >>
            tag!("let") >>
            call!(nom::multispace) >>
     decls: separated_list!(ws!(char!(',')), vardeclinit) >>
            terminator >>
   end_pos: call!(super::pos) >>
            (Stmt {
                data: StmtKind::VarDecl(decls),
                loc: SrcLoc::raw(start_pos, end_pos - start_pos),
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
            opt!(call!(nom::multispace)) >>
 start_pos: call!(super::pos) >>
        e1: expr >>
        op: assign_op >>
        e2: expr >>
            terminator >>
   end_pos: call!(super::pos) >>
            (Stmt {
                data: StmtKind::Assign(e1, op, e2),
                loc: SrcLoc::raw(start_pos, end_pos - start_pos),
            })
)));

named!(ret<Stmt>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
 start_pos: call!(super::pos) >>
            tag!("return") >>
         e: opt!(preceded!(call!(nom::multispace), expr)) >>
            terminator >>
   end_pos: call!(super::pos) >>
            (Stmt {
                data: StmtKind::Return(e),
                loc: SrcLoc::raw(start_pos, end_pos - start_pos),
            })
)));

named!(ifstmt<Stmt>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
 start_pos: call!(super::pos) >>
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
   end_pos: call!(super::pos) >>
            (Stmt {
                data: StmtKind::IfStmt {
                    cond,
                    body,
                    elsifs,
                    els,
                },
                loc: SrcLoc::raw(start_pos, end_pos - start_pos),
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
 start_pos: call!(super::pos) >>
            tag!("while") >>
            call!(nom::multispace) >>
      cond: expr >>
            opt!(call!(nom::multispace)) >>
            char!('{') >>
      body: many0!(stmt) >>
            opt!(call!(nom::multispace)) >>
            char!('}') >>
   end_pos: call!(super::pos) >>
            (Stmt {
                data: StmtKind::WhileLoop {
                    cond,
                    body,
                },
                loc: SrcLoc::raw(start_pos, end_pos - start_pos),
            })
)));

named!(forloop<Stmt>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
 start_pos: call!(super::pos) >>
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
   end_pos: call!(super::pos) >>
            (Stmt {
                data: StmtKind::ForLoop {
                    var,
                    spec,
                    body,
                },
                loc: SrcLoc::raw(start_pos, end_pos - start_pos),
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

named!(foralong<Stmt>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
 start_pos: call!(super::pos) >>
            tag!("for") >>
            call!(nom::multispace) >>
      vars: separated_nonempty_list!(ws!(char!(',')), ident) >>
            call!(nom::multispace) >>
            tag!("along") >>
            call!(nom::multispace) >>
     along: expr >>
            opt!(call!(nom::multispace)) >>
            char!('{') >>
      body: many0!(stmt) >>
            opt!(call!(nom::multispace)) >>
            char!('}') >>
   end_pos: call!(super::pos) >>
            (Stmt {
                data: StmtKind::ForAlong {
                    vars,
                    along,
                    body,
                },
                loc: SrcLoc::raw(start_pos, end_pos - start_pos),
            })
)));

// a note on the syntax:
// yeah, the thing <- alloc [...] "placement" syntax sucks, but:
//   1) it avoided an Extremely Vexing Parse (alloc arr[...]; looks
//     like `alloc indexing-expr` not `alloc name-expr bounds`)
//   2) I dunno maybe we'll use it for something else later?

named!(alloc<Stmt>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
 start_pos: call!(super::pos) >>
     array: expr >>
            opt!(call!(nom::multispace)) >>
            tag!("<-") >>
            opt!(call!(nom::multispace)) >>
            tag!("alloc") >>
   extents: alloc_extents >>
            terminator >>
   end_pos: call!(super::pos) >>
            (Stmt {
                data: StmtKind::Alloc(array, extents),
                loc: SrcLoc::raw(start_pos, end_pos - start_pos),
            })
)));

named!(realloc<Stmt>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
 start_pos: call!(super::pos) >>
     array: expr >>
            opt!(call!(nom::multispace)) >>
            tag!("<-") >>
            opt!(call!(nom::multispace)) >>
            tag!("realloc") >>
   extents: realloc_extents >>
            terminator >>
   end_pos: call!(super::pos) >>
            (Stmt {
                data: StmtKind::ReAlloc(array, extents.0, extents.1),
                loc: SrcLoc::raw(start_pos, end_pos - start_pos),
            })
)));

named!(alloc_extents<Vec<AllocExtent>>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
            char!('[') >>
   extents: separated_nonempty_list!(ws!(char!(',')), dim_extent) >>
            opt!(call!(nom::multispace)) >>
            char!(']') >>
            (extents)
)));

named!(realloc_extents<(usize, AllocExtent)>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
            char!('[') >>
   predims: many0!(ws!(char!(','))) >>
    extent: dim_extent >>
            opt!(call!(nom::multispace)) >>
            char!(']') >>
            (predims.len(), extent)
)));

named!(dim_extent<AllocExtent>, alt_complete!(
    range_extent
  | along_extent
));

named!(range_extent<AllocExtent>, complete!(do_parse!(
    lb: opt!(complete!(terminated!(
            expr,
            preceded!(
                opt!(call!(nom::multispace)),
                char!(':'))
        ))) >>
    ub: expr >>
        (AllocExtent::Range(lb, ub))
)));

named!(along_extent<AllocExtent>, complete!(do_parse!(
    tag!("along") >>
    call!(nom::multispace) >>
 e: expr >>
    (AllocExtent::Along(e))
)));

named!(dealloc<Stmt>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
 start_pos: call!(super::pos) >>
            tag!("dealloc") >>
         e: preceded!(call!(nom::multispace), expr) >>
            terminator >>
   end_pos: call!(super::pos) >>
            (Stmt {
                data: StmtKind::DeAlloc(e),
                loc: SrcLoc::raw(start_pos, end_pos - start_pos),
            })
)));

named!(print<Stmt>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
 start_pos: call!(super::pos) >>
            tag!("print") >>
            call!(nom::multispace) >>
     exprs: separated_list!(ws!(char!(',')), expr) >>
            terminator >>
   end_pos: call!(super::pos) >>
            (Stmt {
                data: StmtKind::Print(exprs),
                loc: SrcLoc::raw(start_pos, end_pos - start_pos),
            })
)));

named!(terminator<char>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    char!(';')
)));
