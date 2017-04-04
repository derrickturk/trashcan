//! trashcan's sub-parsers for statements

use super::{ParseError, ParseResult, SrcLoc};
#[macro_use]
use super::bits::*;
use super::op::*;
use super::ident::*;
use super::expr::*;

use ast::*;

pub fn stmt(input: &[u8]) -> ParseResult<Stmt> {
    alt!(input,
        decl(input)
      ; ret(input)
      ; print(input)
      ; ifstmt(input)
      ; whileloop(input)
      ; foralong(input) // must try this before forloop
      ; forloop(input)
      ; alloc(input)
      ; realloc(input)
      ; dealloc(input)
      ; assignment(input)
      ; exprstmt(input)
    )
}

fn decl(input: &[u8]) -> ParseResult<Stmt> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, _) = require!(keyword_immediate(i, b"let"));
    let (i, _) = require!(multispace(i));
    // cut on error from this point
    //   downstream cuts handle required type or initializer-expr, so...
    let (i, decls) = require_or_cut!(delimited_at_least_one(i,
        vardeclinit,
        |i| chain!(i,
            |i| opt(i, multispace) =>
            |i| byte(i, b',')
        )) => ParseError::ExpectedIdent);
    let (i, _) = require_or_cut!(terminator(i));
    let (i, end_pos) = require!(pos(i));
    ok!(i, Stmt {
        data: StmtKind::VarDecl(decls),
        loc: SrcLoc::raw(start_pos, end_pos - start_pos),
    })
}

#[inline]
fn vardeclinit(input: &[u8]) -> ParseResult<(Ident, Type, Option<Expr>)> {
    let (i, decl) = require!(vardecl(input));
    let (i, init) = require!(opt!(varinit(i)));
    ok!(i, (decl.0, decl.1, init))
}

#[inline]
fn vardecl(input: &[u8]) -> ParseResult<(Ident, Type)> {
    let (i, name) = require!(ident(input));
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require!(byte(i, b':'));
    let (i, ty) = require_or_cut!(typename(i) => ParseError::ExpectedTypename);
    ok!(i, (name, ty))
}

#[inline]
fn varinit(input: &[u8]) -> ParseResult<Expr> {
    let (i, _) = opt(input, multispace)?;
    let (i, _) = require!(byte(i, b'='));
    cut_if_err!(expr(i) => ParseError::ExpectedExpr)
}

#[inline]
fn ret(input: &[u8]) -> ParseResult<Stmt> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, _) = require!(keyword_immediate(i, b"return"));
    let (i, e) = require!(opt!(chain!(i,
        |i| multispace(i) =>
        |i| expr(i)
    )));
    // after this point we can cut on error
    let (i, _) = require_or_cut!(terminator(i));
    let (i, end_pos) = require!(pos(i));
    ok!(i, Stmt {
        data: StmtKind::Return(e),
        loc: SrcLoc::raw(start_pos, end_pos - start_pos),
    })
}

fn print(input: &[u8]) -> ParseResult<Stmt> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, _) = require!(keyword_immediate(i, b"print"));

    // we can't cut if we don't see whitespace:
    //   consider e.g. printf(1,2,3);
    let (i, _) = require!(multispace(i));

    // cut on error after this point
    let (i, exprs) = require_or_cut!(delimited_at_least_one(i,
        expr,
        |i| chain!(i,
            |i| opt(i, multispace) =>
            |i| byte(i, b',')
        )));
    let (i, _) = require_or_cut!(terminator(i));
    let (i, end_pos) = require_or_cut!(pos(i));
    ok!(i, Stmt {
        data: StmtKind::Print(exprs),
        loc: SrcLoc::raw(start_pos, end_pos - start_pos),
    })
}

fn ifstmt(input: &[u8]) -> ParseResult<Stmt> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, _) = require!(keyword_immediate(i, b"if"));
    let (i, _) = require!(multispace(i));
    // after here we can cut on error
    let (i, cond) = require_or_cut!(expr(i));
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b'{'));
    let (i, body) = require_or_cut!(many(i, stmt));
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b'}'));
    let (i, elsifs) = require_or_cut!(many(i, elsif));
    let (i, els) = require_or_cut!(opt(i, els));
    let (i, end_pos) = require!(pos(i));
    ok!(i, Stmt {
        data: StmtKind::IfStmt {
            cond,
            body,
            elsifs,
            els,
        },
        loc: SrcLoc::raw(start_pos, end_pos - start_pos),
    })
}

#[inline]
fn elsif(input: &[u8]) -> ParseResult<(Expr, Vec<Stmt>)> {
    let (i, _) = require!(keyword(input, b"else"));
    // have to backtrack to input here or hit ambiguity with "els"
    let (i, _) = require!(input, multispace(i));
    let (i, _) = require!(input, keyword_immediate(i, b"if"));
    // after this point we should cut on error
    let (i, _) = require_or_cut!(input, multispace(i));
    let (i, cond) = require_or_cut!(expr(i) => ParseError::ExpectedExpr);
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b'{'));
    let (i, body) = require_or_cut!(many(i, stmt));
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b'}'));
    ok!(i, (cond, body))
}

#[inline]
fn els(input: &[u8]) -> ParseResult<Vec<Stmt>> {
    let (i, _) = require!(keyword(input, b"else"));
    let (i, _) = opt(i, multispace)?;
    // can cut after this point
    let (i, _) = require_or_cut!(byte(i, b'{'));
    let (i, body) = require_or_cut!(many(i, stmt));
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b'}'));
    ok!(i, body)
}

fn whileloop(input: &[u8]) -> ParseResult<Stmt> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, _) = require!(keyword_immediate(i, b"while"));
    let (i, _) = require!(multispace(i));
    // after here we can cut on error
    let (i, cond) = require_or_cut!(expr(i));
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b'{'));
    let (i, body) = require_or_cut!(many(i, stmt));
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b'}'));
    let (i, end_pos) = require!(pos(i));
    ok!(i, Stmt {
        data: StmtKind::WhileLoop {
            cond,
            body,
        },
        loc: SrcLoc::raw(start_pos, end_pos - start_pos),
    })
}

// must be tried before forloop or forloop will cut on lack of type ascription
//   in vars
fn foralong(input: &[u8]) -> ParseResult<Stmt> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, _) = require!(keyword_immediate(i, b"for"));
    let (i, _) = require!(multispace(i));
    let (i, vars) = require!(delimited_at_least_one(i,
        ident,
        |i| chain!(i,
            |i| opt(i, multispace) =>
            |i| byte(i, b','))));
    let (i, _) = require!(multispace(i));
    let (i, _) = require!(keyword_immediate(i, b"along"));
    // after this point, cut on error
    let (i, _) = require_or_cut!(multispace(i));
    let (i, along) = require_or_cut!(expr(i) => ParseError::ExpectedExpr);
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b'{'));
    let (i, body) = require_or_cut!(many(i, stmt));
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b'}'));
    let (i, end_pos) = require_or_cut!(pos(i));
    ok!(i, Stmt {
        data: StmtKind::ForAlong {
            vars,
            along,
            body,
        },
        loc: SrcLoc::raw(start_pos, end_pos - start_pos),
    })
}

fn forloop(input: &[u8]) -> ParseResult<Stmt> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, _) = require!(keyword_immediate(i, b"for"));
    let (i, _) = require!(multispace(i));
    // after this point, cut on error
    let (i, var) = require_or_cut!(forvardecl(i));
    let (i, spec) = require_or_cut!(alt!(i,
        for_range(i) => |(from, to, step)| ForSpec::Range(from, to, step)
      ; for_each(i) => ForSpec::Each
    ) => ParseError::ExpectedForSpecifier);
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b'{'));
    let (i, body) = require_or_cut!(many(i, stmt));
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b'}'));
    let (i, end_pos) = require_or_cut!(pos(i));
    ok!(i, Stmt {
        data: StmtKind::ForLoop {
            var,
            spec,
            body,
        },
        loc: SrcLoc::raw(start_pos, end_pos - start_pos),
    })
}

#[inline]
fn forvardecl(input: &[u8]) -> ParseResult<(Ident, Type, ParamMode)> {
    let (i, name) = require!(ident(input) => ParseError::ExpectedIdent);
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require!(byte(i, b':'));
    let (i, byref) = require!(opt!(chain!(i,
        |i| multispace(i) =>
        |i| byte(i, b'&'))));
    let (i, ty) = require!(typename(i) => ParseError::ExpectedTypename);
    ok!(i,
        (name, ty, byref.map(|_| ParamMode::ByRef).unwrap_or(ParamMode::ByVal)))
}

#[inline]
fn for_range(input: &[u8]) -> ParseResult<(Expr, Expr, Option<Expr>)> {
    let (i, _) = opt(input, multispace)?;
    let (i, _) = require!(byte(i, b'='));
    // we can cut on error after this point
    let (i, first) = require_or_cut!(expr(i) => ParseError::ExpectedExpr);
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b':'));
    let (i, last) = require_or_cut!(expr(i) => ParseError::ExpectedExpr);
    let (i, step) = require!(opt!(chain!(i,
        |i| opt(i, multispace) =>
        |i| byte(i, b':') =>
        |i| cut_if_err!(expr(i) => ParseError::ExpectedExpr))));
    ok!(i, (first, last, step))
}

#[inline]
fn for_each(input: &[u8]) -> ParseResult<Expr> {
    let (i, _) = require!(keyword(input, b"in"));
    let (i, _) = require!(multispace(i));
    cut_if_err!(expr(i) => ParseError::ExpectedExpr)
}

// a note on the syntax:
// yeah, the thing <- alloc [...] "placement" syntax sucks, but:
//   1) it avoided an Extremely Vexing Parse (alloc arr[...]; looks
//     like `alloc indexing-expr` not `alloc name-expr bounds`)
//   2) I dunno maybe we'll use it for something else later?

fn alloc(input: &[u8]) -> ParseResult<Stmt> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, array) = require!(expr(i));
    let (i, _) = require!(keyword(i, b"<-"));
    let (i, _) = require!(keyword(i, b"alloc"));
    // after here, we should cut on error
    let (i, extents) = require_or_cut!(alloc_extents(i));
    let (i, _) = require_or_cut!(terminator(i));
    let (i, end_pos) = require!(pos(i));
    ok!(i, Stmt {
        data: StmtKind::Alloc(array, extents),
        loc: SrcLoc::raw(start_pos, end_pos - start_pos),
    })
}

fn realloc(input: &[u8]) -> ParseResult<Stmt> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, array) = require!(expr(i));
    let (i, _) = require!(keyword(i, b"<-"));
    let (i, _) = require!(keyword(i, b"realloc"));
    // after here, we should cut on error
    let (i, extents) = require_or_cut!(realloc_extents(i));
    let (i, _) = require_or_cut!(terminator(i));
    let (i, end_pos) = require!(pos(i));
    ok!(i, Stmt {
        data: StmtKind::ReAlloc(array, extents.0, extents.1),
        loc: SrcLoc::raw(start_pos, end_pos - start_pos),
    })
}

fn alloc_extents(input: &[u8]) -> ParseResult<Vec<AllocExtent>> {
    let (i, _) = opt(input, multispace)?;
    let (i, _) = require!(byte(i, b'['));
    let (i, extents) = require!(delimited_at_least_one(i,
        dim_extent,
        |i| chain!(i,
            |i| opt(i, multispace) =>
            |i| byte(i, b','))
    ) => ParseError::ExpectedDimSpecifier);
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require!(byte(i, b']'));
    ok!(i, extents)
}

fn realloc_extents(input: &[u8]) -> ParseResult<(usize, AllocExtent)> {
    let (i, _) = opt(input, multispace)?;
    let (i, _) = require!(byte(i, b'['));
    let (i, predims) = require!(many(i,
        |i| chain!(i,
            |i| opt(i, multispace) =>
            |i| byte(i, b','))));
    let (i, extent) = require!(dim_extent(i)
      => ParseError::ExpectedDimSpecifier);
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require!(byte(i, b']'));
    ok!(i, (predims.len(), extent))
}

#[inline]
fn dim_extent(input: &[u8]) -> ParseResult<AllocExtent> {
    alt!(input,
        range_extent(input)
      ; along_extent(input)
    )
}

#[inline]
fn range_extent(input: &[u8]) -> ParseResult<AllocExtent> {
    let (i, lb) = require!(opt(input, |i| {
        let (i, e) = require!(expr(i));
        let (i, _) = opt(i, multispace)?;
        let (i, _) = require!(byte(i, b':'));
        ok!(i, e)
    }));
    let (i, ub) = require!(expr(i));
    ok!(i, AllocExtent::Range(lb, ub))
}

fn along_extent(input: &[u8]) -> ParseResult<AllocExtent> {
    let (i, _) = require!(keyword(input, b"along"));
    let (i, _) = require_or_cut!(multispace(i));
    let (i, e) = require_or_cut!(expr(i) => ParseError::ExpectedExpr);
    ok!(i, AllocExtent::Along(e))
}

fn dealloc(input: &[u8]) -> ParseResult<Stmt> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, _) = require!(keyword_immediate(i, b"dealloc"));
    let (i, _) = require!(multispace(i));
    // cut on error after this point
    let (i, e) = require_or_cut!(expr(i) => ParseError::ExpectedExpr);
    let (i, _) = require_or_cut!(terminator(i));
    let (i, end_pos) = require!(pos(i));
    ok!(i, Stmt {
        data: StmtKind::DeAlloc(e),
        loc: SrcLoc::raw(start_pos, end_pos - start_pos),
    })
}

#[inline]
fn assignment(input: &[u8]) -> ParseResult<Stmt> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, e1) = require!(expr(i));
    let (i, op) = require!(assign_op(i));
    // cut on error after here
    let (i, e2) = require_or_cut!(expr(i) => ParseError::ExpectedExpr);
    let (i, _) = require_or_cut!(terminator(i));
    let (i, end_pos) = require_or_cut!(pos(i));
    ok!(i, Stmt {
        data: StmtKind::Assign(e1, op, e2),
        loc: SrcLoc::raw(start_pos, end_pos - start_pos),
    })
}

#[inline]
pub fn exprstmt(input: &[u8]) -> ParseResult<Stmt> {
    let (i, e) = require!(expr(input));
    let (i, _) = require_or_cut!(terminator(i));
    let loc = e.loc.clone();
    ok!(i, Stmt {
        data: StmtKind::ExprStmt(e),
        loc,
    })
}

#[inline]
fn terminator(input: &[u8]) -> ParseResult<()> {
    let (i, _) = opt(input, multispace)?;
    let (i, _) = require!(byte(i, b';'));
    ok!(i, ())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_stmts() {
        expect_parse!(stmt(b"print f(x);") => Stmt {
            data: StmtKind::Print(_),
            ..
        });

        expect_parse!(stmt(b"\nprint f(x), x[17], \"bobby\";") => Stmt {
            data: StmtKind::Print(_),
            ..
        });

        expect_parse!(stmt(b"printf(x);") => Stmt {
            data: StmtKind::ExprStmt(Expr {
                data: ExprKind::Call(_, _, _),
                ..
            }),
            ..
        });

        expect_parse!(stmt(b"xs<-alloc [ 10 ];") => Stmt {
            data: StmtKind::Alloc(_, _),
            ..
        });

        expect_parse!(stmt(b"xs <- alloc[1, 17:23,9,x:f(90)];") => Stmt {
            data: StmtKind::Alloc(_, _),
            ..
        });

        expect_parse!(stmt(b"xs <- realloc [1];") => Stmt {
            data: StmtKind::ReAlloc(_, 0, _),
            ..
        });

        expect_parse!(stmt(b"xs <- realloc [, , , x[1]:y[f.x(9)]];") => Stmt {
            data: StmtKind::ReAlloc(_, 3, _),
            ..
        });

        expect_parse!(stmt(b" let x: i32 = 17;") => Stmt {
            data: StmtKind::VarDecl(_), .. });

        expect_parse!(stmt(b" let x: i32, y: i32[,,,] , z: some::ty;") => Stmt {
            data: StmtKind::VarDecl(_), .. });

        expect_parse!(stmt(b" x::y[17] *= f(32);") => Stmt {
            data: StmtKind::Assign(_, AssignOp::MulAssign, _), .. });

        expect_parse!(stmt(b"\
            if x > y { \
                x += 17.32 * x < 7 ? 3 : 5; \
            }") => Stmt {
            data: StmtKind::IfStmt { .. }, .. });

        expect_parse!(stmt(b"\
            if x > y { \
            } else if true{ \
                print \"wahtever\" ;
            }") => Stmt {
            data: StmtKind::IfStmt { .. }, .. });

        expect_parse!(stmt(b"\
            if x > y { \
            } else { \
            }") => Stmt {
            data: StmtKind::IfStmt { .. }, .. });

        expect_parse!(stmt(b"\
            if x > y { \
            } else if false || 17 {\
                return;\
                return;\
                return;\
            } else { \
                return;\
            }") => Stmt {
            data: StmtKind::IfStmt { .. }, .. });

        expect_parse!(stmt(b" while 1 { return 17; }") =>
          Stmt { data: StmtKind::WhileLoop{ .. }, .. });

        expect_parse!(stmt(b" for x: i32 = 1:10 { print x; }") => Stmt {
            data: StmtKind::ForLoop {
                spec: ForSpec::Range(_, _, _),
                ..
            },
            ..
        });

        expect_parse!(stmt(b" for x: i32 in xs { print x; }") => Stmt {
            data: StmtKind::ForLoop {
                spec: ForSpec::Each(_),
                ..
            },
            ..
        });

        expect_parse!(stmt(b" for x: &something in xs { print x; }") => Stmt {
            data: StmtKind::ForLoop {
                spec: ForSpec::Each(_),
                ..
            },
            ..
        });

        expect_parse!(stmt(b" for x,y,z along arr { print arr[x, y, z]; }") =>
          Stmt {
              data: StmtKind::ForAlong {
                  ..
              },
              ..
          });

        expect_parse_cut!(stmt(b" x::y[17] += ;") => ParseError::ExpectedExpr);

        expect_parse_cut!(stmt(b"let x = 17;") => ParseError::ExpectedIdent);

        expect_parse_cut!(stmt(b" let x: i32 = 17, y = 9;") =>
            ParseError::ExpectedByte(b';'));

        expect_parse_cut!(stmt(b" let x: i32 = 17, y: 23 = 9;") =>
            ParseError::ExpectedTypename);

        expect_parse_cut!(stmt(b" print f(x[])") => ParseError::ExpectedExpr);

        expect_parse_cut!(stmt(b"xs <- alloc[:::];") =>
          ParseError::ExpectedDimSpecifier);

        expect_parse_cut!(stmt(b"\
            if x > y { \
            } else if {\
                return;\
                return;\
                return;\
            } else if false || 17 { \
                return;\
            }") => ParseError::ExpectedExpr);

        expect_parse_cut!(stmt(b" for x in xs { print x; }") => 
          ParseError::ExpectedByte(b':'));

        expect_parse_cut!(stmt(b" for x: i32 = ! { print x; }") => 
          ParseError::ExpectedExpr);

        expect_parse_cut!(stmt(b" for x, y, z along { print x; }") => 
          ParseError::ExpectedExpr);
    }
}
