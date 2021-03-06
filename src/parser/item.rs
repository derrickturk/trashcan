//! trashcan's sub-parsers for items

use super::{ParseErrorKind, CutParseResult, SrcLoc};
use super::bits::*;
use super::lit::*;
use super::ident::*;
use super::stmt::*;

use ast::*;

pub fn normal_item(input: &[u8]) -> CutParseResult<NormalItem> {
    alt!(input,
        fundef(input) => NormalItem::Function
      ; structdef(input) => NormalItem::Struct
      ; staticdef(input) => NormalItem::Static
      ; constantdef(input) => NormalItem::Const
    )
}

pub fn fundef(input: &[u8]) -> CutParseResult<FunDef> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, access) = require!(access(i));
    let (i, _) = require!(keyword_immediate(i, b"fn"));
    let (i, _) = require!(multispace(i));

    // cut on error after this point
    let (i, name) = require_or_cut!(ident(i) => ParseErrorKind::ExpectedIdent);

    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b'('));

    let (i, params) = require_or_cut!(delimited(i,
        fnparam,
        |i| chain!(i,
            |i| opt(i, multispace) =>
            |i| byte(i, b',')
        )));

    let (i, _) = opt(i, multispace)?;
    let (i, optparams) = require_or_cut!(opt!(optparams(i)));
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b')'));
    let (i, ret) = require!(opt!(i, fnret(i)));

    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b'{'));
    let (i, body) = require_or_cut!(many(i, stmt));
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b'}'));
    let (i, end_pos) = require!(pos(i));
    ok!(i, FunDef {
        name,
        access,
        params,
        optparams,
        ret: ret.unwrap_or(Type::Void),
        body,
        loc: SrcLoc::raw(start_pos, end_pos - start_pos),
    })
}

pub fn structdef(input: &[u8]) -> CutParseResult<StructDef> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, access) = require!(access(i));
    let (i, _) = require!(keyword_immediate(i, b"struct"));
    let (i, _) = require!(multispace(i));
    // cut on error after this point
    let (i, name) = require_or_cut!(ident(i) => ParseErrorKind::ExpectedIdent);
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b'{'));
    let (i, members) = require_or_cut!(delimited_at_least_one(i,
        structmem,
        |i| chain!(i,
            |i| opt(i, multispace) =>
            |i| byte(i, b',')
        )));
    let (i, _) = opt(i, multispace)?;
    let (i, _) = opt!(byte(i, b','))?;
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b'}'));
    let (i, end_pos) = require_or_cut!(pos(i));
    ok!(i, StructDef {
        name,
        access,
        members,
        loc: SrcLoc::raw(start_pos, end_pos - start_pos),
    })
}

pub fn staticdef(input: &[u8]) -> CutParseResult<Static> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, access) = require!(access(i));
    let (i, _) = require!(keyword_immediate(i, b"static"));
    let (i, _) = require!(multispace(i));
    // cut on error after this point
    let (i, name) = require_or_cut!(ident(i) => ParseErrorKind::ExpectedIdent);
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b':'));
    let (i, ty) = require_or_cut!(typename(i) =>
      ParseErrorKind::ExpectedTypename);
    let (i, init) = require_or_cut!(opt!(chain!(i,
        |i| opt(i, multispace) =>
        |i| byte(i, b'=') =>
        |i| opt(i, multispace) =>
        |i| cut_if_err!(literal(i) => ParseErrorKind::ExpectedLiteral)
    )));
    let (i, _) = require_or_cut!(terminator(i));
    let (i, end_pos) = require_or_cut!(pos(i));
    ok!(i, Static {
        name,
        access,
        ty,
        init,
        loc: SrcLoc::raw(start_pos, end_pos - start_pos),
    })
}

pub fn constantdef(input: &[u8]) -> CutParseResult<Constant> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, access) = require!(access(i));
    let (i, _) = require!(keyword_immediate(i, b"const"));
    let (i, _) = require!(multispace(i));
    // cut on error after this point
    let (i, name) = require_or_cut!(ident(i) => ParseErrorKind::ExpectedIdent);
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b':'));
    let (i, ty) = require_or_cut!(typename(i) =>
      ParseErrorKind::ExpectedTypename);
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b'='));
    let (i, _) = opt(i, multispace)?;
    let (i, value) = require_or_cut!(literal(i) =>
      ParseErrorKind::ExpectedLiteral);
    let (i, _) = require_or_cut!(terminator(i));
    let (i, end_pos) = require_or_cut!(pos(i));
    ok!(i, Constant {
        name,
        access,
        ty,
        value,
        loc: SrcLoc::raw(start_pos, end_pos - start_pos),
    })
}

#[inline]
fn fnparam(input: &[u8]) -> CutParseResult<FunParam> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, name) = require!(ident(i) => ParseErrorKind::ExpectedIdent);
    // cut on error after this point
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b':'));
    let (i, byref) = require_or_cut!(opt!(chain!(i,
        |i| opt(i, multispace) =>
        |i| byte(i, b'&')
    )));
    let (i, ty) = require_or_cut!(typename(i) =>
      ParseErrorKind::ExpectedTypename);
    let (i, end_pos) = require!(pos(i));
    ok!(i, FunParam {
        name,
        ty,
        mode: match byref {
            Some(_) => ParamMode::ByRef,
            None => ParamMode::ByVal,
        },
        loc: SrcLoc::raw(start_pos, end_pos - start_pos),
    })
}

#[inline]
fn optparams(input: &[u8]) -> CutParseResult<FunOptParams> {
    let (i, _) = opt(input, multispace)?;
    let (i, _) = require!(byte(i, b';'));
    // cut on error below here
    cut_if_err!(alt!(i,
        varargs(i)
      ; delimited_at_least_one(i,
            optfnparam,
            |i| chain!(i,
                |i| opt(i, multispace) =>
                |i| byte(i, b',')
        )) => FunOptParams::Named
    ) => ParseErrorKind::ExpectedOptParams)
}

#[inline]
fn varargs(input: &[u8]) -> CutParseResult<FunOptParams> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, name) = require!(ident(i));
    let (i, _) = require!(keyword(i, b"..."));
    let (i, end_pos) = require!(pos(i));
    ok!(i, FunOptParams::VarArgs(
            name,
            SrcLoc::raw(start_pos, end_pos - start_pos)
    ))
}

#[inline]
fn optfnparam(input: &[u8]) -> CutParseResult<(FunParam, Literal)> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, name) = require!(ident(i) => ParseErrorKind::ExpectedIdent);
    // cut on error after this point
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b':'));
    let (i, byref) = require_or_cut!(opt!(chain!(i,
        |i| opt(i, multispace) =>
        |i| byte(i, b'&')
    )));
    let (i, ty) = require_or_cut!(typename(i) =>
      ParseErrorKind::ExpectedTypename);
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b'=') =>
      ParseErrorKind::ExpectedDefaultArgument);
    let (i, default) = require_or_cut!(literal(i) =>
      ParseErrorKind::ExpectedDefaultArgument);
    let (i, end_pos) = require!(pos(i));
    ok!(i, (
        FunParam {
            name,
            ty,
            mode: match byref {
                Some(_) => ParamMode::ByRef,
                None => ParamMode::ByVal,
            },
            loc: SrcLoc::raw(start_pos, end_pos - start_pos),
        },
        default
    ))
}

#[inline]
fn fnret(input: &[u8]) -> CutParseResult<Type> {
    let (i, _) = require!(keyword(input, b"->"));
    cut_if_err!(typename(i) => ParseErrorKind::ExpectedTypename)
}

#[inline]
fn structmem(input: &[u8]) -> CutParseResult<StructMem> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, name) = require!(ident(i) => ParseErrorKind::ExpectedIdent);
    // cut on error after this point
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b':'));
    let (i, ty) = require_or_cut!(typename(i) =>
      ParseErrorKind::ExpectedTypename);
    let (i, end_pos) = require_or_cut!(pos(i));
    ok!(i, StructMem {
        name,
        ty,
        loc: SrcLoc::raw(start_pos, end_pos - start_pos),
    })
}

#[inline]
fn access(input: &[u8]) -> CutParseResult<Access> {
    let (i, _) = opt(input, multispace)?;
    let (i, access) = require!(opt(i, |i| {
        let (i, _) = require!(keyword_immediate(i, b"pub"));
        let (i, _) = require!(multispace(i));
        ok!(i, Access::Public)
    }));
    ok!(i, access.unwrap_or(Access::Private))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_items() {
        expect_parse!(normal_item(b" struct x { y: f64, }") =>
          NormalItem::Struct(_));
        expect_parse!(normal_item(b" fn f() -> i32 { return 1; }") =>
          NormalItem::Function(_));
        expect_parse!(normal_item(b" fn f(;xs...) -> i32 { return 1; }") =>
          NormalItem::Function(_));
        expect_parse!(normal_item(b" static m: i32 = 7 ;") =>
          NormalItem::Static(_));
        expect_parse!(normal_item(b" const m: i32 = 7 ;") =>
          NormalItem::Const(_));
    }

    #[test]
    fn parse_fns() {
        expect_parse!(fundef(b" pub fn f() -> i32 { return 1; }") =>
          FunDef { .. });
        expect_parse!(fundef(b" fn f(x: i32, y: f64) -> f64[,,] { }") =>
          FunDef { .. });
        expect_parse!(fundef(b" fn f(x: i32, y: f64; z: i32 = 7) {}") =>
          FunDef { .. });
        expect_parse_cut!(fundef(b" fn f( x, y)") =>
          ParseErrorKind::ExpectedByte(b':'));
        expect_parse_cut!(fundef(b" fn f( x: !, y)") =>
          ParseErrorKind::ExpectedTypename);
        expect_parse_cut!(fundef(b" fn f( x: i32; y: i32)") =>
          ParseErrorKind::ExpectedDefaultArgument);
    }

    #[test]
    fn parse_ret() {
        expect_parse!(fnret(b" -> i32[,,]" ) => Type::Array(_, _));
        expect_parse_cut!(fnret(b" -> []" ) =>
          ParseErrorKind::ExpectedTypename);
    }

    #[test]
    fn parse_structs() {
        expect_parse!(structdef(b" struct whatever { x: i32 }") =>
          StructDef { .. });
        expect_parse!(structdef(b" pub struct whatever { x: i32 , y: f64[,],}") =>
          StructDef { .. });
        expect_parse_cut!(structdef(b"struct ! {x:i32}") =>
          ParseErrorKind::ExpectedIdent);
        expect_parse_cut!(structdef(b"struct y {!}") =>
          ParseErrorKind::ExpectedIdent);
        expect_parse_cut!(structdef(b"struct y {x : ! }") =>
          ParseErrorKind::ExpectedTypename);
    }

    #[test]
    fn parse_statics() {
        expect_parse!(staticdef(b" static x: destroyer;") => Static { .. });
        expect_parse!(staticdef(b" static m: i32 = 7 ;") => Static { .. });
        expect_parse_cut!(staticdef(b" static m: i32 = ! ;") =>
          ParseErrorKind::ExpectedLiteral);
    }

    #[test]
    fn parse_constants() {
        expect_parse!(constantdef(b" const m: i32 = 7 ;") => Constant { .. });
        expect_parse_cut!(constantdef(b" const m: i32 = ! ;") =>
          ParseErrorKind::ExpectedLiteral);
    }

    #[test]
    fn parse_access() {
        expect_parse!(access(b"  pub ") => Access::Public);
        expect_parse!(access(b"   ") => Access::Private);
    }
}
