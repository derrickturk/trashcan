//! trashcan's sub-parsers for items

use super::{ParseError, ParseResult, SrcLoc};
#[macro_use]
use super::bits::*;
use super::lit::*;
use super::ident::*;
use super::stmt::*;

use ast::*;

pub fn normal_item(input: &[u8]) -> ParseResult<NormalItem> {
    alt!(input,
        fundef(input) => NormalItem::Function
      ; structdef(input) => NormalItem::Struct
    )
}

pub fn fundef(input: &[u8]) -> ParseResult<FunDef> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, access) = require!(access(i));
    let (i, _) = require!(keyword_immediate(i, b"fn"));
    let (i, _) = require!(multispace(i));

    // cut on error after this point
    let (i, name) = require_or_cut!(ident(i) => ParseError::ExpectedIdent);

    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b'('));

    let (i, params) = require_or_cut!(delimited(i,
        fnparam,
        |i| chain!(i,
            |i| opt(i, multispace) =>
            |i| byte(i, b',')
        )));

    let (i, _) = opt(i, multispace)?;
    let (i, optparams) = require!(opt!(chain!(i,
        |i| byte(i, b';') =>
        |i| delimited_at_least_one(i,
            optfnparam,
            |i| chain!(i,
                |i| opt(i, multispace) =>
                |i| byte(i, b',')
            )))));

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
        optparams: optparams.unwrap_or(Vec::new()),
        ret: ret.unwrap_or(Type::Void),
        body,
        loc: SrcLoc::raw(start_pos, end_pos - start_pos),
    })
}

pub fn structdef(input: &[u8]) -> ParseResult<StructDef> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, access) = require!(access(i));
    let (i, _) = require!(keyword_immediate(i, b"struct"));
    let (i, _) = require!(multispace(i));
    // cut on error after this point
    let (i, name) = require_or_cut!(ident(i) => ParseError::ExpectedIdent);
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

#[inline]
fn fnparam(input: &[u8]) -> ParseResult<FunParam> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, name) = require!(ident(i) => ParseError::ExpectedIdent);
    // cut on error after this point
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b':'));
    let (i, byref) = require_or_cut!(opt!(chain!(i,
        |i| opt(i, multispace) =>
        |i| byte(i, b'&')
    )));
    let (i, ty) = require_or_cut!(typename(i) => ParseError::ExpectedTypename);
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
fn optfnparam(input: &[u8]) -> ParseResult<(FunParam, Literal)> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, name) = require!(ident(i) => ParseError::ExpectedIdent);
    // cut on error after this point
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b':'));
    let (i, byref) = require_or_cut!(opt!(chain!(i,
        |i| opt(i, multispace) =>
        |i| byte(i, b'&')
    )));
    let (i, ty) = require_or_cut!(typename(i) => ParseError::ExpectedTypename);
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b'=') =>
      ParseError::ExpectedDefaultArgument);
    let (i, default) = require_or_cut!(literal(i) =>
      ParseError::ExpectedDefaultArgument);
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
fn fnret(input: &[u8]) -> ParseResult<Type> {
    let (i, _) = require!(keyword(input, b"->"));
    cut_if_err!(typename(i) => ParseError::ExpectedTypename)
}

#[inline]
fn structmem(input: &[u8]) -> ParseResult<StructMem> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, name) = require!(ident(i) => ParseError::ExpectedIdent);
    // cut on error after this point
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b':'));
    let (i, ty) = require_or_cut!(typename(i) => ParseError::ExpectedTypename);
    let (i, end_pos) = require_or_cut!(pos(i));
    ok!(i, StructMem {
        name,
        ty,
        loc: SrcLoc::raw(start_pos, end_pos - start_pos),
    })
}

#[inline]
fn access(input: &[u8]) -> ParseResult<Access> {
    let (i, _) = opt(input, multispace)?;
    let (i, access) = require!(opt(i, |i| {
        let (i, access) = require!(keyword_immediate(i, b"pub"));
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
          ParseError::ExpectedByte(b':'));
        expect_parse_cut!(fundef(b" fn f( x: !, y)") =>
          ParseError::ExpectedTypename);
        expect_parse_cut!(fundef(b" fn f( x: i32; y: i32)") =>
          ParseError::ExpectedDefaultArgument);
    }

    #[test]
    fn parse_ret() {
        expect_parse!(fnret(b" -> i32[,,]" ) => Type::Array(_, _));
        expect_parse_cut!(fnret(b" -> []" ) => ParseError::ExpectedTypename);
    }

    #[test]
    fn parse_structs() {
        expect_parse!(structdef(b" struct whatever { x: i32 }") =>
          StructDef { .. });
        expect_parse!(structdef(b" pub struct whatever { x: i32 , y: f64[,],}") =>
          StructDef { .. });
        expect_parse_cut!(structdef(b"struct ! {x:i32}") =>
          ParseError::ExpectedIdent);
        expect_parse_cut!(structdef(b"struct y {!}") =>
          ParseError::ExpectedIdent);
        expect_parse_cut!(structdef(b"struct y {x : ! }") =>
          ParseError::ExpectedTypename);
    }

    #[test]
    fn parse_access() {
        expect_parse!(access(b"  pub ") => Access::Public);
        expect_parse!(access(b"   ") => Access::Private);
    }
}
