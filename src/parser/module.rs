//! trashcan's sub-parsers for dumpsters and modules

use super::{ParseErrorKind, CutParseResult, SrcLoc};
#[macro_use]
use super::bits::*;
use super::ident::*;
use super::item::*;

use ast::*;

#[inline]
pub fn dumpster(input: &[u8]) -> CutParseResult<Dumpster> {
    let (i, modules) = require!(at_least_one(input, module) =>
      ParseErrorKind::ExpectedModule);
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require!(end_of_input(i));
    ok!(i, Dumpster { modules })
}

#[inline]
pub fn module(input: &[u8]) -> CutParseResult<Module> {
    alt!(input,
        normal_module(input)
    )
}

#[inline]
fn normal_module(input: &[u8]) -> CutParseResult<Module> {
    let (i, _) = opt(input, multispace)?;
    let (i, start_pos) = require!(pos(i));
    let (i, _) = require!(keyword_immediate(i, b"mod"));
    let (i, _) = require!(multispace(i));
    // cut on error after this point
    let (i, name) = require_or_cut!(ident(i) => ParseErrorKind::ExpectedIdent);
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b'{'));
    let (i, items) = require_or_cut!(many(i, normal_item));
    let (i, _) = opt(i, multispace)?;
    let (i, _) = require_or_cut!(byte(i, b'}'));
    let (i, end_pos) = require!(pos(i));
    ok!(i, Module {
        name,
        data: ModuleKind::Normal(items),
        loc: SrcLoc::raw(start_pos, end_pos - start_pos),
    })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_dumpsters() {
        expect_parse!(dumpster(b"mod m { struct x { y: i32 } } \
                                 mod z { fn f() { } }") =>
          Dumpster { .. });
        expect_parse_err!(dumpster(b"mod m {};") =>
          ParseErrorKind::InvalidTrailingContent);
    }

    #[test]
    fn parse_modules() {
        expect_parse!(module(b" mod m { }") =>
          Module { data: ModuleKind::Normal(_), .. });
        expect_parse!(module(b" mod m { fn f() { } struct x { y: i32 }}") =>
          Module { data: ModuleKind::Normal(_), .. });
        expect_parse_cut!(module(b"mod { fn f() { } }") =>
          ParseErrorKind::ExpectedIdent);
    }
}
