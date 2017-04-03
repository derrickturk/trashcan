//! trashcan's sub-parsers for operators

use super::{ParseError, ParseResult};
#[macro_use]
use super::bits::*;

use ast::*;

#[inline]
pub fn assign_op(input: &[u8]) -> ParseResult<AssignOp> {
    let (i, _) = opt(input, multispace)?;
    alt!(i,
        byte(i, b'=') => |_| AssignOp::Assign
      ; keyword_immediate(i, b"+=") => |_| AssignOp::AddAssign
      ; keyword_immediate(i, b"-=") => |_| AssignOp::SubAssign
      ; keyword_immediate(i, b"*=") => |_| AssignOp::MulAssign
      ; keyword_immediate(i, b"/=") => |_| AssignOp::DivAssign
      ; keyword_immediate(i, b"%=") => |_| AssignOp::ModAssign
      ; keyword_immediate(i, b"^=") => |_| AssignOp::PowAssign
      ; keyword_immediate(i, b"@=") => |_| AssignOp::StrCatAssign
      ; keyword_immediate(i, b"&=") => |_| AssignOp::BitAndAssign
      ; keyword_immediate(i, b"|=") => |_| AssignOp::BitOrAssign
      ; keyword_immediate(i, b"&&=") => |_| AssignOp::LogAndAssign
      ; keyword_immediate(i, b"||=") => |_| AssignOp::LogOrAssign
    )
}

#[inline]
pub fn un_op(input: &[u8]) -> ParseResult<UnOp> {
    let (i, _) = opt(input, multispace)?;
    alt!(i,
        byte(i, b'-') => |_| UnOp::Negate
      ; byte(i, b'~') => |_| UnOp::BitNot
      ; byte(i, b'!') => |_| UnOp::LogNot
      ; byte(i, b'&') => |_| UnOp::AddressOf
    )
}

#[inline]
pub fn pow_op(input: &[u8]) -> ParseResult<BinOp> {
    let (i, _) = opt(input, multispace)?;
    let (i, _) = require!(byte(i, b'^'));
    ok!(i, BinOp::Pow)
}

#[inline]
pub fn muldiv_op(input: &[u8]) -> ParseResult<BinOp> {
    let (i, _) = opt(input, multispace)?;
    alt!(i,
        byte(i, b'*') => |_| BinOp::Mul
      ; byte(i, b'/') => |_| BinOp::Div
      ; byte(i, b'%') => |_| BinOp::Mod
    )
}

#[inline]
pub fn addsub_op(input: &[u8]) -> ParseResult<BinOp> {
    let (i, _) = opt(input, multispace)?;
    alt!(i,
        byte(i, b'+') => |_| BinOp::Add
      ; byte(i, b'-') => |_| BinOp::Sub
      ; byte(i, b'@') => |_| BinOp::StrCat
    )
}

#[inline]
pub fn cmp_op(input: &[u8]) -> ParseResult<BinOp> {
    let (i, _) = opt(input, multispace)?;
    alt!(i,
        keyword_immediate(i, b"<=") => |_| BinOp::LtEq
      ; keyword_immediate(i, b">=") => |_| BinOp::GtEq
      ; match byte(i, b'<')? {
            (i, Ok(_)) => {
                // disambiguate from <- "operator" by lookahead
                if let Some(&b'-') = i.first() {
                        err!(i, ParseError::LookAhead)
                    } else {
                        ok!(i, BinOp::Lt)
                    }
            },

            (_, Err(e)) => err!(i, e),
        }
      ; byte(i, b'>') => |_| BinOp::Gt
    )
}

#[inline]
pub fn eq_op(input: &[u8]) -> ParseResult<BinOp> {
    let (i, _) = opt(input, multispace)?;
    alt!(i,
        keyword_immediate(i, b"===") => |_| BinOp::IdentEq
      ; keyword_immediate(i, b"!==") => |_| BinOp::NotIdentEq
      ; keyword_immediate(i, b"==") => |_| BinOp::Eq
      ; keyword_immediate(i, b"!=") => |_| BinOp::NotEq
    )
}

#[inline]
pub fn bitand_op(input: &[u8]) -> ParseResult<BinOp> {
    let (i, _) = opt(input, multispace)?;
    let (i, _) = require!(byte(i, b'&'));
    // disambiguate from && operator by lookahead
    if let Some(&b'&') = i.first() {
        err!(input, ParseError::LookAhead)
    } else {
        ok!(i, BinOp::BitAnd)
    }
}

#[inline]
pub fn bitor_op(input: &[u8]) -> ParseResult<BinOp> {
    let (i, _) = opt(input, multispace)?;
    let (i, _) = require!(byte(i, b'|'));
    // disambiguate from && operator by lookahead
    if let Some(&b'|') = i.first() {
        err!(input, ParseError::LookAhead)
    } else {
        ok!(i, BinOp::BitOr)
    }
}

#[inline]
pub fn logand_op(input: &[u8]) -> ParseResult<BinOp> {
    let (i, _) = opt(input, multispace)?;
    let (i, _) = require!(keyword_immediate(i, b"&&"));
    ok!(i, BinOp::LogAnd)
}

#[inline]
pub fn logor_op(input: &[u8]) -> ParseResult<BinOp> {
    let (i, _) = opt(input, multispace)?;
    let (i, _) = require!(keyword_immediate(i, b"||"));
    ok!(i, BinOp::LogOr)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_assign_ops() {
        expect_parse!(assign_op(b" =") => AssignOp::Assign);
        expect_parse!(assign_op(b"&&=") => AssignOp::LogAndAssign);
        expect_parse_err!(assign_op(b"**=") => _);
    }

    #[test]
    fn parse_un_ops() {
        expect_parse!(un_op(b"&") => UnOp::AddressOf);
        expect_parse_err!(un_op(b"x") => _);
    }

    #[test]
    fn parse_bin_ops() {
        expect_parse!(pow_op(b" ^") => BinOp::Pow);
        expect_parse!(muldiv_op(b"*") => BinOp::Mul);
        expect_parse!(addsub_op(b"\t\t@") => BinOp::StrCat);
        expect_parse!(cmp_op(b">") => BinOp::Gt);
        expect_parse!(cmp_op(b"<=") => BinOp::LtEq);
        expect_parse!(eq_op(b"===") => BinOp::IdentEq);
        expect_parse!(bitand_op(b"&") => BinOp::BitAnd);
        expect_parse!(bitor_op(b"|") => BinOp::BitOr);
        expect_parse!(logand_op(b"  &&") => BinOp::LogAnd);
        expect_parse!(logor_op(b"  ||") => BinOp::LogOr);
        expect_parse_err!(pow_op(b"xx") => _);
    }
}
