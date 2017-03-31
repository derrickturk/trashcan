use super::{ParseError, ParseResult};
#[macro_use]
use super::bits::*;

use ast::*;

fn literal_null(input: &[u8]) -> ParseResult<Literal> {
    alt!(input,
        keyword(input, b"nullptr") => |_| Literal::NullPtr
      ; keyword(input, b"nullvar") => |_| Literal::NullVar
      ; keyword(input, b"emptyvar") => |_| Literal::EmptyVar
    )
}

fn literal_bool(input: &[u8]) -> ParseResult<Literal> {
    alt!(input,
        keyword(input, b"true") => |_| Literal::Bool(true)
      ; keyword(input, b"false") => |_| Literal::Bool(false)
    )
}

fn literal_int(input: &[u8]) -> ParseResult<Literal> {
    let (input, _) = opt(input, multispace)?;
    let (num, digits) = require!(digits(input));

    let (input, tag) = require!(opt!(alt!(input,
        keyword_immediate(input, b"u8")
    )));

    Ok((input, Ok(match tag {
        None => Literal::Int32(17),
        Some(b"u8") => Literal::UInt8(17),
        _ => panic!("reeeeeeeeeeeeeeeeeeeeeeeee")
    })))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_nulls() {
        expect_parse!(literal_null(b"nullptr") => Literal::NullPtr);
        expect_parse!(literal_null(b" nullvar") => Literal::NullVar);
        expect_parse!(literal_null(b"\nemptyvar") => Literal::EmptyVar);
    }

    #[test]
    fn parse_bools() {
        expect_parse!(literal_bool(b"   true") => Literal::Bool(true));
        expect_parse!(literal_bool(b"false") => Literal::Bool(false));
        expect_parse_err!(literal_bool(b"fake") => _);
    }
}
