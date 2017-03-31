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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_nulls() {
        expect_parse!(literal_null(b"nullptr") => Literal::NullPtr);
        expect_parse!(literal_null(b" nullvar") => Literal::NullVar);
        expect_parse!(literal_null(b"\nemptyvar") => Literal::EmptyVar);
    }
}
