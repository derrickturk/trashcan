//! trashcan's parser and affiliated types

#[derive(Clone)]
pub struct SrcLoc {
    pub file: String,
    pub line: u32,
    pub start: u32,
    pub len: u32,
}

use ast::*;

/*

named!(dumpster(&[u8]) -> Dumpster, map!(
    many1!(module),
    |mods| Dumpster { modules: mods }
));

// no idea WTF this is blowing up on
named!(module(&[u8]) -> Module, ws!(do_parse!(
            tag!("mod") >>
      name: ident >>
  contents: delimited!(char!('{'), tag!("meat"), char!('}')) >>
            unimplemented!()
            )));

// named!(ident(&[u8]) -> Ident, 

*/

named!(bin_op(&[u8]) -> BinOp, alt_complete!(
    map!(one_of!("+-*/%^@<>&|"), |c| match c {
        '+' => BinOp::Add,
        '-' => BinOp::Sub,
        '*' => BinOp::Mul,
        '/' => BinOp::Div,
        '%' => BinOp::Mod,
        '^' => BinOp::Pow,
        '@' => BinOp::StrCat,
        '<' => BinOp::Lt,
        '>' => BinOp::Gt,
        '&' => BinOp::BitAnd,
        '|' => BinOp::BitOr,
        _ => panic!("internal parser error")
    })
));

#[cfg(test)]
mod test {
    use super::*;
    use nom::IResult;

    #[test]
    fn parse_bin_op() {
        if let IResult::Done(_, BinOp::Add) = bin_op("+".as_bytes()) {
        } else {
            panic!("didn't parse BinOp::Add");
        }
    }
}
