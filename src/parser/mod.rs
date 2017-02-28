//! trashcan's parser and affiliated types

use std::str;
use nom::{self, IResult, ErrorKind};

use ast::*;

// for now...
macro_rules! empty_loc {
    () => {
        SrcLoc {
            file: String::new(),
            line: 0,
            start: 0,
            len: 0,
        }
    }
}

mod expr;
use self::expr::*;
mod ident;
use self::ident::*;
mod op;
use self::op::*;
mod lit;
use self::lit::*;

#[derive(Clone, Debug)]
pub struct SrcLoc {
    pub file: String,
    pub line: u32,
    pub start: u32,
    pub len: u32,
}

pub enum CustomErrors {
    KeywordAsIdent,
    InvalidEscape,
}

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

*/

named!(pub stmt<Stmt>, alt_complete!(
    decl
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
 decls: separated_list!(ws!(char!(',')), vardecl) >>
        terminator >>
        (Stmt {
            data: StmtKind::VarDecl(decls),
            loc: empty_loc!()
        })
)));

named!(vardecl<(Ident, Type, Option<Expr>)>, complete!(do_parse!(
  name: ident >>
        opt!(call!(nom::multispace)) >>
        char!(':') >>
    ty: typename >>
  init: varinit >>
        (name, ty, init)
)));

named!(varinit<Option<Expr>>, complete!(opt!(do_parse!(
    opt!(call!(nom::multispace)) >>
    char!('=') >>
 e: expr >>
    (e)
))));

named!(terminator<char>, complete!(preceded!(
    opt!(call!(nom::multispace)),
    char!(';')
)));

// TODO: do we need to pre-emptively tag idents that conflict with VB keywords?
//   forbid them?
//   prepend some goofy ©high-ASCII char?
// answer: yes, in codegen (not here)

#[cfg(test)]
mod test {
    use super::*;
    use nom::{self, IResult, ErrorKind};

    #[test]
    fn parse_literal_strs() {
        let s = "it is only fitting that\ttabs are embedded here.\n";
        let lit_s = format!("\"{}\"", s);

        if let IResult::Done(_, Literal::String(l)) = literal(lit_s.as_bytes()) {
            assert_eq!(s, l);
        } else {
            panic!("failed to parse literal string.");
        }

        let res = literal(b"\"an invalid \\xescape sequence\\\"");
        assert!(res.is_err());
    }

    #[test]
    fn parse_literal_ints() {
        if let IResult::Done(_, Literal::UInt8(17u8)) = literal("17u8".as_bytes()) {
        } else {
            panic!("didn't parse literal 17u8");
        }

        match literal("12345".as_bytes()) {
            IResult::Done(_, Literal::Int32(12345)) => { },
            res => panic!("didn't parse literal 12345: {:?}", res)
        }

        match literal("12345u8".as_bytes()) {
            IResult::Done(_, _) => panic!("parsed 12345u8"),
            _ => { }
        }
    }

    #[test]
    fn parse_literal_floats() {
        match literal("1.".as_bytes()) {
            IResult::Done(_, Literal::Float64(1.0)) => { },
            res => panic!("didn't parse literal 1.: {:?}", res),
        }

        match literal("1.3f32".as_bytes()) {
            IResult::Done(_, Literal::Float32(1.3f32)) => { },
            _ => panic!("didn't parse literal 1.3f32")
        }
    }

    #[test]
    fn parse_literal() {
        if let IResult::Done(_, Literal::Bool(true)) = literal("true".as_bytes()) {
        } else {
            panic!("didn't parse literal true");
        }

        let res = literal("not!good".as_bytes());
        assert!(res.is_err());
    }

    #[test]
    fn parse_assign_ops() {
        if let IResult::Done(_, AssignOp::Assign) = assign_op("=".as_bytes()) {
        } else {
            panic!("didn't parse AssignOp::Assign");
        }

        if let IResult::Done(_, AssignOp::BitAndAssign) = assign_op("&=".as_bytes()) {
        } else {
            panic!("didn't parse AssignOp::BitAndAssign");
        }

        assert!(assign_op("xx=".as_bytes()).is_err());
    }

    #[test]
    fn parse_un_ops() {
        if let IResult::Done(_, UnOp::Negate) = un_op("-".as_bytes()) {
        } else {
            panic!("didn't parse UnOp::Negate");
        }

        assert!(un_op("x".as_bytes()).is_err());
    }

    #[test]
    fn parse_bin_ops() {
        if let IResult::Done(_, BinOp::Add) = addsub_op("+".as_bytes()) {
        } else {
            panic!("didn't parse BinOp::Add");
        }

        if let IResult::Done(_, BinOp::BitAnd) = bitand_op("&".as_bytes()) {
        } else {
            panic!("didn't parse BinOp::BitAnd");
        }

        if let IResult::Done(_, BinOp::LogAnd) = logand_op("&&".as_bytes()) {
        } else {
            panic!("didn't parse BinOp::LogAnd");
        }
    }

    #[test]
    fn parse_ident() {
        let res = ident("_abcdef".as_bytes());
        assert!(res.is_err());

        match ident("a_23".as_bytes()) {
            IResult::Done(_, Ident(s)) => assert_eq!(s, "a_23"),
            _ => panic!("couldn't parse ident")
        }

        match ident("this".as_bytes()) {
            IResult::Done(_, Ident(s)) => assert_eq!(s, "this"),
            _ => panic!("couldn't parse 'this' as ident")
        }

        match ident(b"for") {
            IResult::Error(ErrorKind::Custom(e))
                if e == CustomErrors::KeywordAsIdent as u32 => { },
            res => panic!("didn't fail keyword-as-ident: {:?}, res")
        }

        match ident("fortuna".as_bytes()) {
            IResult::Done(_, Ident(s)) => assert_eq!(s, "fortuna"),
            _ => panic!("couldn't parse ident")
        }
    }

    #[test]
    fn parse_type() {
        let res = typename("_abcdef".as_bytes());
        assert!(res.is_err());

        match typename("boogaloo".as_bytes()) {
            IResult::Done(_, Type::Deferred(Ident(s))) => {
                assert!(s == "boogaloo")
            },
            _ => panic!("couldn't parse deferred-ident type")
        }

        match typename("i32".as_bytes()) {
            IResult::Done(_, Type::Int32) => { },
            _ => panic!("couldn't parse i32")
        }
    }

    #[test]
    fn parse_path() {
        let res = path("_this::wont::even::start".as_bytes());
        assert!(res.is_err());

        match path("ident".as_bytes()) {
            IResult::Done(_, Path(vec)) => assert_eq!(vec.len(), 1),
            _ => panic!("didn't parse single-ident path")
        }

        match path("an_ident".as_bytes()) {
            IResult::Done(_, Path(vec)) => assert_eq!(vec.len(), 1),
            _ => panic!("didn't parse single-ident path")
        }

        match path("some_module::an_ident".as_bytes()) {
            IResult::Done(_, Path(vec)) => assert_eq!(vec.len(), 2),
            _ => panic!("didn't parse two-ident path")
        }

        match path("some_module::some::\nother ::  thing".as_bytes()) {
            IResult::Done(_, Path(vec)) => {
                for &Ident(ref s) in &vec {
                    println!("ident: {}", s);
                }
                assert_eq!(vec.len(), 4);
            }
            _ => panic!("didn't parse messy path")
        }
    }

    #[test]
    fn parse_expr() {
        let e = b"32.5";
        match expr(e) {
            IResult::Done(_, Expr { data: ExprKind::Lit(Literal::Float64(32.5)), loc: _ }) => { },
            res => panic!("didn't parse literal expr: {:?}", res)
        }

        let e = b"some::modules::array[23]";
        match expr(e) {
            IResult::Done(_, Expr { data: ExprKind::Index(e1, e2), loc: _ }) => {
                match *e1 {
                    Expr { data: ExprKind::Name(p), loc: _ } => { },
                    res => panic!("indexing expr: didn't parse e1 as path: {:?}", res)
                }

                match *e2 {
                    Expr { data: ExprKind::Lit(Literal::Int32(23)), loc: _ } => { },
                    res => panic!("indexing expr: didn't parse e2 as literal: {:?}", res)
                }
            },
            res => panic!("didn't parse indexing expr: {:?}", res)
        }

        let e = b"some::modules::array[some.other.array[23]]";
        assert!(expr(e).is_done());

        let e = b"some::fun(1, 2, x[2], other())";
        assert!(expr(e).is_done());

        let e = b"x ? f(23) : y[17]";
        assert!(expr(e).is_done());

        let e = b"!f(2) ? f(~23) : y[17]";
        assert!(expr(e).is_done());

        let e = b"!(f(2) ? f(~23) : y[17])";
        assert!(expr(e).is_done());

        let e = b"2 ^ 3";
        assert!(expr(e).is_done());

        let e = b"(2 + 3 * 7 && f(9) | ~x[17]) @ \"bob\"";
        assert!(expr(e).is_done());

        let e = b"f(17).x + some_mod::f(23).foo(99)";
        assert!(expr(e).is_done());
    }

    #[test]
    fn parse_stmt() {
        let s = b"f(17);"; 
        assert!(stmt(s).is_done());

        let s = b"let x: i32 = 17;"; 
        assert!(stmt(s).is_done());

        let s = b"let y: unknown, x: i32 = 17;"; 
        assert!(stmt(s).is_done());

        let s = b"lety: unknown, x: i32 = 17;"; 
        assert!(stmt(s).is_err());

        let s = b"17"; 
        assert!(stmt(s).is_err());
    }
}
