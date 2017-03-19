//! trashcan's parser and affiliated types

use nom;

use ast::*;

pub enum CustomErrors {
    KeywordAsIdent,
    InvalidEscape,
    InvalidTrailingContent,
}

macro_rules! expect_parse {
    ($e:expr ; $i:ident => $p:pat) => {
        match $i($e) {
            nom::IResult::Done(s, $p) => { assert_eq!(s.len(), 0) },
            r => panic!("{:?}", r),
        }
    }
}

mod item;
mod ident;
mod stmt;
mod expr;
mod op;
mod lit;
mod srcloc;

use self::item::*;
use self::ident::*;
// TODO: for now (pending error conversion)
pub use self::srcloc::*;

struct MappedSource {
    // processed source for parser
    src: Vec<u8>,
    // inclusive
    gaps: Vec<(usize, usize)>,
    // line beginnings
    lines: Vec<usize>,
}

pub fn parse_dumpster(src: &[u8]) -> Result<Dumpster, nom::ErrorKind> {
    let map = strip_comments(src);
    match dumpster(&map.src) {
        nom::IResult::Done(rest, dumpster) => {
            if rest.len() == 0 {
                Ok(rebase_srclocs(dumpster, map.src.as_ptr() as usize))
            } else {
                Err(nom::ErrorKind::Custom(
                        CustomErrors::InvalidTrailingContent as u32))
            }
        },

        nom::IResult::Error(e) => Err(e),

        nom::IResult::Incomplete(_) => panic!("dumpster fire: \
          nom::IResult::Incomplete leaked from parser"),
    }
}

named!(pub dumpster<Dumpster>, complete!(map!(
    terminated!(
        many1!(module),
        opt!(complete!(call!(nom::multispace)))),
    |mods| Dumpster {
        modules: mods
    })));

named!(pub module<Module>, alt_complete!(
    normal_module
));

named!(normal_module<Module>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
 start_pos: call!(pos) >>
            tag!("mod") >>
            call!(nom::multispace) >>
      name: ident >>
            opt!(call!(nom::multispace)) >>
            char!('{') >>
     items: many0!(normal_item) >>
            opt!(call!(nom::multispace)) >>
            char!('}') >>
   end_pos: call!(pos) >>
            (Module {
                name,
                data: ModuleKind::Normal(items),
                loc: SrcLoc::raw(start_pos, end_pos - start_pos),
            })
)));

fn strip_comments(input: &[u8]) -> MappedSource {
    let mut in_line_comment = false;
    let mut in_block_comment = false;
    let mut in_quote = false;

    let mut res = MappedSource {
        src: Vec::new(),
        gaps: Vec::new(),
        lines: vec![0],
    };

    let mut gap_begin = 0usize;

    let mut bytes = input.iter().cloned().enumerate();

    while let Some((pos, c)) = bytes.next() {
        if in_line_comment {
            if c == b'\n' {
                in_line_comment = false;
                res.src.push(b'\n');
                res.gaps.push((gap_begin, pos));
                res.lines.push(pos + 1);
            }
            continue;
        }

        if in_block_comment {
            if c == b'*' {
                match bytes.next() {
                    Some((pos, b'/')) => {
                        in_block_comment = false;
                        // TODO: what the fuck does this mean for the gap count
                        res.src.push(b' '); // replace block comment by space
                        res.gaps.push((gap_begin, pos));
                    },
                    Some((pos, b'\n')) => {
                        res.src.push(b'\n');
                        res.lines.push(pos + 1);
                    },
                    None => {
                        return res;
                    },
                    _ => {}
                }
            } else if c == b'\n' {
                res.src.push(b'\n');
                res.lines.push(pos + 1);
            }
            continue;
        }

        if in_quote {
            if c == b'\\' {
                match bytes.next() {
                    Some((_, c)) => {
                        res.src.push(b'\\');
                        res.src.push(c);
                    },
                    None => {
                        return res;
                    }
                }
                continue;
            } else if c == b'\n' {
                res.lines.push(pos + 1);
            } else if c == b'"' {
                in_quote = false;
            }
            res.src.push(c);
        } else if c == b'/' {
            match bytes.next() {
                Some((_, b'/')) => {
                    in_line_comment = true;
                    gap_begin = pos;
                },
                Some((_, b'*')) => {
                    in_block_comment = true;
                    gap_begin = pos;
                },
                Some((_, c)) => {
                    res.src.push(b'/');
                    res.src.push(c);
                }
                None => {
                    return res;
                }
            }
        } else {
            if c == b'"' {
                in_quote = true;
            }

            res.src.push(c);
        }
    }
    res
}

fn pos(input: &[u8]) -> nom::IResult<&[u8], usize> {
    nom::IResult::Done(input, input.as_ptr() as usize)
}

#[cfg(test)]
mod test {
    use super::*;
    use nom::{self, IResult, ErrorKind};

    #[test]
    fn test_alloc() {
        expect_parse!(b"some_array <- alloc[10];"; stmt => Stmt { .. });
        expect_parse!(b"some_array <- alloc[1:10];"; stmt => Stmt { .. });
        expect_parse!(b"some_array <- alloc[1, 2, 3];"; stmt => Stmt { .. });
        expect_parse!(b"some_array <- alloc[1:2, 2:3, 4:5];"; stmt => Stmt { .. });
        expect_parse!(b"some_array <- alloc[1:10, 5, 1:5];"; stmt => Stmt { .. });
        expect_parse!(b"some_array <- alloc[5, 1:20, 5];"; stmt => Stmt { .. });

        expect_parse!(b"some_array <- realloc[,,5];"; stmt => Stmt { .. });
        expect_parse!(b"some_array <- realloc[5:10];"; stmt => Stmt { .. });
        expect_parse!(b"some_array <- realloc[10];"; stmt => Stmt { .. });
        expect_parse!(b"some_array <- realloc[,,5:10];"; stmt => Stmt { .. });

        expect_parse!(b"dealloc some_array;"; stmt => Stmt { .. });
    }

    // TODO: ALL TEST ARE NOW BAD
    /*
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
            IResult::Done(_, Ident(s, None)) => assert_eq!(s, "a_23"),
            _ => panic!("couldn't parse ident")
        }

        match ident("this".as_bytes()) {
            IResult::Done(_, Ident(s, None)) => assert_eq!(s, "this"),
            _ => panic!("couldn't parse 'this' as ident")
        }

        match ident(b"for") {
            IResult::Error(ErrorKind::Custom(e))
                if e == CustomErrors::KeywordAsIdent as u32 => { },
            res => panic!("didn't fail keyword-as-ident: {:?}, res")
        }

        match ident("fortuna".as_bytes()) {
            IResult::Done(_, Ident(s, None)) => assert_eq!(s, "fortuna"),
            _ => panic!("couldn't parse ident")
        }
    }

    #[test]
    fn parse_type() {
        let res = typename("_abcdef".as_bytes());
        assert!(res.is_err());

        expect_parse!(b"boogaloo"; typename => Type::Deferred(_));
        expect_parse!(b"i32"; typename => Type::Int32);
        expect_parse!(b"f64[]"; typename => Type::Array(_, _));
        expect_parse!(b"f64[10]"; typename => Type::Array(_, _));
        expect_parse!(b"f64[10; 1:17]"; typename => Type::Array(_, _));
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
                for &Ident(ref s, _) in &vec {
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

        let e = b"x.f[3]";
        assert!(expr(e).is_done());

        let e = b"f().x.g()[17][3]";
        assert!(expr(e).is_done());

        let e = b"!!!!!f().x.g()[17][3] @ \"bob\"";
        assert!(expr(e).is_done());

        // yup, it's an expr (we don't make promises)
        let e = b"`Debug.Print UBound(x)`";
        expect_parse!(e; expr => Expr { data: ExprKind::VbExpr { .. }, .. });
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

        let s = b"some_mod::x = f[17] * 3 + x::y.g(9);";
        assert!(stmt(s).is_done());

        let s = b"x::y[17].g @= \"bob\" @ damn ? \"jones\" : \"eh\";";
        expect_parse!(s; stmt => Stmt { data: StmtKind::Assign(_, _, _), .. });

        let s = b"return;";
        expect_parse!(s; stmt => Stmt { data: StmtKind::Return(None), .. });

        let s = b"return 17;";
        expect_parse!(s; stmt => Stmt { data: StmtKind::Return(_), .. });

        let s = b"return17;";
        expect_parse!(s; stmt => Stmt { data: StmtKind::ExprStmt(_), .. });

        let s = b"print f(17);";
        expect_parse!(s; stmt => Stmt { data: StmtKind::Print(_), .. });

        let s = b"if x > 17 { return 3; }";
        expect_parse!(s; stmt => Stmt { data: StmtKind::IfStmt { .. }, .. });

        let s = b"
        if x > 17 {
            return 3;
        } else if y > 3 {
            x = y^2;
        } else if f(x) {
            return 2;
        }";
        expect_parse!(s; stmt => Stmt { data: StmtKind::IfStmt { .. }, .. });

        let s = b"
        if x > 17 {
            return 3;
        } else if f(x) {
            return 2;\
        } else {\
            x @= \"bob\";
        }";
        expect_parse!(s; stmt => Stmt { data: StmtKind::IfStmt { .. }, .. });

        let s = b"
        while i < len(arr) {
            print arr[x];
            i += 1;
        }";
        expect_parse!(s; stmt => Stmt { data: StmtKind::WhileLoop { .. }, .. });

        let s = b"for x: i32 = 1:7 { print x; }";
        expect_parse!(s; stmt => Stmt { data: StmtKind::ForLoop { .. }, .. });

        let s = b"for x: i32 = y ? 7 : 8 : f(3) : -2 { print x; }";
        expect_parse!(s; stmt => Stmt { data: StmtKind::ForLoop { .. }, .. });

        let s = b"for x: f64 in arr { x *= 7; }";
        expect_parse!(s; stmt => Stmt { data: StmtKind::ForLoop { .. }, .. });
    }

    #[test]
    fn parse_fn() {
        let f = b"fn f (x: i32, y: &obj) { print x; }";
        expect_parse!(f; fundef => FunDef { access: Access::Private, ret: None, .. });

        let f = b"pub fn g (y: f64, z: udt) -> udt { z.x += y; return z; }";
        expect_parse!(f; fundef => FunDef { access: Access::Public, ret: Some(_), .. });
    }

    #[test]
    fn parse_mod() {
        let m = b"mod example {

            pub fn f (y: f64) {
                print y / 17.0;
            }

            pub fn g (y: f64, z: udt) -> udt {
                z.x += y;
                return z;
            }

        }";
        expect_parse!(m; module => Module { data: ModuleKind::Normal(_), .. });
    }

    #[test]
    fn parse_dumpster() {
        let d = b"
        mod example { }
        mod actual_stuff {
            fn x() { print \"whatever\"; }
        }";
        expect_parse!(d; dumpster => Dumpster { .. });
    }

    #[test]
    fn test_comments() {
        let thing = b"
            // some goofy comment
            print /* not this but instead */ that
            \"here's a quote /* ignore this comment */ and // that one\"
            /* \"quotes cant hide\" here */ or // here \"whatever\"
            ";
        let nc = strip_comments(thing);
        // any easy way to compare &[u8]?
    }
    */
}
