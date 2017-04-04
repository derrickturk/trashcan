//! trashcan's sub-parsers for items

use super::{ParseError, ParseResult, SrcLoc};
#[macro_use]
use super::bits::*;
use super::op::*;
use super::ident::*;
use super::expr::*;

use ast::*;

/*

named!(pub normal_item<NormalItem>, alt_complete!(
    fundef => { |f| NormalItem::Function(f) }
  | structdef => { |s| NormalItem::Struct(s) }
));

named!(pub fundef<FunDef>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
 start_pos: call!(super::pos) >>
    access: opt_access >>
            tag!("fn") >>
            call!(nom::multispace) >>
      name: ident >>
            opt!(call!(nom::multispace)) >>
            char!('(') >>
    params: separated_list!(ws!(char!(',')), fnparam) >>
            opt!(call!(nom::multispace)) >>
 optparams: opt!(preceded!(
                char!(';'), // TODO: this is kind of gross, but 
                            //   I do like setting apart the optionals visually,
                            //   since they MUST come at the end
                            // also: avoid expr <-> optarg ambiguity hell
                            //   when calling!
                separated_nonempty_list!(ws!(char!(',')), optfnparam)
            )) >>
            opt!(call!(nom::multispace)) >>
            char!(')') >>
       ret: opt!(fnret) >>
            opt!(call!(nom::multispace)) >>
            char!('{') >>
      body: many0!(stmt) >>
            opt!(call!(nom::multispace)) >>
            char!('}') >>
   end_pos: call!(super::pos) >>
            (FunDef {
                name,
                access,
                params,
                optparams: optparams.unwrap_or(vec![]),
                ret: ret.unwrap_or(Type::Void),
                body,
                loc: SrcLoc::raw(start_pos, end_pos - start_pos),
            })
)));

named!(pub fnparam<FunParam>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
 start_pos: call!(super::pos) >>
      name: ident >>
            opt!(call!(nom::multispace)) >>
            char!(':') >>
    byref:  opt!(preceded!(
                opt!(nom::multispace),
                char!('&'))) >>
        ty: typename >>
   end_pos: call!(super::pos) >>
            (FunParam {
                name,
                ty,
                mode: match byref {
                    Some(_) => ParamMode::ByRef,
                    None => ParamMode::ByVal,
                },
                loc: SrcLoc::raw(start_pos, end_pos - start_pos),
            })
)));

named!(pub optfnparam<(FunParam, Literal)>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
 start_pos: call!(super::pos) >>
      name: ident >>
            opt!(call!(nom::multispace)) >>
            char!(':') >>
    byref:  opt!(preceded!(
                opt!(nom::multispace),
                char!('&'))) >>
        ty: typename >>
            opt!(call!(nom::multispace)) >>
            char!('=') >>
   default: literal >>
   end_pos: call!(super::pos) >>
            (
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
            )
)));

named!(pub structdef<StructDef>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
 start_pos: call!(super::pos) >>
    access: opt_access >>
            tag!("struct") >>
            call!(nom::multispace) >>
      name: ident >>
            opt!(call!(nom::multispace)) >>
            char!('{') >>
   members: separated_nonempty_list!(ws!(char!(',')), structmem) >>
            opt!(call!(nom::multispace)) >>
            opt!(char!(',')) >>
            opt!(call!(nom::multispace)) >>
            char!('}') >>
   end_pos: call!(super::pos) >>
            (StructDef {
                name,
                access,
                members,
                loc: SrcLoc::raw(start_pos, end_pos - start_pos),
            })
)));

named!(pub structmem<StructMem>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
 start_pos: call!(super::pos) >>
      name: ident >>
            opt!(call!(nom::multispace)) >>
            char!(':') >>
        ty: typename >>
   end_pos: call!(super::pos) >>
            (StructMem {
                name,
                ty,
                loc: SrcLoc::raw(start_pos, end_pos - start_pos),
            })
)));

named!(fnret<Type>, complete!(do_parse!(
        opt!(call!(nom::multispace)) >>
        tag!("->") >>
    ty: typename >>
        (ty)
)));

*/

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
    fn parse_access() {
        expect_parse!(access(b"  pub ") => Access::Public);
        expect_parse!(access(b"   ") => Access::Private);
    }
}
