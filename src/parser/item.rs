//! trashcan's sub-parsers for items

use nom::{self, IResult, ErrorKind};

use ast::*;
use super::*;
use super::ident::*;
use super::expr::*;
use super::stmt::*;

named!(pub normal_item<NormalItem>, alt_complete!(
    fundef => { |f| NormalItem::Function(f) }
  | structdef => { |s| NormalItem::Struct(s) }
));

named!(pub fundef<FunDef>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
    access: opt_access >>
            tag!("fn") >>
            call!(nom::multispace) >>
      name: ident >>
            opt!(call!(nom::multispace)) >>
            char!('(') >>
    params: separated_list!(ws!(char!(',')), fnparam) >>
            opt!(call!(nom::multispace)) >>
            char!(')') >>
       ret: opt!(fnret) >>
            opt!(call!(nom::multispace)) >>
            char!('{') >>
      body: many0!(stmt) >>
            opt!(call!(nom::multispace)) >>
            char!('}') >>
            (FunDef {
                name: name,
                access: access,
                params: params,
                ret: ret.unwrap_or(Type::Void),
                body: body,
                loc: empty_loc!(),
            })
)));

named!(pub fnparam<FunParam>, complete!(do_parse!(
      name: ident >>
            opt!(call!(nom::multispace)) >>
            char!(':') >>
    byref:  opt!(preceded!(
                opt!(nom::multispace),
                char!('&'))) >>
        ty: typename >>
            (FunParam {
                name: name,
                ty: ty,
                mode: match byref {
                    Some(_) => ParamMode::ByRef,
                    None => ParamMode::ByVal,
                },
                loc: empty_loc!()
            })
)));

named!(pub structdef<StructDef>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
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
            (StructDef {
                name: name,
                access: access,
                members: members,
                loc: empty_loc!(),
            })
)));

named!(pub structmem<StructMem>, complete!(do_parse!(
      name: ident >>
            opt!(call!(nom::multispace)) >>
            char!(':') >>
        ty: typename >>
            (StructMem {
                name: name,
                ty: ty,
                loc: empty_loc!()
            })
)));

named!(fnret<Type>, complete!(do_parse!(
        opt!(call!(nom::multispace)) >>
        tag!("->") >>
    ty: typename >>
        (ty)
)));

named!(opt_access<Access>, complete!(do_parse!(
            opt!(call!(nom::multispace)) >>
    access: opt!(terminated!(
                tag!("pub"),
                call!(nom::multispace))) >>
            (match access {
                Some(_) => Access::Public,
                None => Access::Private,
            })
)));
