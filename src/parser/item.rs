//! trashcan's sub-parsers for items

use nom::{self, IResult, ErrorKind};

use ast::*;
use super::*;
use super::ident::*;
use super::expr::*;
use super::stmt::*;

named!(pub normal_item<NormalItem>, alt_complete!(
    fundef => { |f| NormalItem::Function(f) }
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
                typ: ty,
                mode: match byref {
                    Some(_) => ParamMode::ByRef,
                    None => ParamMode::ByVal,
                },
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
