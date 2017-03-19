//! trashcan's sub-parsers for items

use nom;

use ast::*;
use super::SrcLoc;
use super::ident::*;
use super::stmt::*;
use super::lit::*;

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
                char!('|'), // TODO: this is kind of gross, but 
                            //   I do like setting apart the optionals visually,
                            //   since they MUST come at the end
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
