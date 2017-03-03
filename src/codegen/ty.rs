//! code generation for trashcan bits/atoms

use std::io;
use std::io::Write;

use ast::*;
use super::*;

pub enum TypePos {
    Decl,
    FunParam,
    FunRet,
}

// TODO: need context here (are we in fn-ret, param, or decl position?)
impl Emit<TypePos> for Type {
    fn emit<W: Write>(&self, out: &mut W, ctxt: TypePos, indent: u32)
      -> io::Result<()> {
        let base = basename(self);
        match self {
            &Type::Deferred(_) =>
                panic!("internal compiler error: unresolved type in codegen"),

            &Type::Array(_, ref bounds) => {
                match ctxt {
                    TypePos::Decl => {
                        emit_bounds(out, bounds)?;
                        write!(out, " As {}", base)
                    },
                    TypePos::FunParam => write!(out, "() As {}", base),
                    TypePos::FunRet => write!(out, "() As {}", base),
                }
            },

            _ => write!(out, " As {}", base),
        }
    }
}

fn basename(ty: &Type) -> &str {
    match ty {
        &Type::Bool => "Boolean",
        &Type::UInt8 => "Byte",
        &Type::Int16 => "Integer",
        &Type::Int32 => "Long",
        &Type::IntPtr => "LongPtr",
        &Type::Float32 => "Single",
        &Type::Float64 => "Double",
        &Type::String => "String",
        &Type::Currency => "Currency",
        &Type::Date => "Date",
        &Type::Variant => "Variant",
        &Type::Obj => "Object",
        &Type::Array(ref basety, _) => basename(basety),
        &Type::Object(ref i) => i.0.as_str(),
        &Type::Struct(ref i) => i.0.as_str(),
        &Type::Enum(ref i) => i.0.as_str(),
        &Type::Deferred(ref i) => i.0.as_str(),
    }
}

fn emit_bounds<W: Write>(out: &mut W, bounds: &Vec<(i32, i32)>)
  -> io::Result<()> {
    for &(l, u) in bounds {
        write!(out, "{} To {}", l, u)?;
    }
    Ok(())
}
