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
    fn emit<W: Write>(&self, out: &mut W, symtab: &SymbolTable,
      ctxt: TypePos, indent: u32) -> io::Result<()> {
        match *self {
            Type::Deferred(ref i) =>
                panic!("internal compiler error: unresolved type \
                  {:?} in codegen", i),

            Type::Array(_, ref bounds) => {
                match ctxt {
                    TypePos::Decl => {
                        emit_bounds(out, bounds)?;
                        out.write_all(b" As ")?;
                        emit_basename(out, symtab, self)
                    },

                    TypePos::FunParam | TypePos::FunRet => {
                        out.write_all(b"() As ")?;
                        emit_basename(out, symtab, self)
                    },
                }
            },

            _ => {
                out.write_all(b" As ")?;
                emit_basename(out, symtab, self)
            }
        }
    }
}

fn emit_basename<W: Write>(out: &mut W, symtab: &SymbolTable, ty: &Type)
  -> io::Result<()> {
    match ty {
        &Type::Bool => out.write_all(b"Boolean"),
        &Type::UInt8 => out.write_all(b"Byte"),
        &Type::Int16 => out.write_all(b"Integer"),
        &Type::Int32 => out.write_all(b"Long"),
        &Type::IntPtr => out.write_all(b"LongPtr"),
        &Type::Float32 => out.write_all(b"Single"),
        &Type::Float64 => out.write_all(b"Double"),
        &Type::String => out.write_all(b"String"),
        &Type::Currency => out.write_all(b"Currency"),
        &Type::Date => out.write_all(b"Date"),
        &Type::Variant => out.write_all(b"Variant"),
        &Type::Obj => out.write_all(b"Object"),
        &Type::Array(ref basety, _) => emit_basename(out, symtab, basety),
        &Type::Object(ref path) => path.emit(out, symtab, (), 0),
        &Type::Struct(ref path) => path.emit(out, symtab, (), 0),
        &Type::Enum(ref path) => path.emit(out, symtab, (), 0),
        &Type::Deferred(ref path) => path.emit(out, symtab, (), 0),
        &Type::Void =>
            panic!("internal compiler error: tried to emit void type"),
    }
}

fn emit_bounds<W: Write>(out: &mut W, bounds: &Vec<(i32, i32)>)
  -> io::Result<()> {
    out.write_all(b"(")?;
    for (i, &(l, u)) in bounds.iter().enumerate() {
        if i != 0 {
            out.write_all(b", ")?;
        }
        write!(out, "{} To {}", l, u)?;
    }
    out.write_all(b")")
}
