//! trashcan's type judgment and checking tools

use ast::*;
use super::*;

pub fn typecheck(dumpster: &Dumpster) -> AnalysisResult<Dumpster> {
    Ok(dumpster.clone())
}

/// Context in which expression typecheck takes place: module name, optional
///   function name
pub struct ExprCtxt(pub Ident, pub Option<Ident>);

// TODO: need context here
pub fn type_of(expr: &Expr, symtab: &SymbolTable, ctxt: &ExprCtxt)
  -> AnalysisResult<Type> {
    match expr.data {
        ExprKind::Lit(ref lit) => match *lit {
            Literal::NullPtr => Ok(Type::Obj),
            Literal::NullVar => Ok(Type::Variant),
            Literal::EmptyVar => Ok(Type::Variant),
            Literal::Bool(_) => Ok(Type::Bool),
            Literal::UInt8(_) => Ok(Type::UInt8),
            Literal::Int16(_) => Ok(Type::Int16),
            Literal::Int32(_) => Ok(Type::Int32),
            Literal::IntPtr(_) => Ok(Type::IntPtr),
            Literal::Float32(_) => Ok(Type::Float32),
            Literal::Float64(_) => Ok(Type::Float64),
            Literal::String(_) => Ok(Type::String),
            Literal::Currency(_) => Ok(Type::Currency),
            Literal::Date(_) => Ok(Type::Date),
        },

        // qualified::name (must denote a module item)
        ExprKind::Name(Path(Some(ref module), ref ident)) => {
            match symtab.get(&module.0) {
                None => Err(AnalysisError {
                    kind: AnalysisErrorKind::NotDefined,
                    regarding: Some(module.0.clone()),
                    loc: expr.loc.clone(),
                }),

                Some(ref symtab) => match symtab.get(&ident.0) {
                    None => Err(AnalysisError {
                        kind: AnalysisErrorKind::NotDefined,
                        regarding: Some(ident.0.clone()),
                        loc: expr.loc.clone(),
                    }),

                    Some(ref sym) => match **sym {
                        Symbol::Const(ref ty) => Ok(ty.clone()),
                        Symbol::Value(ref ty, _) => Ok(ty.clone()),
                        Symbol::Type(ref ty) => Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("path denotes a type, \
                                                    not a value")),
                            loc: expr.loc.clone(),
                        }),
                        Symbol::Fun { ref def, .. } =>  Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("path denotes a type, \
                                                    not a value")),
                            loc: expr.loc.clone(),
                        }),
                    },
                }
            }
        },

        // local_name (may denote module item in current module, or local name
        //   in current item)
        ExprKind::Name(Path(None, ref ident)) => {
            // TODO
            unimplemented!()
        },

        ExprKind::Index(ref expr, ref index) => {
            let expr_t = type_of(expr, symtab, ctxt)?;
            let index_t = type_of(index, symtab, ctxt)?;
            if !may_coerce(&index_t, &Type::Int32) {
                return Err(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(String::from("index not coercible to i32")),
                    loc: index.loc.clone(),
                });
            }

            match expr_t {
                Type::Array(ref base_t, _) => Ok((**base_t).clone()),
                _ => Err(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(String::from("indexed expression not of \
                                            indexible type")),
                    loc: expr.loc.clone(),
                })
            }
        },

        _ => unimplemented!(),
    }
}

pub fn may_coerce(from: &Type, to: &Type) -> bool {
    match *from {
        Type::Bool => match *to {
            Type::Bool
          | Type::UInt8
          | Type::Int16
          | Type::Int32
          | Type::IntPtr
          | Type::Variant => true,
            _ => false,
        },

        Type::UInt8 => match *to {
            Type::UInt8
          | Type::Int16
          | Type::Int32
          | Type::IntPtr
          | Type::Float32
          | Type::Float64
          | Type::Variant => true,
            _ => false,
        },

        Type::Int16 => match *to {
            Type::Int16
          | Type::Int32
          | Type::IntPtr
          | Type::Float32
          | Type::Float64
          | Type::Variant => true,
            _ => false,
        },

        Type::Int32 => match *to {
            Type::Int32
          | Type::IntPtr
          | Type::Float32
          | Type::Float64
          | Type::Variant => true,
            _ => false,
        },

        Type::IntPtr => match *to {
            Type::IntPtr
          | Type::Float64
          | Type::Variant => true,
            _ => false,
        },

        Type::Float32 => match *to {
            Type::Float32
          | Type::Float64
          | Type::Variant => true,
            _ => false,
        },

        Type::Float64 => match *to {
            Type::Float64
          | Type::Variant => true,
            _ => false,
        },

        Type::String => match *to {
            Type::String
          | Type::Variant => true,
            _ => false,
        },

        Type::Variant => match *to {
            Type::Array(_, ref dims) if !dims.is_empty() => false,
            _ => true,
        },

        Type::Obj => match *to {
            Type::Obj
          | Type::Variant
          | Type::Object(_) => true,
            _ => false,
        },

        Type::Array(ref basety, _) => match *to {
            Type::Array(ref targetty, ref dims) =>
                targetty == basety && dims.is_empty(),
            _ => false,
        },

        // TODO: thread the symbol table through here
        //   and check actual subtyping info
        Type::Object(ref path) => match *to {
            Type::Obj
          | Type::Variant
          | Type::Object(_) => true,
            _ => false,
        },

        Type::Struct(ref path) => match *to {
            Type::Struct(ref path2) => path == path2,
            _ => false,
        },

        Type::Enum(ref path) => match *to {
            Type::Enum(ref path2) => path == path2,
            Type::Int32 => true,
            _ => false,
        },

        Type::Deferred(ref path) => panic!("internal compiler error:\
            attempt to coerce-check unresolved type {:?}", path),

        _ => panic!("we haven't figured out the rules for this type yet."),
    }
}
