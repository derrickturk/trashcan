//! trashcan's type judgment and checking tools

use ast::*;
use super::*;

/// Typecheck a dumpster, resolving Deferred types along the way
pub fn typecheck(dumpster: Dumpster, symtab: &SymbolTable)
  -> AnalysisResult<Dumpster> {
    Ok(Dumpster {
        modules: dumpster.modules.into_iter().map(|m| {
            let ctxt = ExprCtxt(m.name.clone(), None);
            match m.data {
                ModuleKind::Normal(items) => {
                    Ok(Module {
                        name: m.name,
                        data: ModuleKind::Normal(items.into_iter().map(|i| {
                            typecheck_item(i, symtab, &ctxt)
                        }).collect::<Result<_, _>>()?),
                        loc: m.loc,
                    })
                }
            }
        }).collect::<Result<_, _>>()?
    })
}

/// Context in which expression typecheck takes place: module name, optional
///   function name
pub struct ExprCtxt(pub Ident, pub Option<Ident>);

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
        ExprKind::Name(ref path) => {
            match *path_in_context(path, symtab, ctxt, &expr.loc)? {
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
            }
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

        ExprKind::Call(ref path, ref args) => {
            let fun = match *path_in_context(path, symtab, ctxt, &expr.loc)? {
                Symbol::Fun { ref def, .. } => Ok(def),
                _ => Err(AnalysisError {
                    kind: AnalysisErrorKind::FnCallError,
                    regarding: Some(format!("{} does not denote a function",
                                            path)),
                    loc: expr.loc.clone(),
                })
            }?;

            if args.len() != fun.params.len() {
                return Err(AnalysisError {
                    kind: AnalysisErrorKind::FnCallError,
                    regarding: Some(format!("{} requires {} arguments; \
                      {} were provided", path, fun.params.len(), args.len())),
                    loc: expr.loc.clone(),
                })
            }

            for (i, param) in fun.params.iter().enumerate() {
                let arg_type = type_of(&args[i], symtab, ctxt)?.decay();

                match param.mode {
                    ParamMode::ByRef =>
                        if param.typ != arg_type {
                            return Err(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(format!(
                                  "parameter {} has type &{}; type {} provided",
                                  param.name, param.typ, arg_type)),
                                loc: expr.loc.clone(),
                            })
                        },
                    ParamMode::ByVal =>
                        if !may_coerce(&arg_type, &param.typ) {
                            return Err(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(format!(
                                  "parameter {} has type {}; type {} provided",
                                  param.name, param.typ, arg_type)),
                                loc: expr.loc.clone(),
                            })
                        },
                }
            }

            Ok(fun.ret.clone())
        },

        _ => { Ok(Type::Variant) } // unimplemented!(),
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
            Type::Void => false,
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

        Type::Void => false,

        _ => panic!("we haven't figured out the rules for this type yet."),
    }
}

fn path_in_context<'a>(path: &Path, symtab: &'a SymbolTable, ctxt: &ExprCtxt,
  err_loc: &SrcLoc) -> AnalysisResult<&'a Symbol> {
    // TODO: check access here

    let module = match *path {
        Path(None, _) => &(ctxt.0).0,
        Path(Some(ref module), _) => &module.0
    };

    let symtab = match symtab.get(module) {
        None => Err(AnalysisError {
            kind: AnalysisErrorKind::NotDefined,
            regarding: Some(format!("{}", module)),
            loc: err_loc.clone(),
        }),

        Some(symtab) => Ok(symtab)
    }?;

    // path had no module component, and we're inside a function:
    //   look in function locals first
    if path.0.is_none() && ctxt.1.is_some() {
        if let Some(&Symbol::Fun { ref locals, .. }) =
          symtab.get(&ctxt.1.as_ref().unwrap().0) {
            if let Some(sym) = locals.get(&(path.1).0) {
                return Ok(sym);
            }
        } else {
            panic!("internal compiler error: no function record for {}",
                   ctxt.1.as_ref().unwrap());
        }
    }

    match symtab.get(&(path.1).0) {
        None => Err(AnalysisError {
            kind: AnalysisErrorKind::NotDefined,
            regarding: Some(format!("{}", path)),
            loc: err_loc.clone(),
        }),

        Some(sym) => Ok(sym)
    }
}

fn typecheck_item(item: NormalItem, symtab: &SymbolTable, ctxt: &ExprCtxt)
  -> AnalysisResult<NormalItem> {
    match item {
        NormalItem::Function(def) =>
            Ok(NormalItem::Function(typecheck_fundef(def, symtab, ctxt)?)),
    }
}

fn typecheck_fundef(def: FunDef, symtab: &SymbolTable, ctxt: &ExprCtxt)
  -> AnalysisResult<FunDef> {
    let inner_ctxt = ExprCtxt(ctxt.0.clone(), Some(def.name.clone()));

    Ok(FunDef {
        name: def.name,
        access: def.access,
        params: def.params.into_iter().map(|p| {
            match p.mode {
                ParamMode::ByRef => Ok(()),
                ParamMode::ByVal => match p.typ {
                    Type::Array(_, _) => Err(AnalysisError {
                        kind: AnalysisErrorKind::FnCallError,
                        regarding: Some(String::from("array types cannot \
                          be passed by value")),
                        loc: p.loc.clone(),
                    }),

                    Type::Struct(_) => Err(AnalysisError {
                        kind: AnalysisErrorKind::FnCallError,
                        regarding: Some(String::from("struct types cannot \
                          be passed by value")),
                        loc: p.loc.clone(),
                    }),

                    _ => Ok(()),
                },
            }?;
            Ok(p)
        }).collect::<Result<_, _>>()?,
        ret: def.ret,
        body: def.body.into_iter().map(|s| {
            typecheck_stmt(s, symtab, &inner_ctxt)
        }).collect::<Result<_, _>>()?,
        loc: def.loc,
    })
}

fn typecheck_stmt(stmt: Stmt, symtab: &SymbolTable, ctxt: &ExprCtxt)
  -> AnalysisResult<Stmt> {
    match stmt.data {
        StmtKind::ExprStmt(ref expr) => {
            match expr.data {
                ExprKind::Call(_, _)
              | ExprKind::MemberInvoke(_, _, _)
              | ExprKind::VbExpr(_) => {},

                _ => return Err(AnalysisError {
                    kind: AnalysisErrorKind::StmtExprErr,
                    regarding: None,
                    loc: expr.loc.clone(),
                })
            };

            let ty = type_of(&expr, symtab, ctxt)?;
        },

        StmtKind::VarDecl(ref decls) => {
            for &(ref ident, ref ty, ref init) in decls {
                if let Some(ref init) = *init {
                    let init_ty = type_of(init, symtab, ctxt)?;
                    if !may_coerce(&init_ty, &ty) {
                        return Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("initializer not \
                              coercible to declared type")),
                            loc: stmt.loc.clone(),
                        })
                    }
                }
            }
        },

        _ => { } //unimplemented!()
    }

    Ok(stmt)
}
