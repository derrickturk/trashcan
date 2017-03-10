//! trashcan's type judgment and checking tools

use ast::*;
use super::*;

/// Context in which expression typecheck takes place: module name, optional
///   function name
pub struct ExprCtxt(pub Ident, pub Option<Ident>);

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

                // TODO: these guys get their own namespace
                Symbol::Type(ref ty) => Err(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(format!("{} denotes a type, not a value",
                                            path)),
                    loc: expr.loc.clone(),
                }),

                Symbol::Fun { .. } =>  Err(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(format!("{} denotes a function, not a value",
                                            path)),
                    loc: expr.loc.clone(),
                }),
            }
        },

        // TODO: what about indexing a Variant
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

        // TODO: member (we need type definitions first)

        // TODO: member invoke (we need type definitions first)

        ExprKind::UnOpApp(ref expr, ref op) => {
            let expr_ty = type_of(&**expr, symtab, ctxt)?;
            match *op {
                UnOp::Negate => {
                    if !expr_ty.might_be_numeric() {
                        return Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("unary negation of \
                              non-numeric expression")),
                            loc: expr.loc.clone(),
                        });
                    }
                    Ok(expr_ty.clone())
                },

                UnOp::BitNot => {
                    if !expr_ty.might_be_bitwise() {
                        return Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("bitwise complement \
                              of non-bitwise expression")),
                            loc: expr.loc.clone(),
                        });
                    }
                    Ok(expr_ty.clone())
                },

                UnOp::LogNot => {
                    if !may_coerce(&expr_ty, &Type::Bool) {
                        return Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("logical complement \
                              of non-boolean expression")),
                            loc: expr.loc.clone(),
                        });
                    }
                    Ok(Type::Bool)
                },

                UnOp::AddressOf => {
                    if !expr.is_lvalue() {
                        return Err(AnalysisError {
                            kind: AnalysisErrorKind::InvalidExpr,
                            regarding: Some(String::from("attempt to take \
                              address of non-lvalue")),
                            loc: expr.loc.clone(),
                        });
                    }
                    Ok(Type::IntPtr)
                },
            }
        },

        ExprKind::BinOpApp(ref lhs, ref rhs, ref op) => {
            let lhs_ty = type_of(&**lhs, symtab, ctxt)?;
            let rhs_ty = type_of(&**rhs, symtab, ctxt)?;
            let ub_ty = upper_bound_type(&lhs_ty, &rhs_ty).ok_or(
                AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(format!("no common type for {} and {}",
                      lhs_ty, rhs_ty)),
                    loc: expr.loc.clone(),
                })?;

            match *op {
                BinOp::Add
              | BinOp::Sub
              | BinOp::Mul
              | BinOp::Div
              | BinOp::Mod 
              | BinOp::Pow => {
                    if !lhs_ty.might_be_numeric()
                      || !rhs_ty.might_be_numeric() {
                        return Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("non-numeric type in \
                              numeric operation")),
                            loc: expr.loc.clone(),
                        });
                    }
                },

                BinOp::StrCat => {
                    if !lhs_ty.might_be_string()
                      || !rhs_ty.might_be_string() {
                        return Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("non-string type in \
                              string operation")),
                            loc: expr.loc.clone(),
                        });
                    }
                },

                BinOp::Eq | BinOp::NotEq => {
                    if !lhs_ty.is_scalar() || !rhs_ty.is_scalar() {
                        return Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("non-scalar type in \
                              equality test")),
                            loc: expr.loc.clone(),
                        });
                    }

                    return Ok(Type::Bool);
                },

                // TODO: we might codegen this as an addressof comparison,
                //   eventually
                BinOp::IdentEq | BinOp::NotIdentEq => {
                    if let Some(false) = lhs_ty.is_object() {
                        return Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("non-object type in \
                              object identity test")),
                            loc: expr.loc.clone(),
                        });
                    }

                    if let Some(false) = rhs_ty.is_object() {
                        return Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("non-object type in \
                              object identity test")),
                            loc: expr.loc.clone(),
                        });
                    }

                    return Ok(Type::Bool);
                },

                BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq => {
                    if !(lhs_ty.might_be_numeric()
                         || lhs_ty.might_be_string())
                      || !(rhs_ty.might_be_numeric()
                         || rhs_ty.might_be_string()) {
                        return Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("non-comparable type \
                              in comparison operation")),
                            loc: expr.loc.clone(),
                        });
                    }

                    return Ok(Type::Bool);
                },

                BinOp::BitAnd | BinOp::BitOr => {
                    if !lhs_ty.might_be_bitwise()
                      || !rhs_ty.might_be_bitwise() {
                        return Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("non-bitwise type in \
                              bitwise operation")),
                            loc: expr.loc.clone(),
                        });
                    }
                },

                BinOp::LogAnd | BinOp::LogOr => {
                    if !may_coerce(&lhs_ty, &Type::Bool)
                      || !may_coerce(&rhs_ty, &Type::Bool) {
                        return Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("non-boolean type in \
                              logical operation")),
                            loc: expr.loc.clone(),
                        });
                    }
                },
            };

            Ok(ub_ty)
        },

        ExprKind::CondExpr { ref cond, ref if_expr, ref else_expr } => {
            if !may_coerce(&type_of(cond, symtab, ctxt)?, &Type::Bool) {
                return Err(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(String::from("non-boolean expression as \
                      conditional-expression condition")),
                    loc: cond.loc.clone(),
                });
            }

            let if_ty = type_of(if_expr, symtab, ctxt)?;
            let else_ty = type_of(else_expr, symtab, ctxt)?;
            let ub_ty = upper_bound_type(&if_ty, &else_ty).ok_or(AnalysisError {
                kind: AnalysisErrorKind::TypeError,
                regarding: Some(format!("no common type for {} and {}",
                  if_ty, else_ty)),
                loc: expr.loc.clone(),
            })?;

            Ok(ub_ty)
        },

        _ => { Ok(Type::Variant) } // unimplemented!(),
    }
}

pub fn upper_bound_type(lhs: &Type, rhs: &Type) -> Option<Type> {
    // TODO: revisit this for object types
    if lhs == rhs {
        Some(lhs.clone())
    } else if may_coerce(rhs, lhs) {
        Some(lhs.clone())
    } else if may_coerce(lhs, rhs) {
        Some(rhs.clone())
    } else {
        None
    }
}

pub fn may_coerce(from: &Type, to: &Type) -> bool {
    match *from {
        Type::Bool => match *to {
            Type::Bool
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
        Type::Object(_) => match *to {
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
    let module = match *path {
        Path(None, _) => &(ctxt.0).0,
        Path(Some(ref module), _) => &module.0
    };

    // TODO: check access here
    let allow_private = module == &(ctxt.0).0;

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
                ParamMode::ByRef => match p.typ {
                    Type::Array(_, ref bounds) if !bounds.is_empty() =>
                        Err(AnalysisError {
                            kind: AnalysisErrorKind::FnCallError,
                            regarding: Some(String::from("array types cannot \
                              specify bounds when used as parameters.")),
                            loc: p.loc.clone(),
                        }),
                    _ => Ok(())
                },

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

            if &p.name == inner_ctxt.1.as_ref().unwrap() {
                return Err(AnalysisError {
                    kind: AnalysisErrorKind::DuplicateSymbol,
                    regarding: Some(format!("parameter {} has same name \
                      as function", p.name)),
                    loc: p.loc.clone(),
                })
            }

            Ok(p)
        }).collect::<Result<_, _>>()?,
        ret: def.ret,
        body: def.body.into_iter().map(|s| {
            // TODO: somewhere in here check that we actually return a value
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
                    kind: AnalysisErrorKind::InvalidStmt,
                    regarding: None,
                    loc: expr.loc.clone(),
                })
            };

            let _ = type_of(&expr, symtab, ctxt)?;
        },

        StmtKind::VarDecl(ref decls) => {
            for &(ref ident, ref ty, ref init) in decls {
                if let Some(ref fun) = ctxt.1 {
                    if ident == fun {
                        return Err(AnalysisError {
                            kind: AnalysisErrorKind::InvalidStmt,
                            regarding: Some(format!("{} has same name as \
                              enclosing function (for now)", ident)),
                            loc: stmt.loc.clone(),
                        });
                    }
                }

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

        StmtKind::Assign(ref lhs, ref op, ref rhs) => { 
            if !lhs.is_lvalue() {
                return Err(AnalysisError {
                    kind: AnalysisErrorKind::InvalidStmt,
                    regarding: Some(String::from("assignment to non-lvalue \
                      expression")),
                    loc: lhs.loc.clone(),
                });
            }

            let lhs_ty = type_of(lhs, symtab, ctxt)?;
            let rhs_ty = type_of(rhs, symtab, ctxt)?;
            match *op {
                AssignOp::Assign => { },

                AssignOp::AddAssign
              | AssignOp::SubAssign
              | AssignOp::MulAssign
              | AssignOp::DivAssign
              | AssignOp::PowAssign
              | AssignOp::SubAssign
                if !lhs_ty.might_be_numeric()
                  || !rhs_ty.might_be_numeric() => {
                    return Err(AnalysisError {
                        kind: AnalysisErrorKind::TypeError,
                        regarding: Some(String::from("non-numeric \
                          expression in numeric-operation assignment")),
                        loc: stmt.loc.clone(),
                    })
                },

                AssignOp::StrCatAssign
                if !lhs_ty.might_be_string()
                  || !rhs_ty.might_be_string() => {
                    return Err(AnalysisError {
                        kind: AnalysisErrorKind::TypeError,
                        regarding: Some(String::from("non-string \
                          expression in string-operation assignment")),
                        loc: stmt.loc.clone(),
                    })
                },

                AssignOp::BitAndAssign
              | AssignOp::BitOrAssign
                if !lhs_ty.might_be_string()
                  || !rhs_ty.might_be_string() => {
                    return Err(AnalysisError {
                        kind: AnalysisErrorKind::TypeError,
                        regarding: Some(String::from("non-bitwise \
                          expression in bitwise-operation assignment")),
                        loc: stmt.loc.clone(),
                    })
                },

                AssignOp::LogAndAssign
              | AssignOp::LogOrAssign
                if !may_coerce(&lhs_ty, &Type::Bool)
                  || !may_coerce(&rhs_ty, &Type::Bool) => {
                    return Err(AnalysisError {
                        kind: AnalysisErrorKind::TypeError,
                        regarding: Some(String::from("non-boolean \
                          expression in logical-operation assignment")),
                        loc: stmt.loc.clone(),
                    })
                },

                _ => {},
            }

            // TODO: do we like this rule in every case?
            if !may_coerce(&rhs_ty, &lhs_ty) {
                return Err(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(format!("expression not \
                      coercible to {}", lhs_ty)),
                    loc: stmt.loc.clone(),
                })
            }
        },

        StmtKind::Return(ref expr) => {
            if let Some(ref fun) = ctxt.1 {
                let ctxt_path = Path(Some(ctxt.0.clone()), fun.clone());
                if let Symbol::Fun { ref def, .. } = *path_in_context(
                  &ctxt_path, symtab, ctxt, &stmt.loc)? {
                    match def.ret {
                        Type::Void => if expr.is_some() {
                            return Err(AnalysisError {
                                kind: AnalysisErrorKind::InvalidStmt,
                                regarding: Some(String::from("return with \
                                  value from non-void function")),
                                loc: stmt.loc.clone(),
                            });
                        },

                        ref ret_ty => match *expr {
                            None => return Err(AnalysisError {
                                kind: AnalysisErrorKind::InvalidStmt,
                                regarding: Some(String::from("return without \
                                  expression from non-void function")),
                                loc: stmt.loc.clone(),
                            }),

                            Some(ref expr) => {
                                let expr_ty = type_of(expr, symtab, ctxt)?;
                                if !may_coerce(&expr_ty, ret_ty) {
                                    return Err(AnalysisError {
                                        kind: AnalysisErrorKind::TypeError,
                                        regarding: Some(format!("return value not \
                                          coercible to {}", ret_ty)),
                                        loc: stmt.loc.clone(),
                                    });
                                }
                            }
                        }
                    }
                } else {
                    panic!("internal compiler error: fn definition not\
                      found in symbol table.");
                }
            } else {
                return Err(AnalysisError {
                    kind: AnalysisErrorKind::InvalidStmt,
                    regarding: Some(String::from("return statement outside of\
                      function body")),
                    loc: stmt.loc.clone(),
                });
            }
        },

        StmtKind::IfStmt { cond, body, elsifs, mut els } => {
            let cond_ty = type_of(&cond, symtab, ctxt)?;
            if !may_coerce(&cond_ty, &Type::Bool) {
                return Err(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(String::from(
                      "condition not coercible to bool")),
                    loc: cond.loc.clone(),
                });
            }

            let body = body.into_iter()
                .map(|s| typecheck_stmt(s, symtab, ctxt))
                .collect::<Result<_, _>>()?;

            let elsifs = elsifs.into_iter().map(|(cond, body)| {
                let cond_ty = type_of(&cond, symtab, ctxt)?;
                if !may_coerce(&cond_ty, &Type::Bool) {
                    return Err(AnalysisError {
                        kind: AnalysisErrorKind::TypeError,
                        regarding: Some(String::from(
                          "condition not coercible to bool")),
                        loc: cond.loc.clone(),
                    });
                }

                let body = body.into_iter()
                    .map(|s| typecheck_stmt(s, symtab, ctxt))
                    .collect::<Result<_, _>>()?;

                Ok((cond, body))
            }).collect::<Result<_, _>>()?;

            if let Some(body) = els {
                els = Some(body.into_iter()
                           .map(|s| typecheck_stmt(s, symtab, ctxt))
                           .collect::<Result<_, _>>()?);
            }

            return Ok(Stmt {
                data: StmtKind::IfStmt {
                    cond: cond,
                    body: body,
                    elsifs: elsifs,
                    els: els,
                },
                loc: stmt.loc,
            });
        },

        StmtKind::WhileLoop { cond, body } => {
            let cond_ty = type_of(&cond, symtab, ctxt)?;
            if !may_coerce(&cond_ty, &Type::Bool) {
                return Err(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(String::from(
                      "condition not coercible to bool")),
                    loc: cond.loc.clone(),
                });
            }

            let body = body.into_iter()
                .map(|s| typecheck_stmt(s, symtab, ctxt))
                .collect::<Result<_, _>>()?;

            return Ok(Stmt {
                data: StmtKind::WhileLoop {
                    cond: cond,
                    body: body,
                },
                loc: stmt.loc,
            });
        },

        StmtKind::ForLoop { var, spec, body } => {
            // TODO: changes here as we improve for loop semantics
            match spec {
                ForSpec::Range(ref from, ref to, ref step) => {
                    if !var.1.might_be_numeric() {
                        return Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("non-numeric control \
                              variable in range-based for loop.")),
                            loc: stmt.loc.clone(),
                        });
                    }

                    let to_ty = type_of(to, symtab, ctxt)?;
                    let from_ty = type_of(from, symtab, ctxt)?;

                    if !from_ty.might_be_numeric()
                      || !to_ty.might_be_numeric() {
                        return Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("non-numeric range \
                              expression in range-based for loop.")),
                            loc: stmt.loc.clone(),
                        });
                    }

                    if !may_coerce(&from_ty, &to_ty) {
                        return Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("bounds have \
                              incompatible types in range-based for loop.")),
                            loc: stmt.loc.clone(),
                        });
                    }

                    if let Some(ref step) = *step {
                        let step_ty = type_of(step, symtab, ctxt)?;
                        if !step_ty.might_be_numeric() {
                            return Err(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(String::from("non-numeric step \
                                  expression in range-based for loop.")),
                                loc: stmt.loc.clone(),
                            });
                        }

                        if !may_coerce(&step_ty, &to_ty) {
                            return Err(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(String::from("step has \
                                  incompatible type in range-based for loop.")),
                                loc: stmt.loc.clone(),
                            });
                        }
                    }
                },

                ForSpec::Each(ref expr) => {
                    match var.1 {
                        Type::Variant | Type::Obj | Type::Object(_) => { },
                        _ => return Err(AnalysisError {
                            kind: AnalysisErrorKind::InvalidStmt,
                            regarding: Some(String::from("for-each loop \
                              iteration variable must be var or object type \
                              (for now)")),
                            loc: stmt.loc.clone(),
                        })
                    }

                    let expr_ty = type_of(expr, symtab, ctxt)?;
                    match expr_ty {
                        Type::Array(_, _) | Type::Variant => { },
                        _ => return Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("for-each loop \
                              iteration expression must have array type \
                              (for now)")),
                            loc: stmt.loc.clone(),
                        })

                    }
                },

            };

            let body = body.into_iter()
                .map(|s| typecheck_stmt(s, symtab, ctxt))
                .collect::<Result<_, _>>()?;

            return Ok(Stmt {
                data: StmtKind::ForLoop {
                    var: var,
                    spec: spec,
                    body: body,
                },

                loc: stmt.loc,
            });
        },

        StmtKind::Print(ref expr) => {
            match type_of(expr, symtab, ctxt)? {
                Type::Void => return Err(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(String::from("void function invocation \
                      in print statement")),
                    loc: stmt.loc.clone(),
                }),

                _ => { },
            }
        },
    }

    Ok(stmt)
}
