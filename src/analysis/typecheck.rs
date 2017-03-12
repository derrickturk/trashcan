//! trashcan's type judgment and checking tools

use ast::*;
use super::*;
use visit::ASTVisitor;
use visit::NameCtxt;

/// Context in which expression typecheck takes place: module name, optional
///   function name
pub struct ExprCtxt(pub Ident, pub Option<Ident>);

/// Typecheck a dumpster
pub fn typecheck(dumpster: &Dumpster, symtab: &SymbolTable)
  -> AnalysisResult<()> {
    let mut v = TypecheckVisitor {
        symtab: symtab,
        errors: vec![],
    };

    v.visit_dumpster(dumpster);

    for err in v.errors.drain(..) { // for now
        return Err(err);
    }

    Ok(())
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
            match *symtab.symbol_at_path(path,
              NameCtxt::Value(&ctxt.0, ctxt.1.as_ref(), Access::Private),
              &expr.loc)? {
                Symbol::Const(ref ty) => Ok(ty.clone()),
                Symbol::Value(ref ty, _) => Ok(ty.clone()),
                _ => panic!("internal compiler error: non-value slipped past \
                  lookup typecheck"),
            }
        },

        ExprKind::Index(ref expr, ref indices) => {
            let expr_t = type_of(expr, symtab, ctxt)?;

            for index in indices {
                let index_t = type_of(index, symtab, ctxt)?;
                if !may_coerce(&index_t, &Type::Int32) {
                    return Err(AnalysisError {
                        kind: AnalysisErrorKind::TypeError,
                        regarding: Some(String::from("index not coercible to i32")),
                        loc: index.loc.clone(),
                    });
                }
            }

            match expr_t {
                Type::Array(ref base_t, ref bounds) => {
                    if bounds.len() == 0 || bounds.len() == indices.len() {
                        Ok((**base_t).clone())
                    } else {
                        Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(format!("expression indexed with \
                              {} dimensions; {} required", indices.len(),
                              bounds.len())),
                            loc: expr.loc.clone(),
                        })
                    }
                },

                // not a lot else we can do
                Type::Variant => Ok(Type::Variant),

                _ => Err(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(String::from("indexed expression not of \
                                            indexible type")),
                    loc: expr.loc.clone(),
                })
            }
        },

        ExprKind::Call(ref path, ref args) => {
            let fun = match *symtab.symbol_at_path(path,
              NameCtxt::Function(&ctxt.0, Access::Private), &expr.loc)? {
                Symbol::Fun { ref def, .. } => def,
                _ => panic!("internal compiler error: non-function \
                  slipped past lookup typecheck"),
            };

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
                        if param.ty != arg_type {
                            return Err(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(format!(
                                  "parameter {} has type &{}; type {} provided",
                                  param.name, param.ty, arg_type)),
                                loc: expr.loc.clone(),
                            })
                        },
                    ParamMode::ByVal =>
                        if !may_coerce(&arg_type, &param.ty) {
                            return Err(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(format!(
                                  "parameter {} has type {}; type {} provided",
                                  param.name, param.ty, arg_type)),
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
    if let Type::Deferred(ref path) = *to {
        panic!("attempt to coerce-check deferred type {}", path);
    }

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
            attempt to coerce-check deferred type {}", path),

        Type::Void => false,

        _ => panic!("we haven't figured out the rules for this type yet."),
    }
}

struct TypecheckVisitor<'a> {
    symtab: &'a SymbolTable,
    errors: Vec<AnalysisError>,
}

impl<'a> ASTVisitor for TypecheckVisitor<'a> {
    fn visit_funparam(&mut self, p: &FunParam, m: &Ident, f: &Ident) {
        match p.mode {
            ParamMode::ByRef => match p.ty {
                Type::Array(_, ref bounds) if !bounds.is_empty() =>
                    self.errors.push(AnalysisError {
                        kind: AnalysisErrorKind::FnCallError,
                        regarding: Some(String::from("array types cannot \
                          specify bounds when used as parameters.")),
                        loc: p.loc.clone(),
                    }),
                _ => { },
            },

            ParamMode::ByVal => match p.ty {
                Type::Array(_, _) => self.errors.push(AnalysisError {
                    kind: AnalysisErrorKind::FnCallError,
                    regarding: Some(String::from("array types cannot \
                      be passed by value")),
                    loc: p.loc.clone(),
                }),

                Type::Struct(_) => self.errors.push(AnalysisError {
                    kind: AnalysisErrorKind::FnCallError,
                    regarding: Some(String::from("struct types cannot \
                      be passed by value")),
                    loc: p.loc.clone(),
                }),

                _ => { },
            },
        };

        // these should already be gensymmed away
        if &p.name == f {
            panic!("internal compiler error: \
              parameter {} has same name as function", p.name);
        }

        self.walk_funparam(p, m, f);
    }

    fn visit_stmt(&mut self, stmt: &Stmt, m: &Ident, f: &Ident) {
        let ctxt = ExprCtxt(m.clone(), Some(f.clone()));
        if let Err(e) = typecheck_stmt_shallow(stmt, self.symtab, &ctxt) {
            self.errors.push(e);
        }
        self.walk_stmt(stmt, m, f);
    }
}

fn typecheck_stmt_shallow(stmt: &Stmt, symtab: &SymbolTable, ctxt: &ExprCtxt)
  -> AnalysisResult<()> {
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
                    // these should already be gensymmed away
                    if ident == fun {
                        panic!("internal compiler error: \
                          variable {} has same name as function", ident);
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
                // TODO: symbol_at_ident needed here
                if let Symbol::Fun { ref def, .. } = *symtab.symbol_at_path(
                  &ctxt_path,
                  NameCtxt::Function(&ctxt.0, Access::Private),
                  &stmt.loc)? {
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
                    panic!("internal compiler error: fn definition not \
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

        StmtKind::IfStmt { ref cond, ref elsifs, .. } => {
            let cond_ty = type_of(cond, symtab, ctxt)?;
            if !may_coerce(&cond_ty, &Type::Bool) {
                return Err(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(String::from(
                      "condition not coercible to bool")),
                    loc: cond.loc.clone(),
                });
            }

            for &(ref cond, _) in elsifs {
                let cond_ty = type_of(cond, symtab, ctxt)?;
                if !may_coerce(&cond_ty, &Type::Bool) {
                    return Err(AnalysisError {
                        kind: AnalysisErrorKind::TypeError,
                        regarding: Some(String::from(
                          "condition not coercible to bool")),
                        loc: cond.loc.clone(),
                    });
                }
            }

            return Ok(());
        },

        StmtKind::WhileLoop { ref cond, .. } => {
            let cond_ty = type_of(cond, symtab, ctxt)?;
            if !may_coerce(&cond_ty, &Type::Bool) {
                return Err(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(String::from(
                      "condition not coercible to bool")),
                    loc: cond.loc.clone(),
                });
            }

            return Ok(());
        },

        StmtKind::ForLoop { ref var, ref spec, .. } => {
            // TODO: changes here as we improve for loop semantics
            match *spec {
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

                // TODO: maybe use for-each by-ref to signify local
                //   lvalue rebinding?
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
                        Type::Array(_, _)
                      | Type::Obj
                      | Type::Object(_)
                      | Type::Variant => { },

                        _ => return Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(format!("for-each loop \
                              iteration expression must have array or object \
                              type; found type {}", expr_ty)),
                            loc: stmt.loc.clone(),
                        })

                    }
                },

            };

            return Ok(());
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

    Ok(())
}
