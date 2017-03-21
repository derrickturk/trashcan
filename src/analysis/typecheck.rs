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
        symtab,
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
        ExprKind::Lit(ref lit) => Ok(lit.ty()),

        // qualified::name (must denote a module item)
        ExprKind::Name(ref path) => {
            match *symtab.symbol_at_path(path,
              NameCtxt::Value(&ctxt.0, ctxt.1.as_ref(), Access::Private),
              &expr.loc)? {
                Symbol::Const(ref ty) => Ok(ty.clone()),
                Symbol::Value(ref ty, _) => Ok(ty.clone()),
                _ => panic!("dumpster fire: non-value slipped past \
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
                    if bounds.dims() == indices.len() {
                        Ok((**base_t).clone())
                    } else {
                        Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(format!("expression indexed with \
                              {} dimensions; {} required", indices.len(),
                              bounds.dims())),
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

        // TODO aaaaaaa
        ExprKind::Call(ref path, ref args, ref optargs) => {
            let fun = match *symtab.symbol_at_path(path,
              NameCtxt::Function(&ctxt.0, Access::Private), &expr.loc)? {
                Symbol::Fun { ref def, .. } => def,
                _ => panic!("dumpster fire: non-function \
                  slipped past lookup typecheck"),
            };

            typeof_fn_call(fun, args, optargs, symtab, ctxt, path, &expr.loc)
        },

        ExprKind::Member(ref expr, ref mem) => {
            let expr_ty = type_of(&**expr, symtab, ctxt)?;
            match expr_ty {
                // for now
                Type::Variant | Type::Obj | Type::Object(_) =>
                    Ok(Type::Variant),

                Type::Struct(ref path) => {
                    if let Ok(&Symbol::Struct { ref members, .. }) = symtab
                      .symbol_at_path(path,
                                      NameCtxt::Type(&ctxt.0, Access::Private),
                                      &expr.loc) {
                        match members.get(&mem.0).cloned() {
                            Some(ty) => Ok(ty),
                            None => {
                                return Err(AnalysisError {
                                    kind: AnalysisErrorKind::NotDefined,
                                    regarding: Some(format!(
                                      "member {} of struct {}", mem, path)),
                                    loc: expr.loc.clone(),
                                })
                            }
                        }
                    } else {
                        panic!("dumpster fire: struct definition not found");
                    }
                },

                Type::Deferred(ref path) => panic!("dumpster fire:
                  deferred type {} in type checking pass", path),

                ty => {
                    return Err(AnalysisError {
                        kind: AnalysisErrorKind::TypeError,
                        regarding: Some(format!("attempt to access member \
                          of type {} (not struct, class, obj, or var)", ty)),
                        loc: expr.loc.clone(),
                    });
                }
            }
        },

        ExprKind::MemberInvoke(ref expr, ref mem, ref args) => {
            let expr_ty = type_of(&**expr, symtab, ctxt)?;
            match expr_ty {
                // for now
                Type::Variant | Type::Obj | Type::Object(_) =>
                    Ok(Type::Variant),

                ty => {
                    return Err(AnalysisError {
                        kind: AnalysisErrorKind::TypeError,
                        regarding: Some(format!("attempt to invoke member \
                          function of type {} (not class, obj, or var)", ty)),
                        loc: expr.loc.clone(),
                    });
                }
            }

            // eventually use typeof_fn_call
        },

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

        ExprKind::ExtentExpr(ref expr, _, dim) => {
            let expr_ty = type_of(expr, symtab, ctxt)?;
            match expr_ty {
                Type::Array(_, ref bounds) => {
                    if dim < bounds.dims() {
                        Ok(Type::Int32)
                    } else {
                        Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(format!("dimension {} not \
                              valid for type {}", dim, expr_ty)),
                            loc: expr.loc.clone(),
                        })
                    }
                },

                // TODO: maybe allow variants here (checked at runtime)?

                _ => Err(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(format!("cannot get extents for \
                      non-array type {}", expr_ty)),
                    loc: expr.loc.clone(),
                }),
            }
        },

        ExprKind::Cast(ref expr, ref ty) => {
            let expr_ty = type_of(expr, symtab, ctxt)?;
            if may_cast(&expr_ty, ty) {
                Ok(ty.clone())
            } else {
                Err(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(format!("cannot cast expression of type {} \
                      to type {}", expr_ty, ty)),
                    loc: expr.loc.clone(),
                })
            }
        },

        // could be anything
        ExprKind::VbExpr(_) => Ok(Type::Variant),
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
        panic!("dumpster fire: attempt to coerce-check deferred type {}", path);
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
            // can't assign to statically-dimensioned array
            Type::Array(_, ArrayBounds::Static(_)) => false,
            Type::Void => false,
            _ => true,
        },

        Type::Obj => match *to {
            Type::Obj
          | Type::Variant
          | Type::Object(_) => true,
            _ => false,
        },

        Type::Array(ref basety, ref bounds) => match *to {
            // can't assign to statically-dimensioned array
            Type::Array(ref targetty, ArrayBounds::Dynamic(dims)) =>
                targetty == basety && bounds.dims() == dims,

            Type::Variant => {
                match **basety {
                    // can't put structs into Arrays inside Variants :/
                    Type::Struct(_) => false,
                    _ => true,
                }
            },

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

        Type::Deferred(ref path) => panic!("dumpster fire: \
            attempt to coerce-check deferred type {}", path),

        Type::Void => false,

        // TODO: handle currency, date, etc
        _ => panic!("dumpster fire: we haven't figured out the rules for \
          this type yet."),
    }
}

pub fn may_cast(from: &Type, to: &Type) -> bool {
    if let Type::Deferred(ref path) = *to {
        panic!("dumpster fire: attempt to coerce-check deferred type {}", path);
    }

    match *from {
        // as 1 or 0
        Type::Bool => to.might_be_numeric() || *to == Type::Bool,

        Type::UInt8
      | Type::Int16
      | Type::Int32
      | Type::IntPtr
      | Type::Float32
      | Type::Float64 => to.might_be_numeric(),

        Type::String => to.might_be_string() || to.might_be_numeric(),

        Type::Variant => match *to {
            // can't assign to statically-dimensioned array
            Type::Array(_, ArrayBounds::Static(_)) => false,
            Type::Void => false,
            _ => true,
        },

        Type::Obj => match *to {
            Type::Obj
          | Type::Variant
          | Type::Object(_) => true,
            _ => false,
        },

        // for now, don't allow cast array-to-array (even if exact type!)
        Type::Array(ref basety, _) => match *to {
            Type::Variant => {
                match **basety {
                    // can't put structs into Arrays inside Variants :/
                    Type::Struct(_) => false,
                    _ => true,
                }
            },

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

        Type::Struct(_) => false,

        Type::Enum(_) => to.might_be_numeric(),

        Type::Deferred(ref path) => panic!("dumpster fire: \
            attempt to coerce-check deferred type {}", path),

        Type::Void => false,

        // TODO: handle currency, date, etc
        _ => panic!("dumpster fire: we haven't figured out the rules for \
          this type yet."),
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
                Type::Array(_, ArrayBounds::Static(_)) =>
                    self.errors.push(AnalysisError {
                        kind: AnalysisErrorKind::FnCallError,
                        regarding: Some(String::from("array types cannot \
                          specify static bounds when used as parameters.")),
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
            panic!("dumpster fire: \
              parameter {} has same name as function", p.name);
        }

        self.walk_funparam(p, m, f);
    }

    fn visit_optparam(&mut self, p: &(FunParam, Literal),
      m: &Ident, f: &Ident) {
        // run defaults to typecheck param, literal
        self.walk_optparam(p, m, f);

        let (ref p, ref default) = *p;

        if !p.ty.is_scalar() {
            self.errors.push(AnalysisError {
                kind: AnalysisErrorKind::FnCallError,
                regarding: Some(format!("non-scalar type {} cannot be used \
                  as an optional parameter", p.ty)),
                loc: p.loc.clone(),
            });
        }

        let lit_ty = default.ty();

        match p.mode {
            ParamMode::ByRef =>
                if p.ty != lit_ty {
                    self.errors.push(AnalysisError {
                        kind: AnalysisErrorKind::TypeError,
                        regarding: Some(format!(
                          "optional parameter {} has type &{}; \
                            default of type {} provided",
                          p.name, p.ty, lit_ty)),
                        loc: p.loc.clone(),
                    });
                },

            ParamMode::ByVal =>
                if !may_coerce(&lit_ty, &p.ty) {
                    self.errors.push(AnalysisError {
                        kind: AnalysisErrorKind::TypeError,
                        regarding: Some(format!(
                          "optional parameter {} has type {}; \
                            default of type {} provided",
                          p.name, p.ty, lit_ty)),
                        loc: p.loc.clone(),
                    });
                },
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt, m: &Ident, f: &Ident) {
        let ctxt = ExprCtxt(m.clone(), Some(f.clone()));
        if let Err(e) = typecheck_stmt_shallow(stmt, self.symtab, &ctxt) {
            self.errors.push(e);
        }
        self.walk_stmt(stmt, m, f);
    }

    fn visit_allocextent(&mut self, extent: &AllocExtent, m: &Ident,
      f: &Ident, loc: &SrcLoc) {
        let ctxt = ExprCtxt(m.clone(), Some(f.clone()));
        if let Err(e) = typecheck_allocextent(extent, self.symtab, &ctxt, loc) {
            self.errors.push(e);
        }
        self.walk_allocextent(extent, m, f, loc);
    }

}

fn typecheck_stmt_shallow(stmt: &Stmt, symtab: &SymbolTable, ctxt: &ExprCtxt)
  -> AnalysisResult<()> {
    match stmt.data {
        StmtKind::ExprStmt(ref expr) => {
            match expr.data {
                ExprKind::Call(_, _, _)
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
                        panic!("dumpster fire: \
                          variable {} has same name as function", ident);
                    }
                }

                if let Some(ref init) = *init {
                    let init_ty = type_of(init, symtab, ctxt)?;
                    if !may_coerce(&init_ty, &ty) {
                        return Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(format!("initializer (of type {}) \
                              not coercible to declared type {}", init_ty, ty)),
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
                    panic!("dumpster fire: fn definition not \
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

        StmtKind::ForLoop { var: (ref var, ref ty, ref mode), ref spec, .. } => {
            match *spec {
                ForSpec::Range(ref from, ref to, ref step) => {
                    if *mode == ParamMode::ByRef {
                        return Err(AnalysisError {
                            kind: AnalysisErrorKind::InvalidStmt,
                            regarding: Some(format!("reference variable \
                              {} cannot be used with for loop over range",
                              var)),
                            loc: stmt.loc.clone(),
                        });
                    }

                    if !ty.might_be_numeric() {
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
                    let expr_ty = type_of(expr, symtab, ctxt)?;
                    match expr_ty {
                        Type::Array(ref base, _) => {
                            match *mode {
                                ParamMode::ByVal => {
                                    if !may_coerce(base, ty) {
                                        return Err(AnalysisError {
                                            kind: AnalysisErrorKind::TypeError,
                                            regarding: Some(format!(
                                              "element type {} not coercible \
                                                to variable type {}",
                                              base, ty)),
                                            loc: stmt.loc.clone(),
                                        });
                                    }
                                },

                                ParamMode::ByRef => {
                                    if **base != *ty {
                                        return Err(AnalysisError {
                                            kind: AnalysisErrorKind::TypeError,
                                            regarding: Some(format!(
                                              "loop variable {} has type &{}; \
                                              element type {} provided",
                                              var, ty, base)),
                                            loc: stmt.loc.clone(),
                                        });
                                    }
                                },
                            }
                        },

                        Type::Variant
                      | Type::Obj
                      | Type::Object(_) => {
                            if *mode == ParamMode::ByRef {
                                return Err(AnalysisError {
                                    kind: AnalysisErrorKind::TypeError,
                                    regarding: Some(format!("reference \
                                      variable {} cannot be used to loop over \
                                      expression of object or var type (for now)",
                                      var)),
                                    loc: stmt.loc.clone(),
                                });
                            }
                        },

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

        StmtKind::ForAlong { ref vars, ref along, .. } => {
            let along_ty = type_of(along, symtab, ctxt)?;
            let dims = match along_ty {
                Type::Array(_, ref bounds) => bounds.dims(),

                // TODO: maybe allow variants (checked at runtime)?

                _ => return Err(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(format!("for-along loop over non-array \
                      expression of type {}", along_ty)),
                    loc: stmt.loc.clone(),
                })
            };

            if vars.len() > dims {
                return Err(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(format!("for-along loop iteration \
                      variable count ({}) exceeds iterated array \
                      dimension ({})", vars.len(), dims)),
                    loc: stmt.loc.clone(),
                });
            };
        },

        StmtKind::Alloc(ref expr, ref extents) => {
            match type_of(expr, symtab, ctxt)? {
                Type::Array(_, ArrayBounds::Dynamic(dims)) => {
                    let extent_dims = extents.len();

                    if extent_dims == 1 {
                        match extents[0] {
                            AllocExtent::Along(ref other) => {
                                let other_ty = type_of(other, symtab, &ctxt)?;
                                match other_ty {
                                    Type::Array(_, ref bounds) => {
                                        if bounds.dims() < dims {
                                            return Err(AnalysisError {
                                                kind: AnalysisErrorKind::TypeError,
                                                regarding: Some(format!(
                                                  "along expression does not \
                                                  have enough dimensions ({}) \
                                                  for alloc expression extents \
                                                  ({})", bounds.dims(), dims)),
                                                loc: stmt.loc.clone(),
                                            });
                                        }

                                        // we have a single big-enough array
                                        return Ok(());
                                    },

                                    // handled by allocextent visitor
                                    _ => { },
                                }
                            },

                            AllocExtent::Range(_, _) => { },
                        }
                    } else {
                        for (dim, extent) in extents.iter().enumerate() {
                            match *extent {
                                AllocExtent::Along(ref other) => {
                                    let other_ty = type_of(other, symtab, &ctxt)?;
                                    match other_ty {
                                        Type::Array(_, ref bounds) => {
                                            if bounds.dims() <= dim {
                                                return Err(AnalysisError {
                                                    kind: AnalysisErrorKind::TypeError,
                                                    regarding: Some(format!(
                                                      "along expression does not \
                                                      have enough dimensions ({}) \
                                                      for dimension {} of alloc \
                                                      expression extents",
                                                      bounds.dims(), dim)),
                                                    loc: stmt.loc.clone(),
                                                });
                                            }
                                        },

                                        // handled by allocextent visitor
                                        _ => { },
                                    }
                                },

                                AllocExtent::Range(_, _) => { },
                            }
                        }
                    }

                    if dims != extent_dims {
                        return Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(format!("extents dimensions ({}) \
                              do not match array dimensions ({}) in alloc",
                              extent_dims, dims)),
                            loc: stmt.loc.clone(),
                        });
                    }
                },

                Type::Array(_, ArrayBounds::Static(_)) => {
                    return Err(AnalysisError {
                        kind: AnalysisErrorKind::InvalidStmt,
                        regarding: Some(String::from("attempt to allocate \
                          statically-dimensioned array")),
                        loc: stmt.loc.clone(),
                    });
                },

                ref ty => {
                    return Err(AnalysisError {
                        kind: AnalysisErrorKind::InvalidStmt,
                        regarding: Some(format!("attempt to allocate \
                          expression of non-array type {}", ty)),
                        loc: stmt.loc.clone(),
                    });
                },
            }
        },

        StmtKind::ReAlloc(ref expr, preserved, ref extent) => {
            match type_of(expr, symtab, ctxt)? {
                Type::Array(_, ArrayBounds::Dynamic(dims)) => {
                    match *extent {
                        AllocExtent::Along(ref expr) => {
                            let expr_ty = type_of(expr, symtab, &ctxt)?;
                            match expr_ty {
                                Type::Array(_, ref bounds) => {
                                    let dims = bounds.dims();
                                    if dims < preserved + 1 {
                                        return Err(AnalysisError {
                                            kind: AnalysisErrorKind::TypeError,
                                            regarding: Some(format!("along \
                                              expression of type {} does not \
                                              have enough dimensions in \
                                              realloc", expr_ty)),
                                            loc: stmt.loc.clone(),
                                        });
                                    }
                                },

                                // checked in allocextent visitor
                                _ => { },
                            }
                        },

                        // checked in allocextent visitor
                        AllocExtent::Range(_, _) => { },
                    };

                    if dims != preserved + 1 {
                        return Err(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(format!("extents dimensions ({}) \
                              do not match array dimensions ({}) in realloc",
                              preserved + 1, dims)),
                            loc: stmt.loc.clone(),
                        });
                    }
                },

                Type::Array(_, ArrayBounds::Static(_)) => {
                    return Err(AnalysisError {
                        kind: AnalysisErrorKind::InvalidStmt,
                        regarding: Some(String::from("attempt to reallocate \
                          statically-dimensioned array")),
                        loc: stmt.loc.clone(),
                    });
                },

                ref ty => {
                    return Err(AnalysisError {
                        kind: AnalysisErrorKind::InvalidStmt,
                        regarding: Some(format!("attempt to reallocate \
                          expression of non-array type {}", ty)),
                        loc: stmt.loc.clone(),
                    });
                },
            }
        },

        StmtKind::DeAlloc(ref expr) => {
            match type_of(expr, symtab, ctxt)? {
                Type::Array(_, ArrayBounds::Dynamic(_)) => { },

                Type::Array(_, ArrayBounds::Static(_)) => {
                    return Err(AnalysisError {
                        kind: AnalysisErrorKind::InvalidStmt,
                        regarding: Some(String::from("attempt to deallocate \
                          statically-dimensioned array")),
                        loc: stmt.loc.clone(),
                    });
                },

                ref ty => {
                    return Err(AnalysisError {
                        kind: AnalysisErrorKind::InvalidStmt,
                        regarding: Some(format!("attempt to deallocate \
                          expression of non-array type {}", ty)),
                        loc: stmt.loc.clone(),
                    });
                },
            }
        },

        StmtKind::Print(ref exprs) => {
            for expr in exprs {
                match type_of(expr, symtab, ctxt)? {
                    Type::Void => return Err(AnalysisError {
                        kind: AnalysisErrorKind::TypeError,
                        regarding: Some(String::from("void function invocation \
                          in print statement")),
                        loc: stmt.loc.clone(),
                    }),

                    _ => { },
                }
            }
        },
    }

    Ok(())
}

fn typecheck_allocextent(extent: &AllocExtent, symtab: &SymbolTable,
  ctxt: &ExprCtxt, loc: &SrcLoc) -> AnalysisResult<()> {
    // TODO: maybe allow variants (checked at runtime)?

    match *extent {
        AllocExtent::Along(ref expr) => {
            match type_of(expr, symtab, &ctxt)? {
                Type::Array(_, _) => Ok(()),

                ty => Err(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(format!("along expression of non-array \
                      type {}", ty)),
                    loc: loc.clone(),
                }),
            }
        },

        AllocExtent::Range(ref lb, ref ub) => {
            if let Some(ref lb) = *lb {
                let extent_ty = type_of(lb, symtab, ctxt)?;
                if !may_coerce(&extent_ty, &Type::Int32) {
                    return Err(AnalysisError {
                        kind: AnalysisErrorKind::TypeError,
                        regarding: Some(String::from(
                          "array extent bound not \
                            coercible to i32")),
                        loc: loc.clone(),
                    });
                }
            }

            let extent_ty = type_of(ub, symtab, ctxt)?;
            if !may_coerce(&extent_ty, &Type::Int32) {
                return Err(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(String::from(
                      "array extent bound not coercible \
                      to i32")),
                    loc: loc.clone(),
                });
            }

            Ok(())
        },
    }
}

fn typeof_fn_call(fun: &FunDef, args: &Vec<Expr>, optargs: &Vec<(Ident, Expr)>,
  symtab: &SymbolTable, ctxt: &ExprCtxt, invoke_path: &Path,
  invoke_loc: &SrcLoc) -> AnalysisResult<Type> {
    if args.len() < fun.params.len() {
        return Err(AnalysisError {
            kind: AnalysisErrorKind::FnCallError,
            regarding: Some(format!("{} requires {} arguments; \
              {} were provided", invoke_path, fun.params.len(), args.len())),
            loc: invoke_loc.clone(),
        })
    }

    if !optargs.is_empty() && args.len() > fun.params.len() {
        return Err(AnalysisError {
            kind: AnalysisErrorKind::FnCallError,
            regarding: Some(String::from("positional optional arguments \
              cannot be mixed with by-name optional arguments (sorry!)")),
            loc: invoke_loc.clone(),
        });
    }

    if args.len() > fun.params.len() + fun.optparams.len() {
        return Err(AnalysisError {
            kind: AnalysisErrorKind::FnCallError,
            regarding: Some(format!("{} requires {} arguments{}; \
              {} were provided", invoke_path,
              fun.params.len(),
              if fun.optparams.is_empty() {
                  String::from("")
              } else {
                  format!(" (+ {} optional)", fun.optparams.len())
              },
              args.len())),
            loc: invoke_loc.clone(),
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
                        loc: args[i].loc.clone(),
                    })
                },
            ParamMode::ByVal =>
                if !may_coerce(&arg_type, &param.ty) {
                    return Err(AnalysisError {
                        kind: AnalysisErrorKind::TypeError,
                        regarding: Some(format!(
                          "parameter {} has type {}; type {} provided",
                          param.name, param.ty, arg_type)),
                        loc: args[i].loc.clone(),
                    })
                },
        }
    }

    if optargs.is_empty() {
        // any optional arguments are positional
        let optargs = &args[fun.params.len()..];
        for (i, &(ref param, _)) in fun.optparams.iter().enumerate() {
            // argument was provided
            if i < optargs.len() {
                let arg_type = type_of(&optargs[i], symtab, ctxt)?.decay();

                match param.mode {
                    ParamMode::ByRef =>
                        if param.ty != arg_type {
                            return Err(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(format!(
                                  "parameter {} has type &{}; type {} \
                                    provided",
                                  param.name, param.ty, arg_type)),
                                loc: optargs[i].loc.clone(),
                            })
                        },
                    ParamMode::ByVal =>
                        if !may_coerce(&arg_type, &param.ty) {
                            return Err(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(format!(
                                  "parameter {} has type {}; type {} \
                                    provided",
                                  param.name, param.ty, arg_type)),
                                loc: optargs[i].loc.clone(),
                            })
                        },
                }
            }
        }
    } else {
        // TODO
        panic!("dumpster fire: I don't know how to handle those yet!");
    }

    Ok(fun.ret.clone())
}
