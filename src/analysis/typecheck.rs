//! trashcan's type judgment and checking tools

use ast::*;
use super::*;
use visit::ASTVisitorMut;
use visit::NameCtxt;

use std::collections::HashSet;

/// Typecheck a dumpster
pub fn typecheck(dumpster: &mut Dumpster, symtab: &SymbolTable)
  -> AnalysisResultMany<()> {
    let mut v = TypecheckVisitor {
        symtab,
        errors: Vec::new(),
    };

    v.visit_dumpster(dumpster);

    if v.errors.is_empty() {
        Ok(())
    } else {
        Err(v.errors)
    }
}

pub fn is_constexpr(expr: &Expr, symtab: &SymbolTable, module: &Ident,
  function: Option<&Ident>) -> AnalysisResult<bool> {
      match expr.data {
          ExprKind::Lit(_) => Ok(true),

          ExprKind::Name(ref path) => {
              match *symtab.symbol_at_path(path,
                NameCtxt::Value(module, function, Access::Private),
                &expr.loc)? {
                  Symbol::Const(_, _) => Ok(true),
                  _ => Ok(false),
              }
          },

          _ => Ok(false),
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
          | Type::Currency
          | Type::Variant => true,
            _ => false,
        },

        Type::Int16 => match *to {
            Type::Int16
          | Type::Int32
          | Type::IntPtr
          | Type::Float32
          | Type::Float64
          | Type::Currency
          | Type::Variant => true,
            _ => false,
        },

        Type::Int32 => match *to {
            Type::Int32
          | Type::IntPtr
          | Type::Float32
          | Type::Float64
          | Type::Currency
          | Type::Variant => true,
            _ => false,
        },

        Type::IntPtr => match *to {
            Type::IntPtr
          | Type::Float64
          | Type::Variant => true,
            _ => false,
        },

        // TODO: allow currency to float coercion?

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

        Type::Currency => match *to {
            Type::Currency
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
      | Type::Float64
      | Type::Currency => to.might_be_numeric(),

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

// used to fail a shallow check if subexprs previously failed typecheck
macro_rules! try_type {
    ($ex:expr) => {
        match $ex.ty {
            Some(ref ty) => ty,
            None => return,
        }
    }
}

// used to fail a shallow check if e.g. symbol lookup fails
macro_rules! try_collect {
    ($e:expr => $errs:expr) => {
        match $e {
            Ok(r) => r,
            Err(e) => {
                $errs.push(e);
                return;
            },
        }
    }
}

struct TypecheckVisitor<'a> {
    symtab: &'a SymbolTable,
    errors: Vec<AnalysisError>,
}

impl<'a> ASTVisitorMut for TypecheckVisitor<'a> {
    fn visit_fundef(&mut self, def: &mut FunDef, m: &Ident) {
        self.walk_fundef(def, m);

        // private-in-public check: a pub fn may not have a private return type
        if def.access == Access::Public {
            if let Ok(Access::Private) =
              self.symtab.type_access(&def.ret, m, &def.loc) {
                self.errors.push(AnalysisError {
                    kind: AnalysisErrorKind::PrivateInPublic,
                    regarding: Some(format!("return type {} of pub fn {}::{} \
                      is private to mod {}", def.ret, m, def.name, m)),
                    loc: def.loc.clone(),
                });
            }
        }
    }

    fn visit_funparam(&mut self, p: &mut FunParam, m: &Ident, f: &Ident) {
        self.walk_funparam(p, m, f);

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
    }

    fn visit_optparam(&mut self, p: &mut (FunParam, Literal),
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

    fn visit_static(&mut self, s: &mut Static, m: &Ident) {
        self.walk_static(s, m);

        // private-in-public check: a pub static may not have a private type
        if s.access == Access::Public {
            if let Ok(Access::Private) =
              self.symtab.type_access(&s.ty, m, &s.loc) {
                self.errors.push(AnalysisError {
                    kind: AnalysisErrorKind::PrivateInPublic,
                    regarding: Some(format!("type {} of pub static {}::{} \
                      is private to mod {}", s.ty, m, s.name, m)),
                    loc: s.loc.clone(),
                });
            }
        }

        match s.init {
            Some(ref lit) => if !may_coerce(&lit.ty(), &s.ty) {
                self.errors.push(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(format!(
                      "static {}::{} has type {}; initializer of type {} \
                        provided", m, s.name, s.ty, lit.ty())),
                    loc: s.loc.clone(),
                });
            },

            None => { },
        };
    }

    fn visit_constant(&mut self, c: &mut Constant, m: &Ident) {
        self.walk_constant(c, m);

        // no private-in-public check: all constable types are public

        if !may_coerce(&c.value.ty(), &c.ty) {
            self.errors.push(AnalysisError {
                kind: AnalysisErrorKind::TypeError,
                regarding: Some(format!(
                  "const {}::{} has type {}; initializer of type {} \
                    provided", m, c.name, c.ty, c.value.ty())),
                loc: c.loc.clone(),
            });
        }
    }

    fn visit_structmem(&mut self, mem: &mut StructMem, m: &Ident, st: &Ident) {
        self.walk_structmem(mem, m, st);

        // TODO: also recurse into nested types
        // recursive type check
        if mem.ty == Type::Struct(Path(Some(m.clone()), st.clone())) {
            self.errors.push(AnalysisError {
                kind: AnalysisErrorKind::RecursiveType,
                regarding: Some(format!("member {} of struct {}::{} makes type \
                  recursive", mem.name, m, st)),
                loc: mem.loc.clone(),
            })
        }

        // private-in-public check: a pub struct may not have a
        //   private member type
        let st_access = match *self.symtab.symbol_at_path(
          &Path(None, st.clone()), NameCtxt::Type(m, Access::Private),
          &mem.loc).expect(
              "dumpster fire: couldn't retrieve struct definition") {
            Symbol::Struct { ref def, .. } => def.access,
            _ => panic!("dumpster fire: struct wasn't a struct"),
        };

        if st_access == Access::Public {
            if let Ok(Access::Private) =
              self.symtab.type_access(&mem.ty, m, &mem.loc) {
                self.errors.push(AnalysisError {
                    kind: AnalysisErrorKind::PrivateInPublic,
                    regarding: Some(format!("type {} of member {} \
                      of struct {} is private to mod {}",
                      mem.ty, mem.name, st, m)),
                    loc: mem.loc.clone(),
                });
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt, m: &Ident, f: &Ident) {
        self.walk_stmt(stmt, m, f);
        self.typecheck_stmt_shallow(stmt, m, f);
    }

    fn visit_expr(&mut self, expr: &mut Expr, module: &Ident,
      function: Option<&Ident>) {
        // ok, here's the reasoning.
        // our goal is to:
        //   * typecheck every expr
        //   * typecheck every expr once
        //   * not repeat errors when we recurse in/out of exprs
        //   * avoid weird Option<Result<<Type>>> types
        //
        // so, to typecheck an expr:
        //   * walk all subexpressions and typecheck
        //   * if any subexprs are not typed, they hit errors,
        //   *   so done (don't double up)
        //   * apply shallow typecheck to expr, given any
        //       subexpr types
        //   * report shallow errors if any

        // first, walk subexprs and typecheck
        self.walk_expr(expr, module, function);

        // now do a shallow check on our type, iff all subexprs
        //   were successfully typed
        expr.ty = match expr.data {
            ExprKind::Lit(ref lit) => Some(lit.ty()),

            // qualified::name (must denote a module item)
            ExprKind::Name(ref path) => {
                match *try_collect!(self.symtab.symbol_at_path(
                  path,
                  NameCtxt::Value(module, function, Access::Private),
                  &expr.loc) => self.errors) {
                    Symbol::Const(ref ty, _) => Some(ty.clone()),
                    Symbol::Value(ref ty, _, _) => Some(ty.clone()),
                    _ => panic!("dumpster fire: non-value slipped past \
                      lookup typecheck"),
                }
            },

            ExprKind::Index(ref expr, ref indices) => {
                let expr_t = try_type!(expr);

                for index in indices {
                    let index_t = try_type!(index);
                    if !may_coerce(index_t, &Type::Int32) {
                        self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("index not coercible to i32")),
                            loc: index.loc.clone(),
                        });
                    }
                }

                match *expr_t {
                    Type::Array(ref base_t, ref bounds) => {
                        if bounds.dims() == indices.len() {
                            Some((**base_t).clone())
                        } else {
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(format!("expression indexed with \
                                  {} dimensions; {} required", indices.len(),
                                  bounds.dims())),
                                loc: expr.loc.clone(),
                            });
                            None
                        }
                    },

                    // not a lot else we can do
                    Type::Variant => Some(Type::Variant),

                    _ => {
                        self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("indexed expression \
                              not of indexible type")),
                            loc: expr.loc.clone(),
                        });
                        None
                    }
                }
            },

            ExprKind::Call(ref path, ref args, ref optargs) => {
                let fun = match *try_collect!(self.symtab.symbol_at_path(
                  path,
                  NameCtxt::Function(module, Access::Private),
                  &expr.loc) => self.errors) {
                    Symbol::Fun { ref def, .. } => def,
                    _ => panic!("dumpster fire: non-function \
                      slipped past lookup typecheck"),
                };

                self.typecheck_fn_call(fun, args, optargs, path, &expr.loc);
                Some(fun.ret.clone())
            },

            ExprKind::Member(ref expr, ref mem) => {
                match *try_type!(expr) {
                    // for now
                    Type::Variant | Type::Obj | Type::Object(_) =>
                        Some(Type::Variant),

                    Type::Struct(ref path) => {
                        let members = match *try_collect!(
                          self.symtab.symbol_at_path(
                              path,
                              NameCtxt::Type(module, Access::Private),
                              &expr.loc) => self.errors) {
                            Symbol::Struct { ref members, .. } => members,
                            _ => panic!("dumpster fire: non-struct slipped \
                              past lookup typecheck"),
                        };

                        match members.get(&mem.0).cloned() {
                            Some(ty) => Some(ty),
                            None => {
                                self.errors.push(AnalysisError {
                                    kind: AnalysisErrorKind::NotDefined,
                                    regarding: Some(format!(
                                      "member {} of struct {}", mem, path)),
                                    loc: expr.loc.clone(),
                                });
                                None
                            }
                        }
                    },

                    Type::Deferred(ref path) => panic!("dumpster fire:
                      deferred type {} in type checking pass", path),

                    ref ty => {
                        self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(format!("attempt to access member \
                              of type {} (not struct, class, obj, or var)", ty)),
                            loc: expr.loc.clone(),
                        });
                        None
                    }
                }
            },

            ExprKind::MemberInvoke(ref expr, ref _mem, ref _args) => {
                match *try_type!(expr) {
                    // for now
                    Type::Variant | Type::Obj | Type::Object(_) =>
                        Some(Type::Variant),

                    ref ty => {
                        self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(format!("attempt to invoke member \
                              function of type {} (not class, obj, or var)", ty)),
                            loc: expr.loc.clone(),
                        });
                        None
                    }
                }

                // eventually use typecheck_fn_call
            },

            ExprKind::UnOpApp(ref expr, ref op) => {
                let expr_ty = try_type!(expr);
                match *op {
                    UnOp::Negate => {
                        if !expr_ty.might_be_numeric() {
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(String::from("unary negation of \
                                  non-numeric expression")),
                                loc: expr.loc.clone(),
                            });
                            None
                        } else {
                            Some(expr_ty.clone())
                        }
                    },

                    UnOp::BitNot => {
                        if !expr_ty.might_be_bitwise() {
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(String::from("bitwise complement \
                                  of non-bitwise expression")),
                                loc: expr.loc.clone(),
                            });
                            None
                        } else {
                            Some(expr_ty.clone())
                        }
                    },

                    UnOp::LogNot => {
                        if !may_coerce(&expr_ty, &Type::Bool) {
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(String::from("logical complement \
                                  of non-boolean expression")),
                                loc: expr.loc.clone(),
                            });
                            None
                        } else {
                            Some(Type::Bool)
                        }
                    },

                    UnOp::AddressOf => {
                        if !expr.is_lvalue() {
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::InvalidExpr,
                                regarding: Some(String::from("attempt to take \
                                  address of non-lvalue")),
                                loc: expr.loc.clone(),
                            });
                            None
                        } else {
                            Some(Type::IntPtr)
                        }
                    },
                }
            },

            ExprKind::BinOpApp(ref lhs, ref rhs, ref op) => {
                let lhs_ty = try_type!(lhs);
                let rhs_ty = try_type!(rhs);
                let ub_ty = match upper_bound_type(lhs_ty, rhs_ty) {
                    None => {
                        self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(format!("no common type for {} and {}",
                              lhs_ty, rhs_ty)),
                            loc: expr.loc.clone(),
                        });
                        return;
                    },

                    Some(ty) => ty,
                };

                match *op {
                    BinOp::Add
                  | BinOp::Sub
                  | BinOp::Mul
                  | BinOp::Div
                  | BinOp::Mod 
                  | BinOp::Pow => {
                        if !lhs_ty.might_be_numeric()
                          || !rhs_ty.might_be_numeric() {
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(String::from("non-numeric type in \
                                  numeric operation")),
                                loc: expr.loc.clone(),
                            });
                            None
                        } else {
                            Some(ub_ty)
                        }
                    },

                    BinOp::StrCat => {
                        if !lhs_ty.might_be_string()
                          || !rhs_ty.might_be_string() {
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(String::from("non-string type in \
                                  string operation")),
                                loc: expr.loc.clone(),
                            });
                            None
                        } else {
                            Some(ub_ty)
                        }
                    },

                    BinOp::Eq | BinOp::NotEq => {
                        if !lhs_ty.is_scalar() || !rhs_ty.is_scalar() {
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(String::from("non-scalar type in \
                                  equality test")),
                                loc: expr.loc.clone(),
                            });
                            None
                        } else {
                            Some(Type::Bool)
                        }
                    },

                    // TODO: we might codegen this as an addressof comparison,
                    //   eventually
                    BinOp::IdentEq | BinOp::NotIdentEq => {
                        let mut both_maybe_object = true;

                        if let Some(false) = lhs_ty.is_object() {
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(String::from("non-object type in \
                                  object identity test")),
                                loc: lhs.loc.clone(),
                            });
                            both_maybe_object = false;
                        }

                        if let Some(false) = rhs_ty.is_object() {
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(String::from("non-object type in \
                                  object identity test")),
                                loc: rhs.loc.clone(),
                            });
                            both_maybe_object = false;
                        }

                        if both_maybe_object {
                            Some(Type::Bool)
                        } else {
                            None
                        }
                    },

                    BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq => {
                        if !(lhs_ty.might_be_numeric()
                             || lhs_ty.might_be_string())
                          || !(rhs_ty.might_be_numeric()
                             || rhs_ty.might_be_string()) {
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(String::from("non-comparable type \
                                  in comparison operation")),
                                loc: expr.loc.clone(),
                            });
                            None
                        } else {
                            Some(Type::Bool)
                        }
                    },

                    BinOp::BitAnd | BinOp::BitOr => {
                        if !lhs_ty.might_be_bitwise()
                          || !rhs_ty.might_be_bitwise() {
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(String::from("non-bitwise type in \
                                  bitwise operation")),
                                loc: expr.loc.clone(),
                            });
                            None
                        } else {
                            Some(ub_ty)
                        }
                    },

                    BinOp::LogAnd | BinOp::LogOr => {
                        if !may_coerce(&lhs_ty, &Type::Bool)
                          || !may_coerce(&rhs_ty, &Type::Bool) {
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(String::from("non-boolean type in \
                                  logical operation")),
                                loc: expr.loc.clone(),
                            });
                            None
                        } else {
                            Some(ub_ty)
                        }
                    },
                }
            },

            ExprKind::CondExpr { ref cond, ref if_expr, ref else_expr } => {
                if !may_coerce(try_type!(cond), &Type::Bool) {
                    self.errors.push(AnalysisError {
                        kind: AnalysisErrorKind::TypeError,
                        regarding: Some(String::from("non-boolean expression as \
                          conditional-expression condition")),
                        loc: cond.loc.clone(),
                    });
                }

                let if_ty = try_type!(if_expr);
                let else_ty = try_type!(else_expr);
                let ub_ty = upper_bound_type(&if_ty, &else_ty);
                match ub_ty {
                    None => {
                        self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(format!("no common type for {} and {}",
                              if_ty, else_ty)),
                            loc: expr.loc.clone(),
                        });
                        None
                    },

                    Some(ub_ty) => Some(ub_ty),
                }
            },

            ExprKind::ExtentExpr(ref expr, _, dim) => {
                let expr_ty = try_type!(expr);
                match *expr_ty {
                    Type::Array(_, ref bounds) => {
                        if dim < bounds.dims() {
                            Some(Type::Int32)
                        } else {
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(format!("dimension {} not \
                                  valid for type {}", dim, expr_ty)),
                                loc: expr.loc.clone(),
                            });
                            None
                        }
                    },

                    // TODO: maybe allow variants here (checked at runtime)?

                    _ => {
                        self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(format!("cannot get extents for \
                              non-array type {}", expr_ty)),
                            loc: expr.loc.clone(),
                        });
                        None
                    },
                }
            },

            ExprKind::Cast(ref expr, ref ty) => {
                let expr_ty = try_type!(expr);
                if may_cast(expr_ty, ty) {
                    Some(ty.clone())
                } else {
                    self.errors.push(AnalysisError {
                        kind: AnalysisErrorKind::TypeError,
                        regarding: Some(format!("cannot cast expression of type {} \
                          to type {}", expr_ty, ty)),
                        loc: expr.loc.clone(),
                    });
                    None
                }
            },

            // could be anything
            ExprKind::VbExpr(_) => Some(Type::Variant),
        };
    }

    fn visit_allocextent(&mut self, extent: &mut AllocExtent, m: &Ident,
      f: &Ident, loc: &SrcLoc) {
        self.walk_allocextent(extent, m, f, loc);
        self.typecheck_allocextent(extent, loc);
    }
}

impl<'a> TypecheckVisitor<'a> {

    fn typecheck_stmt_shallow(&mut self, stmt: &Stmt, module: &Ident,
      function: &Ident) {
        match stmt.data {
            StmtKind::ExprStmt(ref expr) => {
                match expr.data {
                    ExprKind::Call(_, _, _)
                  | ExprKind::MemberInvoke(_, _, _)
                  | ExprKind::VbExpr(_) => {},

                    _ => self.errors.push(AnalysisError {
                        kind: AnalysisErrorKind::InvalidStmt,
                        regarding: None,
                        loc: expr.loc.clone(),
                    })
                };
            },

            StmtKind::VarDecl(ref decls) => {
                for &(ref ident, ref ty, ref init) in decls {
                    // these should already be gensymmed away
                    if ident == function {
                        panic!("dumpster fire: \
                          variable {} has same name as function", ident);
                    }

                    if let Some(ref init) = *init {
                        let init_ty = try_type!(init);
                        if !may_coerce(&init_ty, &ty) {
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(format!("initializer (of type {}) \
                                  not coercible to declared type {}", init_ty, ty)),
                                loc: stmt.loc.clone(),
                            });
                        }
                    }
                }
            },

            StmtKind::Assign(ref lhs, ref op, ref rhs) => { 
                if !lhs.is_lvalue() {
                    self.errors.push(AnalysisError {
                        kind: AnalysisErrorKind::InvalidStmt,
                        regarding: Some(String::from("assignment to non-lvalue \
                          expression")),
                        loc: lhs.loc.clone(),
                    });
                    return;
                }

                let lhs_ty = try_type!(lhs);
                let rhs_ty = try_type!(rhs);

                if try_collect!(is_constexpr(
                  lhs, &self.symtab, module, Some(function)) => self.errors) {
                    self.errors.push(AnalysisError {
                        kind: AnalysisErrorKind::InvalidStmt,
                        regarding: Some(String::from("assignment to const \
                          expression")),
                        loc: lhs.loc.clone(),
                    });
                    return;
                }

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
                        self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("non-numeric \
                              expression in numeric-operation assignment")),
                            loc: stmt.loc.clone(),
                        });
                    },

                    AssignOp::StrCatAssign
                    if !lhs_ty.might_be_string()
                      || !rhs_ty.might_be_string() => {
                        self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("non-string \
                              expression in string-operation assignment")),
                            loc: stmt.loc.clone(),
                        });
                    },

                    AssignOp::BitAndAssign
                  | AssignOp::BitOrAssign
                    if !lhs_ty.might_be_string()
                      || !rhs_ty.might_be_string() => {
                        self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("non-bitwise \
                              expression in bitwise-operation assignment")),
                            loc: stmt.loc.clone(),
                        });
                    },

                    AssignOp::LogAndAssign
                  | AssignOp::LogOrAssign
                    if !may_coerce(&lhs_ty, &Type::Bool)
                      || !may_coerce(&rhs_ty, &Type::Bool) => {
                        self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("non-boolean \
                              expression in logical-operation assignment")),
                            loc: stmt.loc.clone(),
                        });
                    },

                    _ => {},
                }

                // TODO: do we like this rule in every case?
                if !may_coerce(&rhs_ty, &lhs_ty) {
                    self.errors.push(AnalysisError {
                        kind: AnalysisErrorKind::TypeError,
                        regarding: Some(format!("expression not \
                          coercible to {}", lhs_ty)),
                        loc: stmt.loc.clone(),
                    });
                }
            },

            StmtKind::Return(ref expr) => {
                let fun_path = Path(Some(module.clone()), function.clone());
                // TODO: symbol_at_ident needed here
                if let Symbol::Fun { ref def, .. } = *try_collect!(
                  self.symtab.symbol_at_path(
                      &fun_path,
                      NameCtxt::Function(module, Access::Private),
                      &stmt.loc) => self.errors) {
                    match def.ret {
                        Type::Void => if expr.is_some() {
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::InvalidStmt,
                                regarding: Some(String::from("return with \
                                  value from non-void function")),
                                loc: stmt.loc.clone(),
                            });
                        },

                        ref ret_ty => match *expr {
                            None => self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::InvalidStmt,
                                regarding: Some(String::from("return without \
                                  expression from non-void function")),
                                loc: stmt.loc.clone(),
                            }),

                            Some(ref expr) => {
                                let expr_ty = try_type!(expr);
                                if !may_coerce(&expr_ty, ret_ty) {
                                    self.errors.push(AnalysisError {
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
            },

            StmtKind::IfStmt { ref cond, ref elsifs, .. } => {
                let cond_ty = try_type!(cond);
                if !may_coerce(&cond_ty, &Type::Bool) {
                    self.errors.push(AnalysisError {
                        kind: AnalysisErrorKind::TypeError,
                        regarding: Some(String::from(
                          "condition not coercible to bool")),
                        loc: cond.loc.clone(),
                    });
                }

                for &(ref cond, _) in elsifs {
                    let cond_ty = try_type!(cond);
                    if !may_coerce(&cond_ty, &Type::Bool) {
                        self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from(
                              "condition not coercible to bool")),
                            loc: cond.loc.clone(),
                        });
                    }
                }
            },

            StmtKind::WhileLoop { ref cond, .. } => {
                let cond_ty = try_type!(cond);
                if !may_coerce(cond_ty, &Type::Bool) {
                    self.errors.push(AnalysisError {
                        kind: AnalysisErrorKind::TypeError,
                        regarding: Some(String::from(
                          "condition not coercible to bool")),
                        loc: cond.loc.clone(),
                    });
                }
            },

            StmtKind::ForLoop { var: (ref var, ref ty, ref mode), ref spec, .. } => {
                match *spec {
                    ForSpec::Range(ref from, ref to, ref step) => {
                        if *mode == ParamMode::ByRef {
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::InvalidStmt,
                                regarding: Some(format!("reference variable \
                                  {} cannot be used with for loop over range",
                                  var)),
                                loc: stmt.loc.clone(),
                            });
                        }

                        if !ty.might_be_numeric() {
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(String::from("non-numeric control \
                                  variable in range-based for loop.")),
                                loc: stmt.loc.clone(),
                            });
                        }

                        let to_ty = try_type!(to);
                        let from_ty = try_type!(from);

                        if !from_ty.might_be_numeric()
                          || !to_ty.might_be_numeric() {
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(String::from("non-numeric range \
                                  expression in range-based for loop.")),
                                loc: stmt.loc.clone(),
                            });
                        }

                        if !may_coerce(&from_ty, &to_ty) {
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(String::from("bounds have \
                                  incompatible types in range-based for loop.")),
                                loc: stmt.loc.clone(),
                            });
                        }

                        if let Some(ref step) = *step {
                            let step_ty = try_type!(step);
                            if !step_ty.might_be_numeric() {
                                self.errors.push(AnalysisError {
                                    kind: AnalysisErrorKind::TypeError,
                                    regarding: Some(String::from("non-numeric step \
                                      expression in range-based for loop.")),
                                    loc: stmt.loc.clone(),
                                });
                            }

                            if !may_coerce(&step_ty, &to_ty) {
                                self.errors.push(AnalysisError {
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
                        match *try_type!(expr) {
                            Type::Array(ref base, _) => {
                                match *mode {
                                    ParamMode::ByVal => {
                                        if !may_coerce(base, ty) {
                                            self.errors.push(AnalysisError {
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
                                            self.errors.push(AnalysisError {
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
                                    self.errors.push(AnalysisError {
                                        kind: AnalysisErrorKind::TypeError,
                                        regarding: Some(format!("reference \
                                          variable {} cannot be used to loop over \
                                          expression of object or var type (for now)",
                                          var)),
                                        loc: stmt.loc.clone(),
                                    });
                                }
                            },

                            ref expr_ty => self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(format!("for-each loop \
                                  iteration expression must have array or object \
                                  type; found type {}", expr_ty)),
                                loc: stmt.loc.clone(),
                            })

                        }
                    },

                };
            },

            StmtKind::ForAlong { ref vars, ref along, .. } => {
                let dims = match *try_type!(along) {
                    Type::Array(_, ref bounds) => bounds.dims(),

                    // TODO: maybe allow variants (checked at runtime)?

                    ref along_ty => {
                        self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(format!("for-along loop over \
                              non-array expression of type {}", along_ty)),
                            loc: stmt.loc.clone(),
                        });
                        return;
                    }
                };

                if vars.len() > dims {
                    self.errors.push(AnalysisError {
                        kind: AnalysisErrorKind::TypeError,
                        regarding: Some(format!("for-along loop iteration \
                          variable count ({}) exceeds iterated array \
                          dimension ({})", vars.len(), dims)),
                        loc: stmt.loc.clone(),
                    });
                };
            },

            StmtKind::Alloc(ref expr, ref extents) => {
                match *try_type!(expr) {
                    Type::Array(_, ArrayBounds::Dynamic(dims)) => {
                        let extent_dims = extents.len();

                        if extent_dims == 1 {
                            match extents[0] {
                                AllocExtent::Along(ref other) => {
                                    match *try_type!(other) {
                                        Type::Array(_, ref bounds) => {
                                            if bounds.dims() < dims {
                                                self.errors.push(AnalysisError {
                                                    kind: AnalysisErrorKind::TypeError,
                                                    regarding: Some(format!(
                                                      "along expression does not \
                                                      have enough dimensions ({}) \
                                                      for alloc expression extents \
                                                      ({})", bounds.dims(), dims)),
                                                    loc: stmt.loc.clone(),
                                                });
                                            }

                                            // otherwise, we have a single
                                            // big-enough array
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
                                        match *try_type!(other) {
                                            Type::Array(_, ref bounds) => {
                                                if bounds.dims() <= dim {
                                                    self.errors.push(AnalysisError {
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
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(format!("extents dimensions ({}) \
                                  do not match array dimensions ({}) in alloc",
                                  extent_dims, dims)),
                                loc: stmt.loc.clone(),
                            });
                        }
                    },

                    Type::Array(_, ArrayBounds::Static(_)) => {
                        self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::InvalidStmt,
                            regarding: Some(String::from("attempt to allocate \
                              statically-dimensioned array")),
                            loc: stmt.loc.clone(),
                        });
                    },

                    ref ty => {
                        self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::InvalidStmt,
                            regarding: Some(format!("attempt to allocate \
                              expression of non-array type {}", ty)),
                            loc: stmt.loc.clone(),
                        });
                    },
                }
            },

            StmtKind::ReAlloc(ref expr, preserved, ref extent) => {
                match *try_type!(expr) {
                    Type::Array(_, ArrayBounds::Dynamic(dims)) => {
                        match *extent {
                            AllocExtent::Along(ref expr) => {
                                let expr_ty = try_type!(expr);
                                match *expr_ty {
                                    Type::Array(_, ref bounds) => {
                                        let dims = bounds.dims();
                                        if dims < preserved + 1 {
                                            self.errors.push(AnalysisError {
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
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(format!("extents dimensions ({}) \
                                  do not match array dimensions ({}) in realloc",
                                  preserved + 1, dims)),
                                loc: stmt.loc.clone(),
                            });
                        }
                    },

                    Type::Array(_, ArrayBounds::Static(_)) => {
                        self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::InvalidStmt,
                            regarding: Some(String::from("attempt to \
                              reallocate statically-dimensioned array")),
                            loc: stmt.loc.clone(),
                        });
                    },

                    ref ty => {
                        self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::InvalidStmt,
                            regarding: Some(format!("attempt to reallocate \
                              expression of non-array type {}", ty)),
                            loc: stmt.loc.clone(),
                        });
                    },
                }
            },

            StmtKind::DeAlloc(ref expr) => {
                match *try_type!(expr) {
                    Type::Array(_, ArrayBounds::Dynamic(_)) => { },

                    Type::Array(_, ArrayBounds::Static(_)) => {
                        self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::InvalidStmt,
                            regarding: Some(String::from("attempt to \
                              deallocate statically-dimensioned array")),
                            loc: stmt.loc.clone(),
                        });
                    },

                    ref ty => {
                        self.errors.push(AnalysisError {
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
                    match *try_type!(expr) {
                        Type::Void => self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from("void function \
                              invocation in print statement")),
                            loc: stmt.loc.clone(),
                        }),

                        _ => { },
                    }
                }
            },
        }
    }

    fn typecheck_allocextent(&mut self, extent: &AllocExtent, loc: &SrcLoc) {
        // TODO: maybe allow variants (checked at runtime)?

        match *extent {
            AllocExtent::Along(ref expr) => {
                match *try_type!(expr) {
                    Type::Array(_, _) => { },

                    ref ty => self.errors.push(AnalysisError {
                        kind: AnalysisErrorKind::TypeError,
                        regarding: Some(format!("along expression of non-array \
                          type {}", ty)),
                        loc: loc.clone(),
                    }),
                }
            },

            AllocExtent::Range(ref lb, ref ub) => {
                if let Some(ref lb) = *lb {
                    let extent_ty = try_type!(lb);
                    if !may_coerce(&extent_ty, &Type::Int32) {
                        self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(String::from(
                              "array extent bound not \
                                coercible to i32")),
                            loc: loc.clone(),
                        });
                    }
                }

                let extent_ty = try_type!(ub);
                if !may_coerce(&extent_ty, &Type::Int32) {
                    self.errors.push(AnalysisError {
                        kind: AnalysisErrorKind::TypeError,
                        regarding: Some(String::from(
                          "array extent bound not coercible \
                          to i32")),
                        loc: loc.clone(),
                    });
                }
            },
        }
    }

    fn typecheck_fn_call(&mut self, fun: &FunDef, args: &Vec<Expr>,
      optargs: &Vec<(Ident, Expr)>, invoke_path: &Path,
      invoke_loc: &SrcLoc) {
        if args.len() < fun.params.len() {
            self.errors.push(AnalysisError {
                kind: AnalysisErrorKind::FnCallError,
                regarding: Some(format!("{} requires {} arguments; \
                  {} were provided", invoke_path, fun.params.len(), args.len())),
                loc: invoke_loc.clone(),
            });
            return;
        }

        if !optargs.is_empty() && args.len() > fun.params.len() {
            self.errors.push(AnalysisError {
                kind: AnalysisErrorKind::FnCallError,
                regarding: Some(String::from("positional optional arguments \
                  cannot be mixed with by-name optional arguments (sorry!)")),
                loc: invoke_loc.clone(),
            });
            return;
        }

        if let Some(max_optargs) =
          fun.optparams.as_ref().map(|o| o.max_len()).unwrap_or(Some(0)) {
            if args.len() > fun.params.len() + max_optargs {
                self.errors.push(AnalysisError {
                    kind: AnalysisErrorKind::FnCallError,
                    regarding: Some(format!("{} requires {} arguments{}; \
                      {} were provided", invoke_path,
                      fun.params.len(),
                      if max_optargs != 0 {
                          format!(" (+ {} optional)", max_optargs)
                      } else {
                          String::from("")
                      },
                      args.len())),
                    loc: invoke_loc.clone(),
                });
            }
        }

        for (i, param) in fun.params.iter().enumerate() {
            let arg_type = try_type!(args[i]).decay();

            match param.mode {
                ParamMode::ByRef =>
                    if param.ty != arg_type {
                        self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(format!(
                              "parameter {} has type &{}; type {} provided",
                              param.name, param.ty, arg_type)),
                            loc: args[i].loc.clone(),
                        });
                    },
                ParamMode::ByVal =>
                    if !may_coerce(&arg_type, &param.ty) {
                        self.errors.push(AnalysisError {
                            kind: AnalysisErrorKind::TypeError,
                            regarding: Some(format!(
                              "parameter {} has type {}; type {} provided",
                              param.name, param.ty, arg_type)),
                            loc: args[i].loc.clone(),
                        });
                    },
            }
        }

        if optargs.is_empty() {
            // any optional arguments are positional
            let optargs = &args[fun.params.len()..];
            match fun.optparams {
                Some(FunOptParams::Named(ref optparams)) => {
                    for (i, &(ref param, _)) in optparams.iter().enumerate() {
                        // argument was provided
                        if i < optargs.len() {
                            let arg_ty = try_type!(optargs[i]).decay();

                            match param.mode {
                                ParamMode::ByRef =>
                                    if param.ty != arg_ty {
                                        self.errors.push(AnalysisError {
                                            kind: AnalysisErrorKind::TypeError,
                                            regarding: Some(format!(
                                              "parameter {} has type &{}; type {} \
                                                provided",
                                              param.name, param.ty, arg_ty)),
                                            loc: optargs[i].loc.clone(),
                                        });
                                    },
                                ParamMode::ByVal =>
                                    if !may_coerce(&arg_ty, &param.ty) {
                                        self.errors.push(AnalysisError {
                                            kind: AnalysisErrorKind::TypeError,
                                            regarding: Some(format!(
                                              "parameter {} has type {}; type {} \
                                                provided",
                                              param.name, param.ty, arg_ty)),
                                            loc: optargs[i].loc.clone(),
                                        });
                                    },
                            }
                        }
                    }
                },

                Some(FunOptParams::VarArgs(_, _)) => {
                    for arg in optargs.iter() {
                        let arg_ty = try_type!(arg).decay();
                        if !may_coerce(&arg_ty, &Type::Variant) {
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::TypeError,
                                regarding: Some(format!(
                                    "variadic optional argument has type {}; not \
                                      coercible to var", arg_ty)),
                                loc: arg.loc.clone(),
                            });
                        }
                    }
                },

                None => { },
            }
        } else {
            let optparams = match fun.optparams {
                Some(FunOptParams::Named(ref optparams)) => optparams,
                _ => {
                    self.errors.push(AnalysisError {
                        kind: AnalysisErrorKind::FnCallError,
                        regarding: Some(format!("fn {} called with \
                          named optional arguments; none specified in \
                          function definition",
                          invoke_path)),
                        loc: optargs[0].1.loc.clone(),
                    });
                    return;
                },
            };

            let mut seen = HashSet::new();

            for &(ref argname, ref arg) in optargs {
                let which = optparams.iter().enumerate()
                    .find(|&(_, &(ref param, _))| {
                        match param.name {
                            Ident(ref name, None) => *name == argname.0,
                            Ident(_, Some(ref prev)) => *prev == argname.0,
                        }
                    });

                match which {
                    Some((i, _)) => {
                        if seen.contains(&i) {
                            self.errors.push(AnalysisError {
                                kind: AnalysisErrorKind::DuplicateSymbol,
                                regarding: Some(format!("optional argument {} to \
                                  {} was duplicated", argname, invoke_path)),
                                loc: arg.loc.clone(),
                            });
                        }
                        seen.insert(i);

                        let param = &optparams[i].0;
                        let arg_ty = try_type!(arg).decay();
                        match param.mode {
                            ParamMode::ByRef =>
                                if param.ty != arg_ty {
                                    self.errors.push(AnalysisError {
                                        kind: AnalysisErrorKind::TypeError,
                                        regarding: Some(format!(
                                          "parameter {} has type &{}; type {} \
                                            provided",
                                          param.name, param.ty, arg_ty)),
                                        loc: arg.loc.clone(),
                                    });
                                },
                            ParamMode::ByVal =>
                                if !may_coerce(&arg_ty, &param.ty) {
                                    self.errors.push(AnalysisError {
                                        kind: AnalysisErrorKind::TypeError,
                                        regarding: Some(format!(
                                          "parameter {} has type {}; type {} \
                                            provided",
                                          param.name, param.ty, arg_ty)),
                                        loc: arg.loc.clone(),
                                    });
                                },
                        }
                    },

                    None => self.errors.push(AnalysisError {
                        kind: AnalysisErrorKind::FnCallError,
                        regarding: Some(format!("{} has no optional argument {}",
                          invoke_path, argname)),
                        loc: arg.loc.clone(),
                    }),
                }
            }
        }
    }
}
