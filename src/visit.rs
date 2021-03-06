//! experimental: an AST visitor

use ast::*;
use parser::SrcLoc;

// we're going to take the mut-macro idea from the rustc MIR visitors:
// https://github.com/rust-lang/rust/blob/master/src/librustc/mir/visit.rs
// and the walk_* free function defaults from the libsyntax AST visitor at
// https://github.com/rust-lang/rust/tree/master/src/libsyntax
// that's a lie; now I see why you can't use free functions with the mut-macro;
// hint: concat_idents is useless

#[derive(Copy, Clone, Debug)]
pub enum NameCtxt<'a> {
    // definition contexts: we know where we are

    DefModule,

                // module in which definition occurs
    DefFunction(&'a Ident),

            // module in which definition occurs
    DefType(&'a Ident),

             // module  // function (may be Option once globals happen)
    DefValue(&'a Ident, Option<&'a Ident>, &'a Type, Access),
                                           // type,  access of value

             // module  // function          // parameter mode
    DefParam(&'a Ident, &'a Ident, &'a Type, ParamMode),
                                   // parameter type

                // module  // type of constant
    DefConstant(&'a Ident, &'a Type, Access),
                                     // access of constant

              // module  // enclosing type
    DefMember(&'a Ident, &'a Ident, &'a Type),
                                    // type of member

    // lookup contexts: we are looking for something, from somewhere

    Module,

             // module from which lookup happens
    Function(&'a Ident, Access),
                       // access with which we can see (Public or Private ...)

         // module
    Type(&'a Ident, Access),
                   // access (Public = from other module ...)

          // module  // function        // (Private = from same module ...)
    Value(&'a Ident, Option<&'a Ident>, Access),

           // module  // we might know the type
    Member(&'a Ident, Option<&'a Ident>, Access),
                                         // access

    // these should never be altered
    OptArgName,
}

macro_rules! make_ast_vistor {
    ($trait_name: ident, $($_mut:ident)*) => {
        pub trait $trait_name {
            fn visit_dumpster(&mut self, d: & $($_mut)* Dumpster) {
                self.walk_dumpster(d)
            }

            fn visit_module(&mut self, m: & $($_mut)* Module) {
                self.walk_module(m)
            }

            fn visit_normal_item(&mut self, i: & $($_mut)* NormalItem,
              module: &Ident) {
                self.walk_normal_item(i, module)
            }

            fn visit_fundef(&mut self, def: & $($_mut)* FunDef,
              module: &Ident) {
                self.walk_fundef(def, module)
            }

            fn visit_funparam(&mut self, param: & $($_mut)* FunParam,
              module: &Ident, function: &Ident) {
                self.walk_funparam(param, module, function)
            }

            fn visit_optparams(&mut self, params: & $($_mut)* FunOptParams,
              module: &Ident, function: &Ident) {
                self.walk_optparams(params, module, function)
            }

            fn visit_optparam(&mut self, param: & $($_mut)* (FunParam, Literal),
              module: &Ident, function: &Ident) {
                self.walk_optparam(param, module, function)
            }

            fn visit_structdef(&mut self, def: & $($_mut)* StructDef,
              module: &Ident) {
                self.walk_structdef(def, module)
            }

            fn visit_structmem(&mut self, m: & $($_mut)* StructMem,
              module: &Ident, st: &Ident) {
                self.walk_structmem(m, module, st)
            }

            fn visit_static(&mut self, s: & $($_mut)* Static, module: &Ident) {
                self.walk_static(s, module)
            }

            fn visit_constant(&mut self, c: & $($_mut)* Constant,
              module: &Ident) {
                self.walk_constant(c, module)
            }

            fn visit_stmt(&mut self, stmt: & $($_mut)* Stmt,
              module: &Ident, function: &Ident) {
                self.walk_stmt(stmt, module, function)
            }

            fn visit_expr(&mut self, expr: & $($_mut)* Expr,
              module: &Ident, function: Option<&Ident>) {
                self.walk_expr(expr, module, function)
            }

            // TODO: move variable into forspec; this might make many
            //   things easier
            fn visit_forspec(&mut self, spec: & $($_mut)* ForSpec,
              module: &Ident, function: &Ident, loc: &SrcLoc) {
                self.walk_forspec(spec, module, function, loc)
            }

            fn visit_allocextent(&mut self, extent: & $($_mut)* AllocExtent,
              module: &Ident, function: &Ident, loc: &SrcLoc) {
                self.walk_allocextent(extent, module, function, loc)
            }

            // TODO: can we ever not be in a function when we walk a path?
            fn visit_path(&mut self, p: & $($_mut)* Path, ctxt: NameCtxt,
              loc: &SrcLoc) {
                self.walk_path(p, ctxt, loc)
            }

            fn visit_type(&mut self, ty: & $($_mut)* Type, module: &Ident,
              loc: &SrcLoc) {
                self.walk_type(ty, module, loc)
            }

            fn visit_ident(&mut self, _i: & $($_mut)* Ident, _ctxt: NameCtxt,
              _loc: &SrcLoc) {
                // do nothing
            }

            fn visit_literal(&mut self, _lit: & $($_mut)* Literal,
              _module: &Ident, _function: Option<&Ident>, _loc: &SrcLoc) {
                // do nothing
            }

            fn visit_vbexpr(&mut self, _data: & $($_mut)* Vec<u8>,
              _module: &Ident, _function: Option<&Ident>, _loc: &SrcLoc) {
                // do nothing
            }

            fn visit_arraybounds(&mut self, _bounds: & $($_mut)* ArrayBounds,
              _base_ty: & $($_mut)* Type, _module: &Ident, _loc: &SrcLoc) {
                // do nothing
            }

            fn visit_srcloc(&mut self, _srcloc: & $($_mut)* SrcLoc) {
                // do nothing
            }

            // BELOW THIS LINE
            //   do not override; these functions provide tree traversal

            fn walk_dumpster(&mut self, d: & $($_mut)* Dumpster) {
                for m in & $($_mut)* d.modules {
                    self.visit_module(m);
                }
            }

            fn walk_module(&mut self, m: & $($_mut)* Module) {
                let Module {
                    ref $($_mut)* name,
                    ref $($_mut)* data,
                    ref $($_mut)* loc,
                } = *m;

                self.visit_ident(name, NameCtxt::DefModule, loc);

                match *data {
                    ModuleKind::Normal(ref $($_mut)* items) => {
                        for i in items {
                            self.visit_normal_item(i, name);
                        }
                    },
                }

                self.visit_srcloc(loc);
            }

            fn walk_normal_item(&mut self, i: & $($_mut)* NormalItem,
              module: &Ident) {
                match *i {
                    NormalItem::Function(ref $($_mut)* def) =>
                        self.visit_fundef(def, module),
                    NormalItem::Struct(ref $($_mut)* def) =>
                        self.visit_structdef(def, module),
                    NormalItem::Static(ref $($_mut)* def) =>
                        self.visit_static(def, module),
                    NormalItem::Const(ref $($_mut)* def) =>
                        self.visit_constant(def, module),
                }
            }

            fn walk_fundef(&mut self, def: & $($_mut)* FunDef, module: &Ident) {
                let FunDef {
                    ref $($_mut)* name,
                    access: ref $($_mut)* _access,
                    ref $($_mut)* params,
                    ref $($_mut)* optparams,
                    ref $($_mut)* ret,
                    ref $($_mut)* body,
                    ref $($_mut)* loc,
                } = *def;

                self.visit_ident(name, NameCtxt::DefFunction(module), loc);

                for p in params {
                    self.visit_funparam(p, module, name);
                }

                match *optparams {
                    None => { },
                    Some(ref $($_mut)* optparams) =>
                        self.visit_optparams(optparams, module, name),
                };

                self.visit_type(ret, module, loc);

                for stmt in body {
                    self.visit_stmt(stmt, module, name);
                }

                self.visit_srcloc(loc);
            }

            fn walk_funparam(&mut self, param: & $($_mut)* FunParam,
              module: &Ident, function: &Ident) {
                let FunParam {
                    ref $($_mut)* name,
                    ref $($_mut)* ty,
                    ref $($_mut)* mode,
                    ref $($_mut)* loc,
                } = *param;

                self.visit_ident(name,
                  NameCtxt::DefParam(module, function, ty, *mode), loc);
                self.visit_type(ty, module, loc);
                self.visit_srcloc(loc);
            }

            fn walk_optparams(&mut self, params: & $($_mut)* FunOptParams,
              module: &Ident, function: &Ident) {
                match *params {
                    FunOptParams::VarArgs(
                        ref $($_mut)* name,
                        ref $($_mut)* loc
                    ) => {
                        self.visit_ident(name,
                          NameCtxt::DefParam(module, function,
                            &Type::VarArgsArray,
                            ParamMode::ByRef
                          ),
                          loc);
                        self.visit_srcloc(loc);
                    },

                    FunOptParams::Named(ref $($_mut)* optparams) => {
                        for p in optparams {
                            self.visit_optparam(p, module, function);
                        }
                    },
                }
            }

            fn walk_optparam(&mut self, param: & $($_mut)* (FunParam, Literal),
              module: &Ident, function: &Ident) {
                let (ref $($_mut)* param, ref $($_mut)* default) = *param;
                self.visit_funparam(param, module, function);
                self.visit_literal(default, module, Some(function), &param.loc);
            }

            fn walk_structdef(&mut self, def: & $($_mut)* StructDef,
              module: &Ident) {
                let StructDef {
                    ref $($_mut)* name,
                    access: ref $($_mut)* _access,
                    ref $($_mut)* members,
                    ref $($_mut)* loc,
                } = *def;

                self.visit_ident(name, NameCtxt::DefType(module), loc);
                for m in members {
                    self.visit_structmem(m, module, name);
                }

                self.visit_srcloc(loc);
            }

            fn walk_structmem(&mut self, m: & $($_mut)* StructMem,
              module: &Ident, st: &Ident) {
                let StructMem {
                    ref $($_mut)* name,
                    ref $($_mut)* ty,
                    ref $($_mut)* loc,
                } = *m;

                self.visit_ident(name,
                  NameCtxt::DefMember(module, st, ty), loc);
                self.visit_type(ty, module, loc);
                self.visit_srcloc(loc);
            }

            fn walk_static(&mut self, s: & $($_mut)* Static, module: &Ident) {
                let Static {
                    ref $($_mut)* name,
                    ref $($_mut)* access,
                    ref $($_mut)* ty,
                    ref $($_mut)* init,
                    ref $($_mut)* loc,
                } = *s;

                self.visit_ident(name,
                  NameCtxt::DefValue(module, None, ty, *access), loc);
                self.visit_type(ty, module, loc);
                if let Some(ref $($_mut)* init) = *init {
                    self.visit_literal(init, module, None, loc);
                }
                self.visit_srcloc(loc);
            }

            fn walk_constant(&mut self, c: & $($_mut)* Constant,
              module: &Ident) {
                let Constant {
                    ref $($_mut)* name,
                    ref $($_mut)* access,
                    ref $($_mut)* ty,
                    ref $($_mut)* value,
                    ref $($_mut)* loc,
                } = *c;

                self.visit_ident(name,
                  NameCtxt::DefConstant(module, ty, *access), loc);
                self.visit_type(ty, module, loc);
                self.visit_literal(value, module, None, loc);
                self.visit_srcloc(loc);
            }

            fn walk_stmt(&mut self, stmt: & $($_mut)* Stmt,
              module: &Ident, function: &Ident) {
                let Stmt {
                    ref $($_mut)* data,
                    ref $($_mut)* loc,
                } = *stmt;

                match *data {
                    StmtKind::ExprStmt(ref $($_mut)* expr) =>
                        self.visit_expr(expr, module, Some(function)),

                    StmtKind::VarDecl(ref $($_mut)* decls) => {
                        for & $($_mut)* (
                            ref $($_mut)* ident,
                            ref $($_mut)* ty,
                            ref $($_mut)* init
                        ) in decls {
                            self.visit_ident(ident,
                              NameCtxt::DefValue(module, Some(function),
                                ty, Access::Private),
                              loc);
                            self.visit_type(ty, module, loc);
                            match *init {
                                Some(ref $($_mut)* init) =>
                                    self.visit_expr(init, module,
                                      Some(function)),
                                None => {},
                            }
                        }
                    },

                    StmtKind::Assign(
                        ref $($_mut)* lhs,
                        _,
                        ref $($_mut)* rhs
                    ) => {
                        self.visit_expr(lhs, module, Some(function));
                        self.visit_expr(rhs, module, Some(function));
                    },

                    StmtKind::Return(Some(ref $($_mut)* expr)) =>
                        self.visit_expr(expr, module, Some(function)),

                    StmtKind::Return(None) => {},

                    StmtKind::IfStmt {
                        ref $($_mut)* cond,
                        ref $($_mut)* body,
                        ref $($_mut)* elsifs,
                        ref $($_mut)* els,
                    } => {
                        self.visit_expr(cond, module, Some(function));
                        for stmt in body {
                            self.visit_stmt(stmt, module, function);
                        }

                        for & $($_mut)* (
                            ref $($_mut)* cond,
                            ref $($_mut)* body
                        ) in elsifs {
                            self.visit_expr(cond, module, Some(function));
                            for stmt in body {
                                self.visit_stmt(stmt, module, function);
                            }
                        }

                        match *els {
                            Some(ref $($_mut)* body) => {
                                for stmt in body {
                                    self.visit_stmt(stmt, module, function);
                                }
                            },
                            None => {},
                        }
                    },

                    StmtKind::WhileLoop {
                        ref $($_mut)* cond,
                        ref $($_mut)* body,
                    } => {
                        self.visit_expr(cond, module, Some(function));
                        for stmt in body {
                            self.visit_stmt(stmt, module, function);
                        }
                    },

                    StmtKind::ForLoop {
                        ref $($_mut)* var,
                        ref $($_mut)* spec,
                        ref $($_mut)* body,
                    } => {
                        let (
                            ref $($_mut)* ident,
                            ref $($_mut)* ty,
                            ref $($_mut)* _mode
                        ) = *var;
                        self.visit_ident(ident,
                          NameCtxt::DefValue(module, Some(function), ty,
                            Access::Private), loc);
                        self.visit_type(ty, module, loc);
                        self.visit_forspec(spec, module, function, loc);
                        for stmt in body {
                            self.visit_stmt(stmt, module, function);
                        }
                    },

                    StmtKind::ForAlong {
                        ref $($_mut)* vars,
                        ref $($_mut)* along,
                        ref $($_mut)* body,
                    } => {
                        for var in vars {
                            self.visit_ident(var,
                              NameCtxt::DefValue(module, Some(function),
                                &Type::Int32, Access::Private),
                              loc);
                        }

                        self.visit_expr(along, module, Some(function));

                        for stmt in body {
                            self.visit_stmt(stmt, module, function);
                        }
                    },

                    StmtKind::Alloc(
                        ref $($_mut)* expr,
                        ref $($_mut)* extents
                    ) => {
                        self.visit_expr(expr, module, Some(function));
                        for extent in extents {
                            self.visit_allocextent(extent, module,
                              function, loc);
                        }
                    },

                    StmtKind::ReAlloc(
                        ref $($_mut)* expr,
                        ref $($_mut)* _preserved,
                        ref $($_mut)* extent
                    ) => {
                        self.visit_expr(expr, module, Some(function));
                        self.visit_allocextent(extent, module,
                          function, loc);
                    },

                    StmtKind::DeAlloc(ref $($_mut)* expr) => {
                        self.visit_expr(expr, module, Some(function));
                    },

                    StmtKind::Print(ref $($_mut)* exprs) => {
                        for expr in exprs {
                            self.visit_expr(expr, module, Some(function));
                        }
                    }
                }

                self.visit_srcloc(loc);
            }

            fn walk_expr(&mut self, expr: & $($_mut)* Expr,
              module: &Ident, function: Option<&Ident>) {
                let Expr {
                    ref $($_mut)* data,
                    ref $($_mut)* ty,
                    ref $($_mut)* loc,
                } = *expr;

                match *data {
                    ExprKind::Lit(ref $($_mut)* lit) =>
                        self.visit_literal(lit, module, function, loc),

                    ExprKind::Name(ref $($_mut)* path) => self.visit_path(
                        path,
                        NameCtxt::Value(module, function, Access::Private),
                        loc),

                    ExprKind::Index(
                        ref $($_mut)* expr,
                        ref $($_mut)* indices
                    ) => {
                        self.visit_expr(expr, module, function);
                        for i in indices {
                            self.visit_expr(i, module, function);
                        }
                    },

                    ExprKind::Call(
                        ref $($_mut)* path,
                        ref $($_mut)* args,
                        ref $($_mut)* optargs
                    ) => {
                        self.visit_path(path,
                          NameCtxt::Function(module, Access::Private), loc);
                        for a in args {
                            self.visit_expr(a, module, function);
                        }
                        for & $($_mut)* (ref $($_mut)* i, ref $($_mut)* e)
                          in optargs {
                            self.visit_ident(i, NameCtxt::OptArgName, &loc);
                            self.visit_expr(e, module, function);
                        }
                    },

                    ExprKind::Member(
                        ref $($_mut)* expr,
                        ref $($_mut)* ident,
                    ) => {
                        self.visit_expr(expr, module, function);
                        self.visit_ident(ident,
                          NameCtxt::Member(module, None, Access::Private), loc);
                    },

                    ExprKind::MemberInvoke(
                        ref $($_mut)* expr,
                        ref $($_mut)* ident,
                        ref $($_mut)* args,
                    ) => {
                        self.visit_expr(expr, module, function);
                        self.visit_ident(ident,
                          NameCtxt::Member(module, None, Access::Private), loc);
                        for a in args {
                            self.visit_expr(a, module, function);
                        }
                    },

                    ExprKind::UnOpApp(ref $($_mut)* expr, _) =>
                        self.visit_expr(expr, module, function),

                    ExprKind::BinOpApp(
                        ref $($_mut)* lhs,
                        ref $($_mut)* rhs,
                        _
                    ) => {
                        self.visit_expr(lhs, module, function);
                        self.visit_expr(rhs, module, function);
                    },

                    ExprKind::CondExpr {
                        ref $($_mut)* cond,
                        ref $($_mut)* if_expr,
                        ref $($_mut)* else_expr,
                    } => {
                        self.visit_expr(cond, module, function);
                        self.visit_expr(if_expr, module, function);
                        self.visit_expr(else_expr, module, function);
                    },

                    ExprKind::ExtentExpr(
                        ref $($_mut)* expr,
                        ref $($_mut)* _kind,
                        ref $($_mut)* _dim
                    ) => {
                        self.visit_expr(expr, module, function);
                    },

                    ExprKind::Cast(
                        ref $($_mut)* expr,
                        ref $($_mut)* ty,
                    ) => {
                        self.visit_expr(expr, module, function);
                        self.visit_type(ty, module, & $($_mut)* expr.loc);
                    },

                    ExprKind::VbExpr(ref $($_mut)* data) =>
                        self.visit_vbexpr(data, module, function, loc),
                }

                if let Some(ref $($_mut)* ty) = *ty {
                    self.visit_type(ty, module, &loc);
                }

                self.visit_srcloc(loc);
            }

            fn walk_forspec(&mut self, spec: & $($_mut)* ForSpec,
              module: &Ident, function: &Ident, _loc: &SrcLoc) {
                match *spec {
                    ForSpec::Range(
                        ref $($_mut)* from,
                        ref $($_mut)* to,
                        ref $($_mut)* step,
                    ) => {
                        self.visit_expr(from, module, Some(function));
                        self.visit_expr(to, module, Some(function));
                        match *step {
                            Some(ref $($_mut)* step) =>
                                self.visit_expr(step, module, Some(function)),
                            None => {},
                        }
                    },

                    ForSpec::Each(ref $($_mut)* of) =>
                        self.visit_expr(of, module, Some(function))
                }
            }

            fn walk_allocextent(&mut self, extent: & $($_mut)* AllocExtent,
              module: &Ident, function: &Ident, _loc: &SrcLoc) {
                match *extent {
                    AllocExtent::Along(ref $($_mut)* expr) => {
                        self.visit_expr(expr, module, Some(function));
                    },

                    AllocExtent::Range(
                        ref $($_mut)* lb,
                        ref $($_mut)* ub
                    ) => {
                        match *lb {
                            Some(ref $($_mut)* lb) =>
                                self.visit_expr(lb, module, Some(function)),
                            None => {},
                        };
                        self.visit_expr(ub, module, Some(function));
                    },
                };
            }

            fn walk_path(&mut self, p: & $($_mut)* Path, ctxt: NameCtxt,
              loc: &SrcLoc) {
                if let Some(ref $($_mut)* module) = p.0 {
                    self.visit_ident(module, NameCtxt::Module, loc);
                }
                let (i, inner_ctxt) = self.ident_ctxt_from_path(p, ctxt);
                self.visit_ident(i, inner_ctxt, loc);
            }

            fn walk_type(&mut self, ty: & $($_mut)* Type, module: &Ident,
              loc: &SrcLoc) {
                match *ty {
                    Type::Array(ref $($_mut)* base, ref $($_mut)* bounds) => {
                        self.visit_type(base, module, loc);
                        self.visit_arraybounds(bounds, base, module, loc);
                    },

                    Type::Struct(ref $($_mut)* path) => {
                        self.visit_path(path,
                          NameCtxt::Type(module, Access::Private), loc)
                    },

                    Type::Object(ref $($_mut)* path) => {
                        self.visit_path(path,
                          NameCtxt::Type(module, Access::Private), loc)
                    },

                    Type::Deferred(ref $($_mut)* path) => {
                        self.visit_path(path,
                          NameCtxt::Type(module, Access::Private), loc)
                    },

                    _ => {},
                }
            }

            // this doesn't use self at all, but can't be a free function
            //   because blah blah mut blah blah concat_idents whatever
            // TODO: the lifetime seems super sketchy here but it satisfies
            //   the borrow checker
            fn ident_ctxt_from_path<'a>(&mut self, p: &'a $($_mut)* Path,
              ctxt: NameCtxt<'a>) -> (&'a $($_mut)* Ident, NameCtxt<'a>) {
                match *p {
                    Path(Some(ref $($_mut)* path_m), ref $($_mut)* i) => {
                        let inner_ctxt = match ctxt {
                            NameCtxt::Module | NameCtxt::DefModule =>
                                panic!("dumpster fire: path as module name"),

                            NameCtxt::DefFunction(_)
                          | NameCtxt::DefType(_)
                          | NameCtxt::DefValue(_, _, _, _)
                          | NameCtxt::DefParam(_, _, _, _)
                          | NameCtxt::DefConstant(_, _, _)
                          | NameCtxt::DefMember(_, _, _) =>
                                panic!("dumpster fire: path as name \
                                       definition"),

                            NameCtxt::OptArgName =>
                                panic!("dumpster fire: path as optional \
                                       argument name"),

                            // TODO: do we want any notion of "inheriting"
                            //   access from the original lookup?
                            // can this ever possibly matter?
                            NameCtxt::Function(lookup_m, _) =>
                                NameCtxt::Function(
                                    path_m,
                                    if path_m == lookup_m {
                                        Access::Private
                                    } else {
                                        Access::Public
                                    }),

                            NameCtxt::Type(lookup_m, _) =>
                                NameCtxt::Type(
                                    path_m,
                                    if path_m == lookup_m {
                                        Access::Private
                                    } else {
                                        Access::Public
                                    }),

                            // you can't designate a function local by a
                            //   two-part path, so we're ok to drop the lookup
                            //   function from the context
                            NameCtxt::Value(lookup_m, _, _) =>
                                NameCtxt::Value(
                                    path_m,
                                    None,
                                    if path_m == lookup_m {
                                        Access::Private
                                    } else {
                                        Access::Public
                                    }),

                            NameCtxt::Member(_, _, _) =>
                                panic!("dumpster fire: path as member name"),
                        };
                        (i, inner_ctxt)
                    },

                    Path(None, ref $($_mut)* i) => {
                        (i, ctxt)
                    },
                }
            }
        }
    }
}

make_ast_vistor!(ASTVisitor,);
make_ast_vistor!(ASTVisitorMut, mut);
