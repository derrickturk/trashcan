//! experimental: an AST visitor

use ast::*;

// we're going to take the mut-macro idea from the rustc MIR visitors:
// https://github.com/rust-lang/rust/blob/master/src/librustc/mir/visit.rs
// and the walk_* free function defaults from the libsyntax AST visitor at
// https://github.com/rust-lang/rust/tree/master/src/libsyntax
// that's a lie; now I see why you can't use free functions with the mut-macro;
// hint: concat_idents is useless

#[derive(Copy, Clone, Debug)]
pub enum NameCtxt<'a> {
    Module,
             // module
    Function(Option<&'a Ident>),
         // module
    Type(Option<&'a Ident>),
          // module  // function
    Value(&'a Ident, Option<&'a Ident>),
           // module          // type
    Member(Option<&'a Ident>, Option<&'a Ident>),
}

macro_rules! make_ast_vistor {
    ($trait_name: ident, $($_mut:ident)*) => {
        // can force Sized here or use ASTVisitor + ?Sized everywhere
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

            fn visit_structdef(&mut self, def: & $($_mut)* StructDef,
              module: &Ident) {
                self.walk_structdef(def, module)
            }

            fn visit_structmem(&mut self, m: & $($_mut)* StructMem,
              module: &Ident, st: &Ident) {
                self.walk_structmem(m, module, st)
            }

            fn visit_stmt(&mut self, stmt: & $($_mut)* Stmt,
              module: &Ident, function: &Ident) {
                self.walk_stmt(stmt, module, function)
            }

            fn visit_expr(&mut self, expr: & $($_mut)* Expr,
              module: &Ident, function: &Ident) {
                self.walk_expr(expr, module, function)
            }

            // TODO: move variable into forspec; this might make many
            //   things easier
            fn visit_forspec(&mut self, spec: & $($_mut)* ForSpec,
              module: &Ident, function: &Ident) {
                self.walk_forspec(spec, module, function)
            }

            // TODO: can we ever not be in a function when we walk a path?
            fn visit_path(&mut self, p: & $($_mut)* Path, ctxt: NameCtxt) {
                self.walk_path(p, ctxt)
            }

            fn visit_type(&mut self, ty: & $($_mut)* Type, module: &Ident) {
                // do nothing
            }

            // TODO: UsedAs or IdentCtxt or...?
            fn visit_ident(&mut self, i: & $($_mut)* Ident, ctxt: NameCtxt) {
                // do nothing
            }

            fn visit_literal(&mut self, lit: & $($_mut)* Literal,
              module: &Ident, function: &Ident) {
                // do nothing
            }

            fn visit_vbexpr(&mut self, data: & $($_mut)* Vec<u8>,
              module: &Ident, function: &Ident) {
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

                self.visit_ident(name, NameCtxt::Module);

                match *data {
                    ModuleKind::Normal(ref $($_mut)* items) => {
                        for i in items {
                            self.visit_normal_item(i, name);
                        }
                    },
                }
            }

            fn walk_normal_item(&mut self, i: & $($_mut)* NormalItem,
              module: &Ident) {
                match *i {
                    NormalItem::Function(ref $($_mut)* def) =>
                        self.visit_fundef(def, module),
                    NormalItem::Struct(ref $($_mut)* def) =>
                        self.visit_structdef(def, module),
                }
            }

            fn walk_fundef(&mut self, def: & $($_mut)* FunDef, module: &Ident) {
                let FunDef {
                    ref $($_mut)* name,
                    ref $($_mut)* access,
                    ref $($_mut)* params,
                    ref $($_mut)* ret,
                    ref $($_mut)* body,
                    ref $($_mut)* loc,
                } = *def;

                self.visit_ident(name, NameCtxt::Function(Some(module)));
                for p in params {
                    self.visit_funparam(p, module, name);
                }
                self.visit_type(ret, module);
                for stmt in body {
                    self.visit_stmt(stmt, module, name);
                }
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
                  NameCtxt::Value(module, Some(function)));
                self.visit_type(ty, module);
            }

            fn walk_structdef(&mut self, def: & $($_mut)* StructDef,
              module: &Ident) {
                let StructDef {
                    ref $($_mut)* name,
                    ref $($_mut)* access,
                    ref $($_mut)* members,
                    ref $($_mut)* loc,
                } = *def;

                self.visit_ident(name, NameCtxt::Type(Some(module)));
                for m in members {
                    self.visit_structmem(m, module, name);
                }
            }

            fn walk_structmem(&mut self, m: & $($_mut)* StructMem,
              module: &Ident, st: &Ident) {
                let StructMem {
                    ref $($_mut)* name,
                    ref $($_mut)* ty,
                    ref $($_mut)* loc,
                } = *m;

                self.visit_ident(name,
                  NameCtxt::Member(Some(module), Some(st)));
                self.visit_type(ty, module);
            }

            // TODO: maybe each pattern should have its own visit function
            fn walk_stmt(&mut self, stmt: & $($_mut)* Stmt,
              module: &Ident, function: &Ident) {
                let Stmt {
                    ref $($_mut)* data,
                    ref $($_mut)* loc,
                } = *stmt;

                match *data {
                    StmtKind::ExprStmt(ref $($_mut)* expr) =>
                        self.visit_expr(expr, module, function),

                    StmtKind::VarDecl(ref $($_mut)* decls) => {
                        for & $($_mut)* (
                            ref $($_mut)* ident,
                            ref $($_mut)* ty,
                            ref $($_mut)* init
                        ) in decls {
                            self.visit_ident(ident,
                              NameCtxt::Value(module, Some(function)));
                            self.visit_type(ty, module);
                            match *init {
                                Some(ref $($_mut)* init) =>
                                    self.visit_expr(init, module, function),
                                None => {},
                            }
                        }
                    },

                    StmtKind::Assign(
                        ref $($_mut)* lhs,
                        _,
                        ref $($_mut)* rhs
                    ) => {
                        self.visit_expr(lhs, module, function);
                        self.visit_expr(rhs, module, function);
                    },

                    StmtKind::Return(Some(ref $($_mut)* expr)) =>
                        self.visit_expr(expr, module, function),

                    StmtKind::Return(None) => {},

                    StmtKind::IfStmt {
                        ref $($_mut)* cond,
                        ref $($_mut)* body,
                        ref $($_mut)* elsifs,
                        ref $($_mut)* els,
                    } => {
                        self.visit_expr(cond, module, function);
                        for stmt in body {
                            self.visit_stmt(stmt, module, function);
                        }

                        for & $($_mut)* (
                            ref $($_mut)* cond,
                            ref $($_mut)* body
                        ) in elsifs {
                            self.visit_expr(cond, module, function);
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
                        self.visit_expr(cond, module, function);
                        for stmt in body {
                            self.visit_stmt(stmt, module, function);
                        }
                    },

                    StmtKind::ForLoop {
                        ref $($_mut)* var,
                        ref $($_mut)* spec,
                        ref $($_mut)* body,
                    } => {
                        let (ref $($_mut)* ident, ref $($_mut)* ty) = *var;
                        self.visit_ident(ident,
                          NameCtxt::Value(module, Some(function)));
                        self.visit_type(ty, module);
                        self.visit_forspec(spec, module, function);
                        for stmt in body {
                            self.visit_stmt(stmt, module, function);
                        }
                    },

                    StmtKind::Print(ref $($_mut)* expr) =>
                        self.visit_expr(expr, module, function),
                }
            }

            // TODO: maybe each pattern should have its own visit function
            fn walk_expr(&mut self, expr: & $($_mut)* Expr,
              module: &Ident, function: &Ident) {
                let Expr {
                    ref $($_mut)* data,
                    ref $($_mut)* loc,
                } = *expr;

                match *data {
                    ExprKind::Lit(ref $($_mut)* lit) =>
                        self.visit_literal(lit, module, function),

                    ExprKind::Name(ref $($_mut)* path) =>
                        self.visit_path(path,
                          NameCtxt::Value(module, Some(function))),

                    ExprKind::Index(
                        ref $($_mut)* expr,
                        ref $($_mut)* indices
                    ) => {
                        self.visit_expr(expr, module, function);
                        for i in indices {
                            self.visit_expr(i, module, function);
                        }
                    },

                    ExprKind::Call(ref $($_mut)* path, ref $($_mut)* args) => {
                        self.visit_path(path, NameCtxt::Function(Some(module)));
                        for a in args {
                            self.visit_expr(a, module, function);
                        }
                    },

                    ExprKind::Member(
                        ref $($_mut)* expr,
                        ref $($_mut)* ident,
                    ) => {
                        self.visit_expr(expr, module, function);
                        // TODO: do we want to do this? it doesn't really fit
                        //   the pattern
                        self.visit_ident(ident,
                          NameCtxt::Member(Some(module), None));
                    },

                    ExprKind::MemberInvoke(
                        ref $($_mut)* expr,
                        ref $($_mut)* ident,
                        ref $($_mut)* args,
                    ) => {
                        self.visit_expr(expr, module, function);
                        // TODO: do we want to do this? it doesn't really fit
                        //   the pattern
                        self.visit_ident(ident,
                          NameCtxt::Member(Some(module), None));
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

                    ExprKind::VbExpr(ref $($_mut)* data) =>
                        self.visit_vbexpr(data, module, function),
                }
            }

            fn walk_forspec(&mut self, spec: & $($_mut)* ForSpec,
              module: &Ident, function: &Ident) {
                match *spec {
                    ForSpec::Range(
                        ref $($_mut)* from,
                        ref $($_mut)* to,
                        ref $($_mut)* step,
                    ) => {
                        self.visit_expr(from, module, function);
                        self.visit_expr(to, module, function);
                        match *step {
                            Some(ref $($_mut)* step) =>
                                self.visit_expr(step, module, function),
                            None => {},
                        }
                    },

                    ForSpec::Each(ref $($_mut)* of) =>
                        self.visit_expr(of, module, function)
                }
            }

            fn walk_path(&mut self, p: & $($_mut)* Path, ctxt: NameCtxt) {
                match *p {
                    Path(Some(ref $($_mut)* m), ref $($_mut)* i) => {
                        self.visit_ident(m, NameCtxt::Module);
                        let inner_ctxt = match ctxt {
                            NameCtxt::Module =>
                                panic!("internal compiler error: path as \
                                       module name"),
                            NameCtxt::Function(_) =>
                                NameCtxt::Function(Some(m)),
                            NameCtxt::Type(_) =>
                                NameCtxt::Type(Some(m)),
                            NameCtxt::Value(_, _) =>
                                NameCtxt::Value(m, None),
                            NameCtxt::Member(_, _) =>
                                panic!("internal compiler error: path as \
                                       member name"),
                        };
                        self.visit_ident(i, inner_ctxt);
                    },

                    Path(None, ref $($_mut)* i) => {
                        self.visit_ident(i, ctxt);
                    },
                }
            }
        }
    }
}

make_ast_vistor!(ASTVisitor,);
make_ast_vistor!(ASTVisitorMut, mut);
