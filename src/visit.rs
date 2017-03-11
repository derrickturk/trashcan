//! experimental: an AST visitor

use ast::*;

// we're going to take the mut-macro idea from the rustc MIR visitors:
// https://github.com/rust-lang/rust/blob/master/src/librustc/mir/visit.rs
// and the walk_* free function defaults from the libsyntax AST visitor at
// https://github.com/rust-lang/rust/tree/master/src/libsyntax
// that's a lie; now I see why you can't use free functions with the mut-macro;
// hint: concat_idents is useless

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

            fn visit_path(&mut self, p: & $($_mut)* Path, module: &Ident,
              function: Option<&Ident>) {
                self.walk_path(p, module, function)
            }

            fn visit_type(&mut self, ty: & $($_mut)* Type, module: &Ident) {
                // do nothing
            }

            fn visit_ident(&mut self, i: & $($_mut)* Ident,
              module: Option<&Ident>, function: Option<&Ident>,
              typename: Option<&Ident>) {
                // do nothing
            }

            fn visit_literal(&mut self, lit: & $($_mut)* Literal,
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

                self.visit_ident(name, None, None, None);

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

                self.visit_ident(name, Some(module), None, None);
                for p in params {
                    self.visit_funparam(p, module, name);
                }
                self.visit_type(ret, module);
            }

            fn walk_funparam(&mut self, param: & $($_mut)* FunParam,
              module: &Ident, function: &Ident) {
                let FunParam {
                    ref $($_mut)* name,
                    ref $($_mut)* ty,
                    ref $($_mut)* mode,
                    ref $($_mut)* loc,
                } = *param;

                self.visit_ident(name, Some(module), Some(function), None);
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

                self.visit_ident(name, Some(module), None, None);
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

                self.visit_ident(name, Some(module), None, Some(st));
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
                            self.visit_ident(ident, Some(module),
                              Some(function), None);
                            self.visit_type(ty, module);
                            match *init {
                                Some(ref $($_mut)* init) =>
                                    self.visit_expr(init, module, function),
                                None => {},
                            }
                        }
                    },

                    _ => unimplemented!(),
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

                    _ => unimplemented!(),
                }
            }

            fn walk_path(&mut self, p: & $($_mut)* Path, module: &Ident,
              function: Option<&Ident>) {
                match *p {
                    Path(Some(ref $($_mut)* m), ref $($_mut)* i) => {
                        self.visit_ident(m, None, None, None);
                        self.visit_ident(i, Some(m), function, None);
                    },

                    Path(None, ref $($_mut)* i) => {
                        self.visit_ident(i, Some(module), function, None);
                    },
                }
            }
        }
    }
}

make_ast_vistor!(ASTVisitor,);
make_ast_vistor!(ASTVisitorMut, mut);