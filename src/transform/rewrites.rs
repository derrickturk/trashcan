use ast::*;
use analysis::*;
use super::gensym::*;
use parser::SrcLoc;

use fold;
use fold::ASTFolder;

/// combine multiple dumpsters into one
pub fn merge_dumpsters(dumpsters: Vec<Dumpster>) -> Dumpster {
    Dumpster {
        modules: dumpsters.into_iter()
            .flat_map(|d| d.modules.into_iter()).collect()
    }
}

/// replace casts with other expressions where necessary
pub fn cast_rewrite(dumpster: Dumpster, _symtab: &mut SymbolTable) -> Dumpster {
    let mut f = CastRewriteFolder { };
    f.fold_dumpster(dumpster)
}

/// replace logical-op expressions (and conditions) with short-circuiting
/// equivalents
pub fn short_circuit_logicals(dumpster: Dumpster, symtab: &mut SymbolTable)
  -> Dumpster {
    let mut f = ShortCircuitLogicalsFolder::new(symtab);
    f.fold_dumpster(dumpster)
}

/// replace for-each on arrays with equivalent range loops
pub fn array_loop_rewrite(dumpster: Dumpster, symtab: &mut SymbolTable)
  -> Dumpster {
    let mut f = ArrayLoopRewriteFolder::new(symtab);
    f.fold_dumpster(dumpster)
}

/// replace for-along loops with equivalent nested range loops
pub fn along_loop_rewrite(dumpster: Dumpster) -> Dumpster {
    let mut f = AlongLoopRewriteFolder::new();
    f.fold_dumpster(dumpster)
}

/// rewrite alloc-along exprs to equivalent range exprs
pub fn alloc_along_rewrite(dumpster: Dumpster, _symtab: &mut SymbolTable)
  -> Dumpster {
    let mut f = AllocAlongRewriteFolder;
    f.fold_dumpster(dumpster)
}

struct CastRewriteFolder {
    // we'll need these for object-type gensyms
    // symtab: &'a mut SymbolTable,
    // before_stmt_stack: Vec<Vec<Stmt>>,
}

impl CastRewriteFolder {
    /*
    fn new(symtab: &'a mut SymbolTable) -> Self {
        CastRewriteFolder {
            symtab,
            // before_stmt_stack: Vec::new(),
        }
    }
    */
}

impl ASTFolder for CastRewriteFolder {
    fn fold_expr(&mut self, expr: Expr, module: &Ident,
      function: Option<&Ident>) -> Expr {
        let Expr { data, ty, loc } =
            fold::noop_fold_expr(self, expr, module, function);

        let data = match data {
            ExprKind::Cast(expr, cast_ty) => {
                let expr_ty = expr.ty.as_ref()
                  .expect("dumpster fire: untyped expression \
                          in cast rewriter")
                  .clone(); // TODO: this is stupid and only to satisfy the
                            //   borrow checker

                // no-op cast: just lift out the expression
                if expr_ty == cast_ty {
                    return *expr;
                }

                // no special implementations when casting to variant
                //   (we're just boxing)
                if cast_ty == Type::Variant {
                    return Expr {
                        data: ExprKind::Cast(expr, cast_ty),
                        ty,
                        loc,
                    };
                }

                let loc = expr.loc.clone();

                match expr_ty {
                    Type::Bool => ExprKind::CondExpr {
                        cond: expr,
                        if_expr: Box::new(Expr {
                            data: ExprKind::Lit(
                              Literal::num_of_type(&cast_ty, 1)
                              .expect("dumpster fire: bad numeric type \
                                in cast rewriter")),
                            ty: Some(cast_ty.clone()),
                            loc: loc.clone(),
                        }),
                        else_expr: Box::new(Expr {
                            data: ExprKind::Lit(
                              Literal::num_of_type(&cast_ty, 0)
                              .expect("dumpster fire: bad numeric type \
                                in cast rewriter")),
                            ty: Some(cast_ty.clone()),
                            loc,
                        })
                    },

                    Type::Obj | Type::Object(_) => {
                        panic!("TODO: object dynamic casts")
                    },

                    _ => ExprKind::Cast(expr, cast_ty),
                }
            },

            data => data,
        };

        Expr {
            data,
            ty,
            loc,
        }
    }
}

struct ShortCircuitLogicalsFolder<'a> {
    symtab: &'a mut SymbolTable,
    before_stmt_stack: Vec<Vec<Stmt>>,
}

impl<'a> ShortCircuitLogicalsFolder<'a> {
    fn new(symtab: &'a mut SymbolTable) -> Self {
        ShortCircuitLogicalsFolder {
            symtab,
            before_stmt_stack: Vec::new(),
        }
    }
}

impl<'a> ASTFolder for ShortCircuitLogicalsFolder<'a> {
    fn fold_stmt_list(&mut self, stmts: Vec<Stmt>, module: &Ident,
      function: &Ident) -> Vec<Stmt> {
        stmts.into_iter().flat_map(|stmt| {
            let stmt = self.fold_stmt(stmt, module, function);

            let mut before_stmts = self.before_stmt_stack.pop()
                .expect("dumpster fire: error in before statement stack");
            let mut result: Vec<_> = before_stmts.drain(..).collect();

            // am I crazy?
            result = self.fold_stmt_list(result, module, function);

            result.push(stmt);
            result
        }).collect()
    }

    fn fold_stmt(&mut self, stmt: Stmt, module: &Ident,
      function: &Ident) -> Stmt {
        // push a new before-context
        self.before_stmt_stack.push(Vec::new());

        // first recurse into the statement...
        let Stmt { data, loc } =
            fold::noop_fold_stmt(self, stmt, module, function);

        let data = match data {
            StmtKind::Assign(lhs, op, rhs) => {
                match op {
                    AssignOp::LogAndAssign => StmtKind::IfStmt {
                        cond: lhs.clone(),
                        body: vec![
                            Stmt {
                                data: StmtKind::Assign(
                                          lhs, AssignOp::LogAndAssign, rhs),
                                loc: loc.clone(),
                            }
                        ],
                        elsifs: vec![],
                        els: None
                    },

                    AssignOp::LogOrAssign => StmtKind::IfStmt {
                        cond: Expr {
                            data: ExprKind::UnOpApp(
                                      Box::new(lhs.clone()), UnOp::LogNot),
                            ty: lhs.ty.clone(),
                            loc: loc.clone(),
                        },
                        body: vec![
                            Stmt {
                                data: StmtKind::Assign(
                                          lhs, AssignOp::LogOrAssign, rhs),
                                loc: loc.clone(),
                            }
                        ],
                        elsifs: vec![],
                        els: None,
                    },

                    op => StmtKind::Assign(lhs, op, rhs),
                }
            },

            s => s,
        };

        Stmt {
            data,
            loc,
        }
    }

    fn fold_expr(&mut self, expr: Expr, module: &Ident,
      function: Option<&Ident>) -> Expr {
        // first recurse into the expression...
        let Expr { data, ty, loc } =
            fold::noop_fold_expr(self, expr, module, function);

        let data = match data {
            ExprKind::BinOpApp(lhs, rhs, op) => {
                match op {
                    BinOp::LogAnd => {
                        // use a gensym for this term
                        let g = gensym(None);

                        let before_stmts = self.before_stmt_stack.last_mut()
                          .expect("dumpster fire: \
                                  error in before statement stack");

                        // add symbol table entry for it
                        self.symtab.add_value_entry(&g, module, function,
                          &Type::Bool, Access::Private, &loc)
                          .expect("dumpster fire: \
                                  failure adding symtab entry for gensym");

                        // push a declaration for it
                        before_stmts.push(Stmt {
                            data: StmtKind::VarDecl(vec![
                              (g.clone(), Type::Bool, Some(*lhs))
                            ]),
                            loc: loc.clone(),
                        });

                        // build a path-expression for it
                        let g_expr = Expr {
                            data: ExprKind::Name(Path(None, g)),
                            ty: Some(Type::Bool),
                            loc: loc.clone(),
                        };

                        // push an &&= for it, using existing short-circuiting
                        //   rules in .fold_stmt(...)
                        before_stmts.push(Stmt {
                            data: StmtKind::Assign(
                                g_expr.clone(),
                                AssignOp::LogAndAssign,
                                *rhs
                            ),
                            loc: loc.clone(),
                        });

                        // use the gensym as our new expression
                        g_expr.data
                    },

                    BinOp::LogOr => {
                        // use a gensym for this term
                        let g = gensym(None);

                        let before_stmts = self.before_stmt_stack.last_mut()
                          .expect("dumpster fire: \
                                  error in before statement stack");

                        // add symbol table entry for it
                        self.symtab.add_value_entry(&g, module, function,
                          &Type::Bool, Access::Private, &loc)
                          .expect("dumpster fire: \
                                  failure adding symtab entry for gensym");

                        // push a declaration for it
                        before_stmts.push(Stmt {
                            data: StmtKind::VarDecl(vec![
                              (g.clone(), Type::Bool, Some(*lhs))
                            ]),
                            loc: loc.clone(),
                        });

                        // build a path-expression for it
                        let g_expr = Expr {
                            data: ExprKind::Name(Path(None, g)),
                            ty: Some(Type::Bool),
                            loc: loc.clone(),
                        };

                        // push an &&= for it, using existing short-circuiting
                        //   rules in .fold_stmt(...)
                        before_stmts.push(Stmt {
                            data: StmtKind::Assign(
                                g_expr.clone(),
                                AssignOp::LogOrAssign,
                                *rhs
                            ),
                            loc: loc.clone(),
                        });

                        // use the gensym as our new expression
                        g_expr.data
                    },

                    op => {
                        ExprKind::BinOpApp(lhs, rhs, op)
                    },
                }
            },

            ExprKind::CondExpr { cond, if_expr, else_expr } => {
                let g = gensym(None);

                let this_expr = Expr {
                    data: ExprKind::CondExpr {
                        cond: cond.clone(),
                        if_expr: if_expr.clone(),
                        else_expr: else_expr.clone()
                    },
                    ty: ty.clone(),
                    loc: loc.clone(),
                };

                let ty = this_expr.ty.as_ref()
                  .expect("dumpster fire: \
                     untyped condexpr in short-ciruiter");

                // add symbol table entry for g
                self.symtab.add_value_entry(&g, module, function,
                  &ty, Access::Private, &loc).expect("dumpster fire: \
                                    failure adding symtab entry for gensym");

                let before_stmts = self.before_stmt_stack.last_mut()
                  .expect("dumpster fire: error in before statement stack");

                // push declaration for g
                before_stmts.push(Stmt {
                    data: StmtKind::VarDecl(vec![
                        (g.clone(), ty.clone(), None)
                    ]),
                    loc: loc.clone(),
                });

                let g_expr = Expr {
                    data: ExprKind::Name(Path(None, g)),
                    ty: Some(ty.clone()),
                    loc: loc.clone(),
                };

                before_stmts.push(Stmt {
                    data: StmtKind::IfStmt {
                        cond: *cond,
                        body: vec![
                            Stmt {
                                data: StmtKind::Assign(
                                  g_expr.clone(), AssignOp::Assign, *if_expr),
                                loc: loc.clone(),
                            }
                        ],
                        // TODO: we could use this for condexpr chains
                        elsifs: vec![],
                        els: Some(vec![
                           Stmt {
                               data: StmtKind::Assign(
                                 g_expr.clone(), AssignOp::Assign, *else_expr),
                               loc: loc.clone()
                           }
                        ]),
                    },

                    loc: loc.clone(),
                });

                g_expr.data
            },

            e => e,
        };

        Expr {
            data,
            ty,
            loc,
        }
    }
}

struct ArrayLoopRewriteFolder<'a> {
    symtab: &'a mut SymbolTable,
    before_stmt_stack: Vec<Vec<Stmt>>,
}

impl<'a> ArrayLoopRewriteFolder<'a> {
    fn new(symtab: &'a mut SymbolTable) -> Self {
        ArrayLoopRewriteFolder {
            symtab,
            before_stmt_stack: Vec::new(),
        }
    }

    // TODO: this whole loop-and-a-half is ugly hot garbage
    fn array_for_loop(&mut self, var: Ident, ty: Type, mode: ParamMode,
      expr: Expr, base: &Type, bounds: &ArrayBounds, mut body: Vec<Stmt>,
      loc: &SrcLoc, module: &Ident, function: &Ident) -> StmtKind {
        let dims = bounds.dims();

        // build indexing gensyms by dimension
        // inclusive ranges would be nice here...
        let mut g_iters: Vec<_> = (1..dims + 1).map(|_| gensym(None)).collect();

        let index_expr = Expr {
            data: ExprKind::Index(Box::new(expr.clone()),
              g_iters.iter().cloned().map(|g| Expr {
                  data: ExprKind::Name(Path(None, g)),
                  ty: Some(Type::Int32),
                  loc: loc.clone(),
              }).collect()),
            ty: Some(base.clone()),
            loc: loc.clone(),
        };

        // add gensyms to symbol table
        for g in &g_iters {
            self.symtab.add_value_entry(g, module, Some(function),
              &Type::Int32, Access::Private, &loc)
              .expect("dumpster fire: failure adding symtab entry for gensym");
        }

        let mut body = match mode {
            ParamMode::ByVal => {
                // push a before-declaration for the original variable
                let before_stmts = self.before_stmt_stack.last_mut()
                  .expect("dumpster fire: \
                          error in before statement stack");

                before_stmts.push(Stmt {
                    data: StmtKind::VarDecl(vec![
                      (var.clone(), ty.clone(), None)
                    ]),
                    loc: loc.clone(),
                });

                // and copy into it at the beginning of each iteration
                let copy_stmt = Stmt {
                    data: StmtKind::Assign(
                        Expr {
                            data: ExprKind::Name(Path(None, var)),
                            ty: Some(ty),
                            loc: loc.clone(),
                        },
                        AssignOp::Assign,
                        index_expr,
                    ),
                    loc: loc.clone(),
                };

                body.insert(0, copy_stmt);

                body
            },

            ParamMode::ByRef => {
                let mut subst_folder = ScopedExprSubstitutionFolder {
                    orig: var,
                    replace: index_expr,
                    module: module.clone(),
                    function: Some(function.clone()),
                };

                subst_folder.fold_stmt_list(body, module, function)
            },

        };

        // nested inner loops, if needed
        for dim in (1..dims).rev() {
            let spec = ForSpec::Range(
                // TODO: gross
                Expr {
                    data: ExprKind::ExtentExpr(
                              Box::new(expr.clone()),
                              ExtentKind::First,
                              dim),
                    ty: Some(Type::Int32),
                    loc: loc.clone(),
                },

                Expr {
                    data: ExprKind::ExtentExpr(
                              Box::new(expr.clone()),
                              ExtentKind::Last,
                              dim),
                    ty: Some(Type::Int32),
                    loc: loc.clone(),
                },
                None
            );

            body = vec![Stmt {
                data: StmtKind::ForLoop {
                    var: (g_iters.pop().unwrap(),
                      Type::Int32, ParamMode::ByVal),
                    spec,
                    body,
                },
                loc: loc.clone(),
            }];
        }

        let var = (g_iters.pop().unwrap(), Type::Int32, ParamMode::ByVal);
        let spec = ForSpec::Range(
            Expr {
                data: ExprKind::ExtentExpr(
                          Box::new(expr.clone()),
                          ExtentKind::First,
                          0usize),
                ty: Some(Type::Int32),
                loc: loc.clone(),
            },

            Expr {
                data: ExprKind::ExtentExpr(
                          Box::new(expr.clone()),
                          ExtentKind::Last,
                          0usize),
                ty: Some(Type::Int32),
                loc: loc.clone(),
            },

            None
        );

        StmtKind::ForLoop {
            var,
            spec,
            body,
        }
    }

}

impl<'a> ASTFolder for ArrayLoopRewriteFolder<'a> {
    fn fold_stmt_list(&mut self, stmts: Vec<Stmt>, module: &Ident,
      function: &Ident) -> Vec<Stmt> {
        stmts.into_iter().flat_map(|stmt| {
            let stmt = self.fold_stmt(stmt, module, function);

            let mut before_stmts = self.before_stmt_stack.pop()
                .expect("dumpster fire: error in before statement stack");
            let mut result: Vec<_> = before_stmts.drain(..).collect();

            // am I crazy?
            result = self.fold_stmt_list(result, module, function);

            result.push(stmt);
            result
        }).collect()
    }

    fn fold_stmt(&mut self, stmt: Stmt, module: &Ident,
      function: &Ident) -> Stmt {
        // push a new before-context
        self.before_stmt_stack.push(Vec::new());

        // first recurse into the statement...
        let Stmt { data, loc } =
            fold::noop_fold_stmt(self, stmt, module, function);

        let data = match data {
            StmtKind::ForLoop { var: (var, ty, mode), spec, body } => {
                match spec {
                    ForSpec::Range(first, last, step) =>
                        StmtKind::ForLoop {
                            var: (var, ty, mode),
                            spec: ForSpec::Range(first, last, step),
                            body,
                        },

                    ForSpec::Each(expr) => {
                        let expr_ty = expr.ty.as_ref()
                            .expect("dumpster fire: \
                                    untyped expr in loop rewriter")
                            .clone(); // TODO: this is stupid and only to
                                      //   satisfy the borrow checker

                        match expr_ty {
                            Type::Array(ref base, ref bounds) =>
                                self.array_for_loop(var, ty, mode, expr,
                                  base, bounds, body, &loc, module, function),

                            _ => StmtKind::ForLoop {
                                var: (var, ty, mode),
                                spec: ForSpec::Each(expr),
                                body,
                            },
                        }
                    },
                }
            },

            s => s,
        };

        Stmt {
            data,
            loc,
        }
    }

    fn fold_expr(&mut self, expr: Expr, module: &Ident,
      function: Option<&Ident>) -> Expr {
        // first recurse into the expression...
        let Expr { data, ty, loc } =
            fold::noop_fold_expr(self, expr, module, function);

        let data = match data {
            ExprKind::BinOpApp(lhs, rhs, op) => {
                match op {
                    BinOp::LogAnd => {
                        // use a gensym for this term
                        let g = gensym(None);

                        let before_stmts = self.before_stmt_stack.last_mut()
                          .expect("dumpster fire: \
                                  error in before statement stack");

                        // add symbol table entry for it
                        self.symtab.add_value_entry(&g, module, function,
                          &Type::Bool, Access::Private, &loc)
                          .expect("dumpster fire: \
                                  failure adding symtab entry for gensym");

                        // push a declaration for it
                        before_stmts.push(Stmt {
                            data: StmtKind::VarDecl(vec![
                              (g.clone(), Type::Bool, Some(*lhs))
                            ]),
                            loc: loc.clone(),
                        });

                        // build a path-expression for it
                        let g_expr = Expr {
                            data: ExprKind::Name(Path(None, g)),
                            ty: Some(Type::Bool),
                            loc: loc.clone(),
                        };

                        // push an &&= for it, using existing short-circuiting
                        //   rules in .fold_stmt(...)
                        before_stmts.push(Stmt {
                            data: StmtKind::Assign(
                                g_expr.clone(),
                                AssignOp::LogAndAssign,
                                *rhs
                            ),
                            loc: loc.clone(),
                        });

                        // use the gensym as our new expression
                        g_expr.data
                    },

                    BinOp::LogOr => {
                        // use a gensym for this term
                        let g = gensym(None);

                        let before_stmts = self.before_stmt_stack.last_mut()
                          .expect("dumpster fire: \
                                  error in before statement stack");

                        // add symbol table entry for it
                        self.symtab.add_value_entry(&g, module, function,
                          &Type::Bool, Access::Private, &loc)
                          .expect("dumpster fire: \
                                  failure adding symtab entry for gensym");

                        // push a declaration for it
                        before_stmts.push(Stmt {
                            data: StmtKind::VarDecl(vec![
                              (g.clone(), Type::Bool, Some(*lhs))
                            ]),
                            loc: loc.clone(),
                        });

                        // build a path-expression for it
                        let g_expr = Expr {
                            data: ExprKind::Name(Path(None, g)),
                            ty: Some(Type::Bool),
                            loc: loc.clone(),
                        };

                        // push an &&= for it, using existing short-circuiting
                        //   rules in .fold_stmt(...)
                        before_stmts.push(Stmt {
                            data: StmtKind::Assign(
                                g_expr.clone(),
                                AssignOp::LogOrAssign,
                                *rhs
                            ),
                            loc: loc.clone(),
                        });

                        // use the gensym as our new expression
                        g_expr.data
                    },

                    op => {
                        ExprKind::BinOpApp(lhs, rhs, op)
                    },
                }
            },

            ExprKind::CondExpr { cond, if_expr, else_expr } => {
                let g = gensym(None);

                let this_expr = Expr {
                    data: ExprKind::CondExpr {
                        cond: cond.clone(),
                        if_expr: if_expr.clone(),
                        else_expr: else_expr.clone()
                    },
                    ty: ty.clone(),
                    loc: loc.clone(),
                };
                
                let ty = this_expr.ty.as_ref()
                  .expect("dumpster fire: \
                     untypeable condexpr in short-ciruiter");

                // add symbol table entry for g
                self.symtab.add_value_entry(&g, module, function,
                  &ty, Access::Private, &loc).expect("dumpster fire: \
                                    failure adding symtab entry for gensym");

                let before_stmts = self.before_stmt_stack.last_mut()
                  .expect("dumpster fire: error in before statement stack");

                // push declaration for g
                before_stmts.push(Stmt {
                    data: StmtKind::VarDecl(vec![
                        (g.clone(), ty.clone(), None)
                    ]),
                    loc: loc.clone(),
                });

                let g_expr = Expr {
                    data: ExprKind::Name(Path(None, g)),
                    ty: Some(ty.clone()),
                    loc: loc.clone(),
                };

                before_stmts.push(Stmt {
                    data: StmtKind::IfStmt {
                        cond: *cond,
                        body: vec![
                            Stmt {
                                data: StmtKind::Assign(
                                  g_expr.clone(), AssignOp::Assign, *if_expr),
                                loc: loc.clone(),
                            }
                        ],
                        // TODO: we could use this for condexpr chains
                        elsifs: vec![],
                        els: Some(vec![
                           Stmt {
                               data: StmtKind::Assign(
                                 g_expr.clone(), AssignOp::Assign, *else_expr),
                               loc: loc.clone()
                           }
                        ]),
                    },

                    loc: loc.clone(),
                });

                g_expr.data
            },

            e => e,
        };

        Expr {
            data,
            ty,
            loc,
        }
    }
}

struct AlongLoopRewriteFolder;

impl AlongLoopRewriteFolder {
    fn new() -> Self {
        Self {
        }
    }
}

impl ASTFolder for AlongLoopRewriteFolder {
    fn fold_stmt(&mut self, stmt: Stmt, module: &Ident, function: &Ident)
      -> Stmt {
        // first recurse...
        let Stmt { data, loc } =
            fold::noop_fold_stmt(self, stmt, module, function);

        let data = match data {
            StmtKind::ForAlong { mut vars, along, mut body } => {
                let dims = vars.len();

                for dim in (0..dims).rev() {
                    body = vec![
                        Stmt {
                            data: StmtKind::ForLoop {
                                var: (
                                         vars.pop().unwrap(),
                                         Type::Int32,
                                         ParamMode::ByVal
                                ),
                                spec: ForSpec::Range(
                                    Expr {
                                        data: ExprKind::ExtentExpr(
                                            Box::new(along.clone()),
                                            ExtentKind::First,
                                            dim
                                        ),
                                        ty: Some(Type::Int32),
                                        loc: loc.clone(),
                                    },
                                    Expr {
                                        data: ExprKind::ExtentExpr(
                                            Box::new(along.clone()),
                                            ExtentKind::Last,
                                            dim
                                        ),
                                        ty: Some(Type::Int32),
                                        loc: loc.clone(),
                                    },
                                    None
                                ),
                                body,
                            },
                            loc: loc.clone(),
                        }
                    ];
                }

                body.pop().unwrap().data
            },

            data => data,
        };

        Stmt {
            data,
            loc,
        }
    }
}

struct AllocAlongRewriteFolder;

impl ASTFolder for AllocAlongRewriteFolder {
    fn fold_stmt(&mut self, Stmt { data, loc }: Stmt, module: &Ident,
      function: &Ident) -> Stmt {
        let data = match data {

            StmtKind::Alloc(expr, mut extents) => {
                if extents.len() == 1 {
                    let howmany = match extents[0] {
                        AllocExtent::Along(_) => {
                            let expr_ty = expr.ty.as_ref()
                              .expect("dumpster fire: \
                                      untyped condexpr in realloc rewriter");

                            if let Type::Array(_, ref bounds) = *expr_ty {
                                Some(bounds.dims() - 1)
                            } else {
                                panic!("dumpster fire: \
                                       non-array in realloc rewriter");
                            }
                        },

                        _ => None,
                    };
                    
                    if let Some(howmany) = howmany {
                        for _ in 0..howmany {
                            let extent = extents[0].clone();
                            extents.push(extent);
                        }
                    }
                }

                let extents = extents.into_iter().enumerate()
                    .map(|(i, extent)| {
                        match extent {
                            AllocExtent::Along(other) =>
                                AllocExtent::Range(
                                    Some(Expr {
                                        data: ExprKind::ExtentExpr(
                                                  Box::new(other.clone()),
                                                  ExtentKind::First,
                                                  i),
                                        ty: Some(Type::Int32),
                                        loc: other.loc.clone(),
                                    }),

                                    Expr {
                                        data: ExprKind::ExtentExpr(
                                                  Box::new(other.clone()),
                                                  ExtentKind::Last,
                                                  i),
                                        ty: Some(Type::Int32),
                                        loc: other.loc.clone(),
                                    },
                                ),

                            extent => extent,
                        }
                }).collect();

                StmtKind::Alloc(expr, extents)
            },

            StmtKind::ReAlloc(expr, preserved, extent) => {
                match extent {
                    AllocExtent::Along(other) => {
                        let extent = AllocExtent::Range(
                            Some(Expr {
                                data: ExprKind::ExtentExpr(
                                          Box::new(other.clone()),
                                          ExtentKind::First,
                                          preserved),
                                ty: Some(Type::Int32),
                                loc: other.loc.clone(),
                            }),

                            Expr {
                                data: ExprKind::ExtentExpr(
                                          Box::new(other.clone()),
                                          ExtentKind::Last,
                                          preserved),
                                ty: Some(Type::Int32),
                                loc: other.loc.clone(),
                            }
                        );

                        StmtKind::ReAlloc(expr, preserved, extent)
                    },

                    extent => StmtKind::ReAlloc(expr, preserved, extent),
                }
            },

            data => data
        };

        fold::noop_fold_stmt(self, Stmt { data, loc },
          module, function)
    }
}
