//! experimental: an AST folder

use ast::*;
use parser::SrcLoc;
use visit::NameCtxt;

// based on the ASTVisitor and the libsyntax folder at
// https://github.com/rust-lang/rust/blob/master/src/libsyntax/fold.rs

pub trait ASTFolder {
    fn fold_dumpster(&mut self, d: Dumpster) -> Dumpster {
        noop_fold_dumpster(self, d)
    }

    fn fold_module(&mut self, m: Module) -> Module {
        noop_fold_module(self, m)
    }

    fn fold_normal_item_list(&mut self, items: Vec<NormalItem>, module: &Ident)
      -> Vec<NormalItem> {
        noop_fold_normal_item_list(self, items, module)
    }

    fn fold_normal_item(&mut self, item: NormalItem, module: &Ident)
      -> NormalItem {
        noop_fold_normal_item(self, item, module)
    }

    fn fold_fundef(&mut self, def: FunDef, module: &Ident) -> FunDef {
        noop_fold_fundef(self, def, module)
    }

    fn fold_funparam_list(&mut self, params: Vec<FunParam>, module: &Ident,
      function: &Ident) -> Vec<FunParam> {
        noop_fold_funparam_list(self, params, module, function)
    }

    fn fold_funparam(&mut self, param: FunParam, module: &Ident,
      function: &Ident) -> FunParam {
        noop_fold_funparam(self, param, module, function)
    }

    fn fold_optparams(&mut self, optparams: FunOptParams, module: &Ident,
      function: &Ident) -> FunOptParams {
        noop_fold_optparams(self, optparams, module, function)
    }

    fn fold_optparam_list(&mut self, params: Vec<(FunParam, Literal)>,
      module: &Ident, function: &Ident) -> Vec<(FunParam, Literal)> {
        noop_fold_optparam_list(self, params, module, function)
    }

    fn fold_optparam(&mut self, param: (FunParam, Literal), module: &Ident,
      function: &Ident) -> (FunParam, Literal) {
        noop_fold_optparam(self, param, module, function)
    }

    fn fold_structdef(&mut self, def: StructDef, module: &Ident) -> StructDef {
        noop_fold_structdef(self, def, module)
    }

    fn fold_structmem_list(&mut self, mems: Vec<StructMem>, module: &Ident,
      st: &Ident) -> Vec<StructMem> {
        noop_fold_structmem_list(self, mems, module, st)
    }

    fn fold_structmem(&mut self, m: StructMem, module: &Ident, st: &Ident)
      -> StructMem {
        noop_fold_structmem(self, m, module, st)
    }

    fn fold_static(&mut self, s: Static, module: &Ident) -> Static {
        noop_fold_static(self, s, module)
    }

    fn fold_constant(&mut self, c: Constant, module: &Ident) -> Constant {
        noop_fold_constant(self, c, module)
    }

    fn fold_stmt_list(&mut self, stmts: Vec<Stmt>, module: &Ident,
      function: &Ident) -> Vec<Stmt> {
        noop_fold_stmt_list(self, stmts, module, function)
    }

    fn fold_stmt(&mut self, stmt: Stmt, module: &Ident, function: &Ident)
      -> Stmt {
        noop_fold_stmt(self, stmt, module, function)
    }

    fn fold_expr_list(&mut self, exprs: Vec<Expr>, module: &Ident,
      function: Option<&Ident>) -> Vec<Expr> {
        noop_fold_expr_list(self, exprs, module, function)
    }

    fn fold_expr(&mut self, expr: Expr, module: &Ident,
      function: Option<&Ident>) -> Expr {
        noop_fold_expr(self, expr, module, function)
    }

    // TODO: move variable into forspec; this might make many
    //   things easier
    fn fold_forspec(&mut self, spec: ForSpec, module: &Ident,
      function: &Ident, loc: &SrcLoc) -> ForSpec {
        noop_fold_forspec(self, spec, module, function, loc)
    }

    fn fold_allocextent_list(&mut self, extents: Vec<AllocExtent>,
      module: &Ident, function: &Ident, loc: &SrcLoc) -> Vec<AllocExtent> {
        noop_fold_allocextent_list(self, extents, module, function, loc)
    }

    fn fold_allocextent(&mut self, extent: AllocExtent,
      module: &Ident, function: &Ident, loc: &SrcLoc) -> AllocExtent {
        noop_fold_allocextent(self, extent, module, function, loc)
    }

    // TODO: can we ever not be in a function when we walk a path?
    fn fold_path(&mut self, p: Path, ctxt: NameCtxt, loc: &SrcLoc) -> Path {
        noop_fold_path(self, p, ctxt, loc)
    }

    fn fold_type(&mut self, ty: Type, module: &Ident, loc: &SrcLoc) -> Type {
        noop_fold_type(self, ty, module, loc)
    }

    // below: "do-nothing" defaults (terminal nodes)

    fn fold_ident(&mut self, i: Ident, _ctxt: NameCtxt, _loc: &SrcLoc)
      -> Ident {
        i
    }

    fn fold_literal(&mut self, lit: Literal,
      _module: &Ident, _function: Option<&Ident>, _loc: &SrcLoc) -> Literal {
        lit
    }

    fn fold_vbexpr(&mut self, data: Vec<u8>,
      _module: &Ident, _function: Option<&Ident>, _loc: &SrcLoc) -> Vec<u8> {
        data
    }

    fn fold_arraybounds(&mut self, bounds: ArrayBounds, _base_ty: &Type,
      _module: &Ident, _loc: &SrcLoc) -> ArrayBounds {
        bounds
    }

    fn fold_srcloc(&mut self, srcloc: SrcLoc) -> SrcLoc {
        srcloc
    }
}

pub fn noop_fold_dumpster<F: ASTFolder + ?Sized>(folder: &mut F,
  Dumpster { modules }: Dumpster) -> Dumpster {
    Dumpster {
        modules: modules.into_iter().map(|m| folder.fold_module(m)).collect()
    }
}

pub fn noop_fold_module<F: ASTFolder + ?Sized>(folder: &mut F,
  Module { name, data, loc }: Module) -> Module {
    let name = folder.fold_ident(name, NameCtxt::DefModule, &loc);
    let data = match data {
        ModuleKind::Normal(items) =>
            ModuleKind::Normal(folder.fold_normal_item_list(items, &name)),
    };
    let loc = folder.fold_srcloc(loc);

    Module {
        name,
        data,
        loc,
    }
}

pub fn noop_fold_normal_item_list<F: ASTFolder + ?Sized>(folder: &mut F,
  items: Vec<NormalItem>, module: &Ident) -> Vec<NormalItem> {
    items.into_iter().map(|i| folder.fold_normal_item(i, module)).collect()
}

pub fn noop_fold_normal_item<F: ASTFolder + ?Sized>(folder: &mut F,
  i: NormalItem, module: &Ident) -> NormalItem {
    match i {
        NormalItem::Function(def) =>
            NormalItem::Function(folder.fold_fundef(def, module)),
        NormalItem::Struct(def) =>
            NormalItem::Struct(folder.fold_structdef(def, module)),
        NormalItem::Static(def) =>
            NormalItem::Static(folder.fold_static(def, module)),
        NormalItem::Const(def) =>
            NormalItem::Const(folder.fold_constant(def, module)),
    }
}

pub fn noop_fold_fundef<F: ASTFolder + ?Sized>(folder: &mut F,
  FunDef { name, access, params, optparams, ret, body, loc }: FunDef,
  module: &Ident)
  -> FunDef {
    let name = folder.fold_ident(name, NameCtxt::DefFunction(module), &loc);
    // TODO: hook for fold_access
    let params = folder.fold_funparam_list(params, module, &name);
    let optparams = optparams.map(|o| folder.fold_optparams(o, module, &name));
    let ret = folder.fold_type(ret, module, &loc);
    let body = folder.fold_stmt_list(body, module, &name);
    let loc = folder.fold_srcloc(loc);

    FunDef {
        name,
        params,
        optparams,
        access,
        ret,
        body,
        loc,
    }
}

pub fn noop_fold_funparam_list<F: ASTFolder + ?Sized>(folder: &mut F,
  params : Vec<FunParam>, module: &Ident, function: &Ident) -> Vec<FunParam> {
    params.into_iter().map(|p| folder.fold_funparam(p, module, function))
        .collect()
}

pub fn noop_fold_funparam<F: ASTFolder + ?Sized>(folder: &mut F,
  FunParam { name, ty, mode, loc } : FunParam, module: &Ident,
  function: &Ident) -> FunParam {
    let name = folder.fold_ident(name,
      NameCtxt::DefParam(module, function, &ty, mode), &loc);
    let ty = folder.fold_type(ty, module, &loc);
    // TODO: fold_mode?
    let loc = folder.fold_srcloc(loc);
    FunParam {
        name,
        ty,
        mode,
        loc,
    }
}

pub fn noop_fold_optparams<F: ASTFolder + ?Sized>(folder: &mut F,
  optparams: FunOptParams, module: &Ident, function: &Ident) -> FunOptParams {
    match optparams {
        FunOptParams::VarArgs(name, loc) => {
            let loc = folder.fold_srcloc(loc);
            FunOptParams::VarArgs(
                folder.fold_ident(name,
                  NameCtxt::DefParam(
                      module,
                      function,
                      &Type::VarArgsArray,
                      ParamMode::ByRef
                  ),
                  &loc),
                  loc
            )
        },

        FunOptParams::Named(list) =>
            FunOptParams::Named(
                folder.fold_optparam_list(list, module, function)),
    }
}

pub fn noop_fold_optparam_list<F: ASTFolder + ?Sized>(folder: &mut F,
  params : Vec<(FunParam, Literal)>, module: &Ident, function: &Ident)
  -> Vec<(FunParam, Literal)> {
    params.into_iter().map(|p| folder.fold_optparam(p, module, function))
        .collect()
}

pub fn noop_fold_optparam<F: ASTFolder + ?Sized>(folder: &mut F,
  (param, default) : (FunParam, Literal), module: &Ident, function: &Ident)
  -> (FunParam, Literal) {
    let param = folder.fold_funparam(param, module, function);
    let default = folder.fold_literal(default, module,
      Some(function), &param.loc);
    (param, default)
}

pub fn noop_fold_structdef<F: ASTFolder + ?Sized>(folder: &mut F,
  StructDef { name, access, members, loc } : StructDef, module: &Ident)
  -> StructDef {
    let name = folder.fold_ident(name, NameCtxt::DefType(module), &loc);
    let members = folder.fold_structmem_list(members, module, &name);
    let loc = folder.fold_srcloc(loc);
    StructDef {
        name,
        access,
        members,
        loc,
    }
}

pub fn noop_fold_structmem_list<F: ASTFolder + ?Sized>(folder: &mut F,
  mems : Vec<StructMem>, module: &Ident, st: &Ident) -> Vec<StructMem> {
    mems.into_iter().map(|m| folder.fold_structmem(m, module, st)).collect()
}

pub fn noop_fold_structmem<F: ASTFolder + ?Sized>(folder: &mut F,
  StructMem { name, ty, loc } : StructMem, module: &Ident, st: &Ident)
  -> StructMem {
    let name = folder.fold_ident(name,
      NameCtxt::DefMember(module, st, &ty), &loc);
    let ty = folder.fold_type(ty, module, &loc);
    let loc = folder.fold_srcloc(loc);
    StructMem {
        name,
        ty,
        loc,
    }
}

pub fn noop_fold_static<F: ASTFolder + ?Sized>(folder: &mut F,
  Static { name, access, ty, init, loc } : Static, module: &Ident) -> Static {
    let name = folder.fold_ident(name,
      NameCtxt::DefValue(module, None, &ty, access), &loc);
    let ty = folder.fold_type(ty, module, &loc);
    let init = init.map(|i| folder.fold_literal(i, module, None, &loc));
    let loc = folder.fold_srcloc(loc);
    Static {
        name,
        access,
        ty,
        init,
        loc,
    }
}

pub fn noop_fold_constant<F: ASTFolder + ?Sized>(folder: &mut F,
  Constant { name, access, ty, value, loc } : Constant,
  module: &Ident) -> Constant {
    let name = folder.fold_ident(name,
      NameCtxt::DefConstant(module, &ty, access), &loc);
    let ty = folder.fold_type(ty, module, &loc);
    let value = folder.fold_literal(value, module, None, &loc);
    let loc = folder.fold_srcloc(loc);
    Constant {
        name,
        access,
        ty,
        value,
        loc,
    }
}

pub fn noop_fold_stmt_list<F: ASTFolder + ?Sized>(folder: &mut F,
  stmts: Vec<Stmt>, module: &Ident, function: &Ident) -> Vec<Stmt> {
    stmts.into_iter().map(|s| folder.fold_stmt(s, module, function)).collect()
}

// TODO: maybe each pattern should have its own visit function
// TODO: move loc out
pub fn noop_fold_stmt<F: ASTFolder + ?Sized>(folder: &mut F,
  Stmt { data, loc }: Stmt, module: &Ident, function: &Ident) -> Stmt {
    let data = match data {
        StmtKind::ExprStmt(expr) =>
            StmtKind::ExprStmt(folder.fold_expr(expr, module, Some(function))),

        StmtKind::VarDecl(decls) =>
            StmtKind::VarDecl(decls.into_iter().map(|(ident, ty, init)| {
                (
                    folder.fold_ident(
                        ident, 
                        NameCtxt::DefValue(module, Some(function),
                          &ty, Access::Private),
                        &loc),
                    folder.fold_type(ty, module, &loc),
                    init.map(|init|
                      folder.fold_expr(init, module, Some(function)))
                )
            }).collect()),

        StmtKind::Assign(lhs, op, rhs) =>
            StmtKind::Assign(
                folder.fold_expr(lhs, module, Some(function)),
                op,
                folder.fold_expr(rhs, module, Some(function))),

        StmtKind::Return(Some(expr)) =>
            StmtKind::Return(
                Some(folder.fold_expr(expr, module, Some(function)))),

        StmtKind::Return(None) =>
            StmtKind::Return(None),

        StmtKind::IfStmt { cond, body, elsifs, els } =>
            StmtKind::IfStmt {
                cond: folder.fold_expr(cond, module, Some(function)),
                body: folder.fold_stmt_list(body, module, function),
                elsifs: elsifs.into_iter().map(|(cond, body)| {
                    (
                        folder.fold_expr(cond, module, Some(function)),
                        folder.fold_stmt_list(body, module, function),
                    )
                }).collect(),
                els: els.map(|body|
                  folder.fold_stmt_list(body, module, function)),
            },

        StmtKind::WhileLoop { cond, body } =>
            StmtKind::WhileLoop {
                cond: folder.fold_expr(cond, module, Some(function)),
                body: folder.fold_stmt_list(body, module, function),
            },

        StmtKind::ForLoop { var: (ident, ty, mode), spec, body } =>
            StmtKind::ForLoop {
                var: (
                    folder.fold_ident(
                        ident,
                        NameCtxt::DefValue(module, Some(function),
                          &ty, Access::Private),
                        &loc),
                    folder.fold_type(ty, module, &loc),
                    mode,
                ),
                spec: folder.fold_forspec(spec, module, function, &loc),
                body: folder.fold_stmt_list(body, module, function),
            },

        StmtKind::ForAlong { vars, along, body } =>
            StmtKind::ForAlong {
                vars: vars.into_iter().map(|var|
                    folder.fold_ident(var,
                      NameCtxt::DefValue(module, Some(function), &Type::Int32,
                        Access::Private),
                      &loc)
                ).collect(),
                along: folder.fold_expr(along, module, Some(function)),
                body: folder.fold_stmt_list(body, module, function),
            },

        StmtKind::Alloc(expr, extents) =>
            StmtKind::Alloc(
                folder.fold_expr(expr, module, Some(function)),
                folder.fold_allocextent_list(extents, module, function, &loc)
            ),

        StmtKind::ReAlloc(expr, preserved, extent) =>
            StmtKind::ReAlloc(
                folder.fold_expr(expr, module, Some(function)),
                preserved,
                folder.fold_allocextent(extent, module, function, &loc)
            ),

        StmtKind::DeAlloc(expr) =>
            StmtKind::DeAlloc(folder.fold_expr(expr, module, Some(function))),

        StmtKind::Print(exprs) =>
            StmtKind::Print(folder.fold_expr_list(exprs, module,
              Some(function))),
    };

    let loc = folder.fold_srcloc(loc);

    Stmt {
        data,
        loc,
    }
}

pub fn noop_fold_expr_list<F: ASTFolder + ?Sized>(folder: &mut F,
  exprs: Vec<Expr>, module: &Ident, function: Option<&Ident>) -> Vec<Expr> {
    exprs.into_iter().map(|expr| folder.fold_expr(expr, module, function))
        .collect()
}

// TODO: maybe each pattern should have its own visit function
pub fn noop_fold_expr<F: ASTFolder + ?Sized>(folder: &mut F,
  Expr { data, ty, loc }: Expr, module: &Ident, function: Option<&Ident>)
  -> Expr {
    let data = match data {
        ExprKind::Lit(lit) =>
            ExprKind::Lit(folder.fold_literal(lit, module,
              function, &loc)),

        ExprKind::Name(path) =>
            ExprKind::Name(folder.fold_path(path,
              NameCtxt::Value(module, function, Access::Private), &loc)),

        ExprKind::Index(expr, indices) =>
            ExprKind::Index(
                Box::new(folder.fold_expr(*expr, module, function)),
                // TODO: fold_indices?
                indices.into_iter()
                  .map(|i| folder.fold_expr(i, module, function))
                  .collect()
            ),

        ExprKind::Call(path, args, optargs) =>
            ExprKind::Call(
              folder.fold_path(path,
                NameCtxt::Function(module, Access::Private), &loc),
              folder.fold_expr_list(args, module, function),
              optargs.into_iter().map(|(i, e)| { (
                  folder.fold_ident(i, NameCtxt::OptArgName, &loc),
                  folder.fold_expr(e, module, function)
              ) }).collect()
            ),

        ExprKind::Member(expr, ident) =>
            ExprKind::Member(
                Box::new(folder.fold_expr(*expr, module, function)),
              folder.fold_ident(ident,
                NameCtxt::Member(module, None, Access::Private), &loc)),

        ExprKind::MemberInvoke(expr, ident, args) =>
            ExprKind::MemberInvoke(
                Box::new(folder.fold_expr(*expr, module, function)),
                folder.fold_ident(ident,
                  NameCtxt::Member(module, None, Access::Private), &loc),
                folder.fold_expr_list(args, module, function)
            ),

        ExprKind::UnOpApp(expr, op) =>
            ExprKind::UnOpApp(
                Box::new(folder.fold_expr(*expr, module, function)),
                op),

        ExprKind::BinOpApp(lhs, rhs, op) => ExprKind::BinOpApp(
            Box::new(folder.fold_expr(*lhs, module, function)),
            Box::new(folder.fold_expr(*rhs, module, function)),
            op),

        ExprKind::CondExpr { cond, if_expr, else_expr } => ExprKind::CondExpr {
            cond: Box::new(folder.fold_expr(*cond, module, function)),
            if_expr: Box::new(folder.fold_expr(*if_expr, module, function)),
            else_expr: Box::new(folder.fold_expr(*else_expr, module, function))
        },

        ExprKind::ExtentExpr(expr, kind, dim) =>
            ExprKind::ExtentExpr(
                Box::new(folder.fold_expr(*expr, module, function)),
                kind,
                dim
            ),

        ExprKind::Cast(expr, ty) => {
            let expr = folder.fold_expr(*expr, module, function);
            let ty = folder.fold_type(ty, module, &expr.loc);
            ExprKind::Cast(Box::new(expr), ty)
        },

        ExprKind::VbExpr(data) =>
            ExprKind::VbExpr(folder.fold_vbexpr(data, module, function, &loc)),
    };

    let ty = ty.map(|ty| folder.fold_type(ty, module, &loc));

    let loc = folder.fold_srcloc(loc);

    Expr {
        data,
        ty,
        loc,
    }
}

pub fn noop_fold_forspec<F: ASTFolder + ?Sized>(folder: &mut F, spec: ForSpec,
  module: &Ident, function: &Ident, _loc: &SrcLoc) -> ForSpec {
    match spec {
        ForSpec::Range(from, to, step) =>
            ForSpec::Range(
                folder.fold_expr(from, module, Some(function)),
                folder.fold_expr(to, module, Some(function)),
                step.map(|step| folder.fold_expr(step, module, Some(function)))
            ),

        ForSpec::Each(of) =>
            ForSpec::Each(folder.fold_expr(of, module, Some(function))),
    }
}

fn noop_fold_allocextent_list<F: ASTFolder + ?Sized>(folder: &mut F,
  extents: Vec<AllocExtent>, module: &Ident, function: &Ident, loc: &SrcLoc)
  -> Vec<AllocExtent> {
    extents.into_iter()
        .map(|extent| folder.fold_allocextent(extent, module, function, loc))
        .collect()
}

fn noop_fold_allocextent<F: ASTFolder + ?Sized>(folder: &mut F,
  extent: AllocExtent, module: &Ident, function: &Ident, _loc: &SrcLoc)
  -> AllocExtent {
    match extent {
        AllocExtent::Along(expr) =>
            AllocExtent::Along(folder.fold_expr(expr, module, Some(function))),
        AllocExtent::Range(lb, ub) =>
            AllocExtent::Range(
                lb.map(|lb| folder.fold_expr(lb, module, Some(function))),
                folder.fold_expr(ub, module, Some(function))
            ),
    }
}

pub fn noop_fold_path<F: ASTFolder + ?Sized>(folder: &mut F,
  Path(module, ident): Path, ctxt: NameCtxt, loc: &SrcLoc) -> Path {
    let module = module.map(|module|
      folder.fold_ident(module, NameCtxt::Module, &loc));

    let path = Path(module.clone(), ident.clone());
    let (_, inner_ctxt) = ident_ctxt_from_path(&path, ctxt);

    let ident = folder.fold_ident(ident, inner_ctxt, &loc);
    Path(module, ident)
}

pub fn noop_fold_type<F: ASTFolder + ?Sized>(folder: &mut F, ty: Type,
  module: &Ident, loc: &SrcLoc) -> Type {
    match ty {
        Type::Array(base, bounds) => {
            let base = folder.fold_type(*base, module, loc);
            let bounds = folder.fold_arraybounds(bounds, &base, module, loc);
            Type::Array(Box::new(base), bounds)
        },

        Type::Struct(path) =>
            Type::Struct(folder.fold_path(path,
              NameCtxt::Type(module, Access::Private), loc)),

        Type::Object(path) =>
            Type::Object(folder.fold_path(path,
              NameCtxt::Type(module, Access::Private), loc)),

        Type::Deferred(path) =>
            Type::Deferred(folder.fold_path(path,
              NameCtxt::Type(module, Access::Private), loc)),

        _ => ty,
    }
}

fn ident_ctxt_from_path<'a>(p: &'a Path, ctxt: NameCtxt<'a>)
  -> (&'a Ident, NameCtxt<'a>) {
    match *p {
        Path(Some(ref path_m), ref ident) => {
            let inner_ctxt = match ctxt {
                NameCtxt::Module | NameCtxt::DefModule =>
                    panic!("dumpster fire: path as module name"),

                NameCtxt::DefFunction(_)
              | NameCtxt::DefType(_)
              | NameCtxt::DefValue(_, _, _, _)
              | NameCtxt::DefParam(_, _, _, _)
              | NameCtxt::DefConstant(_, _, _)
              | NameCtxt::DefMember(_, _, _) =>
                    panic!("dumpster fire: path as name definition"),

                NameCtxt::OptArgName =>
                    panic!("dumpster fire: path as optional argument name"),

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
            (ident, inner_ctxt)
        },

        Path(None, ref ident) => {
            (ident, ctxt)
        },
    }
}
