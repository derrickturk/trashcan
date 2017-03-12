//! trashcan's AST transforms, used to implement language features

use super::*;
use ast::*;

use visit::NameCtxt;
use visit::ASTVisitor;

use fold;
use fold::ASTFolder;

use std::collections::HashSet;

/// replace names which conflict with VB keywords with gensyms
pub fn vb_keyword_gensym(mut dumpster: Dumpster) -> Dumpster {
    let mut v = VbKeywordGensymCollectVisitor::new();
    v.visit_dumpster(&dumpster);
    for mut r in v.value_renamers {
        dumpster = r.fold_dumpster(dumpster);
    }
    // members here
    for mut r in v.type_renamers {
        dumpster = r.fold_dumpster(dumpster);
    }
    for mut r in v.fn_renamers {
        dumpster = r.fold_dumpster(dumpster);
    }
    for mut r in v.member_renamers {
        dumpster = r.fold_dumpster(dumpster);
    }
    for mut r in v.module_renamers {
        dumpster = r.fold_dumpster(dumpster);
    }
    dumpster
}

/// replace variables with same name as enclosing fn with gensyms
///   (work around VB function return value semantics)
pub fn fn_name_local_gensym(mut dumpster: Dumpster) -> Dumpster {
    let mut v = FnNameLocalGensymCollectVisitor::new();
    v.visit_dumpster(&dumpster);
    for mut r in v.renamers {
        dumpster = r.fold_dumpster(dumpster);
    }
    dumpster
}

/// replace for loop iteration variables with gensyms for pseudo-block scoping
pub fn for_loop_var_gensym(dumpster: Dumpster) -> Dumpster {
    let mut f = ForLoopVarGensymFolder;
    f.fold_dumpster(dumpster)
}

/// replace names which would be duplicates under case-folding
pub fn case_folding_duplicate_gensym(mut dumpster: Dumpster) -> Dumpster {
    let mut v = CaseFoldingDuplicateGensymVisitor::new();
    v.visit_dumpster(&dumpster);
    for mut r in v.value_renamers {
        dumpster = r.fold_dumpster(dumpster);
    }
    // members here
    for mut r in v.type_renamers {
        dumpster = r.fold_dumpster(dumpster);
    }
    for mut r in v.fn_renamers {
        dumpster = r.fold_dumpster(dumpster);
    }
    for mut r in v.member_renamers {
        dumpster = r.fold_dumpster(dumpster);
    }
    for mut r in v.module_renamers {
        dumpster = r.fold_dumpster(dumpster);
    }
    dumpster
}

enum Rename {
    Module,
    Value,
    Function,
    Type,
    Member,
}

struct VbKeywordGensymCollectVisitor {
    value_renamers: Vec<ScopedSubstitutionFolder>,
    type_renamers: Vec<ScopedSubstitutionFolder>,
    fn_renamers: Vec<ScopedSubstitutionFolder>,
    member_renamers: Vec<ScopedSubstitutionFolder>,
    module_renamers: Vec<ScopedSubstitutionFolder>,
}

impl VbKeywordGensymCollectVisitor {
    fn new() -> Self {
        Self {
            value_renamers: Vec::new(),
            type_renamers: Vec::new(),
            fn_renamers: Vec::new(),
            member_renamers: Vec::new(),
            module_renamers: Vec::new(),
        }
    }
}

impl ASTVisitor for VbKeywordGensymCollectVisitor {
    fn visit_ident(&mut self, ident: &Ident, ctxt: NameCtxt, loc: &SrcLoc) {
        if !VB_KEYWORDS.contains(&ident.0.to_uppercase().as_str()) {
            return;
        }

        let (module, function, what) = match ctxt {
            NameCtxt::DefValue(m, f, _) => (Some(m), f, Rename::Value),
            NameCtxt::DefParam(m, f, _, _) => (Some(m), Some(f), Rename::Value),
            NameCtxt::DefFunction(m) => (Some(m), None, Rename::Function),
            NameCtxt::DefType(m) => (Some(m), None, Rename::Type),
            NameCtxt::DefMember(_, _, _) => (None, None, Rename::Member),
            NameCtxt::DefModule => (None, None, Rename::Module),
            _ => return
        };

        let (values, fns, types, members, modules, dest) = match what {
            Rename::Value =>
                (true, false, false, false, false, &mut self.value_renamers),
            Rename::Function =>
                (false, true, false, false, false, &mut self.fn_renamers),
            Rename::Type =>
                (false, false, true, false, false, &mut self.type_renamers),
            Rename::Member =>
                (false, false, false, true, false, &mut self.member_renamers),
            Rename::Module =>
                (false, false, false, false, true, &mut self.module_renamers),
            _ => panic!(),
        };

        let g = gensym(Some(ident.clone()));
        dest.push(ScopedSubstitutionFolder {
            orig: ident.clone(),
            replace: g,
            module: module.cloned(),
            function: function.cloned(),
            defns: true,
            values: values,
            fns: fns,
            types: types,
            members: members,
            modules: modules,
        });
    }
}

struct FnNameLocalGensymCollectVisitor {
    renamers: Vec<ScopedSubstitutionFolder>,
}

impl FnNameLocalGensymCollectVisitor {
    fn new() -> Self {
        Self {
            renamers: Vec::new(),
        }
    }
}

impl ASTVisitor for FnNameLocalGensymCollectVisitor {
    fn visit_ident(&mut self, ident: &Ident, ctxt: NameCtxt, loc: &SrcLoc) {
        let (module, function) = match ctxt {
            NameCtxt::DefValue(m, Some(f), _) => (m, f),
            NameCtxt::DefParam(m, f, _, _) => (m, f),
            _ => return
        };

        if ident == function {
            let g = gensym(Some(ident.clone()));
            self.renamers.push(ScopedSubstitutionFolder {
                orig: ident.clone(),
                replace: g,
                module: Some(module.clone()),
                function: Some(function.clone()),
                defns: true,
                values: true,
                fns: false,
                types: false,
                members: false,
                modules: false,
            });
        }
    }
}

struct ForLoopVarGensymFolder;

impl ASTFolder for ForLoopVarGensymFolder {
    fn fold_stmt(&mut self, stmt: Stmt, module: &Ident, function: &Ident)
  -> Stmt {
        match stmt.data {
            StmtKind::ForLoop { var: (ident, ty), spec, body } => {
                let g = gensym(Some(ident.clone()));
                let body = {
                    let mut sub = ScopedSubstitutionFolder {
                        orig: ident.clone(),
                        replace: g.clone(),
                        module: Some(module.clone()),
                        function: Some(function.clone()),
                        defns: false, // I think
                        values: true,
                        fns: false,
                        types: false,
                        members: false,
                        modules: false,
                    };

                    sub.fold_stmt_list(body, module, function)
                };

                Stmt {
                    data: StmtKind::ForLoop {
                        var: (g, ty),
                        spec: spec,
                        body: self.fold_stmt_list(body, module, function),
                    },
                    loc: stmt.loc,
                }
            },

            _ => fold::noop_fold_stmt(self, stmt, module, function),
        }
    }
}

struct CaseFoldingDuplicateGensymVisitor {
    value_renamers: Vec<ScopedSubstitutionFolder>,
    type_renamers: Vec<ScopedSubstitutionFolder>,
    fn_renamers: Vec<ScopedSubstitutionFolder>,
    member_renamers: Vec<ScopedSubstitutionFolder>,
    module_renamers: Vec<ScopedSubstitutionFolder>,
    seen: HashSet<(String, Option<String>, Option<String>)>,
                  // casefold   // module      // scope
}

impl CaseFoldingDuplicateGensymVisitor {
    fn new() -> Self {
        Self {
            value_renamers: Vec::new(),
            type_renamers: Vec::new(),
            fn_renamers: Vec::new(),
            member_renamers: Vec::new(),
            module_renamers: Vec::new(),
            seen: HashSet::new(),
        }
    }
}

impl ASTVisitor for CaseFoldingDuplicateGensymVisitor {
    fn visit_ident(&mut self, ident: &Ident, ctxt: NameCtxt, loc: &SrcLoc) {
        let (mut module, mut function, what) = match ctxt {
            NameCtxt::DefValue(m, f, _) => (Some(m), f, Rename::Value),
            NameCtxt::DefParam(m, f, _, _) => (Some(m), Some(f), Rename::Value),
            NameCtxt::DefFunction(m) => (Some(m), None, Rename::Function),
            NameCtxt::DefType(m) => (Some(m), None, Rename::Type),
            // members are tricky: we only care if we see clashing members in
            //   the same type...                   // pun here
            NameCtxt::DefMember(m, t, _) => (Some(m), Some(t), Rename::Member),
            NameCtxt::DefModule => (None, None, Rename::Module),
            _ => return
        };

        // just clone everything; jesus christ
        let casefold = ident.0.to_uppercase();
        let key = (
            casefold,
            module.cloned().map(|i| i.0),
            function.cloned().map(|i| i.0)
        );
        if !self.seen.contains(&key) {
            self.seen.insert(key);
            return;
        }

        // we're a duplicate

        let (values, fns, types, members, modules, dest) = match what {
            Rename::Value =>
                (true, false, false, false, false, &mut self.value_renamers),
            Rename::Function =>
                (false, true, false, false, false, &mut self.fn_renamers),
            Rename::Type =>
                (false, false, true, false, false, &mut self.type_renamers),
            Rename::Member => {
                // ... but we don't really want to do the hard work of
                //   type checking all member accesses, so we'll change the
                //   offender everywhere
                module = None;
                function = None;
                (false, false, false, true, false, &mut self.member_renamers)
            },
            Rename::Module =>
                (false, false, false, false, true, &mut self.module_renamers),
            _ => panic!(),
        };

        let g = gensym(Some(ident.clone()));
        dest.push(ScopedSubstitutionFolder {
            orig: ident.clone(),
            replace: g,
            module: module.cloned(),
            function: function.cloned(),
            defns: true,
            values: values,
            fns: fns,
            types: types,
            members: members,
            modules: modules,
        });
    }
}

/// replace logical-op expressions (and conditions) with short-circuiting
/// equivalents
pub fn short_circuit_logicals(dumpster: Dumpster) -> Dumpster {
    Dumpster {
        modules: dumpster.modules.into_iter().map(|m| {
            Module {
                name: m.name,
                data: match m.data {
                    ModuleKind::Normal(items) =>
                        ModuleKind::Normal(items.into_iter().map(|i| {
                            match i {
                                NormalItem::Function(def) =>
                                    NormalItem::Function(FunDef {
                                        name: def.name,
                                        access: def.access,
                                        params: def.params,
                                        ret: def.ret,
                                        body: short_circuit_logicals_stmts(def.body),
                                        loc: def.loc,
                                    }),

                                item => item,
                            }
                        }).collect()),
                },
                loc: m.loc,
            }
        }).collect()
    }
}

fn short_circuit_logicals_stmts(stmts: Vec<Stmt>) -> Vec<Stmt> {
    stmts
}

pub const VB_KEYWORDS: [&'static str; 152] = [
    "CALL",
    "CASE",
    "CLOSE",
    "CONST",
    "DECLARE",
    "DEFBOOL",
    "DEFBYTE",
    "DEFCUR",
    "DEFDATE",
    "DEFDBL",
    "DEFINT",
    "DEFLNG",
    "DEFLNGLNG",
    "DEFLNGPTR",
    "DEFOBJ",
    "DEFSNG",
    "DEFSTR",
    "DEFVAR",
    "DIM",
    "DO",
    "ELSE",
    "ELSEIF",
    "END",
    "ENDIF",
    "ENUM",
    "ERASE",
    "EVENT",
    "EXIT",
    "FOR",
    "FRIEND",
    "FUNCTION",
    "GET",
    "GLOBAL",
    "GOSUB",
    "GOTO",
    "IF",
    "IMPLEMENTS",
    "INPUT",
    "LET",
    "LOCK",
    "LOOP",
    "LSET",
    "NEXT",
    "ON",
    "OPEN",
    "OPTION",
    "PRINT",
    "PRIVATE",
    "PUBLIC",
    "PUT",
    "RAISEEVENT",
    "REDIM",
    "RESUME",
    "RETURN",
    "RSET",
    "SEEK",
    "SELECT",
    "SET",
    "STATIC",
    "STOP",
    "SUB",
    "TYPE",
    "UNLOCK",
    "WEND",
    "WHILE",
    "WITH",
    "WRITE",
    "REM",
    "ANY",
    "AS",
    "BYREF",
    "BYVAL",
    "CASE",
    "EACH",
    "ELSE",
    "IN",
    "NEW",
    "SHARED",
    "UNTIL",
    "WITHEVENTS",
    "WRITE",
    "OPTIONAL",
    "PARAMARRAY",
    "PRESERVE",
    "SPC",
    "TAB",
    "THEN",
    "TO",
    "ADDRESSOF",
    "AND",
    "EQV",
    "IMP",
    "IS",
    "LIKE",
    "NEW",
    "MOD",
    "NOT",
    "OR",
    "TYPEOF",
    "XOR",
    "ABS",
    "CBOOL",
    "CBYTE",
    "CCUR",
    "CDATE",
    "CDBL",
    "CDEC",
    "CINT",
    "CLNG",
    "CLNGLNG",
    "CLNG",
    "PTR",
    "CSNG",
    "CSTR",
    "CVAR",
    "CVERR",
    "DATE",
    "DEBUG",
    "DOEVENTS",
    "FIX",
    "INT",
    "LEN",
    "LENB",
    "ME",
    "PSET",
    "SCALE",
    "SGN",
    "STRING",
    "ARRAY",
    "CIRCLE",
    "INPUT",
    "INPUTB",
    "LBOUND",
    "SCALE",
    "UBOUND",
    "BOOLEAN",
    "BYTE",
    "CURRENCY",
    "DATE",
    "DOUBLE",
    "INTEGER",
    "LONG",
    "LONGLONG",
    "LONGPTR",
    "SINGLE",
    "STRING",
    "VARIANT",
    "TRUE",
    "FALSE",
    "NOTHING",
    "EMPTY",
    "NULL",
];
