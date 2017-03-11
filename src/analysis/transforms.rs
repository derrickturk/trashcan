//! trashcan's AST transforms, used to implement language features

use super::*;
use ast::*;

pub fn vba_keyword_gensym(dumpster: Dumpster) -> Dumpster {
    unimplemented!()
}

pub fn fn_name_local_gensym(dumpster: Dumpster) -> Dumpster {
    unimplemented!()
}

/// replace for loop iteration variables with gensyms for pseudo-block scoping
pub fn for_loop_var_gensym(dumpster: Dumpster) -> Dumpster {
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
                                        body: for_loop_var_gensym_stmts(def.body),
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

fn for_loop_var_gensym_stmts(stmts: Vec<Stmt>) -> Vec<Stmt> {
    stmts.into_iter().map(|s| {
        let sub_var = match s.data {
            StmtKind::ForLoop { ref var, .. } => Some(var.0.clone()),
            _ => None,
        };

        if let Some(var) = sub_var {
            let newvar = gensym(Some(var.clone()));
            s.substitute(&var, &newvar)
        } else {
            s
        }
    }).collect()
}

fn short_circuit_logicals_stmts(stmts: Vec<Stmt>) -> Vec<Stmt> {
    stmts
}

pub const VBA_KEYWORDS: [&'static str; 155] = [
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
    "GOT",
    "O",
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
    "RESU",
    "ME",
    "RETURN",
    "RSET",
    "SEEK",
    "SELECT",
    "SET",
    "STATIC",
    "STOP",
    "SU",
    "B",
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
