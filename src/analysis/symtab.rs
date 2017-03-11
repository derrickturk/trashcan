//! trashcan's symbol table 

use super::*;
use ast::*;

use std::collections::HashMap;

// TODO: types get their own namespace
// TODO: need to deal with case-insensitivity
/// A symbol table entry
#[derive(Clone, Debug)]
pub enum Symbol {
    /// a constant definition
    Const(Type),

    /// e.g. x: i32
    Value(Type, Option<ParamMode>),

    /// e.g. f : (i32, i32) -> i32
    Fun {
        def: FunDef,
        locals: HashMap<String, Symbol>,
    },

    /// e.g. struct X { a: i32 }
    Struct {
        def: StructDef,
        members: HashMap<String, Type>,
    },
}

impl Symbol {
    pub fn access(&self) -> Access {
        // TODO: update these
        match *self {
            Symbol::Const(_) => Access::Public,
            Symbol::Value(_, _) => Access::Public,
            Symbol::Fun { ref def, .. } => def.access.clone(),
            Symbol::Struct { ref def, .. } => def.access.clone(),
        }
    }
}

/// The symbol table: scope -> (scope -> symbol|(ident -> symbol))
pub type SymbolTable = HashMap<
    String, // module name
    HashMap<
        String, // item name
        Symbol
    >
>;

pub fn symbol_table(dumpster: &Dumpster) -> AnalysisResult<SymbolTable> {
    let mut symtab: SymbolTable = SymbolTable::new();
    for m in &dumpster.modules {
        if symtab.contains_key(&m.name.0) {
            return Err(AnalysisError {
                kind: AnalysisErrorKind::DuplicateSymbol,
                regarding: Some(format!("mod {}", m.name.0)),
                loc: m.loc.clone(),
            });
        }
        symtab.insert(m.name.0.clone(), HashMap::new());

        match m.data {
            ModuleKind::Normal(ref items) => {
                insert_module_items(symtab.get_mut(&m.name.0).unwrap(), items)?;
            },
        }
    }

    // TODO: implement this
    // resolve_deferred(&mut symtab);

    Ok(symtab)
}

fn insert_module_items(tbl: &mut HashMap<String, Symbol>,
  items: &Vec<NormalItem>) -> AnalysisResult<()> {
    for i in items {
        match *i {
            NormalItem::Function(ref def) => insert_fundef(tbl, def)?,
            NormalItem::Struct(ref def) => insert_structdef(tbl, def)?,
        }
    }

    Ok(())
}

fn insert_fundef(tbl: &mut HashMap<String, Symbol>, def: &FunDef)
  -> AnalysisResult<()> {
    if tbl.contains_key(&def.name.0) {
        return Err(AnalysisError {
            kind: AnalysisErrorKind::DuplicateSymbol,
            regarding: Some(format!("fn {}", def.name.0)),
            loc: def.loc.clone(),
        });
    }

    tbl.insert(def.name.0.clone(), Symbol::Fun {
        def: def.clone(),
        locals: HashMap::new(),
    });

    let locals = match tbl.get_mut(&def.name.0) {
        Some(&mut Symbol::Fun { ref mut locals, .. }) => locals,
        _ => panic!("internal compiler error"),
    };

    for p in &def.params {
        if locals.contains_key(&p.name.0) {
            return Err(AnalysisError {
                kind: AnalysisErrorKind::DuplicateSymbol,
                regarding: Some(format!("parameter {}", p.name.0)),
                loc: p.loc.clone(),
            });
        }
        locals.insert(p.name.0.clone(),
          Symbol::Value(p.ty.clone(), Some(p.mode)));
    }

    for stmt in &def.body {
        match stmt.data {
            StmtKind::VarDecl(ref decls) => {
                for var in decls {
                    if locals.contains_key(&(var.0).0) {
                        return Err(AnalysisError {
                            kind: AnalysisErrorKind::DuplicateSymbol,
                            regarding: Some(format!("variable {}", (var.0).0)),
                            loc: stmt.loc.clone(),
                        });
                    }
                    locals.insert((var.0).0.clone(),
                      Symbol::Value(var.1.clone(), None));
                }
            },

            // TODO: maybe should this gensym?
            StmtKind::ForLoop { ref var, .. } => {
                if locals.contains_key(&(var.0).0) {
                    return Err(AnalysisError {
                        kind: AnalysisErrorKind::DuplicateSymbol,
                        regarding: Some(format!("for-variable {}", (var.0).0)),
                        loc: stmt.loc.clone(),
                    });
                }
                // TODO: might be byref if we do the for-each trick
                locals.insert((var.0).0.clone(),
                  Symbol::Value(var.1.clone(), None));
            },

            _ => {},
        }
    }

    Ok(())
}

fn insert_structdef(tbl: &mut HashMap<String, Symbol>, def: &StructDef)
  -> AnalysisResult<()> {
    if tbl.contains_key(&def.name.0) {
        return Err(AnalysisError {
            kind: AnalysisErrorKind::DuplicateSymbol,
            regarding: Some(format!("struct {}", def.name.0)),
            loc: def.loc.clone(),
        });
    }

    tbl.insert(def.name.0.clone(), Symbol::Struct {
        def: def.clone(),
        members: HashMap::new(),
    });

    let members = match tbl.get_mut(&def.name.0) {
        Some(&mut Symbol::Struct { ref mut members, .. }) => members,
        _ => panic!("internal compiler error"),
    };

    for m in &def.members {
        if members.contains_key(&m.name.0) {
            return Err(AnalysisError {
                kind: AnalysisErrorKind::DuplicateSymbol,
                regarding: Some(format!("parameter {}", m.name.0)),
                loc: m.loc.clone(),
            });
        }
        members.insert(m.name.0.clone(), m.ty.clone());
    }

    Ok(())
}
