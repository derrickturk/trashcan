use super::*;
use ast::*;

use std::collections::HashMap;

/// A symbol table entry
pub enum Symbol {
    /// e.g. x: i32
    Value(Type),

    /// e.g. "SomeObj" => Type::Object("SomeObj")
    Type(Type),

    /// e.g. f : (i32, i32) -> i32
    Fun {
        def: FunDef,
        locals: HashMap<String, Symbol>,
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
                regarding: Some(format!("module {}", m.name.0)),
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

    Ok(symtab)
}

fn insert_module_items(tbl: &mut HashMap<String, Symbol>,
  items: &Vec<NormalItem>) -> AnalysisResult<()> {
    for i in items {
        match *i {
            NormalItem::Function(ref def) => insert_fundef(tbl, def)?,
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
    Ok(())
}
