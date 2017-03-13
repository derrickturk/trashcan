//! trashcan's symbol table (and deferred type resolution)

use super::*;
use ast::*;
use visit::NameCtxt;
use visit::ASTVisitor;

use std::io;
use std::io::Write;

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

// the internal symbol table type
type Symtab = HashMap<
    String, // module name
    HashMap<
        String, // item name
        Symbol
    >
>;

/// The symbol table: scope -> (scope -> symbol|(ident -> symbol))
pub struct SymbolTable {
    // temporarily public
    pub symtab: Symtab,
}

impl SymbolTable {
    pub fn build(dumpster: &Dumpster) -> AnalysisResult<SymbolTable> {
        let mut visitor = SymbolTableBuilder::new();
        visitor.visit_dumpster(dumpster);

        for err in visitor.errors.drain(..) { // for now
            return Err(err);
        }

        // TODO: implement this
        // resolve_deferred(&mut symtab);

        Ok(SymbolTable {
            symtab: visitor.symtab
        })
    }

    // TODO: probably build symbol_at_ident etc (steal guts of
    //   symbol_at_path_unchecked) and reimplement symbol_at_path in terms
    //   of that

    pub fn symbol_at_path(&self, path: &Path,
      ctxt: NameCtxt, err_loc: &SrcLoc) -> AnalysisResult<&Symbol> {
        let sym = self.symbol_at_path_unchecked(path, ctxt, err_loc)?;
        match ctxt {
            NameCtxt::Function(_, _) => match *sym {
                Symbol::Fun { .. } => Ok(sym),

                Symbol::Struct { .. } => Err(AnalysisError {
                    kind: AnalysisErrorKind::FnCallError,
                    regarding: Some(format!("{} denotes a type, not a function",
                      path)),
                    loc: err_loc.clone(),
                }),

                _ => Err(AnalysisError {
                    kind: AnalysisErrorKind::FnCallError,
                    regarding: Some(format!("{} denotes a value, not a function",
                      path)),
                    loc: err_loc.clone(),
                }),
            },

            NameCtxt::Type(_, _) => match *sym {
                Symbol::Struct { .. } => Ok(sym),

                Symbol::Fun { .. } => Err(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(format!("{} denotes a function, not a type",
                      path)),
                    loc: err_loc.clone(),
                }),

                _ => Err(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(format!("{} denotes a value, not a type",
                      path)),
                    loc: err_loc.clone(),
                }),
            },

            NameCtxt::Value(_, _, _) => match *sym {
                Symbol::Const(_) | Symbol::Value(_, _) => Ok(sym),

                Symbol::Fun { .. } => Err(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(format!("{} denotes a function, not a value",
                      path)),
                    loc: err_loc.clone(),
                }),

                Symbol::Struct { .. } => Err(AnalysisError {
                    kind: AnalysisErrorKind::TypeError,
                    regarding: Some(format!("{} denotes a type, not a value",
                      path)),
                    loc: err_loc.clone(),
                }),
            },

            _ => panic!("internal compiler error: invalid context for path lookup")
        }
    }

    pub fn dump<W: Write>(&self, out: &mut W, ind: usize) -> io::Result<()> {
        out.write_all(b"SYMBOL TABLE DUMP\n")?;
        for (ref m, ref tbl) in &self.symtab {
            write!(out, "module {}:\n", m)?;
            dump_sub_tbl(out, tbl, ind + 1)?;
        }
        Ok(())
    }

    fn symbol_at_path_unchecked(&self, path: &Path,
      ctxt: NameCtxt, err_loc: &SrcLoc) -> AnalysisResult<&Symbol> {
        struct DummyVisitor;
        impl ASTVisitor for DummyVisitor { }

        let mut v = DummyVisitor;
        let (ident, ctxt) = v.ident_ctxt_from_path(path, ctxt);

        let (m, scope, access) = match ctxt {
            NameCtxt::Function(m, access) => (m, None, access),
            NameCtxt::Type(m, access) => (m, None, access),
            NameCtxt::Value(m, scope, access) => (m, scope, access),
            _ => panic!("internal compiler error: invalid context for path lookup")
        };

        let allow_private = access == Access::Private;

        let symtab = self.symtab.get(&m.0).ok_or(AnalysisError {
            kind: AnalysisErrorKind::NotDefined,
            regarding: Some(format!("mod {}", m)),
            loc: err_loc.clone(),
        })?;

        // check local scope, if any, first
        match scope {
            Some(fun) => {
                if let Some(&Symbol::Fun { ref locals, .. }) = symtab.get(&fun.0) {
                    if let Some(sym) = locals.get(&ident.0) {
                        if allow_private || sym.access() == Access::Public {
                            return Ok(sym);
                        }
                    }
                } else {
                    panic!("internal compiler error: no function record for {}",
                      fun);
                }
            }
            None => {},
        }

        match symtab.get(&ident.0) {
            Some(sym) => {
                if allow_private || sym.access() == Access::Public {
                    Ok(sym)
                } else {
                    Err(AnalysisError {
                        kind: AnalysisErrorKind::SymbolAccess,
                        regarding: Some(format!("{} is private to {}",
                          path, m)),
                        loc: err_loc.clone(),
                    })
                }
            },

            None => Err(AnalysisError {
                kind: AnalysisErrorKind::NotDefined,
                regarding: Some(format!("{}", path)),
                loc: err_loc.clone(),
            })
        }
    }
}

fn dump_sub_tbl<W: Write>(out: &mut W,
  tbl: &HashMap<String, Symbol>, ind: usize) -> io::Result<()> {
    for (k, sym) in tbl {
        write!(out, "{:in$}item {}: ", "", k, in=ind*4).unwrap();
        match *sym {
            Symbol::Const(ref ty) =>
                write!(out, "constant {:?}\n", ty)?,
            Symbol::Value(ref ty, ref mode) =>
                write!(out, "value {:?} {:?}\n", mode, ty)?,
            Symbol::Fun { ref def, ref locals } => {
                write!(out, "fn {}\n", def.name.0)?;
                dump_sub_tbl(out, locals, ind + 1)?;
            },
            Symbol::Struct { ref def, ref members } => {
                write!(out, "struct {}\n", def.name.0)?;
                for m in members {
                    write!(out, "{:in$}member {}: {:?}\n", "", m.0, m.1,
                      in=(ind + 1)*4).unwrap();
                }
            },
        }
    }
    Ok(())
}

struct SymbolTableBuilder {
    symtab: Symtab,
    errors: Vec<AnalysisError>,
}

impl SymbolTableBuilder {
    fn new() -> Self {
        SymbolTableBuilder {
            symtab: Symtab::new(),
            errors: Vec::new(),
        }
    }
}

impl ASTVisitor for SymbolTableBuilder {
    fn visit_module(&mut self, m: &Module) {
        if self.symtab.contains_key(&m.name.0) {
            self.errors.push(AnalysisError {
                kind: AnalysisErrorKind::DuplicateSymbol,
                regarding: Some(format!("mod {}", m.name)),
                loc: m.loc.clone(),
            });
        } else {
            self.symtab.insert(m.name.0.clone(), HashMap::new());
        }

        self.walk_module(m);
    }

    fn visit_fundef(&mut self, def: &FunDef, m: &Ident) {
        {
            let mod_tab = self.symtab.get_mut(&m.0).expect(
                "internal compiler error: no module entry in symbol table");

            if mod_tab.contains_key(&def.name.0) {
                self.errors.push(AnalysisError {
                    kind: AnalysisErrorKind::DuplicateSymbol,
                    regarding: Some(format!("fn {}::{}", m, def.name)),
                    loc: def.loc.clone(),
                });
            } else {
                mod_tab.insert(def.name.0.clone(), Symbol::Fun {
                    def: def.clone(),
                    locals: HashMap::new(),
                });
            }
        }

        self.walk_fundef(def, m);
    }

    fn visit_structdef(&mut self, def: &StructDef, m: &Ident) {
        {
            let mod_tab = self.symtab.get_mut(&m.0).expect(
                "internal compiler error: no module entry in symbol table");

            if mod_tab.contains_key(&def.name.0) {
                self.errors.push(AnalysisError {
                    kind: AnalysisErrorKind::DuplicateSymbol,
                    regarding: Some(format!("fn {}::{}", m, def.name)),
                    loc: def.loc.clone(),
                });
            } else {
                mod_tab.insert(def.name.0.clone(), Symbol::Struct {
                    def: def.clone(),
                    members: HashMap::new(),
                });
            }
        }

        self.walk_structdef(def, m);
    }

    fn visit_structmem(&mut self, mem: &StructMem, m: &Ident, st: &Ident) {
        {
            let mod_tab = self.symtab.get_mut(&m.0).expect(
                "internal compiler error: no module entry in symbol table");

            let members = match mod_tab.get_mut(&st.0) {
                Some(&mut Symbol::Struct { ref mut members, .. }) => members,
                _ => panic!("internal compiler error"),
            };

            if members.contains_key(&mem.name.0) {
                self.errors.push(AnalysisError {
                    kind: AnalysisErrorKind::DuplicateSymbol,
                    regarding: Some(format!("struct member {}::{}",
                      st, mem.name)),
                    loc: mem.loc.clone(),
                });
            } else {
                members.insert(mem.name.0.clone(), mem.ty.clone());
            }
        }

        self.walk_structmem(mem, m, st);
    }

    fn visit_ident(&mut self, i: &Ident, ctxt: NameCtxt, loc: &SrcLoc) {
        let (module, scope, ty, mode, desc) = match ctxt {
            NameCtxt::DefValue(m, f, ty) =>
                (m, f, ty, None, "variable"),
            NameCtxt::DefParam(m, f, ty, mode) =>
                (m, Some(f), ty, Some(mode), "parameter"),
            _ => { return; },
        };

        let mod_tab = self.symtab.get_mut(&module.0).expect(
            "internal compiler error: no module entry in symbol table");

        if let Some(f) = scope {
            let locals = match mod_tab.get_mut(&f.0) {
                Some(&mut Symbol::Fun { ref mut locals, .. }) => locals,
                _ => panic!("internal compiler error"),
            };

            if locals.contains_key(&i.0) {
                self.errors.push(AnalysisError {
                    kind: AnalysisErrorKind::DuplicateSymbol,
                    regarding: Some(format!("{} {}", desc, i)),
                    loc: loc.clone(),
                });
            } else {
                locals.insert(i.0.clone(), Symbol::Value(ty.clone(), mode));
            }
        } else {
            if mod_tab.contains_key(&i.0) {
                self.errors.push(AnalysisError {
                    kind: AnalysisErrorKind::DuplicateSymbol,
                    regarding: Some(format!("{} {}", desc, i)),
                    loc: loc.clone(),
                });
            } else {
                mod_tab.insert(i.0.clone(), Symbol::Value(ty.clone(), mode));
            }
        }
    }
}
