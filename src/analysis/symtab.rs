//! trashcan's symbol table (and deferred type resolution)

use super::*;
use ast::*;
use visit::NameCtxt;
use visit::ASTVisitor;
use visit::ASTVisitorMut;

use std::io;
use std::io::Write;

use std::collections::HashMap;

// TODO: types get their own namespace... or not
/// A symbol table entry
#[derive(Clone, Debug)]
pub enum Symbol {
    /// a constant definition
    Const(Type, Access),

    /// e.g. x: i32
    Value(Type, Option<ParamMode>, Access),

    /// e.g. f : (i32, i32) -> i32
    Fun {
        def: FunDef,
        locals: Scopetab,
    },

    /// e.g. struct X { a: i32 }
    Struct {
        def: StructDef,
        members: HashMap<String, Type>,
    },
}

impl Symbol {
    pub fn access(&self) -> Access {
        match *self {
            Symbol::Const(_, access) => access,
            Symbol::Value(_, _, access) => access,
            Symbol::Fun { ref def, .. } => def.access,
            Symbol::Struct { ref def, .. } => def.access,
        }
    }
}

// the internal symbol table type
type Symtab = HashMap<
    String, // module name
    Scopetab
>;

// a module's, or scope's symbol table type
type Scopetab = HashMap<
    String, // an item's or value's name
    Symbol
>;

/// The symbol table: scope -> (scope -> symbol|(ident -> symbol))
pub struct SymbolTable {
    symtab: Symtab,
}

impl SymbolTable {
    pub fn build(dumpster: &mut Dumpster) -> AnalysisResultMany<SymbolTable> {
        let mut symtab = SymbolTable::new();
        let mut errors = Vec::new();

        // first make a pass to collect all type declarations into
        //   the symbol table...
        {
            let mut type_collector =
                TypeCollectingSymbolTableBuilder::build(&mut symtab);
            type_collector.visit_dumpster(dumpster);

            match type_collector.result() {
                Ok(_) => { },
                Err(mut errs) => errors.append(&mut errs),
            };
        }

        // then a mutation pass over the AST to resolve Deferred type nodes
        {
            let mut resolver = DeferredResolver::from(&symtab);
            resolver.visit_dumpster(dumpster);

            match resolver.result() {
                Ok(_) => { },
                Err(mut errs) => errors.append(&mut errs),
            };
        }

        // then a final pass to collect values and functions into the symbol
        //   table, using the resolved types
        {
            let mut value_collector =
                ValueCollectingSymbolTableBuilder::build(&mut symtab);
            value_collector.visit_dumpster(dumpster);

            match value_collector.result() {
                Ok(_) => { },
                Err(mut errs) => errors.append(&mut errs),
            };
        }

        if errors.is_empty() {
            Ok(symtab)
        } else {
            Err(errors)
        }
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
                Symbol::Const(_, _) | Symbol::Value(_, _, _) => Ok(sym),

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

            _ => panic!("dumpster fire: invalid context for path lookup")
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

    pub fn add_value_entry(&mut self, ident: &Ident, module: &Ident,
      function: Option<&Ident>, ty: &Type, access: Access, err_loc: &SrcLoc)
      -> AnalysisResult<()> {
        let mut value_collector =
            ValueCollectingSymbolTableBuilder::build(self);
        value_collector.visit_ident(ident,
          NameCtxt::DefValue(module, function, ty, access), err_loc);

        match value_collector.result() {
            Ok(_) => Ok(()),
            Err(errs) => Err(errs.into_iter().next()
              .expect("dumpster fire: empty error vector")),
        }
    }

    pub fn type_access(&self, ty: &Type, module: &Ident, err_loc: &SrcLoc)
      -> AnalysisResult<Access> {
        match *ty {
            Type::Array(ref base, _) => self.type_access(base, module, err_loc),

            Type::Object(_) => Ok(Access::Public), // TODO: for now

            Type::Struct(ref path) => {
                match *self.symbol_at_path(path,
                  NameCtxt::Type(module, Access::Private), err_loc)? {
                    Symbol::Struct { ref def, .. } => Ok(def.access),
                    _ => panic!("dumpster fire: non-struct made it to \
                      access check"),
                }
            },

            Type::Enum(_) => Ok(Access::Public), // TODO: for now,

            Type::Deferred(_) => panic!("dumpster fire: deferred type in \
              access check"),

            _ => Ok(Access::Public),
        }
    }

    fn module_table(&self, module: &Ident) -> Option<&Scopetab> {
        self.symtab.get(&module.0)
    }

    fn module_table_mut(&mut self, module: &Ident) -> Option<&mut Scopetab> {
        self.symtab.get_mut(&module.0)
    }

    fn new() -> SymbolTable {
        SymbolTable {
            symtab: Symtab::new(),
        }
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
            _ => panic!("dumpster fire: invalid context for path lookup")
        };

        let allow_private = access == Access::Private;

        let symtab = self.module_table(m).ok_or(AnalysisError {
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
                    panic!("dumpster fire: no function record for {}",
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
            Symbol::Const(ref ty, _) =>
                write!(out, "constant {}\n", ty)?,
            Symbol::Value(ref ty, ref mode, _) =>
                write!(out, "value {:?} {}\n", mode, ty)?,
            Symbol::Fun { ref def, ref locals } => {
                write!(out, "fn {}\n", def.name.0)?;
                dump_sub_tbl(out, locals, ind + 1)?;
            },
            Symbol::Struct { ref def, ref members } => {
                write!(out, "struct {}\n", def.name.0)?;
                for m in members {
                    write!(out, "{:in$}member {}: {}\n", "", m.0, m.1,
                      in=(ind + 1)*4).unwrap();
                }
            },
        }
    }
    Ok(())
}

struct TypeCollectingSymbolTableBuilder<'a> {
    symtab: &'a mut SymbolTable,
    errors: Vec<AnalysisError>,
}

impl<'a> TypeCollectingSymbolTableBuilder<'a> {
    fn build(symtab: &'a mut SymbolTable) -> Self {
        TypeCollectingSymbolTableBuilder {
            symtab,
            errors: Vec::new(),
        }
    }

    fn result(self) -> AnalysisResultMany<()> {
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors)
        }
    }
}

impl<'a> ASTVisitor for TypeCollectingSymbolTableBuilder<'a> {
    fn visit_module(&mut self, m: &Module) {
        if self.symtab.symtab.contains_key(&m.name.0) {
            self.errors.push(AnalysisError {
                kind: AnalysisErrorKind::DuplicateSymbol,
                regarding: Some(format!("mod {}", m.name)),
                loc: m.loc.clone(),
            });
        } else {
            self.symtab.symtab.insert(m.name.0.clone(), HashMap::new());
        }

        self.walk_module(m);
    }

    fn visit_structdef(&mut self, def: &StructDef, m: &Ident) {
        {
            let mod_tab = self.symtab.module_table_mut(m).expect(
                "dumpster fire: no module entry in symbol table");

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
}

struct ValueCollectingSymbolTableBuilder<'a> {
    symtab: &'a mut SymbolTable,
    errors: Vec<AnalysisError>,
}

impl<'a> ValueCollectingSymbolTableBuilder<'a> {
    fn build(symtab: &'a mut SymbolTable) -> Self {
        ValueCollectingSymbolTableBuilder {
            symtab,
            errors: Vec::new(),
        }
    }

    fn result(self) -> AnalysisResultMany<()> {
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors)
        }
    }
}

impl<'a> ASTVisitor for ValueCollectingSymbolTableBuilder<'a> {
    fn visit_fundef(&mut self, def: &FunDef, m: &Ident) {
        {
            let mod_tab = self.symtab.module_table_mut(m).expect(
                "dumpster fire: no module entry in symbol table");

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

    fn visit_structmem(&mut self, mem: &StructMem, m: &Ident, st: &Ident) {
        {
            let mod_tab = self.symtab.module_table_mut(m).expect(
                "dumpster fire: no module entry in symbol table");

            let members = match mod_tab.get_mut(&st.0) {
                Some(&mut Symbol::Struct { ref mut members, .. }) => members,
                _ => panic!("dumpster fire: \
                  no struct entry for {}::{}", m, st),
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

    fn visit_path(&mut self, p: &Path, ctxt: NameCtxt, loc: &SrcLoc) {
        // TODO something something statics and consts
        //   (related: those guys need lifted to the top of modules)
        // ensure declare-before-use of "local" names
        match *p {
            Path(None, _) => {
                match ctxt {
                    NameCtxt::Value(_, _, _) => {
                        if let Err(e) =
                          self.symtab.symbol_at_path(p, ctxt, loc) {
                            self.errors.push(e);
                            return;
                        }
                    }

                    _ => {},
                }
            },

            _ => {},
        }

        self.walk_path(p, ctxt, loc);
    }

    fn visit_ident(&mut self, i: &Ident, ctxt: NameCtxt, loc: &SrcLoc) {
        let (module, scope, sym, desc) = match ctxt {
            NameCtxt::DefValue(m, f, ty, access) =>
                (m, f, Symbol::Value(ty.clone(), None, access), "variable"),
            NameCtxt::DefParam(m, f, ty, mode) =>
                (m, Some(f), Symbol::Value(ty.clone(), Some(mode), Access::Private),
                  "parameter"),
            NameCtxt::DefConstant(m, ty, access) =>
                (m, None, Symbol::Const(ty.clone(), access), "constant"),
            _ => { return; },
        };

        let mod_tab = self.symtab.module_table_mut(module).expect(
            "dumpster fire: no module entry in symbol table");

        if let Some(f) = scope {
            let locals = match mod_tab.get_mut(&f.0) {
                Some(&mut Symbol::Fun { ref mut locals, .. }) => locals,
                _ => panic!("dumpster fire: \
                  no fn entry in symbol table for {}::{}", module, f),
            };

            if locals.contains_key(&i.0) {
                self.errors.push(AnalysisError {
                    kind: AnalysisErrorKind::DuplicateSymbol,
                    regarding: Some(format!("{} {}", desc, i)),
                    loc: loc.clone(),
                });
            } else {
                locals.insert(i.0.clone(), sym);
            }
        } else {
            if mod_tab.contains_key(&i.0) {
                self.errors.push(AnalysisError {
                    kind: AnalysisErrorKind::DuplicateSymbol,
                    regarding: Some(format!("{} {}", desc, i)),
                    loc: loc.clone(),
                });
            } else {
                mod_tab.insert(i.0.clone(), sym);
            }
        }
    }
}

struct DeferredResolver<'a> {
    symtab: &'a SymbolTable,
    errors: Vec<AnalysisError>,
}

impl<'a> DeferredResolver<'a> {
    fn from(symtab: &'a SymbolTable) -> Self {
        DeferredResolver {
            symtab,
            errors: Vec::new(),
        }
    }

    fn result(self) -> AnalysisResultMany<()> {
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors)
        }
    }
}

impl<'a> ASTVisitorMut for DeferredResolver<'a> {
    fn visit_type(&mut self, t: &mut Type, module: &Ident, loc: &SrcLoc) {
        let new_type = match *t {
            Type::Deferred(ref path) => {
                match self.symtab.symbol_at_path(path,
                  NameCtxt::Type(module, Access::Private), loc) {
                    Ok(&Symbol::Struct { .. }) => {
                        Type::Struct(Path(
                                Some(path.0.as_ref().unwrap_or(module).clone()),
                                path.1.clone()
                        ))
                    },

                    Ok(_) => {
                        panic!("dumpster fire: \
                          type lookup produced non-type");
                    },

                    Err(e) => {
                        self.errors.push(e);
                        return;
                    },
                }
            }

            // this is a goofy way to build this but I don't feel like
            //   fighting the damn borrow checker right now
            _ => {
                // we need to potentially recurse into e.g. array types
                self.walk_type(t, module, loc);
                return;
            }
        };

        *t = new_type;
    }
}
