//! code generation for trashcan items

use std::io;
use std::io::Write;

use ast::*;
use super::*;
use super::bits::*;
use super::ty::*;

impl<'a> Emit<&'a Module> for NormalItem {
    fn emit<W: Write>(&self, out: &mut W, symtab: &SymbolTable,
      ctxt: &'a Module, indent: u32) -> io::Result<()> {
        match *self {
            NormalItem::Function(ref def) =>
                def.emit(out, symtab, ctxt, indent)
        }
    }
}

impl<'a> Emit<&'a Module> for FunDef {
    fn emit<W: Write>(&self, out: &mut W, symtab: &SymbolTable,
      ctxt: &'a Module, indent: u32) -> io::Result<()> {
        self.access.emit(out, symtab, (), indent)?;

        let fnsub = match self.ret {
            None => "Sub",
            Some(_) => "Function",
        };

        write!(out, " {} ", fnsub)?;

        self.name.emit(out, symtab, (), 0)?;

        out.write_all(b"(")?;
        for (i, p) in self.params.iter().enumerate() {
            if i != 0 {
                out.write_all(b", ")?;
            }
            p.emit(out, symtab, (), 0)?;
        }
        out.write_all(b")")?;

        if let Some(ref ty) = self.ret {
            ty.emit(out, symtab, TypePos::FunRet, 0)?;
        }
        out.write_all(b"\n")?;

        for stmt in self.body.iter() {
            stmt.emit(out, symtab, self, indent + 1)?;
        }

        write!(out, "End {}\n", fnsub)?;

        Ok(())
    }
}

impl Emit<()> for FunParam {
    fn emit<W: Write>(&self, out: &mut W, symtab: &SymbolTable,
      ctxt: (), indent: u32) -> io::Result<()> {
        self.mode.emit(out, symtab, (), indent)?;
        out.write_all(b" ")?;
        self.name.emit(out, symtab, (), 0)?;
        self.typ.emit(out, symtab, TypePos::FunParam, 0)
    }
}
