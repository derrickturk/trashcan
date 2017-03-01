//! code generation for trashcan items

use std::io;
use std::io::Write;

use ast::*;
use super::*;
use super::bits::*;

impl Emit for NormalItem {
    fn emit<W: Write>(&self, out: &mut W, indent: u32) -> io::Result<()> {
        match self {
            &NormalItem::Function(ref def) => def.emit(out, indent)
        }
    }
}

impl Emit for FunDef {
    fn emit<W: Write>(&self, out: &mut W, indent: u32) -> io::Result<()> {
        self.access.emit(out, indent)?;

        let fnsub = match self.ret {
            None => "Sub",
            Some(_) => "Function",
        };

        write!(out, " {} ", fnsub)?;

        self.name.emit(out, 0)?;

        out.write_all(b"(")?;
        for (i, p) in self.params.iter().enumerate() {
            if i != 0 {
                out.write_all(b", ")?;
            }
            out.write_all(b"type goes here")?;
        }
        out.write_all(b")")?;

        if let Some(ref ty) = self.ret {
            out.write_all(b" As ")?;
            out.write_all(b"type goes here")?;
        }
        out.write_all(b"\n")?;

        write!(out, "End {}\n", fnsub)?;

        Ok(())
    }
}
