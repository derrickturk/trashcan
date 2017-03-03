//! code generation for trashcan modules

use std::io;
use std::io::Write;

use ast::*;
use super::*;

impl Emit<()> for Module {
    fn emit<W: Write>(&self, out: &mut W, _ctxt: (), indent: u32)
      -> io::Result<()> {
        match self.data {
            ModuleKind::Normal(ref items) => {
                write_normal_header(&self.name, out, indent)?;
                for (i, item) in items.iter().enumerate() {
                    if i != 0 {
                        out.write_all(b"\n")?;
                    }
                    item.emit(out, self, indent)?;
                }
                Ok(())
            },
        }
    }
}

fn write_normal_header<W: Write>(name: &Ident, out: &mut W, indent: u32)
  -> io::Result<()> {
    write!(out, "{:in$}Attribute VB_Name = \"{}\"\n", "", name.0,
           in = (indent * INDENT) as usize)
}
