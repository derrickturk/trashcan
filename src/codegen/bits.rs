//! code generation for trashcan bits/atoms

use std::io;
use std::io::Write;

use ast::*;
use super::*;

impl Emit<()> for Ident {
    fn emit<W: Write>(&self, out: &mut W, _ctxt: (), indent: u32)
      -> io::Result<()> {
        write!(out, "{:in$}{}", "", self.0, in = (indent * INDENT) as usize)
    }
}

impl Emit<()> for Access {
    fn emit<W: Write>(&self, out: &mut W, _ctxt: (), indent: u32)
      -> io::Result<()> {
        write!(out, "{:in$}{}", "", match self {
            &Access::Private => "Private",
            &Access::Public => "Public",
        }, in = (indent * INDENT) as usize)
    }
}

impl Emit<()> for ParamMode {
    fn emit<W: Write>(&self, out: &mut W, _ctxt: (), indent: u32)
      -> io::Result<()> {
        write!(out, "{:in$}{}", "", match self {
            &ParamMode::ByVal => "ByVal",
            &ParamMode::ByRef => "ByRef",
        }, in = (indent * INDENT) as usize)
    }
}
