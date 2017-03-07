//! code generation for trashcan bits/atoms

use std::io;
use std::io::Write;

use ast::*;
use super::*;
use analysis::SymbolTable;

impl Emit<()> for Ident {
    fn emit<W: Write>(&self, out: &mut W, _symtab: &SymbolTable,
      _ctxt: (), indent: u32) -> io::Result<()> {
        write!(out, "{:in$}{}", "", self.0, in = (indent * INDENT) as usize)
    }
}

impl Emit<()> for Access {
    fn emit<W: Write>(&self, out: &mut W, _symtab: &SymbolTable,
      _ctxt: (), indent: u32) -> io::Result<()> {
        write!(out, "{:in$}{}", "", match *self {
            Access::Private => "Private",
            Access::Public => "Public",
        }, in = (indent * INDENT) as usize)
    }
}

impl Emit<()> for ParamMode {
    fn emit<W: Write>(&self, out: &mut W, _symtab: &SymbolTable,
      _ctxt: (), indent: u32) -> io::Result<()> {
        write!(out, "{:in$}{}", "", match *self {
            ParamMode::ByVal => "ByVal",
            ParamMode::ByRef => "ByRef",
        }, in = (indent * INDENT) as usize)
    }
}

// just the "combination" operator
impl Emit<()> for AssignOp {
    fn emit<W: Write>(&self, out: &mut W, _symtab: &SymbolTable,
      _ctxt: (), _indent: u32) -> io::Result<()> {
        let op: &[u8] = match *self {
            AssignOp::AddAssign => b" + ",
            AssignOp::SubAssign => b" - ",
            AssignOp::MulAssign => b" * ",
            AssignOp::DivAssign => b" / ",
            AssignOp::ModAssign => b" Mod ",
            AssignOp::PowAssign => b" ^ ",
            AssignOp::StrCatAssign => b" & ",
            AssignOp::BitAndAssign => b" And ",
            AssignOp::BitOrAssign => b" Or ",
            AssignOp::LogAndAssign => b" And ",
            AssignOp::LogOrAssign => b" Or ",
            AssignOp::Assign => panic!("basic assign should never result in \
                AssignOp::emit()"),
        };
        out.write_all(op)
    }
}
