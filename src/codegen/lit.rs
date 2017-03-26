//! code generation for trashcan literals

use std::io;
use std::io::Write;

use ast::*;
use super::*;
use analysis::SymbolTable;

impl Emit<()> for Literal {
    fn emit<W: Write>(&self, out: &mut W, _symtab: &SymbolTable,
      _ctxt: (), indent: u32) -> io::Result<()> {
        let as_str = match *self {
            Literal::NullPtr => String::from("Nothing"),
            Literal::NullVar => String::from("Null"),
            Literal::EmptyVar => String::from("Empty"),
            Literal::Bool(b) => String::from(if b { "True" } else { "False" }),
            Literal::UInt8(n) => n.to_string(),
            Literal::Int16(n) => n.to_string(),
            Literal::Int32(n) => n.to_string(),
            Literal::IntPtr(n) => n.to_string(),
            Literal::Float32(n) => format!("{}!", n),
            Literal::Float64(n) => format!("{}#", n),
            Literal::Currency(n) => format!("{}@", currency_string(n)),
            Literal::String(ref s) => vb_string(s),
            // TODO: handle wacky types
            _ => panic!("dumpster fire: don't know how to emit that yet"),
        };

        write!(out, "{:in$}{}", "", as_str, in = (indent * INDENT) as usize)
    }
}

fn currency_string(currency: i64) -> String {
    let frac = currency % 10000;
    let whole = currency / 10000;
    // TODO: this may be inadvisable for negative numbers
    format!("{}.{:04}", whole, frac)
}

fn vb_string(s: &String) -> String {
    let mut out = String::from("\"");
    for c in s.chars() {
        match c {
            '"' => out.push_str("\"\""),
            '\n' => out.push_str("\" & vbCrLf & \""),
            '\t' => out.push_str("\" & vbTab & \""),
            c => out.push(c),
        }
    }
    out.push_str("\"");
    out
}
