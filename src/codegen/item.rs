//! code generation for trashcan items

use std::io;
use std::io::Write;

use ast::*;
use super::*;

impl Emit for NormalItem {
    fn emit<W: Write>(&self, out: &mut W, indent: u32) -> io::Result<()> {
        match self {
            &NormalItem::Function(ref def) => Ok(())
        }
    }
}
