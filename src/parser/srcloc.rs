//! trashcan's type for tracking source locations

use super::MappedSource;

use ast::Dumpster;
use fold::ASTFolder;

#[derive(Clone, Debug)]
pub struct SrcLoc {
    pub file: String,
    pub line: u32,
    pub start: usize,
    pub len: usize,
}

impl SrcLoc {
    pub fn empty() -> Self {
        Self {
            file: String::new(),
            line: 0,
            start: 0,
            len: 0,
        }
    }

    pub fn raw(start: usize, len: usize) -> Self {
        Self {
            file: String::new(),
            line: 0,
            start,
            len,
        }
    }
}

pub fn rebase_srclocs(dumpster: Dumpster, map: &MappedSource) -> Dumpster {
    let mut folder = SrcLocRebaseFolder { map };
    folder.fold_dumpster(dumpster)
}

struct SrcLocRebaseFolder<'a> {
    map: &'a MappedSource,
}

impl<'a> ASTFolder for SrcLocRebaseFolder<'a> {
    fn fold_srcloc(&mut self, loc: SrcLoc) -> SrcLoc {
        let (line, start) = self.map.pos_to_line_pos(
            self.map.map_to_original(loc.start - self.map.base()));
        SrcLoc {
            file: self.map.file.clone(),
            line: line,
            start: start,
            len: loc.len,
        }
    }
}
