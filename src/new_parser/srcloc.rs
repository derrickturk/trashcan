//! trashcan's types for tracking source locations

/*
use ast::Dumpster;
use fold::ASTFolder;
*/

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

pub struct MappedSource {
    file: String,
    // processed source for parser
    src: Vec<u8>,
    // inclusive
    gaps: Vec<(usize, usize)>,
    // line beginnings
    lines: Vec<usize>,
}

impl MappedSource {
    fn base(&self) -> usize {
        self.src.as_ptr() as usize
    }

    fn map_to_original(&self, mut pos: usize) -> usize {
        for &(first, skipped) in &self.gaps {
            if first <= pos {
                pos += skipped;
            }
        }
        pos
    }

    // one-based line, char
    fn pos_to_line_pos(&self, pos: usize) -> (u32, usize) {
        self.lines.iter().cloned().take_while(|begin| *begin <= pos)
            .fold((0, 0), |(line, _), line_begin| {
                (line + 1, pos - line_begin + 1)
            })
    }
}

/*

TODO: add me back in

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

*/
