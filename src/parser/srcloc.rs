//! trashcan's types for tracking source locations

use super::{ParseErrorKind, CutParseResult, ParseError, ParseResult};

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
    pub fn src(&self) -> &[u8] {
        &self.src
    }

    pub fn translate_errors<R>(&self, res: CutParseResult<R>)
      -> ParseResult<R> {
        let (i, res) = match res {
            Ok((i, res)) => (i, res),
            Err((i, e)) => (i, Err(e)),
        };

        match res {
            Ok(r) => Ok(r),
            Err(e) => {
                let pos = i.as_ptr() as usize;
                let (line, start) = self.pos_to_line_pos(
                    self.map_to_original(pos - self.base()));
                Err(ParseError {
                    kind: e,
                    loc: SrcLoc {
                        file: self.file.clone(),
                        line,
                        start,
                        len: 0,
                    }
                })
            },
        }
    }

    pub fn rebase_srclocs(&self, dumpster: Dumpster) -> Dumpster {
        let mut folder = SrcLocRebaseFolder { map: self };
        folder.fold_dumpster(dumpster)
    }

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

pub fn map_source(file: &str, input: &[u8]) -> MappedSource {
    let mut in_line_comment = false;
    let mut in_block_comment = false;
    let mut in_quote = false;

    let mut res = MappedSource {
        file: String::from(file),
        src: Vec::new(),
        gaps: Vec::new(),
        lines: vec![0],
    };

    let mut gap_begin = 0;
    let mut skipped = 0;

    let mut bytes = input.iter().cloned().enumerate();

    while let Some((pos, c)) = bytes.next() {
        if in_line_comment {
            if c == b'\n' {
                in_line_comment = false;
                res.src.push(b'\n');
                res.lines.push(pos + 1);
                res.gaps.push((gap_begin, skipped));
            } else {
                skipped += 1;
            }
            continue;
        }

        if in_block_comment {
            if c == b'*' {
                skipped += 1;
                match bytes.next() {
                    Some((pos, b'/')) => {
                        in_block_comment = false;
                        res.src.push(b' '); // replace block comment by space
                        res.gaps.push((gap_begin, skipped));
                    },

                    Some((pos, b'\n')) => {
                        res.src.push(b'\n');
                        res.lines.push(pos + 1);
                    },

                    None => {
                        return res;
                    },

                    _ => {
                        skipped += 1;
                    }
                }
            } else if c == b'\n' {
                res.src.push(b'\n');
                res.lines.push(pos + 1);
            } else {
                skipped += 1;
            }
            continue;
        }

        if in_quote {
            if c == b'\\' {
                match bytes.next() {
                    Some((_, c)) => {
                        res.src.push(b'\\');
                        res.src.push(c);
                    },
                    None => {
                        return res;
                    }
                }
                continue;
            } else if c == b'\n' {
                res.lines.push(pos + 1);
            } else if c == b'"' {
                in_quote = false;
            }
            res.src.push(c);
        } else if c == b'/' {
            match bytes.next() {
                Some((_, b'/')) => {
                    in_line_comment = true;
                    gap_begin = pos;
                    skipped = 2;
                },
                Some((_, b'*')) => {
                    in_block_comment = true;
                    gap_begin = pos;
                    skipped = 2;
                },
                Some((_, c)) => {
                    res.src.push(b'/');
                    res.src.push(c);
                }
                None => {
                    return res;
                }
            }
        } else {
            if c == b'"' {
                in_quote = true;
            }

            if c == b'\n' {
                res.lines.push(pos + 1);
            }

            res.src.push(c);
        }
    }
    res
}

struct SrcLocRebaseFolder<'a> {
    pub map: &'a MappedSource,
}

impl<'a> ASTFolder for SrcLocRebaseFolder<'a> {
    fn fold_srcloc(&mut self, loc: SrcLoc) -> SrcLoc {
        let (line, start) = self.map.pos_to_line_pos(
            self.map.map_to_original(loc.start - self.map.base()));
        SrcLoc {
            file: self.map.file.clone(),
            line,
            start,
            len: loc.len,
        }
    }
}
