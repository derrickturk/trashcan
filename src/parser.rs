//! trashcan's parser and affiliated types

#[derive(Clone)]
pub struct SrcLoc {
    pub file: String,
    pub line: u32,
    pub start: u32,
    pub len: u32,
}
