use crate::span::Span;
// use blake3;

use std::fmt;

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct Pos {
    // file: blake3::Hash,
    pub from_offset: u64,
    pub from_line: u64,
    pub from_column: u64,
    pub upto_offset: u64,
    pub upto_line: u64,
    pub upto_column: u64,
}

impl Pos {
    #[must_use]
    pub fn from_upto(from: Span, upto: Span) -> Self {
        Self {
            from_offset: (from.location_offset() as u64),
            from_line: u64::from(from.location_line()),
            from_column: from.get_utf8_column() as u64,
            upto_offset: (upto.location_offset() as u64),
            upto_line: u64::from(upto.location_line()),
            upto_column: upto.get_utf8_column() as u64,
        }
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}-{}:{}",
            self.from_line, self.from_column, self.upto_line, self.upto_column
        )
    }
}
