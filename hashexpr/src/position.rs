use crate::span::Span;
use blake3;

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
  pub fn from_upto(from: Span, upto: Span) -> Self {
    Pos {
      from_offset: (from.location_offset() as u64),
      from_line: from.location_line() as u64,
      from_column: from.get_utf8_column() as u64,
      upto_offset: (upto.location_offset() as u64),
      upto_line: upto.location_line() as u64,
      upto_column: upto.get_utf8_column() as u64,
    }
  }
}
