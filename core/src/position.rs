use crate::{
  ipld_error::IpldError,
  parse::span::Span,
};

use sp_cid::Cid;
use sp_ipld::Ipld;

use sp_std::{
  borrow::ToOwned,
  convert::TryInto,
  fmt,
};

use alloc::string::String;

/// Source code position of an expression in a file
#[derive(PartialEq, Clone, Copy, Debug)]
pub struct Position {
  pub input: Cid,
  pub from_offset: u64,
  pub from_line: u64,
  pub from_column: u64,
  pub upto_offset: u64,
  pub upto_line: u64,
  pub upto_column: u64,
}

/// Wrapper optional type for Position
#[derive(PartialEq, Clone, Copy)]
pub enum Pos {
  None,
  Some(Position),
}
impl fmt::Debug for Pos {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "") }
}

impl Position {
  ///
  pub fn range(self, input: String) -> String {
    let mut res = String::new();
    let gutter = format!("{}", self.upto_line).len();
    let pad = format!("{: >gutter$}", self.from_line, gutter = gutter).len()
      + 3
      + self.from_column as usize;
    res.push_str(&format!("{}▼\n", " ".to_owned().repeat(pad)));
    for (line_number, line) in input.lines().enumerate() {
      if ((line_number as u64 + 1) >= self.from_line)
        && ((line_number as u64 + 1) <= self.upto_line)
      {
        res.push_str(&format!(
          "{: >gutter$} | {}\n",
          line_number + 1,
          line,
          gutter = gutter
        ));
      }
    }
    let pad = format!("{: >gutter$}", self.upto_line, gutter = gutter).len()
      + 3
      + self.upto_column as usize;
    res.push_str(&format!("{}▲", " ".to_owned().repeat(pad)));
    res
  }

  /// Converts a position into an IPLD object
  pub fn to_ipld(self) -> Ipld {
    Ipld::List(vec![
      Ipld::Link(self.input),
      Ipld::Integer(self.from_offset as i128),
      Ipld::Integer(self.from_line as i128),
      Ipld::Integer(self.from_column as i128),
      Ipld::Integer(self.upto_offset as i128),
      Ipld::Integer(self.upto_line as i128),
      Ipld::Integer(self.upto_column as i128),
    ])
  }

  ///
  pub fn from_upto(input: Cid, from: Span, upto: Span) -> Self {
    Self {
      input,
      from_offset: (from.location_offset() as u64),
      from_line: from.location_line() as u64,
      from_column: from.get_utf8_column() as u64,
      upto_offset: (upto.location_offset() as u64),
      upto_line: upto.location_line() as u64,
      upto_column: upto.get_utf8_column() as u64,
    }
  }

  /// Converts an IPLD object into a position
  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::List(xs) => match xs.as_slice() {
        #[rustfmt::skip]
        [Ipld::Link(cid),
         Ipld::Integer(from_offset),
         Ipld::Integer(from_line),
         Ipld::Integer(from_column),
         Ipld::Integer(upto_offset),
         Ipld::Integer(upto_line),
         Ipld::Integer(upto_column),
        ] => {
          let from_offset: u64 =
            (*from_offset).try_into().map_err(IpldError::U64)?;
          let from_line: u64 =
            (*from_line).try_into().map_err(IpldError::U64)?;
          let from_column: u64 =
            (*from_column).try_into().map_err(IpldError::U64)?;
          let upto_offset: u64 =
            (*upto_offset).try_into().map_err(IpldError::U64)?;
          let upto_line: u64 =
            (*upto_line).try_into().map_err(IpldError::U64)?;
          let upto_column: u64 =
            (*upto_column).try_into().map_err(IpldError::U64)?;
          Ok(Position {
            input: *cid,
            from_offset,
            from_line,
            from_column,
            upto_offset,
            upto_line,
            upto_column})
        }
        xs => Err(IpldError::Position(Ipld::List(xs.to_owned()))),
      },
      xs => Err(IpldError::Position(xs.to_owned())),
    }
  }
}

impl Pos {
  /// Converts a pos into an IPLD object
  pub fn to_ipld(self) -> Ipld {
    match self {
      Self::None => Ipld::Null,
      Self::Some(x) => Position::to_ipld(x),
    }
  }

  /// Converts an IPLD object into a pos
  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Null => Ok(Self::None),
      xs => {
        let pos = Position::from_ipld(&xs)?;
        Ok(Self::Some(pos))
      }
    }
  }

  pub fn from_upto(input: Cid, from: Span, upto: Span) -> Self {
    Pos::Some(Position::from_upto(input, from, upto))
  }
}

impl fmt::Display for Position {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}@{}:{}-{}:{}",
      self.input,
      self.from_line,
      self.from_column,
      self.upto_line,
      self.upto_column
    )
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  use crate::tests::arbitrary_cid;

  impl Arbitrary for Position {
    fn arbitrary(g: &mut Gen) -> Self {
      Position {
        input: arbitrary_cid(g),
        from_offset: Arbitrary::arbitrary(g),
        from_line: Arbitrary::arbitrary(g),
        from_column: Arbitrary::arbitrary(g),
        upto_offset: Arbitrary::arbitrary(g),
        upto_line: Arbitrary::arbitrary(g),
        upto_column: Arbitrary::arbitrary(g),
      }
    }
  }

  impl Arbitrary for Pos {
    fn arbitrary(g: &mut Gen) -> Self {
      let x: bool = Arbitrary::arbitrary(g);
      if x { Self::None } else { Self::Some(Arbitrary::arbitrary(g)) }
    }
  }

  #[quickcheck]
  fn position_ipld(x: Position) -> bool {
    match Position::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }

  #[quickcheck]
  fn pos_ipld(x: Pos) -> bool {
    match Pos::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}
