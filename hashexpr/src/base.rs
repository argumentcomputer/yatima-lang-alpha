use crate::{
  error::{
    ParseError,
    ParseErrorKind,
  },
  span::Span,
};
use core::fmt;
use nom::{
  self,
  branch::alt,
  bytes::complete::tag,
  combinator::value,
  IResult,
  InputTakeAtPosition,
};

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Base {
  _2,
  _8,
  _10,
  _16,
  _32,
  _58,
  _64,
}

impl Default for Base {
  fn default() -> Self { Self::_32 }
}

impl fmt::Display for Base {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self::_2 => write!(f, "#base2"),
      Self::_8 => write!(f, "#base8"),
      Self::_10 => write!(f, "#base10"),
      Self::_16 => write!(f, "#base16"),
      Self::_32 => write!(f, "#base32"),
      Self::_58 => write!(f, "#base58"),
      Self::_64 => write!(f, "#base64"),
    }
  }
}

impl Base {
  pub fn parse_code(i: Span) -> IResult<Span, Self, ParseError<Span>> {
    alt((
      value(Self::_2, tag("b")),
      value(Self::_8, tag("o")),
      value(Self::_10, tag("d")),
      value(Self::_16, tag("x")),
      value(Self::_32, tag("v")),
      value(Self::_58, tag("I")),
      value(Self::_64, tag("~")),
    ))(i)
  }

  /// Get the code corresponding to the base algorithm.
  #[must_use]
  pub const fn code(self) -> char {
    match self {
      Self::_2 => 'b',
      Self::_8 => 'o',
      Self::_10 => 'd',
      Self::_16 => 'x',
      Self::_32 => 'v',
      Self::_58 => 'I',
      Self::_64 => '~',
    }
  }

  #[must_use]
  pub fn base_digits(&self) -> &str {
    match self {
      Self::_2 => "01",
      Self::_8 => "01234567",
      Self::_10 => "0123456789",
      Self::_16 => "0123456789abcdef",
      Self::_32 => "ybndrfg8ejkmcpqxot1uwisza345h769",
      Self::_58 => "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz",
      Self::_64 => {
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
      }
    }
  }

  #[must_use]
  pub fn is_digit(self, x: char) -> bool {
    self.base_digits().chars().any(|y| x == y)
  }

  pub fn encode<I: AsRef<[u8]>>(self, input: I) -> String {
    base_x::encode(self.base_digits(), input.as_ref())
  }

  pub fn decode(
    self,
    input: Span,
  ) -> IResult<Span, Vec<u8>, ParseError<Span>> {
    let (i, o) = input.split_at_position_complete(|x| !self.is_digit(x))?;
    match base_x::decode(self.base_digits(), o.fragment()) {
      Ok(bytes) => Ok((i, bytes)),
      Err(_) => Err(nom::Err::Error(ParseError::new(
        i,
        ParseErrorKind::InvalidBaseEncoding(self),
      ))),
    }
  }

  pub fn decode1(
    self,
    input: Span,
  ) -> IResult<Span, Vec<u8>, ParseError<Span>> {
    let (i, o) = input.split_at_position1_complete(
      |x| !self.is_digit(x),
      nom::error::ErrorKind::Digit,
    )?;
    match base_x::decode(self.base_digits(), o.fragment()) {
      Ok(bytes) => Ok((i, bytes)),
      Err(_) => Err(nom::Err::Error(ParseError::new(
        i,
        ParseErrorKind::InvalidBaseEncoding(self),
      ))),
    }
  }
}

pub fn parse(input: Span) -> IResult<Span, (Base, Vec<u8>), ParseError<Span>> {
  let (i, base) = Base::parse_code(input)?;
  let (i, bytes) = base.decode(i)?;
  Ok((i, (base, bytes)))
}

pub fn encode<T: AsRef<[u8]>>(base: Base, input: T) -> String {
  let input = input.as_ref();
  let mut encoded = base.encode(input.as_ref());
  encoded.insert(0, base.code());
  encoded
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  use rand::Rng;

  impl Arbitrary for Base {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      let x: u32 = g.gen();
      match x % 7 {
        0 => Base::_2,
        1 => Base::_8,
        2 => Base::_10,
        3 => Base::_16,
        4 => Base::_32,
        5 => Base::_58,
        6 => Base::_64,
        _ => panic!("impossible"),
      }
    }
  }

  #[quickcheck]
  fn prop_parse_code_identity(x: Base) -> bool {
    match Base::parse_code(Span::new(&x.code().to_string())) {
      Ok((_, y)) => x == y && y.code() == x.code(),
      _ => false,
    }
  }
  #[quickcheck]
  fn prop_parse_string_identity(x: Base, s: String) -> bool {
    match parse(Span::new(&encode(x, s.clone()))) {
      Ok((_, (y, s2))) => x == y && s.into_bytes() == s2,
      _ => false,
    }
  }
}
