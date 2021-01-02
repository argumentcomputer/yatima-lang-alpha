use crate::{
  base,
  base::Base,
  error::{
    DeserialError,
    ParseError,
  },
  span::Span,
  Atom,
  Expr,
};
use nom::{
  bytes::complete::tag,
  combinator::opt,
  Err,
  Err::Error,
  IResult,
};
use std::fmt;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct Link(blake3::Hash);

impl Link {
  pub fn make(x: &[u8]) -> Link { Link(blake3::hash(x)) }

  pub fn from(x: [u8; 32]) -> Link { Link(blake3::Hash::from(x)) }

  pub fn as_hash(&self) -> &blake3::Hash {
    match self {
      Link(h) => h,
    }
  }

  pub fn as_bytes(&self) -> &[u8; 32] { self.as_hash().as_bytes() }

  pub fn serialize(&self) -> Vec<u8> {
    Expr::Atom(None, Atom::Link(self.clone())).serialize()
  }

  pub fn deserialize(i: &[u8]) -> IResult<&[u8], Link, DeserialError<&[u8]>> {
    match Expr::deserialize(i) {
      Ok((i, Expr::Atom(_, Atom::Link(x)))) => Ok((i, x)),
      Ok((i, _)) => Err(Error(DeserialError::ExpectedLink(i))),
      Err(e) => Err(e),
    }
  }

  pub fn parse(i: &str) -> IResult<Span, Link, ParseError<Span>> {
    let i = Span::new(i);
    let (i, _) = opt(tag("#"))(i)?;
    let (i, (_, raw)) = base::parse(i).map_err(|e| nom::Err::convert(e))?;
    let (_, x) = Link::deserialize(&raw).map_err(|e| match e {
      Err::Incomplete(n) => Err::Incomplete(n),
      Err::Error(e) => {
        Err::Error(ParseError::DeserialErr(i, e.to_owned().input_as_bytes()))
      }
      Err::Failure(e) => {
        Err::Failure(ParseError::DeserialErr(i, e.to_owned().input_as_bytes()))
      }
    })?;
    Ok((i, x))
  }
}

impl fmt::Display for Link {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", base::encode(Base::_58, self.serialize()))
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
    StdThreadGen,
  };
  use rand::Rng;

  impl Arbitrary for Link {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      let bytes: [u8; 32] = [
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
        Arbitrary::arbitrary(g),
      ];
      Link::from(bytes)
    }
  }
  #[quickcheck]
  fn link_deserial_serial(x: Link) -> bool {
    match Link::deserialize(&x.serialize()) {
      Ok((_, y)) => x == y,
      _ => false,
    }
  }
  #[quickcheck]
  fn link_print_parse(x: Link) -> bool {
    match Link::parse(&format!("{}", x)) {
      Ok((_, y)) => x == y,
      _ => false,
    }
  }
}
