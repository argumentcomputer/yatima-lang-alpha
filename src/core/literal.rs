use crate::decode_error::{
  DecodeError,
  Expected,
};
use hashexpr::{
  atom,
  atom::Atom::*,
  base,
  base::Base,
  link::Link,
  Expr,
  Expr::{
    Atom,
    Cons,
  },
};
use num_bigint::{
  BigInt,
  BigUint,
  Sign,
};

use std::fmt;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Literal {
  Natural(BigUint),
  Integer(BigInt),
  BitString(Vec<u8>),
  Text(String),
  Char(char),
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum LitType {
  Natural,
  Integer,
  BitString,
  Text,
  Char,
}

impl fmt::Display for Literal {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use Literal::*;
    match self {
      Natural(x) => write!(f, "0d{}", x.to_str_radix(10)),
      Integer(x) => {
        let sign = match x.sign() {
          Sign::Minus => "-",
          _ => "+",
        };
        write!(f, "{}0d{}", sign, x.to_str_radix(10))
      }
      BitString(x) => {
        let x: &[u8] = x.as_ref();
        write!(f, "~\"{}\"", Base::encode(&Base::_64, x))
      }
      Text(x) => write!(f, "\"{}\"", x.escape_default()),
      Char(x) => write!(f, "'{}'", x.escape_default()),
    }
  }
}

impl Literal {
  pub fn encode(self) -> Expr {
    match self {
      Self::Natural(x) => nat!(x),
      Self::Integer(x) => int!(x),
      Self::BitString(x) => bits!(x),
      Self::Text(x) => text!(x),
      Self::Char(x) => char!(x),
    }
  }

  pub fn decode(x: Expr) -> Result<Self, DecodeError> {
    match x {
      Atom(_, Nat(x)) => Ok(Self::Natural(x)),
      Atom(_, Int(x)) => Ok(Self::Integer(x)),
      Atom(_, Bits(x)) => Ok(Self::BitString(x)),
      Atom(_, Text(x)) => Ok(Self::Text(x)),
      Atom(_, Char(x)) => Ok(Self::Char(x)),
      _ => Err(DecodeError::new(x.position(), vec![Expected::Literal])),
    }
  }
}

impl LitType {
  pub fn encode(self) -> Expr {
    match self {
      Self::Natural => text!("#Natural"),
      Self::Integer => text!("#Integer"),
      Self::BitString => text!("#BitString"),
      Self::Text => text!("#Text"),
      Self::Char => text!("#Char"),
    }
  }

  pub fn decode(x: Expr) -> Result<Self, DecodeError> {
    match x {
      Atom(_, Text(n)) if *n == String::from("#Natural") => Ok(Self::Natural),
      Atom(_, Text(n)) if *n == String::from("#Integer") => Ok(Self::Integer),
      Atom(_, Text(n)) if *n == String::from("#BitString") => {
        Ok(Self::BitString)
      }
      Atom(_, Text(n)) if *n == String::from("#Text") => Ok(Self::Text),
      Atom(_, Text(n)) if *n == String::from("#Char") => Ok(Self::Char),
      _ => Err(DecodeError::new(x.position(), vec![Expected::LitType])),
    }
  }
}

impl fmt::Display for LitType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Natural => write!(f, "#Natural"),
      Self::Integer => write!(f, "#Integer"),
      Self::BitString => write!(f, "#BitString"),
      Self::Text => write!(f, "#Text"),
      Self::Char => write!(f, "#Char"),
    }
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  use rand::{
    prelude::IteratorRandom,
    Rng,
  };

  pub fn arbitrary_bits<G: Gen>(g: &mut G) -> Literal {
    let x: Vec<u8> = Arbitrary::arbitrary(g);
    Literal::BitString(x)
  }

  pub fn arbitrary_text<G: Gen>(g: &mut G) -> Literal {
    let x: String = Arbitrary::arbitrary(g);
    Literal::Text(x)
  }
  pub fn arbitrary_nat<G: Gen>(g: &mut G) -> Literal {
    let v: Vec<u8> = Arbitrary::arbitrary(g);
    let x: BigUint = BigUint::from_bytes_be(&v);
    Literal::Natural(x)
  }

  pub fn arbitrary_int<G: Gen>(g: &mut G) -> Literal {
    let v: Vec<u8> = Arbitrary::arbitrary(g);
    let x: BigInt = BigInt::from_signed_bytes_be(&v);
    Literal::Integer(x)
  }

  impl Arbitrary for Literal {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      let gen = g.gen_range(0, 4);
      match gen {
        0 => arbitrary_nat(g),
        1 => arbitrary_int(g),
        2 => arbitrary_bits(g),
        3 => arbitrary_text(g),
        _ => Self::Char(Arbitrary::arbitrary(g)),
      }
    }
  }
  #[quickcheck]
  fn literal_encode_decode(x: Literal) -> bool {
    match Literal::decode(x.clone().encode()) {
      Ok(y) => x == y,
      _ => false,
    }
  }

  impl Arbitrary for LitType {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      let gen = g.gen_range(0, 5);
      match gen {
        1 => Self::Natural,
        2 => Self::Integer,
        3 => Self::BitString,
        4 => Self::Text,
        _ => Self::Char,
      }
    }
  }
  #[quickcheck]
  fn lit_type_encode_decode(x: LitType) -> bool {
    match LitType::decode(x.clone().encode()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}
