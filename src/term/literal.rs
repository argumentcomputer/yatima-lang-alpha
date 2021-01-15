use crate::decode_error::{
  DecodeError,
  Expected,
};
use hashexpr::{
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
  Nat(u64, BigUint),
  Integer(BigInt),
  Int(u64, BigInt),
  BitString(Vec<u8>),
  BitVector(u64, Vec<u8>),
  Text(String),
  Char(char),
  Link(Link),
  Exception(String),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum LitType {
  Nat,
  Natural,
  Int,
  Integer,
  BitString,
  BitVector,
  Text,
  Char,
  Link,
  Exception,
}

impl fmt::Display for Literal {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use Literal::*;
    match self {
      Nat(len, x) => write!(f, "0d{}:nat{}", x.to_str_radix(10), len),
      Natural(x) => write!(f, "0d{}", x.to_str_radix(10)),
      Integer(x) => {
        let sign = match x.sign() {
          Sign::Minus => "-",
          _ => "+",
        };
        write!(f, "{}0d{}", sign, x.to_str_radix(10))
      }
      Int(len, x) => {
        let sign = match x.sign() {
          Sign::Minus => "-",
          _ => "+",
        };
        write!(f, "{}0d{}:int{}", sign, x.to_str_radix(10), len)
      }
      BitVector(len, x) => {
        let x: &[u8] = x.as_ref();
        write!(f, "#{}:bits{}", base::encode(Base::_64, x), len)
      }
      BitString(x) => {
        let x: &[u8] = x.as_ref();
        write!(f, "#{}:bits", base::encode(Base::_64, x))
      }
      Link(l) => write!(f, "#{}", l),
      Text(x) => write!(f, "\"{}\"", x.escape_default()),
      Char(x) => write!(f, "'{}'", x.escape_default()),
      Exception(x) => write!(f, "#!\"{}\"", x.escape_default()),
    }
  }
}

impl Literal {
  pub fn encode(self) -> Expr {
    match self {
      Self::Nat(len, x) => Atom(None, Nat(x, Some(len))),
      Self::Natural(x) => Atom(None, Nat(x, None)),
      Self::Int(len, x) => Atom(None, Int(x, Some(len))),
      Self::Integer(x) => Atom(None, Int(x, None)),
      Self::BitString(x) => Atom(None, Bits(x, None)),
      Self::BitVector(len, x) => Atom(None, Bits(x, Some(len))),
      Self::Text(x) => Atom(None, Text(x, None)),
      Self::Char(x) => Atom(None, Char(x)),
      Self::Link(x) => Atom(None, Link(x)),
      Self::Exception(x) => Cons(None, vec![
        Atom(None, Symbol(String::from("exception"))),
        Atom(None, Text(x, None)),
      ]),
    }
  }

  pub fn decode(x: Expr) -> Result<Self, DecodeError> {
    match x {
      Atom(_, Nat(x, Some(len))) => Ok(Self::Nat(len, x)),
      Atom(_, Nat(x, None)) => Ok(Self::Natural(x)),
      Atom(_, Int(x, Some(len))) => Ok(Self::Int(len, x)),
      Atom(_, Int(x, None)) => Ok(Self::Integer(x)),
      Atom(_, Bits(x, Some(len))) => Ok(Self::BitVector(len, x)),
      Atom(_, Bits(x, None)) => Ok(Self::BitString(x)),
      Atom(_, Text(x, None)) => Ok(Self::Text(x)),
      Atom(_, Char(x)) => Ok(Self::Char(x)),
      Atom(_, Link(x)) => Ok(Self::Link(x)),
      Cons(pos, xs) => match xs.as_slice() {
        [Atom(_, Symbol(n)), Atom(_, Text(x, None))]
          if *n == String::from("exception") =>
        {
          Ok(Self::Exception(x.to_owned()))
        }
        _ => Err(DecodeError::new(pos, vec![Expected::Literal])),
      },
      _ => Err(DecodeError::new(x.position(), vec![Expected::Literal])),
    }
  }
}

impl LitType {
  pub fn encode(self) -> Expr {
    match self {
      Self::Nat => Atom(None, symb!("#Nat")),
      Self::Natural => Atom(None, symb!("#Natural")),
      Self::Int => Atom(None, symb!("#Int")),
      Self::Integer => Atom(None, symb!("#Integer")),
      Self::BitString => Atom(None, symb!("#BitString")),
      Self::BitVector => Atom(None, symb!("#BitVector")),
      Self::Text => Atom(None, symb!("#Text")),
      Self::Char => Atom(None, symb!("#Char")),
      Self::Link => Atom(None, symb!("#Link")),
      Self::Exception => Atom(None, symb!("#Exception")),
    }
  }

  pub fn decode(x: Expr) -> Result<Self, DecodeError> {
    match x {
      Atom(_, Symbol(n)) if *n == String::from("#Nat") => Ok(Self::Nat),
      Atom(_, Symbol(n)) if *n == String::from("#Natural") => Ok(Self::Natural),
      Atom(_, Symbol(n)) if *n == String::from("#Int") => Ok(Self::Int),
      Atom(_, Symbol(n)) if *n == String::from("#Integer") => Ok(Self::Integer),
      Atom(_, Symbol(n)) if *n == String::from("#BitString") => {
        Ok(Self::BitString)
      }
      Atom(_, Symbol(n)) if *n == String::from("#BitVector") => {
        Ok(Self::BitVector)
      }
      Atom(_, Symbol(n)) if *n == String::from("#Text") => Ok(Self::Text),
      Atom(_, Symbol(n)) if *n == String::from("#Char") => Ok(Self::Char),
      Atom(_, Symbol(n)) if *n == String::from("#Link") => Ok(Self::Link),
      Atom(_, Symbol(n)) if *n == String::from("#Exception") => {
        Ok(Self::Exception)
      }
      _ => Err(DecodeError::new(x.position(), vec![Expected::LitType])),
    }
  }
}

impl fmt::Display for LitType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Nat => write!(f, "#Nat"),
      Self::Natural => write!(f, "#Natural"),
      Self::Int => write!(f, "#Int"),
      Self::Integer => write!(f, "#Integer"),
      Self::BitString => write!(f, "#BitString"),
      Self::BitVector => write!(f, "#BitVector"),
      Self::Link => write!(f, "#Link"),
      Self::Text => write!(f, "#Text"),
      Self::Char => write!(f, "#Char"),
      Self::Exception => write!(f, "#Exception"),
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

  use crate::term::tests::arbitrary_link;

  pub fn arbitrary_bits<G: Gen>(g: &mut G) -> Literal {
    let x: Vec<u8> = Arbitrary::arbitrary(g);
    let b: bool = Arbitrary::arbitrary(g);
    if b {
      Literal::BitVector((x.len() as u64) * 8, x)
    }
    else {
      Literal::BitString(x)
    }
  }

  pub fn arbitrary_text<G: Gen>(g: &mut G) -> Literal {
    let x: String = Arbitrary::arbitrary(g);
    let b: bool = Arbitrary::arbitrary(g);
    Literal::Text(x)
  }
  pub fn arbitrary_nat<G: Gen>(g: &mut G) -> Literal {
    let v: Vec<u8> = Arbitrary::arbitrary(g);
    let x: BigUint = BigUint::from_bytes_be(&v);
    let b: bool = Arbitrary::arbitrary(g);
    if b {
      Literal::Nat(x.to_bytes_be().len() as u64 * 8, x)
    }
    else {
      Literal::Natural(x)
    }
  }
  pub fn arbitrary_int<G: Gen>(g: &mut G) -> Literal {
    let v: Vec<u8> = Arbitrary::arbitrary(g);
    let x: BigInt = BigInt::from_signed_bytes_be(&v);
    let b: bool = Arbitrary::arbitrary(g);
    if b {
      Literal::Int(x.to_signed_bytes_be().len() as u64 * 8, x)
    }
    else {
      Literal::Integer(x)
    }
  }

  impl Arbitrary for Literal {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      let gen = g.gen_range(0, 7);
      match gen {
        0 => arbitrary_nat(g),
        1 => arbitrary_int(g),
        2 => arbitrary_bits(g),
        3 => arbitrary_text(g),
        4 => Self::Char(Arbitrary::arbitrary(g)),
        5 => Self::Link(arbitrary_link(g)),
        _ => Self::Exception(String::from("test_exception")),
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
      let gen = g.gen_range(0, 9);
      match gen {
        0 => Self::Nat,
        1 => Self::Natural,
        2 => Self::Int,
        3 => Self::Integer,
        4 => Self::BitString,
        5 => Self::BitVector,
        6 => Self::Text,
        7 => Self::Char,
        8 => Self::Link,
        _ => Self::Exception,
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
