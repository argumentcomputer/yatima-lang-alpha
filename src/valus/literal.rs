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
  NaturalType,
  Nat(u64, BigUint),
  NatType,
  Integer(BigInt),
  IntegerType,
  Int(u64, BigInt),
  IntType,
  BitString(Vec<u8>),
  BitStringType,
  BitVector(u64, Vec<u8>),
  BitVectorType,
  Text(String),
  TextType,
  Char(char),
  CharType,
  Link(Link),
  LinkType,
  Exception(String),
  ExceptionType,
}

impl fmt::Display for Literal {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::NatType => write!(f, "#Nat"),
      Self::NaturalType => write!(f, "#Natural"),
      Self::IntType => write!(f, "#Int"),
      Self::IntegerType => write!(f, "#Integer"),
      Self::BitStringType => write!(f, "#BitString"),
      Self::BitVectorType => write!(f, "#BitVector"),
      Self::LinkType => write!(f, "#Link"),
      Self::TextType => write!(f, "#Text"),
      Self::CharType => write!(f, "#Char"),
      Self::ExceptionType => write!(f, "#Exception"),
      Self::Nat(len, x) => write!(f, "0d{}:nat{}", x.to_str_radix(10), len),
      Self::Natural(x) => write!(f, "0d{}", x.to_str_radix(10)),
      Self::Integer(x) => {
        let sign = match x.sign() {
          Sign::Minus => "-",
          _ => "+",
        };
        write!(f, "{}0d{}", sign, x.to_str_radix(10))
      }
      Self::Int(len, x) => {
        let sign = match x.sign() {
          Sign::Minus => "-",
          _ => "+",
        };
        write!(f, "{}0d{}:int{}", sign, x.to_str_radix(10), len)
      }
      Self::BitVector(len, x) => {
        let x: &[u8] = x.as_ref();
        write!(f, "#{}:bits{}", base::encode(Base::_64, x), len)
      }
      Self::BitString(x) => {
        let x: &[u8] = x.as_ref();
        write!(f, "#{}:bits", base::encode(Base::_64, x))
      }
      Self::Link(l) => write!(f, "#{}", l),
      Self::Text(x) => write!(f, "\"{}\"", x.escape_default()),
      Self::Char(x) => write!(f, "'{}'", x.escape_default()),
      Self::Exception(x) => write!(f, "#!\"{}\"", x.escape_default()),
    }
  }
}

impl Literal {
  pub fn encode(self) -> Expr {
    match self {
      Self::NatType => Atom(None, Symbol(String::from("#Nat"))),
      Self::NaturalType => Atom(None, Symbol(String::from("#Natural"))),
      Self::IntType => Atom(None, Symbol(String::from("#Int"))),
      Self::IntegerType => Atom(None, Symbol(String::from("#Integer"))),
      Self::BitStringType => Atom(None, Symbol(String::from("#BitString"))),
      Self::BitVectorType => Atom(None, Symbol(String::from("#BitVector"))),
      Self::LinkType => Atom(None, Symbol(String::from("#Link"))),
      Self::TextType => Atom(None, Symbol(String::from("#Text"))),
      Self::CharType => Atom(None, Symbol(String::from("#Char"))),
      Self::ExceptionType => Atom(None, Symbol(String::from("#Exception"))),
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
      Atom(_, Symbol(n)) if *n == String::from("#Nat") => Ok(Self::NatType),
      Atom(_, Symbol(n)) if *n == String::from("#Natural") => {
        Ok(Self::NaturalType)
      }
      Atom(_, Symbol(n)) if *n == String::from("#Int") => Ok(Self::IntType),
      Atom(_, Symbol(n)) if *n == String::from("#Integer") => {
        Ok(Self::IntegerType)
      }
      Atom(_, Symbol(n)) if *n == String::from("#BitString") => {
        Ok(Self::BitStringType)
      }
      Atom(_, Symbol(n)) if *n == String::from("#BitVector") => {
        Ok(Self::BitVectorType)
      }
      Atom(_, Symbol(n)) if *n == String::from("#Text") => Ok(Self::TextType),
      Atom(_, Symbol(n)) if *n == String::from("#Char") => Ok(Self::CharType),
      Atom(_, Symbol(n)) if *n == String::from("#Link") => Ok(Self::LinkType),
      Atom(_, Symbol(n)) if *n == String::from("#Exception") => {
        Ok(Self::ExceptionType)
      }
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
      let gen = g.gen_range(0, 16);
      match gen {
        0 => Self::NatType,
        1 => Self::NaturalType,
        2 => Self::IntType,
        3 => Self::IntegerType,
        4 => Self::BitStringType,
        5 => Self::BitVectorType,
        6 => Self::TextType,
        7 => Self::CharType,
        8 => Self::LinkType,
        9 => Self::ExceptionType,
        10 => arbitrary_nat(g),
        11 => arbitrary_int(g),
        12 => arbitrary_bits(g),
        13 => arbitrary_text(g),
        14 => Self::Char(Arbitrary::arbitrary(g)),
        15 => Self::Link(arbitrary_link(g)),
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
}
