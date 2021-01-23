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
  Nat(Option<u64>, BigUint),
  Int(Option<u64>, BigInt),
  Bits(Option<u64>, Vec<u8>),
  Text(Option<u64>, String),
  Char(char),
  Link(Link),
  Exception(String),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum LitType {
  Nat,
  Int,
  Bits,
  Text,
  Char,
  Link,
  Exception,
}

impl fmt::Display for Literal {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use Literal::*;
    match self {
      Nat(Some(len), x) => write!(f, "0d{}:nat{}", x.to_str_radix(10), len),
      Nat(None, x) => write!(f, "0d{}:nat", x.to_str_radix(10)),
      Int(len, x) => {
        let sign = match x.sign() {
          Sign::Minus => "-",
          _ => "+",
        };
        match len {
          Some(len) => write!(f, "{}0d{}:int{}", sign, x.to_str_radix(10), len),
          None => write!(f, "{}0d{}", sign, x.to_str_radix(10)),
        }
      }
      Bits(Some(l), x) => {
        let x: &[u8] = x.as_ref();
        write!(f, "#{}:bits{}", base::encode(Base::_64, x), l)
      }
      Bits(None, x) => {
        let x: &[u8] = x.as_ref();
        write!(f, "#{}:bits", base::encode(Base::_64, x))
      }
      Link(l) => write!(f, "#{}", l),
      Text(Some(l), x) => write!(f, "\"{}\":text{}", x.escape_default(), l),
      Text(None, x) => write!(f, "\"{}\"", x.escape_default()),
      Char(x) => write!(f, "'{}'", x.escape_default()),
      Exception(x) => write!(f, "#!\"{}\"", x.escape_default()),
    }
  }
}

impl Literal {
  pub fn encode(self) -> Expr {
    match self {
      Self::Nat(len, x) => Atom(None, Nat(x, len)),
      Self::Int(len, x) => Atom(None, Int(x, len)),
      Self::Bits(len, x) => Atom(None, Bits(x, len)),
      Self::Text(len, x) => Atom(None, Text(x, len)),
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
      Atom(_, Nat(x, len)) => Ok(Self::Nat(len, x)),
      Atom(_, Int(x, len)) => Ok(Self::Int(len, x)),
      Atom(_, Bits(x, len)) => Ok(Self::Bits(len, x)),
      Atom(_, Text(x, len)) => Ok(Self::Text(len, x)),
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
      Self::Int => Atom(None, symb!("#Int")),
      Self::Bits => Atom(None, symb!("#Bits")),
      Self::Text => Atom(None, symb!("#Text")),
      Self::Char => Atom(None, symb!("#Char")),
      Self::Link => Atom(None, symb!("#Link")),
      Self::Exception => Atom(None, symb!("#Exception")),
    }
  }

  pub fn decode(x: Expr) -> Result<Self, DecodeError> {
    match x {
      Atom(_, Symbol(n)) if *n == String::from("#Nat") => Ok(Self::Nat),
      Atom(_, Symbol(n)) if *n == String::from("#Int") => Ok(Self::Int),
      Atom(_, Symbol(n)) if *n == String::from("#Bits") => Ok(Self::Bits),
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
      Self::Int => write!(f, "#Int"),
      Self::Bits => write!(f, "#Bits"),
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
    let l = if b { Some((x.len() as u64) * 8) } else { None };
    Literal::Bits(l, x)
  }

  pub fn arbitrary_text<G: Gen>(g: &mut G) -> Literal {
    let x: String = Arbitrary::arbitrary(g);
    let b: bool = Arbitrary::arbitrary(g);
    let l = if b { Some((x.len() as u64) * 8) } else { None };
    Literal::Text(l, x)
  }
  pub fn arbitrary_nat<G: Gen>(g: &mut G) -> Literal {
    let v: Vec<u8> = Arbitrary::arbitrary(g);
    let x: BigUint = BigUint::from_bytes_be(&v);
    let b: bool = Arbitrary::arbitrary(g);
    let l = if b { Some(x.to_bytes_be().len() as u64 * 8) } else { None };
    Literal::Nat(l, x)
  }
  pub fn arbitrary_int<G: Gen>(g: &mut G) -> Literal {
    let v: Vec<u8> = Arbitrary::arbitrary(g);
    let x: BigInt = BigInt::from_signed_bytes_be(&v);
    let b: bool = Arbitrary::arbitrary(g);
    let l =
      if b { Some(x.to_signed_bytes_be().len() as u64 * 8) } else { None };
    Literal::Int(l, x)
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
      let gen = g.gen_range(0, 7);
      match gen {
        0 => Self::Nat,
        1 => Self::Int,
        2 => Self::Bits,
        3 => Self::Text,
        4 => Self::Char,
        5 => Self::Link,
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
