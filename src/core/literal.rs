use crate::decode_error::{
  DecodeError,
  Expected,
};
use hashexpr::{
  base,
  base::Base,
  link::Link,
  AVal,
  AVal::*,
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
  Link(Link),
  Exception(String),
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum LitType {
  Natural,
  Integer,
  BitString,
  Text,
  Char,
  Link,
  Exception,
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
        write!(f, "#\"{}\"", base::encode(Base::_64, x))
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
      Self::Natural(x) => nat!(x),
      Self::Integer(x) => int!(x),
      Self::BitString(x) => bits!(x),
      Self::Text(x) => text!(x),
      Self::Char(x) => char!(x),
      Self::Link(x) => link!(x),
      Self::Exception(x) => Cons(None, vec![symb!("exception"), text!(x)]),
    }
  }

  pub fn decode(x: Expr) -> Result<Self, DecodeError> {
    match x {
      Atom(_, Nat(x)) => Ok(Self::Natural(x)),
      Atom(_, Int(x)) => Ok(Self::Integer(x)),
      Atom(_, Bits(x)) => Ok(Self::BitString(x)),
      Atom(_, Text(x)) => Ok(Self::Text(x)),
      Atom(_, Char(x)) => Ok(Self::Char(x)),
      Atom(_, Link(x)) => Ok(Self::Link(x)),
      Cons(pos, xs) => match xs.as_slice() {
        [Atom(_, Symbol(n)), Atom(_, Text(x))]
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
      Self::Natural => symb!("#Natural"),
      Self::Integer => symb!("#Integer"),
      Self::BitString => symb!("#BitString"),
      Self::Text => symb!("#Text"),
      Self::Char => symb!("#Char"),
      Self::Link => symb!("#Link"),
      Self::Exception => symb!("#Exception"),
    }
  }

  pub fn decode(x: Expr) -> Result<Self, DecodeError> {
    match x {
      Atom(_, Symbol(n)) if *n == String::from("#Natural") => Ok(Self::Natural),
      Atom(_, Symbol(n)) if *n == String::from("#Integer") => Ok(Self::Integer),
      Atom(_, Symbol(n)) if *n == String::from("#BitString") => {
        Ok(Self::BitString)
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
      Self::Natural => write!(f, "#Natural"),
      Self::Integer => write!(f, "#Integer"),
      Self::BitString => write!(f, "#BitString"),
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

  pub fn arbitrary_bits(g: &mut Gen) -> Literal {
    let x: Vec<u8> = Arbitrary::arbitrary(g);
    Literal::BitString(x)
  }

  pub fn arbitrary_text(g: &mut Gen) -> Literal {
    let x: String = Arbitrary::arbitrary(g);
    Literal::Text(x)
  }
  pub fn arbitrary_nat(g: &mut Gen) -> Literal {
    let v: Vec<u8> = Arbitrary::arbitrary(g);
    let x: BigUint = BigUint::from_bytes_be(&v);
    Literal::Natural(x)
  }

  pub fn arbitrary_int(g: &mut Gen) -> Literal {
    let v: Vec<u8> = Arbitrary::arbitrary(g);
    let x: BigInt = BigInt::from_signed_bytes_be(&v);
    Literal::Integer(x)
  }

  impl Arbitrary for Literal {
    fn arbitrary(g: &mut Gen) -> Self {
      let mut rng = rand::thread_rng();
      let gen: u32 = rng.gen_range(0..7);
      //let gen = g.gen_range(0, 7);
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
    fn arbitrary(g: &mut Gen) -> Self {
      let mut rng = rand::thread_rng();
      let gen: u32 = rng.gen_range(0..7);
      //let gen = g.gen_range(0, 7);
      match gen {
        1 => Self::Natural,
        2 => Self::Integer,
        3 => Self::BitString,
        4 => Self::Text,
        5 => Self::Char,
        6 => Self::Link,
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
