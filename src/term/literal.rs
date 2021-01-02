use crate::decode_error::{
  DecodeError,
  Expected,
};
use hashexpr::{
  atom::Atom::*,
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
};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Literal {
  Natural(Option<u64>, BigUint),
  Integer(Option<u64>, BigInt),
  Bits(Option<u64>, Vec<u8>),
  Text(Option<u64>, String),
  Char(char),
  Link(Link),
  Exception(String),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum LitType {
  Natural,
  Integer,
  Bits,
  Text,
  Char,
  Link,
  Exception,
}

impl Literal {
  pub fn encode(self) -> Expr {
    match self {
      Self::Natural(len, x) => Atom(None, Nat(x, len)),
      Self::Integer(len, x) => Atom(None, Int(x, len)),
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
      Atom(_, Nat(x, len)) => Ok(Self::Natural(len, x)),
      Atom(_, Int(x, len)) => Ok(Self::Integer(len, x)),
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
      Self::Natural => Atom(None, symb!("Natural")),
      Self::Integer => Atom(None, symb!("Integer")),
      Self::Bits => Atom(None, symb!("Bits")),
      Self::Text => Atom(None, symb!("Text")),
      Self::Char => Atom(None, symb!("Char")),
      Self::Link => Atom(None, symb!("Link")),
      Self::Exception => Atom(None, symb!("Exception")),
    }
  }

  pub fn decode(x: Expr) -> Result<Self, DecodeError> {
    match x {
      Atom(_, Symbol(n)) if *n == String::from("Natural") => Ok(Self::Natural),
      Atom(_, Symbol(n)) if *n == String::from("Integer") => Ok(Self::Integer),
      Atom(_, Symbol(n)) if *n == String::from("Bits") => Ok(Self::Bits),
      Atom(_, Symbol(n)) if *n == String::from("Text") => Ok(Self::Text),
      Atom(_, Symbol(n)) if *n == String::from("Char") => Ok(Self::Char),
      Atom(_, Symbol(n)) if *n == String::from("Link") => Ok(Self::Link),
      Atom(_, Symbol(n)) if *n == String::from("Exception") => {
        Ok(Self::Exception)
      }
      _ => Err(DecodeError::new(x.position(), vec![Expected::LitType])),
    }
  }
}
