pub mod bytes;
pub mod char;
pub mod int;
pub mod nat;
pub mod text;
pub mod u8;

use std::fmt;

use libipld::ipld::Ipld;

use crate::{
  ipld_error::IpldError,
  literal::Literal,
  term::Term,
};

use crate::prim::{
  bytes::BytesOp,
  char::CharOp,
  int::IntOp,
  nat::NatOp,
  text::TextOp,
  u8::U8Op,
};

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Op {
  Nat(NatOp),
  Int(IntOp),
  Bytes(BytesOp),
  Text(TextOp),
  Char(CharOp),
  U8(U8Op),
}

impl Op {
  pub fn symbol(self) -> String {
    match self {
      Self::Nat(op) => format!("#Nat.{}", op.symbol()),
      Self::Int(op) => format!("#Int.{}", op.symbol()),
      Self::Text(op) => format!("#Text.{}", op.symbol()),
      Self::Bytes(op) => format!("#Bytes.{}", op.symbol()),
      Self::Char(op) => format!("#Char.{}", op.symbol()),
      Self::U8(op) => format!("#U8.{}", op.symbol()),
    }
  }

  pub fn to_ipld(self) -> Ipld {
    match self {
      Self::Nat(op) => Ipld::List(vec![Ipld::Integer(0), op.to_ipld()]),
      Self::Int(op) => Ipld::List(vec![Ipld::Integer(1), op.to_ipld()]),
      Self::Bytes(op) => Ipld::List(vec![Ipld::Integer(2), op.to_ipld()]),
      Self::Text(op) => Ipld::List(vec![Ipld::Integer(3), op.to_ipld()]),
      Self::Char(op) => Ipld::List(vec![Ipld::Integer(4), op.to_ipld()]),
      Self::U8(op) => Ipld::List(vec![Ipld::Integer(6), op.to_ipld()]),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::List(xs) => match xs.as_slice() {
        [Ipld::Integer(0), ys] => NatOp::from_ipld(ys).map(Self::Nat),
        [Ipld::Integer(1), ys] => IntOp::from_ipld(ys).map(Self::Int),
        [Ipld::Integer(2), ys] => BytesOp::from_ipld(ys).map(Self::Bytes),
        [Ipld::Integer(3), ys] => TextOp::from_ipld(ys).map(Self::Text),
        [Ipld::Integer(4), ys] => CharOp::from_ipld(ys).map(Self::Char),
        [Ipld::Integer(6), ys] => U8Op::from_ipld(ys).map(Self::U8),
        xs => Err(IpldError::PrimOp(Ipld::List(xs.to_owned()))),
      },
      xs => Err(IpldError::PrimOp(xs.to_owned())),
    }
  }

  pub fn arity(self) -> u64 {
    match self {
      Self::Nat(op) => op.arity(),
      Self::Int(op) => op.arity(),
      Self::Bytes(op) => op.arity(),
      Self::Text(op) => op.arity(),
      Self::Char(op) => op.arity(),
      Self::U8(op) => op.arity(),
    }
  }

  pub fn apply0(self) -> Option<Literal> {
    match self {
      Self::U8(op) => op.apply0(),
      _ => None,
    }
  }

  pub fn apply1(self, x: Literal) -> Option<Literal> {
    match self {
      Self::Nat(op) => op.apply1(x),
      Self::Int(op) => op.apply1(x),
      Self::Bytes(op) => op.apply1(x),
      Self::Text(op) => op.apply1(x),
      Self::Char(op) => op.apply1(x),
      Self::U8(op) => op.apply1(x),
    }
  }

  pub fn apply2(self, x: Literal, y: Literal) -> Option<Literal> {
    match self {
      Self::Nat(op) => op.apply2(x, y),
      Self::Int(op) => op.apply2(x, y),
      Self::Bytes(op) => op.apply2(x, y),
      Self::Text(op) => op.apply2(x, y),
      Self::Char(op) => op.apply2(x, y),
      Self::U8(op) => op.apply2(x, y),
    }
  }

  pub fn apply3(self, x: Literal, y: Literal, z: Literal) -> Option<Literal> {
    match self {
      Self::Bytes(op) => op.apply3(x, y, z),
      Self::Text(op) => op.apply3(x, y, z),
      _ => None,
    }
  }

  pub fn type_of(self) -> Term {
    match self {
      Self::Nat(op) => op.type_of(),
      Self::Int(op) => op.type_of(),
      Self::Bytes(op) => op.type_of(),
      Self::Text(op) => op.type_of(),
      Self::Char(op) => op.type_of(),
      Self::U8(op) => op.type_of(),
    }
  }
}

impl fmt::Display for Op {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.symbol())
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  use rand::Rng;
  impl Arbitrary for Op {
    fn arbitrary(g: &mut Gen) -> Self {
      let mut rng = rand::thread_rng();
      let gen: u32 = rng.gen_range(0..5);
      match gen {
        0 => Self::Nat(NatOp::arbitrary(g)),
        1 => Self::Int(IntOp::arbitrary(g)),
        2 => Self::Bytes(BytesOp::arbitrary(g)),
        3 => Self::Text(TextOp::arbitrary(g)),
        4 => Self::Char(CharOp::arbitrary(g)),
        _ => Self::U8(U8Op::arbitrary(g)),
      }
    }
  }

  #[quickcheck]
  fn primop_ipld(x: Op) -> bool {
    match Op::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}
