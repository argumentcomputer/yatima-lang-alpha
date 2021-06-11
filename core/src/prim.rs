pub mod bits;
pub mod bool;
pub mod bytes;
pub mod char;
// pub mod i128;
pub mod i16;
pub mod i32;
pub mod i64;
pub mod i8;
pub mod int;
pub mod nat;
pub mod text;
// pub mod u128;
pub mod u16;
pub mod u32;
pub mod u64;
pub mod u8;

use std::fmt;

use libipld::ipld::Ipld;

use crate::{
  ipld_error::IpldError,
  literal::Literal,
  term::Term,
};

use crate::prim::{
  bits::BitsOp,
  bool::BoolOp,
  bytes::BytesOp,
  char::CharOp,
  i16::I16Op,
  i32::I32Op,
  i64::I64Op,
  i8::I8Op,
  int::IntOp,
  nat::NatOp,
  text::TextOp,
  u16::U16Op,
  u32::U32Op,
  u64::U64Op,
  u8::U8Op,
};

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Op {
  Nat(NatOp),
  Int(IntOp),
  Bits(BitsOp),
  Bytes(BytesOp),
  Text(TextOp),
  Char(CharOp),
  Bool(BoolOp),
  U8(U8Op),
  U16(U16Op),
  U32(U32Op),
  U64(U64Op),
  // U128(U128Op),
  I8(I8Op),
  I16(I16Op),
  I32(I32Op),
  I64(I64Op),
  // I128(I128Op),
}

impl Op {
  pub fn symbol(self) -> String {
    match self {
      Self::Nat(op) => format!("#Nat.{}", op.symbol()),
      Self::Int(op) => format!("#Int.{}", op.symbol()),
      Self::Text(op) => format!("#Text.{}", op.symbol()),
      Self::Bits(op) => format!("#Bits.{}", op.symbol()),
      Self::Bytes(op) => format!("#Bytes.{}", op.symbol()),
      Self::Char(op) => format!("#Char.{}", op.symbol()),
      Self::Bool(op) => format!("#Bool.{}", op.symbol()),
      Self::U8(op) => format!("#U8.{}", op.symbol()),
      Self::U16(op) => format!("#U16.{}", op.symbol()),
      Self::U32(op) => format!("#U32.{}", op.symbol()),
      Self::U64(op) => format!("#U64.{}", op.symbol()),
      // Self::U128(op) => format!("#U128.{}", op.symbol()),
      Self::I8(op) => format!("#I8.{}", op.symbol()),
      Self::I16(op) => format!("#I16.{}", op.symbol()),
      Self::I32(op) => format!("#I32.{}", op.symbol()),
      Self::I64(op) => format!("#I64.{}", op.symbol()),
      // Self::I128(op) => format!("#I128.{}", op.symbol()),
    }
  }

  pub fn to_ipld(self) -> Ipld {
    match self {
      Self::Nat(op) => Ipld::List(vec![Ipld::Integer(0), op.to_ipld()]),
      Self::Int(op) => Ipld::List(vec![Ipld::Integer(1), op.to_ipld()]),
      Self::Bits(op) => Ipld::List(vec![Ipld::Integer(2), op.to_ipld()]),
      Self::Bytes(op) => Ipld::List(vec![Ipld::Integer(3), op.to_ipld()]),
      Self::Text(op) => Ipld::List(vec![Ipld::Integer(4), op.to_ipld()]),
      Self::Char(op) => Ipld::List(vec![Ipld::Integer(5), op.to_ipld()]),
      Self::Bool(op) => Ipld::List(vec![Ipld::Integer(6), op.to_ipld()]),
      Self::U8(op) => Ipld::List(vec![Ipld::Integer(7), op.to_ipld()]),
      Self::U16(op) => Ipld::List(vec![Ipld::Integer(8), op.to_ipld()]),
      Self::U32(op) => Ipld::List(vec![Ipld::Integer(9), op.to_ipld()]),
      Self::U64(op) => Ipld::List(vec![Ipld::Integer(10), op.to_ipld()]),
      // Self::U128(op) => Ipld::List(vec![Ipld::Integer(11), op.to_ipld()]),
      Self::I8(op) => Ipld::List(vec![Ipld::Integer(12), op.to_ipld()]),
      Self::I16(op) => Ipld::List(vec![Ipld::Integer(13), op.to_ipld()]),
      Self::I32(op) => Ipld::List(vec![Ipld::Integer(14), op.to_ipld()]),
      Self::I64(op) => Ipld::List(vec![Ipld::Integer(15), op.to_ipld()]),
      // Self::I128(op) => Ipld::List(vec![Ipld::Integer(16), op.to_ipld()]),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::List(xs) => match xs.as_slice() {
        [Ipld::Integer(0), ys] => NatOp::from_ipld(ys).map(Self::Nat),
        [Ipld::Integer(1), ys] => IntOp::from_ipld(ys).map(Self::Int),
        [Ipld::Integer(2), ys] => BitsOp::from_ipld(ys).map(Self::Bits),
        [Ipld::Integer(3), ys] => BytesOp::from_ipld(ys).map(Self::Bytes),
        [Ipld::Integer(4), ys] => TextOp::from_ipld(ys).map(Self::Text),
        [Ipld::Integer(5), ys] => CharOp::from_ipld(ys).map(Self::Char),
        [Ipld::Integer(6), ys] => BoolOp::from_ipld(ys).map(Self::Bool),
        [Ipld::Integer(7), ys] => U8Op::from_ipld(ys).map(Self::U8),
        [Ipld::Integer(8), ys] => U16Op::from_ipld(ys).map(Self::U16),
        [Ipld::Integer(9), ys] => U32Op::from_ipld(ys).map(Self::U32),
        [Ipld::Integer(10), ys] => U64Op::from_ipld(ys).map(Self::U64),
        //[Ipld::Integer(11), ys] => U128Op::from_ipld(ys).map(Self::U128),
        [Ipld::Integer(12), ys] => I8Op::from_ipld(ys).map(Self::I8),
        [Ipld::Integer(13), ys] => I16Op::from_ipld(ys).map(Self::I16),
        [Ipld::Integer(14), ys] => I32Op::from_ipld(ys).map(Self::I32),
        [Ipld::Integer(15), ys] => I64Op::from_ipld(ys).map(Self::I64),
        //[Ipld::Integer(16), ys] => I128Op::from_ipld(ys).map(Self::I128),
        xs => Err(IpldError::PrimOp(Ipld::List(xs.to_owned()))),
      },
      xs => Err(IpldError::PrimOp(xs.to_owned())),
    }
  }

  pub fn arity(self) -> u64 {
    match self {
      Self::Nat(op) => op.arity(),
      Self::Int(op) => op.arity(),
      Self::Bits(op) => op.arity(),
      Self::Bytes(op) => op.arity(),
      Self::Text(op) => op.arity(),
      Self::Char(op) => op.arity(),
      Self::Bool(op) => op.arity(),
      Self::U8(op) => op.arity(),
      Self::U16(op) => op.arity(),
      Self::U32(op) => op.arity(),
      Self::U64(op) => op.arity(),
      // Self::U128(op) => op.arity(),
      Self::I8(op) => op.arity(),
      Self::I16(op) => op.arity(),
      Self::I32(op) => op.arity(),
      Self::I64(op) => op.arity(),
      // Self::I128(op) => op.arity(),
    }
  }

  pub fn apply0(self) -> Option<Literal> {
    match self {
      Self::U8(op) => op.apply0(),
      Self::U16(op) => op.apply0(),
      Self::U32(op) => op.apply0(),
      Self::U64(op) => op.apply0(),
      // Self::U128(op) => op.apply0(),
      Self::I8(op) => op.apply0(),
      Self::I16(op) => op.apply0(),
      Self::I32(op) => op.apply0(),
      Self::I64(op) => op.apply0(),
      // Self::I128(op) => op.apply0(),
      _ => None,
    }
  }

  pub fn apply1(self, x: &Literal) -> Option<Literal> {
    match self {
      Self::Nat(op) => op.apply1(x),
      Self::Int(op) => op.apply1(x),
      Self::Bits(op) => op.apply1(x),
      Self::Bytes(op) => op.apply1(x),
      Self::Text(op) => op.apply1(x),
      Self::Char(op) => op.apply1(x),
      Self::Bool(op) => op.apply1(x),
      Self::U8(op) => op.apply1(x),
      Self::U16(op) => op.apply1(x),
      Self::U32(op) => op.apply1(x),
      Self::U64(op) => op.apply1(x),
      // Self::U128(op) => op.apply1(x),
      Self::I8(op) => op.apply1(x),
      Self::I16(op) => op.apply1(x),
      Self::I32(op) => op.apply1(x),
      Self::I64(op) => op.apply1(x),
      // Self::I128(op) => op.apply1(x),
    }
  }

  pub fn apply2(self, x: &Literal, y: &Literal) -> Option<Literal> {
    match self {
      Self::Nat(op) => op.apply2(x, y),
      Self::Int(op) => op.apply2(x, y),
      Self::Bits(op) => op.apply2(x, y),
      Self::Bytes(op) => op.apply2(x, y),
      Self::Text(op) => op.apply2(x, y),
      Self::Char(op) => op.apply2(x, y),
      Self::Bool(op) => op.apply2(x, y),
      Self::U8(op) => op.apply2(x, y),
      Self::U16(op) => op.apply2(x, y),
      Self::U32(op) => op.apply2(x, y),
      Self::U64(op) => op.apply2(x, y),
      // TODO These break wasm and wasm-pack
      // Self::U128(op) => op.apply2(x, y),
      Self::I8(op) => op.apply2(x, y),
      Self::I16(op) => op.apply2(x, y),
      Self::I32(op) => op.apply2(x, y),
      Self::I64(op) => op.apply2(x, y),
      // Self::I128(op) => op.apply2(x, y),
      _ => None,
    }
  }

  pub fn apply3(
    self,
    x: &Literal,
    y: &Literal,
    z: &Literal,
  ) -> Option<Literal> {
    match self {
      Self::Bytes(op) => op.apply3(x, y, z),
      Self::Bits(op) => op.apply3(x, y, z),
      Self::Text(op) => op.apply3(x, y, z),
      _ => None,
    }
  }

  pub fn type_of(self) -> Term {
    match self {
      Self::Nat(op) => op.type_of(),
      Self::Int(op) => op.type_of(),
      Self::Bits(op) => op.type_of(),
      Self::Bytes(op) => op.type_of(),
      Self::Text(op) => op.type_of(),
      Self::Char(op) => op.type_of(),
      Self::Bool(op) => op.type_of(),
      Self::U8(op) => op.type_of(),
      Self::U16(op) => op.type_of(),
      Self::U32(op) => op.type_of(),
      Self::U64(op) => op.type_of(),
      // Self::U128(op) => op.type_of(),
      Self::I8(op) => op.type_of(),
      Self::I16(op) => op.type_of(),
      Self::I32(op) => op.type_of(),
      Self::I64(op) => op.type_of(),
      // Self::I128(op) => op.type_of(),
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
      let gen: u32 = rng.gen_range(0..13);
      match gen {
        0 => Self::Nat(NatOp::arbitrary(g)),
        1 => Self::Int(IntOp::arbitrary(g)),
        2 => Self::Bytes(BytesOp::arbitrary(g)),
        3 => Self::Text(TextOp::arbitrary(g)),
        4 => Self::Char(CharOp::arbitrary(g)),
        5 => Self::Bool(BoolOp::arbitrary(g)),
        6 => Self::U8(U8Op::arbitrary(g)),
        7 => Self::U16(U16Op::arbitrary(g)),
        8 => Self::U32(U32Op::arbitrary(g)),
        9 => Self::U64(U64Op::arbitrary(g)),
        10 => Self::I8(I8Op::arbitrary(g)),
        11 => Self::I16(I16Op::arbitrary(g)),
        12 => Self::I32(I32Op::arbitrary(g)),
        _ => Self::I64(I64Op::arbitrary(g)),
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
