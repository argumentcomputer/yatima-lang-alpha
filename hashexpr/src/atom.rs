pub use crate::{
  base,
  base::Base,
  link::Link,
};
use num_bigint::{
  BigInt,
  BigUint,
  Sign,
};
use std::fmt;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Atom {
  Link(Link),
  Bits(Vec<u8>),
  Text(String),
  Char(char),
  Nat(BigUint),
  Int(BigInt),
  Bool(bool),
  U8(u8),
  U16(u16),
  U32(u32),
  U64(u64),
  I8(i8),
  I16(i16),
  I32(i32),
  I64(i64),
}

impl Atom {
  #[must_use]
  pub fn type_code(&self) -> Vec<u8> {
    match self {
      Self::Link(_) => vec![0x00],
      Self::Bits(_) => vec![0x01],
      Self::Text(_) => vec![0x02],
      Self::Char(_) => vec![0x03],
      Self::Nat(_) => vec![0x04],
      Self::Int(_) => vec![0x05],
      Self::Bool(_) => vec![0x06],
      Self::U8(_) => vec![0x07],
      Self::U16(_) => vec![0x08],
      Self::U32(_) => vec![0x09],
      Self::U64(_) => vec![0x10],
      Self::I8(_) => vec![0x11],
      Self::I16(_) => vec![0x12],
      Self::I32(_) => vec![0x13],
      Self::I64(_) => vec![0x14],
    }
  }

  #[must_use]
  pub fn data_bytes(&self) -> Vec<u8> {
    match self {
      Self::Link(x) => x.as_bytes().to_vec(),
      Self::Bits(x) => x.clone(),
      Self::Text(x) => x.clone().into_bytes(),
      Self::Char(x) => (*x as u32).to_be_bytes().to_vec(),
      Self::Nat(x) => x.to_bytes_be(),
      Self::Int(x) => x.to_signed_bytes_be(),
      Self::Bool(true) => vec![1],
      Self::Bool(false) => vec![0],
      Self::U8(x) => x.to_be_bytes().to_vec(),
      Self::U16(x) => x.to_be_bytes().to_vec(),
      Self::U32(x) => x.to_be_bytes().to_vec(),
      Self::U64(x) => x.to_be_bytes().to_vec(),
      Self::I8(x) => x.to_be_bytes().to_vec(),
      Self::I16(x) => x.to_be_bytes().to_vec(),
      Self::I32(x) => x.to_be_bytes().to_vec(),
      Self::I64(x) => x.to_be_bytes().to_vec(),
    }
  }
}

impl fmt::Display for Atom {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self::Bits(x) => {
        let x: &[u8] = x.as_ref();
        write!(f, "~\"{}\"", Base::_64.encode(x))
      }
      Self::Link(l) => write!(f, "{}", l),
      Self::Nat(x) => {
        write!(f, "{}", x.to_str_radix(10))
      }
      Self::Int(x) => match x.sign() {
        Sign::Minus => write!(f, "-{}", x.magnitude().to_str_radix(10)),
        _ => write!(f, "+{}", x.to_str_radix(10)),
      },
      Self::Text(x) => write!(f, "\"{}\"", x.escape_default()),
      Self::Char(x) => write!(f, "'{}'", x.escape_default()),
      Self::Bool(true) => write!(f, "true"),
      Self::Bool(false) => write!(f, "false"),
      Self::U8(x) => write!(f, "{}u8", x),
      Self::U16(x) => write!(f, "{}u16", x),
      Self::U32(x) => write!(f, "{}u32", x),
      Self::U64(x) => write!(f, "{}u64", x),
      Self::I8(x) => write!(f, "{}i8", x),
      Self::I16(x) => write!(f, "{}i16", x),
      Self::I32(x) => write!(f, "{}i32", x),
      Self::I64(x) => write!(f, "{}i64", x),
    }
  }
}
