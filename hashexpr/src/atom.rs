use num_bigint::{
  BigInt,
  BigUint,
};

use crate::link::Link;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Atom {
  Link(Link),
  Bits(Vec<u8>, Option<u64>),
  Symbol(String),
  Text(String, Option<u64>),
  Char(char),
  Nat(BigUint, Option<u64>),
  Int(BigInt, Option<u64>),
}

impl Atom {
  pub fn symbol(s: &str) -> Self { Self::Symbol(s.to_owned()) }

  pub fn type_code(&self) -> (Vec<u8>, Option<u64>) {
    match self {
      Self::Link(_) => (vec![0x00], Some(256)),
      Self::Bits(_, l) => (vec![0x01], *l),
      Self::Symbol(_) => (vec![0x02], None),
      Self::Text(_, l) => (vec![0x03], *l),
      Self::Char(_) => (vec![0x04], Some(32)),
      Self::Nat(_, l) => (vec![0x05], *l),
      Self::Int(_, l) => (vec![0x06], *l),
    }
  }

  pub fn data_bytes(&self) -> Vec<u8> {
    match self {
      Self::Link(x) => x.as_bytes().to_vec(),
      Self::Bits(x, _) => x.to_owned(),
      Self::Symbol(x) => x.to_owned().into_bytes(),
      Self::Text(x, _) => x.to_owned().into_bytes(),
      Self::Char(x) => (*x as u32).to_be_bytes().to_vec(),
      Self::Nat(x, _) => x.to_bytes_be(),
      Self::Int(x, _) => x.to_signed_bytes_be(),
    }
  }
}
#[macro_export]
macro_rules! link {
  ($i:expr) => {
    Link($i)
  };
}

#[macro_export]
macro_rules! bits {
  ($n:expr) => {
    Bits($n, None)
  };
  ($n:expr, $i:literal) => {
    Bits($n, Some($i))
  };
}
#[macro_export]
macro_rules! symb {
  ($i:literal) => {
    Symbol(String::from($i))
  };
  ($i:expr) => {
    Symbol($i)
  };
}
#[macro_export]
macro_rules! text {
  ($i:literal) => {
    Text(String::from($i), None)
  };
  ($i:expr) => {
    Text($i, None)
  };
}
