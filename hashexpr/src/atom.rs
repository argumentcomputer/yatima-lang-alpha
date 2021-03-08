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
}

impl Atom {
  pub fn type_code(&self) -> Vec<u8> {
    match self {
      Self::Link(_) => vec![0x00],
      Self::Bits(_) => vec![0x01],
      Self::Text(_) => vec![0x02],
      Self::Char(_) => vec![0x03],
      Self::Nat(_) => vec![0x04],
      Self::Int(_) => vec![0x05],
    }
  }

  pub fn data_bytes(&self) -> Vec<u8> {
    match self {
      Self::Link(x) => x.as_bytes().to_vec(),
      Self::Bits(x) => x.to_owned(),
      Self::Text(x) => x.to_owned().into_bytes(),
      Self::Char(x) => (*x as u32).to_be_bytes().to_vec(),
      Self::Nat(x) => x.to_bytes_be(),
      Self::Int(x) => x.to_signed_bytes_be(),
    }
  }
}

impl fmt::Display for Atom {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self::Bits(x) => {
        let x: &[u8] = x.as_ref();
        write!(f, "~\"{}\"", Base::encode(&Base::_64, x))
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
    }
  }
}
