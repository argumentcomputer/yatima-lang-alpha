use crate::decode_error::{
  DecodeError,
  Expected,
};
use hashexpr::{
  atom,
  atom::Atom::*,
  base::Base,
  Expr,
  Expr::Atom,
};
use num_bigint::{
  BigInt,
  BigUint,
  Sign,
};

use std::fmt;

#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
  Natural(BigUint),
  Integer(BigInt),
  BitString(Vec<u8>),
  Text(String),
  Char(char),
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

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum LitType {
  Natural,
  Integer,
  BitString,
  Text,
  Char,
  Bool,
  U8,
  U16,
  U32,
  U64,
  I8,
  I16,
  I32,
  I64,
}

impl fmt::Display for Literal {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use Literal::*;
    match self {
      Natural(x) => write!(f, "{}", x.to_str_radix(10)),
      Integer(x) => match x.sign() {
        Sign::Minus => write!(f, "-{}", x.magnitude().to_str_radix(10)),
        _ => write!(f, "+{}", x.to_str_radix(10)),
      },
      BitString(x) => {
        let x: &[u8] = x.as_ref();
        write!(f, "~\"{}\"", Base::encode(&Base::_64, x))
      }
      Text(x) => write!(f, "\"{}\"", x.escape_default()),
      Char(x) => write!(f, "'{}'", x.escape_default()),
      Bool(true) => write!(f, "true"),
      Bool(false) => write!(f, "false"),
      U8(x) => write!(f, "{}", x),
      U16(x) => write!(f, "{}", x),
      U32(x) => write!(f, "{}", x),
      U64(x) => write!(f, "{}", x),
      I8(x) => write!(f, "{}", x),
      I16(x) => write!(f, "{}", x),
      I32(x) => write!(f, "{}", x),
      I64(x) => write!(f, "{}", x),
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
      Self::Bool(x) => atom!(Bool(x)),
      Self::U8(x) => atom!(U8(x)),
      Self::U16(x) => atom!(U16(x)),
      Self::U32(x) => atom!(U32(x)),
      Self::U64(x) => atom!(U64(x)),
      Self::I8(x) => atom!(I8(x)),
      Self::I16(x) => atom!(I16(x)),
      Self::I32(x) => atom!(I32(x)),
      Self::I64(x) => atom!(I64(x)),
    }
  }

  pub fn decode(x: Expr) -> Result<Self, DecodeError> {
    match x {
      Atom(_, Nat(x)) => Ok(Self::Natural(x)),
      Atom(_, Int(x)) => Ok(Self::Integer(x)),
      Atom(_, Bits(x)) => Ok(Self::BitString(x)),
      Atom(_, Text(x)) => Ok(Self::Text(x)),
      Atom(_, Char(x)) => Ok(Self::Char(x)),
      Atom(_, Bool(x)) => Ok(Self::Bool(x)),
      Atom(_, U8(x)) => Ok(Self::U8(x)),
      Atom(_, U16(x)) => Ok(Self::U16(x)),
      Atom(_, U32(x)) => Ok(Self::U32(x)),
      Atom(_, U64(x)) => Ok(Self::U64(x)),
      Atom(_, I8(x)) => Ok(Self::I8(x)),
      Atom(_, I16(x)) => Ok(Self::I16(x)),
      Atom(_, I32(x)) => Ok(Self::I32(x)),
      Atom(_, I64(x)) => Ok(Self::I64(x)),
      _ => Err(DecodeError::new(x.position(), vec![Expected::Literal])),
    }
  }
}

impl LitType {
  pub fn encode(self) -> Expr {
    match self {
      Self::Natural => text!("#Natural"),
      Self::Integer => text!("#Integer"),
      Self::BitString => text!("#BitString"),
      Self::Text => text!("#Text"),
      Self::Char => text!("#Char"),
      Self::Bool => text!("#Bool"),
      Self::U8 => text!("#U8"),
      Self::U16 => text!("#U16"),
      Self::U32 => text!("#U32"),
      Self::U64 => text!("#U64"),
      Self::I8 => text!("#I8"),
      Self::I16 => text!("#I16"),
      Self::I32 => text!("#I32"),
      Self::I64 => text!("#I64"),
    }
  }

  pub fn decode(x: Expr) -> Result<Self, DecodeError> {
    match x {
      Atom(_, Text(n)) if *n == String::from("#Natural") => Ok(Self::Natural),
      Atom(_, Text(n)) if *n == String::from("#Integer") => Ok(Self::Integer),
      Atom(_, Text(n)) if *n == String::from("#BitString") => {
        Ok(Self::BitString)
      }
      Atom(_, Text(n)) if *n == String::from("#Text") => Ok(Self::Text),
      Atom(_, Text(n)) if *n == String::from("#Char") => Ok(Self::Char),
      Atom(_, Text(n)) if *n == String::from("#Bool") => Ok(Self::Bool),
      Atom(_, Text(n)) if *n == String::from("#U8") => Ok(Self::U8),
      Atom(_, Text(n)) if *n == String::from("#U16") => Ok(Self::U16),
      Atom(_, Text(n)) if *n == String::from("#U32") => Ok(Self::U32),
      Atom(_, Text(n)) if *n == String::from("#U64") => Ok(Self::U64),
      Atom(_, Text(n)) if *n == String::from("#I8") => Ok(Self::I8),
      Atom(_, Text(n)) if *n == String::from("#I16") => Ok(Self::I16),
      Atom(_, Text(n)) if *n == String::from("#I32") => Ok(Self::I32),
      Atom(_, Text(n)) if *n == String::from("#I64") => Ok(Self::I64),
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
      Self::Text => write!(f, "#Text"),
      Self::Char => write!(f, "#Char"),
      Self::Bool => write!(f, "#Bool"),
      Self::U8 => write!(f, "#U8"),
      Self::U16 => write!(f, "#U16"),
      Self::U32 => write!(f, "#U32"),
      Self::U64 => write!(f, "#U64"),
      Self::I8 => write!(f, "#I8"),
      Self::I16 => write!(f, "#I16"),
      Self::I32 => write!(f, "#I32"),
      Self::I64 => write!(f, "#I64"),
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

  use crate::term::tests::frequency;

  pub fn arbitrary_bits() -> Box<dyn Fn(&mut Gen) -> Literal> {
    Box::new(move |g: &mut Gen| {
      let x: Vec<u8> = Arbitrary::arbitrary(g);
      Literal::BitString(x)
    })
  }

  pub fn arbitrary_text() -> Box<dyn Fn(&mut Gen) -> Literal> {
    Box::new(move |g: &mut Gen| {
      let x: String = Arbitrary::arbitrary(g);
      Literal::Text(x)
    })
  }
  pub fn arbitrary_nat() -> Box<dyn Fn(&mut Gen) -> Literal> {
    Box::new(move |g: &mut Gen| {
      let v: Vec<u8> = Arbitrary::arbitrary(g);
      let x: BigUint = BigUint::from_bytes_be(&v);
      Literal::Natural(x)
    })
  }

  pub fn arbitrary_int() -> Box<dyn Fn(&mut Gen) -> Literal> {
    Box::new(move |g: &mut Gen| {
      let v: Vec<u8> = Arbitrary::arbitrary(g);
      let x: BigInt = BigInt::from_signed_bytes_be(&v);
      Literal::Integer(x)
    })
  }

  impl Arbitrary for Literal {
    fn arbitrary(g: &mut Gen) -> Self {
      frequency(g, vec![
        (1, arbitrary_nat()),
        (1, arbitrary_int()),
        (1, arbitrary_bits()),
        (1, arbitrary_text()),
        (1, Box::new(|g| Self::Char(Arbitrary::arbitrary(g)))),
      ])
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
      let input: Vec<(i64, Box<dyn Fn(&mut Gen) -> LitType>)> = vec![
        (1, Box::new(|_| Self::Natural)),
        (1, Box::new(|_| Self::Integer)),
        (1, Box::new(|_| Self::BitString)),
        (1, Box::new(|_| Self::Text)),
        (1, Box::new(|_| Self::Char)),
      ];
      frequency(g, input)
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
