use libipld::ipld::Ipld;
use num_bigint::BigUint;
use std::fmt;

use crate::{
  ipld_error::IpldError,
  literal::Literal,
  term::Term,
  yatima,
};

use core::convert::{
  TryFrom,
  TryInto,
};

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum BitsOp {
  Cons,
  Len,
  Head,
  Tail,
  Take,
  Drop,
  Append,
  Insert,
  Remove,
  Index,
  ToBytes,
}

impl BitsOp {
  pub fn symbol(self) -> String {
    match self {
      Self::Cons => "cons".to_owned(),
      Self::Len => "len".to_owned(),
      Self::Head => "head".to_owned(),
      Self::Tail => "tail".to_owned(),
      Self::Take => "take".to_owned(),
      Self::Drop => "drop".to_owned(),
      Self::Append => "append".to_owned(),
      Self::Insert => "insert".to_owned(),
      Self::Remove => "remove".to_owned(),
      Self::Index => "index".to_owned(),
      Self::ToBytes => "to_Bytes".to_owned(),
    }
  }

  pub fn from_symbol(x: &str) -> Option<Self> {
    match x {
      "cons" => Some(Self::Cons),
      "len" => Some(Self::Len),
      "head" => Some(Self::Head),
      "tail" => Some(Self::Tail),
      "take" => Some(Self::Take),
      "drop" => Some(Self::Drop),
      "append" => Some(Self::Append),
      "insert" => Some(Self::Insert),
      "remove" => Some(Self::Remove),
      "index" => Some(Self::Index),
      "to_Bytes" => Some(Self::ToBytes),
      _ => None,
    }
  }

  pub fn type_of(self) -> Term {
    match self {
      Self::Cons => yatima!("∀ #Char #Bits -> #Bits"),
      Self::Len => yatima!("∀ #Bits -> #Nat"),
      Self::Head => yatima!("∀ #Bits -> #U8"),
      Self::Tail => yatima!("∀ #Bits -> #Bits"),
      Self::Take => yatima!("∀ #Nat #Bits -> #Bits"),
      Self::Drop => yatima!("∀ #Nat #Bits -> #Bits"),
      Self::Append => yatima!("∀ #Bits #Bits -> #Bits"),
      Self::Insert => yatima!("∀ #U8 #Nat #Bits -> #Bits"),
      Self::Remove => yatima!("∀ #Nat #Bits -> #Bits"),
      Self::Index => yatima!("∀ #Nat #Bits -> #U8"),
      Self::ToBytes => yatima!("∀ #Bits -> #Bytes"),
    }
  }

  pub fn to_ipld(self) -> Ipld {
    match self {
      Self::Cons => Ipld::Integer(0),
      Self::Len => Ipld::Integer(1),
      Self::Head => Ipld::Integer(2),
      Self::Tail => Ipld::Integer(3),
      Self::Take => Ipld::Integer(4),
      Self::Drop => Ipld::Integer(5),
      Self::Append => Ipld::Integer(6),
      Self::Insert => Ipld::Integer(7),
      Self::Remove => Ipld::Integer(8),
      Self::Index => Ipld::Integer(9),
      Self::ToBytes => Ipld::Integer(10),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Integer(0) => Ok(Self::Cons),
      Ipld::Integer(1) => Ok(Self::Len),
      Ipld::Integer(2) => Ok(Self::Head),
      Ipld::Integer(3) => Ok(Self::Tail),
      Ipld::Integer(4) => Ok(Self::Take),
      Ipld::Integer(5) => Ok(Self::Drop),
      Ipld::Integer(6) => Ok(Self::Append),
      Ipld::Integer(7) => Ok(Self::Insert),
      Ipld::Integer(8) => Ok(Self::Remove),
      Ipld::Integer(9) => Ok(Self::Index),
      Ipld::Integer(10) => Ok(Self::ToBytes),
      xs => Err(IpldError::BitsOp(xs.to_owned())),
    }
  }

  pub fn arity(self) -> u64 {
    match self {
      Self::Cons => 2,
      Self::Len => 1,
      Self::Head => 1,
      Self::Tail => 1,
      Self::Take => 2,
      Self::Drop => 2,
      Self::Append => 2,
      Self::Insert => 3,
      Self::Remove => 2,
      Self::Index => 2,
      Self::ToBytes => 1,
    }
  }

  pub fn apply1(self, x: Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x) {
      //(Self::Len, Bits(xs)) => Some(Nat(xs.len().into())),
      //(Self::Head, Bits(mut xs)) => {
      //  let x = xs.pop();
      //  x.map(U8)
      //}
      //(Self::Tail, Bits(mut xs)) => {
      //  xs.pop();
      //  Some(Bits(xs))
      //}
      _ => None,
    }
  }

  pub fn apply2(self, x: Literal, y: Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x, y) {
      //(Self::Cons, U8(x), Bits(mut xs)) => {
      //  xs.push(x);
      //  Some(Bits(xs))
      //}
      //(Self::Drop, Nat(x), Bits(xs)) => {
      //  let (_, ys) = safe_split(x, xs);
      //  Some(Bits(ys))
      //}
      //(Self::Take, Nat(x), Bits(xs)) => {
      //  let (xs, _) = safe_split(x, xs);
      //  Some(Bits(xs))
      //}
      //(Self::Append, Bits(mut xs), Bits(mut ys)) => {
      //  xs.append(&mut ys);
      //  Some(Bits(xs))
      //}
      //(Self::Remove, Nat(idx), Bits(mut xs)) => {
      //  let idx = usize::try_from(idx);
      //  match idx {
      //    Ok(idx) if idx < xs.len() => {
      //      xs.remove(idx);
      //      Some(Bits(xs))
      //    }
      //    _ => Some(Bits(xs)),
      //  }
      //}
      //(Self::Index, Nat(idx), Bits(xs)) => {
      //  let idx = usize::try_from(idx);
      //  match idx {
      //    Ok(idx) if idx < xs.len() => {
      //      let x = xs[idx];
      //      Some(U8(x))
      //    }
      //    _ => None,
      //  }
      //}
      _ => None,
    }
  }

  pub fn apply3(self, x: Literal, y: Literal, z: Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x, y, z) {
      //(Self::Insert, Nat(idx), U8(y), Bits(mut xs)) => {
      //  let idx = usize::try_from(idx);
      //  match idx {
      //    Ok(idx) if idx < xs.len() => {
      //      xs.insert(idx, y);
      //      Some(Bits(xs))
      //    }
      //    _ => Some(Bits(xs)),
      //  }
      //}
      _ => None,
    }
  }
}

// pub fn safe_split(idx: BigUint, mut xs: Vec<u8>) -> (Vec<u8>, Vec<u8>) {
//  let idx = usize::try_from(idx);
//  match idx {
//    Ok(idx) if idx <= xs.len() => {
//      let ys = xs.split_off(idx);
//      (xs, ys)
//    }
//    _ => (xs, vec![]),
//  }
//}

pub fn bits_to_byte(bits: [bool; 8]) -> u8 {
  let mut i = 0;
  for b in bits.iter().rev() {
    if *b {
      i = (i << 1) + 1;
    }
    else {
      i = i << 1;
    }
  }
  i
}

pub fn byte_to_bits(byte: u8) -> [bool; 8] {
  let mut x = byte;
  let mut res: [bool; 8] =
    [false, false, false, false, false, false, false, false];
  for i in 0..=7 {
    res[i] = x % 2 == 1;
    x = x >> 1;
  }
  res
}

pub fn bits_to_bytes(bits: Vec<bool>) -> (usize, Vec<u8>) {
  let len = bits.len();
  let mut x = bits;
  let mut res = Vec::new();
  while x.len() % 8 != 0 {
    x.push(false);
  }
  while !x.is_empty() {
    let rest = x.split_off(8);
    let byte = bits_to_byte(x.try_into().unwrap());
    x = rest;
    res.push(byte)
  }
  (len, res)
}

pub fn bytes_to_bits(len: usize, bytes: Vec<u8>) -> Vec<bool> {
  let mut res = Vec::new();
  for byte in bytes {
    let bits = byte_to_bits(byte);
    for b in bits.iter() {
      res.push(*b)
    }
  }
  res.truncate(len);
  res
}

impl fmt::Display for BitsOp {
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
  impl Arbitrary for BitsOp {
    fn arbitrary(_g: &mut Gen) -> Self {
      let mut rng = rand::thread_rng();
      let gen: u32 = rng.gen_range(0..9);
      match gen {
        0 => Self::Cons,
        1 => Self::Len,
        2 => Self::Head,
        3 => Self::Tail,
        4 => Self::Take,
        5 => Self::Drop,
        6 => Self::Append,
        7 => Self::Insert,
        8 => Self::Remove,
        _ => Self::Index,
      }
    }
  }

  #[quickcheck]
  fn bytes_op_ipld(x: BitsOp) -> bool {
    match BitsOp::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
  #[derive(PartialEq, Eq, Clone, Copy, Debug)]
  struct Bits([bool; 8]);

  impl Arbitrary for Bits {
    fn arbitrary(g: &mut Gen) -> Self {
      let mut res: [bool; 8] =
        [false, false, false, false, false, false, false, false];
      for i in 0..7 {
        res[i] = Arbitrary::arbitrary(g)
      }
      Bits(res)
    }
  }

  #[quickcheck]
  fn test_bits_to_byte(x: Bits) -> bool {
    byte_to_bits(bits_to_byte(x.0)) == x.0
  }

  #[quickcheck]
  fn test_byte_to_bits(x: u8) -> bool { bits_to_byte(byte_to_bits(x)) == x }

  #[quickcheck]
  fn test_bits_to_bytes(x: Vec<bool>) -> bool {
    let (len, x1) = bits_to_bytes(x.clone());
    bytes_to_bits(len, x1) == x
  }

  #[quickcheck]
  fn test_bytes_to_bits(x: Vec<u8>) -> bool {
    let len = x.len() * 8;
    bits_to_bytes(bytes_to_bits(len, x.clone())) == (len, x)
  }
}
