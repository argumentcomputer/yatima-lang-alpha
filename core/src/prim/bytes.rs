use libipld::ipld::Ipld;
use num_bigint::BigUint;
use std::fmt;

use crate::{
  ipld_error::IpldError,
  literal::Literal,
  prim::bits,
  term::Term,
  yatima,
};

use core::convert::TryFrom;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum BytesOp {
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
  ToBits,
}

impl BytesOp {
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
      Self::ToBits => "to_Bits".to_owned(),
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
      "to_Bits" => Some(Self::ToBits),
      _ => None,
    }
  }

  pub fn type_of(self) -> Term {
    match self {
      Self::Cons => yatima!("∀ #U8 #Bytes -> #Bytes"),
      Self::Len => yatima!("∀ #Bytes -> #Nat"),
      Self::Head => yatima!("∀ #Bytes -> #U8"),
      Self::Tail => yatima!("∀ #Bytes -> #Bytes"),
      Self::Take => yatima!("∀ #Nat #Bytes -> #Bytes"),
      Self::Drop => yatima!("∀ #Nat #Bytes -> #Bytes"),
      Self::Append => yatima!("∀ #Bytes #Bytes -> #Bytes"),
      Self::Insert => yatima!("∀ #U8 #Nat #Bytes -> #Bytes"),
      Self::Remove => yatima!("∀ #Nat #Bytes -> #Bytes"),
      Self::Index => yatima!("∀ #Nat #Bytes -> #U8"),
      Self::ToBits => yatima!("∀ #Nat #Bytes -> #Bits"),
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
      Self::ToBits => Ipld::Integer(10),
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
      Ipld::Integer(10) => Ok(Self::ToBits),
      xs => Err(IpldError::BytesOp(xs.to_owned())),
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
      Self::ToBits => 2,
    }
  }

  pub fn apply1(self, x: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x) {
      (Self::Len, Bytes(xs)) => Some(Nat(xs.len().into())),
      (Self::Head, Bytes(xs)) => {
        let x = xs.last();
        x.map(|x| U8(*x))
      }
      (Self::Tail, Bytes(xs)) => {
        let xs = xs[0..xs.len() - 1].to_vec();
        Some(Bytes(xs))
      }
      _ => None,
    }
  }

  pub fn apply2(self, x: &Literal, y: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x, y) {
      (Self::Cons, U8(x), Bytes(xs)) => {
        let mut xs = xs.clone();
        xs.push(*x);
        Some(Bytes(xs))
      }
      (Self::Drop, Nat(x), Bytes(xs)) => {
        let (_, ys) = safe_split(x, xs);
        Some(Bytes(ys))
      }
      (Self::Take, Nat(x), Bytes(xs)) => {
        let (xs, _) = safe_split(x, xs);
        Some(Bytes(xs))
      }
      (Self::Append, Bytes(xs), Bytes(ys)) => {
        let mut xs = xs.clone();
        xs.extend_from_slice(&ys);
        Some(Bytes(xs))
      }
      (Self::Remove, Nat(idx), Bytes(xs)) => {
        let idx = usize::try_from(idx);
        match idx {
          Ok(idx) if idx < xs.len() => {
            let mut xs = xs.clone();
            xs.remove(idx);
            Some(Bytes(xs))
          }
          _ => Some(Bytes(xs.clone())),
        }
      }
      (Self::Index, Nat(idx), Bytes(xs)) => {
        let idx = usize::try_from(idx);
        match idx {
          Ok(idx) if idx < xs.len() => {
            let x = xs[idx];
            Some(U8(x))
          }
          _ => None,
        }
      }
      (Self::ToBits, Nat(idx), Bytes(xs)) => match usize::try_from(idx) {
        Ok(x) => Some(Literal::Bits(bits::bytes_to_bits(x, xs))),
        _ => None,
      },
      _ => None,
    }
  }

  pub fn apply3(
    self,
    x: &Literal,
    y: &Literal,
    z: &Literal,
  ) -> Option<Literal> {
    use Literal::*;
    match (self, x, y, z) {
      (Self::Insert, Nat(idx), U8(y), Bytes(xs)) => {
        let idx = usize::try_from(idx);
        match idx {
          Ok(idx) if idx < xs.len() => {
            let mut xs = xs.clone();
            xs.insert(idx, *y);
            Some(Bytes(xs))
          }
          _ => Some(Bytes(xs.clone())),
        }
      }
      _ => None,
    }
  }
}

pub fn safe_split(idx: &BigUint, xs: &Vec<u8>) -> (Vec<u8>, Vec<u8>) {
  let idx = usize::try_from(idx);
  match idx {
    Ok(idx) if idx <= xs.len() => {
      let ys = xs[0..idx].to_vec();
      let zs = xs[idx..].to_vec();
      (ys, zs)
    }
    _ => (xs.clone(), vec![]),
  }
}

impl fmt::Display for BytesOp {
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
  impl Arbitrary for BytesOp {
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
  fn bytes_op_ipld(x: BytesOp) -> bool {
    match BytesOp::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}
