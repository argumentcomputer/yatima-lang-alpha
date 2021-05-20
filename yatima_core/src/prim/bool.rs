use libipld::ipld::Ipld;
use num_bigint::BigUint;

use std::fmt;

use crate::{
  ipld_error::IpldError,
  literal::Literal,
  term::Term,
  yatima,
};

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum BoolOp {
  Eql,
  Lte,
  Lth,
  Gte,
  Gth,
  And,
  Or,
  Xor,
  Not,
}

impl BoolOp {
  pub fn symbol(self) -> String {
    match self {
      Self::Eql => "eql".to_owned(),
      Self::Lte => "lte".to_owned(),
      Self::Lth => "lth".to_owned(),
      Self::Gte => "gte".to_owned(),
      Self::Gth => "gth".to_owned(),
      Self::And => "and".to_owned(),
      Self::Or => "or".to_owned(),
      Self::Xor => "xor".to_owned(),
      Self::Not => "not".to_owned(),
    }
  }

  pub fn from_symbol(x: &str) -> Option<Self> {
    match x {
      "eql" => Some(Self::Eql),
      "lte" => Some(Self::Lte),
      "lth" => Some(Self::Lth),
      "gte" => Some(Self::Gte),
      "gth" => Some(Self::Gth),
      "and" => Some(Self::And),
      "xor" => Some(Self::Xor),
      "not" => Some(Self::Not),
      _ => None,
    }
  }

  pub fn type_of(self) -> Term {
    match self {
      Self::Eql => yatima!("∀ #Bool #Bool -> #Bool"),
      Self::Lte => yatima!("∀ #Bool #Bool -> #Bool"),
      Self::Lth => yatima!("∀ #Bool #Bool -> #Bool"),
      Self::Gte => yatima!("∀ #Bool #Bool -> #Bool"),
      Self::Gth => yatima!("∀ #Bool #Bool -> #Bool"),
      Self::And => yatima!("∀ #Bool #Bool -> #Bool"),
      Self::Or => yatima!("∀ #Bool #Bool -> #Bool"),
      Self::Xor => yatima!("∀ #Bool #Bool -> #Bool"),
      Self::Not => yatima!("∀ #Bool -> #Bool"),
    }
  }

  pub fn to_ipld(self) -> Ipld {
    match self {
      Self::Eql => Ipld::Integer(0),
      Self::Lte => Ipld::Integer(1),
      Self::Lth => Ipld::Integer(2),
      Self::Gte => Ipld::Integer(3),
      Self::Gth => Ipld::Integer(4),
      Self::And => Ipld::Integer(5),
      Self::Or => Ipld::Integer(6),
      Self::Xor => Ipld::Integer(7),
      Self::Not => Ipld::Integer(8),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Integer(0) => Ok(Self::Eql),
      Ipld::Integer(1) => Ok(Self::Lte),
      Ipld::Integer(2) => Ok(Self::Lth),
      Ipld::Integer(3) => Ok(Self::Gte),
      Ipld::Integer(4) => Ok(Self::Gth),
      Ipld::Integer(5) => Ok(Self::And),
      Ipld::Integer(6) => Ok(Self::Or),
      Ipld::Integer(7) => Ok(Self::Xor),
      Ipld::Integer(8) => Ok(Self::Not),
      xs => Err(IpldError::BoolOp(xs.to_owned())),
    }
  }

  pub fn arity(self) -> u64 {
    match self {
      Self::Eql => 2,
      Self::Lth => 2,
      Self::Lte => 2,
      Self::Gth => 2,
      Self::Gte => 2,
      Self::And => 2,
      Self::Or => 2,
      Self::Xor => 2,
      Self::Not => 1,
    }
  }

  pub fn apply1(self, x: Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x) {
      (Self::Not, Bool(x)) => Some(Bool(!x)),
      _ => None,
    }
  }

  pub fn apply2(self, x: Literal, y: Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x, y) {
      (Self::Eql, Bool(x), Bool(y)) => Some(Bool(x == y)),
      (Self::Lth, Bool(x), Bool(y)) => Some(Bool(x < y)),
      (Self::Lte, Bool(x), Bool(y)) => Some(Bool(x <= y)),
      (Self::Gth, Bool(x), Bool(y)) => Some(Bool(x > y)),
      (Self::And, Bool(x), Bool(y)) => Some(Bool(x & y)),
      (Self::Or, Bool(x), Bool(y)) => Some(Bool(x | y)),
      (Self::Xor, Bool(x), Bool(y)) => Some(Bool(x ^ y)),
      _ => None,
    }
  }
}

impl fmt::Display for BoolOp {
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
  impl Arbitrary for BoolOp {
    fn arbitrary(_g: &mut Gen) -> Self {
      let mut rng = rand::thread_rng();
      let gen: u32 = rng.gen_range(0..8);
      match gen {
        0 => Self::Eql,
        1 => Self::Lte,
        2 => Self::Lth,
        3 => Self::Gte,
        4 => Self::Gth,
        5 => Self::And,
        6 => Self::Or,
        7 => Self::Xor,
        _ => Self::Not,
      }
    }
  }

  #[quickcheck]
  fn nat_op_ipld(x: BoolOp) -> bool {
    match BoolOp::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }

  //#[test]
  // fn test_apply_bin_op() {
  //  assert_eq!(
  //    Some(Literal::Text(ropey::Rope::from_str("foo"))),
  //    apply_bin_op(
  //      PrimOp::TextCons,
  //      Literal::Char('f'),
  //      Literal::Text(ropey::Rope::from_str("oo"))
  //    )
  //  )
  //}
}
