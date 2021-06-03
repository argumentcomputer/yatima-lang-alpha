use sp_ipld::Ipld;
use num_bigint::{
  BigInt,
  Sign,
};

use std::fmt;

use crate::{
  ipld_error::IpldError,
  literal::Literal,
  term::Term,
  yatima,
};

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum IntOp {
  New,
  Sgn,
  Abs,
  Eql,
  Lte,
  Lth,
  Gte,
  Gth,
  Add,
  Sub,
  Mul,
  Div,
  Mod,
}

impl IntOp {
  pub fn symbol(self) -> String {
    match self {
      Self::New => "new".to_owned(),
      Self::Sgn => "sgn".to_owned(),
      Self::Abs => "abs".to_owned(),
      Self::Eql => "eql".to_owned(),
      Self::Lte => "lte".to_owned(),
      Self::Lth => "lth".to_owned(),
      Self::Gte => "gte".to_owned(),
      Self::Gth => "gth".to_owned(),
      Self::Add => "add".to_owned(),
      Self::Sub => "sub".to_owned(),
      Self::Mul => "mul".to_owned(),
      Self::Div => "div".to_owned(),
      Self::Mod => "mod".to_owned(),
    }
  }

  pub fn from_symbol(x: &str) -> Option<Self> {
    match x {
      "new" => Some(Self::New),
      "sgn" => Some(Self::Sgn),
      "abs" => Some(Self::Abs),
      "eql" => Some(Self::Eql),
      "lte" => Some(Self::Lte),
      "lth" => Some(Self::Lth),
      "gte" => Some(Self::Gte),
      "gth" => Some(Self::Gth),
      "add" => Some(Self::Add),
      "sub" => Some(Self::Sub),
      "mul" => Some(Self::Mul),
      "div" => Some(Self::Div),
      "mod" => Some(Self::Mod),
      _ => None,
    }
  }

  pub fn type_of(self) -> Term {
    match self {
      Self::New => yatima!("∀ #Bool #Nat -> #Int"),
      Self::Sgn => yatima!("∀ #Int -> #Bool"),
      Self::Abs => yatima!("∀ #Int -> #Nat"),
      Self::Eql => yatima!("∀ #Int #Int -> #Bool"),
      Self::Lte => yatima!("∀ #Int #Int -> #Bool"),
      Self::Lth => yatima!("∀ #Int #Int -> #Bool"),
      Self::Gte => yatima!("∀ #Int #Int -> #Bool"),
      Self::Gth => yatima!("∀ #Int #Int -> #Bool"),
      Self::Add => yatima!("∀ #Int #Int -> #Int"),
      Self::Sub => yatima!("∀ #Int #Int -> #Int"),
      Self::Mul => yatima!("∀ #Int #Int -> #Int"),
      Self::Div => yatima!("∀ #Int #Int -> #Int"),
      Self::Mod => yatima!("∀ #Int #Int -> #Int"),
    }
  }

  pub fn to_ipld(self) -> Ipld {
    match self {
      Self::New => Ipld::Integer(0),
      Self::Sgn => Ipld::Integer(1),
      Self::Abs => Ipld::Integer(2),
      Self::Eql => Ipld::Integer(3),
      Self::Lte => Ipld::Integer(4),
      Self::Lth => Ipld::Integer(5),
      Self::Gte => Ipld::Integer(6),
      Self::Gth => Ipld::Integer(7),
      Self::Add => Ipld::Integer(8),
      Self::Sub => Ipld::Integer(9),
      Self::Mul => Ipld::Integer(10),
      Self::Div => Ipld::Integer(11),
      Self::Mod => Ipld::Integer(12),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Integer(0) => Ok(Self::New),
      Ipld::Integer(1) => Ok(Self::Sgn),
      Ipld::Integer(2) => Ok(Self::Abs),
      Ipld::Integer(3) => Ok(Self::Eql),
      Ipld::Integer(4) => Ok(Self::Lte),
      Ipld::Integer(5) => Ok(Self::Lth),
      Ipld::Integer(6) => Ok(Self::Gte),
      Ipld::Integer(7) => Ok(Self::Gth),
      Ipld::Integer(8) => Ok(Self::Add),
      Ipld::Integer(9) => Ok(Self::Sub),
      Ipld::Integer(10) => Ok(Self::Mul),
      Ipld::Integer(11) => Ok(Self::Div),
      Ipld::Integer(12) => Ok(Self::Mod),
      xs => Err(IpldError::IntOp(xs.to_owned())),
    }
  }

  pub fn arity(self) -> u64 {
    match self {
      Self::New => 2,
      Self::Sgn => 1,
      Self::Abs => 1,
      Self::Eql => 2,
      Self::Lth => 2,
      Self::Lte => 2,
      Self::Gth => 2,
      Self::Gte => 2,
      Self::Add => 2,
      Self::Sub => 2,
      Self::Mul => 2,
      Self::Div => 2,
      Self::Mod => 2,
    }
  }

  pub fn apply1(self, x: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x) {
      (Self::Sgn, Int(x)) => match x.sign() {
        Sign::NoSign => Some(Int(BigInt::from(0i64))),
        Sign::Plus => Some(Int(BigInt::from(1i64))),
        Sign::Minus => Some(Int(BigInt::from(-1i64))),
      },
      (Self::Abs, Int(x)) => Some(Nat(x.clone().into_parts().1)),
      _ => None,
    }
  }

  pub fn apply2(self, x: &Literal, y: &Literal) -> Option<Literal> {
    use Literal::*;
    let tt = Bool(true);
    let ff = Bool(false);
    let ite = |c| if c { tt } else { ff };
    match (self, x, y) {
      (Self::Eql, Int(x), Int(y)) => Some(ite(x == y)),
      (Self::Lth, Int(x), Int(y)) => Some(ite(x < y)),
      (Self::Lte, Int(x), Int(y)) => Some(ite(x <= y)),
      (Self::Gth, Int(x), Int(y)) => Some(ite(x > y)),
      (Self::Add, Int(x), Int(y)) => Some(Int(x + y)),
      (Self::Sub, Int(x), Int(y)) => Some(Int(x - y)),
      (Self::Mul, Int(x), Int(y)) => Some(Int(x * y)),
      (Self::Div, Int(x), Int(y)) if *y != 0.into() => Some(Int(x / y)),
      (Self::Mod, Int(x), Int(y)) if *y != 0.into() => Some(Int(x % y)),
      _ => None,
    }
  }
}

impl fmt::Display for IntOp {
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
  impl Arbitrary for IntOp {
    fn arbitrary(_g: &mut Gen) -> Self {
      let mut rng = rand::thread_rng();
      let gen: u32 = rng.gen_range(0..12);
      match gen {
        0 => Self::New,
        1 => Self::Sgn,
        2 => Self::Abs,
        3 => Self::Eql,
        4 => Self::Lte,
        5 => Self::Lth,
        6 => Self::Gte,
        7 => Self::Gth,
        8 => Self::Add,
        9 => Self::Sub,
        10 => Self::Mul,
        11 => Self::Div,
        _ => Self::Mod,
      }
    }
  }

  #[quickcheck]
  fn int_op_ipld(x: IntOp) -> bool {
    match IntOp::from_ipld(&x.to_ipld()) {
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
