// use cid::Cid;
// use std::convert::TryInto;

use libipld::ipld::Ipld;

use crate::{
  ipld_error::IpldError,
  literal::Literal,
  term::Term,
  yatima,
};
use num_bigint::{
  BigInt,
  BigUint,
  Sign,
};

use std::fmt;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum PrimOp {
  NatEql,
  NatLth,
  NatLte,
  NatGth,
  NatGte,
  NatSuc,
  NatPre,
  NatAdd,
  NatSub,
  NatMul,
  NatDiv,
  NatMod,
  IntNew,
  IntSgn,
  IntAbs,
  IntEql,
  IntLth,
  IntLte,
  IntGth,
  IntGte,
  IntAdd,
  IntSub,
  IntMul,
  IntDiv,
  IntMod,
  TextCons,
}

impl PrimOp {
  pub fn symbol(self) -> String {
    match self {
      Self::NatSuc => "#Nat.suc".to_owned(),
      Self::NatPre => "#Nat.pre".to_owned(),
      Self::NatEql => "#Nat.eql".to_owned(),
      Self::NatLth => "#Nat.lth".to_owned(),
      Self::NatLte => "#Nat.lte".to_owned(),
      Self::NatGth => "#Nat.gth".to_owned(),
      Self::NatGte => "#Nat.gte".to_owned(),
      Self::NatAdd => "#Nat.add".to_owned(),
      Self::NatSub => "#Nat.sub".to_owned(),
      Self::NatMul => "#Nat.mul".to_owned(),
      Self::NatDiv => "#Nat.div".to_owned(),
      Self::NatMod => "#Nat.mod".to_owned(),
      Self::IntNew => "#Int.new".to_owned(),
      Self::IntSgn => "#Int.sgn".to_owned(),
      Self::IntAbs => "#Int.abs".to_owned(),
      Self::IntEql => "#Int.eql".to_owned(),
      Self::IntLth => "#Int.lth".to_owned(),
      Self::IntLte => "#Int.lte".to_owned(),
      Self::IntGth => "#Int.gth".to_owned(),
      Self::IntGte => "#Int.gte".to_owned(),
      Self::IntAdd => "#Int.add".to_owned(),
      Self::IntSub => "#Int.sub".to_owned(),
      Self::IntMul => "#Int.mul".to_owned(),
      Self::IntDiv => "#Int.div".to_owned(),
      Self::IntMod => "#Int.mod".to_owned(),
      Self::TextCons => "#Text.cons".to_owned(),
    }
  }

  pub fn from_symbol(s: String) -> Option<Self> {
    match s.as_str() {
      "#Nat.suc" => Some(Self::NatSuc),
      "#Nat.pre" => Some(Self::NatPre),
      "#Nat.eql" => Some(Self::NatEql),
      "#Nat.lth" => Some(Self::NatLth),
      "#Nat.lte" => Some(Self::NatLte),
      "#Nat.gth" => Some(Self::NatGth),
      "#Nat.gte" => Some(Self::NatGte),
      "#Nat.add" => Some(Self::NatAdd),
      "#Nat.sub" => Some(Self::NatSub),
      "#Nat.mul" => Some(Self::NatMul),
      "#Nat.div" => Some(Self::NatDiv),
      "#Nat.mod" => Some(Self::NatMod),
      "#Int.new" => Some(Self::IntNew),
      "#Int.sgn" => Some(Self::IntSgn),
      "#Int.abs" => Some(Self::IntAbs),
      "#Int.eql" => Some(Self::IntEql),
      "#Int.lth" => Some(Self::IntLth),
      "#Int.lte" => Some(Self::IntLte),
      "#Int.gth" => Some(Self::IntGth),
      "#Int.gte" => Some(Self::IntGte),
      "#Int.add" => Some(Self::IntAdd),
      "#Int.sub" => Some(Self::IntSub),
      "#Int.mul" => Some(Self::IntMul),
      "#Int.div" => Some(Self::IntDiv),
      "#Int.mod" => Some(Self::IntMod),
      "#Text.cons" => Some(Self::TextCons),
      _ => None,
    }
  }

  pub fn type_of(self) -> Term {
    match self {
      Self::NatSuc => yatima!("∀ #Nat -> #Nat"),
      Self::NatPre => yatima!("∀ #Nat -> #Nat"),
      Self::NatEql => yatima!("∀ #Nat #Nat -> #Bool"),
      Self::NatLth => yatima!("∀ #Nat #Nat -> #Bool"),
      Self::NatLte => yatima!("∀ #Nat #Nat -> #Bool"),
      Self::NatGth => yatima!("∀ #Nat #Nat -> #Bool"),
      Self::NatGte => yatima!("∀ #Nat #Nat -> #Bool"),
      Self::NatAdd => yatima!("∀ #Nat #Nat -> #Nat"),
      Self::NatSub => yatima!("∀ #Nat #Nat -> #Nat"),
      Self::NatMul => yatima!("∀ #Nat #Nat -> #Nat"),
      Self::NatDiv => yatima!("∀ #Nat #Nat -> #Nat"),
      Self::NatMod => yatima!("∀ #Nat #Nat -> #Nat"),
      // Self::IntNew => "#Int.new".to_owned(),
      // Self::IntSgn => "#Int.sgn".to_owned(),
      // Self::IntAbs => "#Int.abs".to_owned(),
      // Self::IntEql => "#Int.eql".to_owned(),
      // Self::IntLth => "#Int.lth".to_owned(),
      // Self::IntLte => "#Int.lte".to_owned(),
      // Self::IntGth => "#Int.gth".to_owned(),
      // Self::IntGte => "#Int.gte".to_owned(),
      // Self::IntAdd => "#Int.add".to_owned(),
      // Self::IntSub => "#Int.sub".to_owned(),
      // Self::IntMul => "#Int.mul".to_owned(),
      // Self::IntDiv => "#Int.div".to_owned(),
      // Self::IntMod => "#Int.mod".to_owned(),
      Self::TextCons => yatima!("∀ #Char #Text -> #Text"),
      _ => todo!(),
    }
  }

  pub fn to_ipld(self) -> Ipld {
    match self {
      Self::NatSuc => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(0)]),
      Self::NatPre => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(1)]),
      Self::NatEql => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(2)]),
      Self::NatLth => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(3)]),
      Self::NatLte => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(4)]),
      Self::NatGth => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(5)]),
      Self::NatGte => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(6)]),
      Self::NatAdd => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(7)]),
      Self::NatSub => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(8)]),
      Self::NatMul => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(9)]),
      Self::NatDiv => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(10)]),
      Self::NatMod => Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(11)]),
      Self::IntNew => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(0)]),
      Self::IntSgn => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(1)]),
      Self::IntAbs => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(2)]),
      Self::IntEql => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(3)]),
      Self::IntLth => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(4)]),
      Self::IntLte => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(5)]),
      Self::IntGth => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(6)]),
      Self::IntGte => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(7)]),
      Self::IntAdd => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(8)]),
      Self::IntSub => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(9)]),
      Self::IntMul => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(10)]),
      Self::IntDiv => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(11)]),
      Self::IntMod => Ipld::List(vec![Ipld::Integer(1), Ipld::Integer(12)]),
      Self::TextCons => Ipld::List(vec![Ipld::Integer(3), Ipld::Integer(0)]),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::List(xs) => match xs.as_slice() {
        [Ipld::Integer(0), Ipld::Integer(0)] => Ok(Self::NatSuc),
        [Ipld::Integer(0), Ipld::Integer(1)] => Ok(Self::NatPre),
        [Ipld::Integer(0), Ipld::Integer(2)] => Ok(Self::NatEql),
        [Ipld::Integer(0), Ipld::Integer(3)] => Ok(Self::NatLth),
        [Ipld::Integer(0), Ipld::Integer(4)] => Ok(Self::NatLte),
        [Ipld::Integer(0), Ipld::Integer(5)] => Ok(Self::NatGth),
        [Ipld::Integer(0), Ipld::Integer(6)] => Ok(Self::NatGte),
        [Ipld::Integer(0), Ipld::Integer(7)] => Ok(Self::NatAdd),
        [Ipld::Integer(0), Ipld::Integer(8)] => Ok(Self::NatSub),
        [Ipld::Integer(0), Ipld::Integer(9)] => Ok(Self::NatMul),
        [Ipld::Integer(0), Ipld::Integer(10)] => Ok(Self::NatDiv),
        [Ipld::Integer(0), Ipld::Integer(11)] => Ok(Self::NatMod),
        [Ipld::Integer(1), Ipld::Integer(0)] => Ok(Self::IntNew),
        [Ipld::Integer(1), Ipld::Integer(1)] => Ok(Self::IntSgn),
        [Ipld::Integer(1), Ipld::Integer(2)] => Ok(Self::IntAbs),
        [Ipld::Integer(1), Ipld::Integer(3)] => Ok(Self::IntEql),
        [Ipld::Integer(1), Ipld::Integer(4)] => Ok(Self::IntLth),
        [Ipld::Integer(1), Ipld::Integer(5)] => Ok(Self::IntLte),
        [Ipld::Integer(1), Ipld::Integer(6)] => Ok(Self::IntGth),
        [Ipld::Integer(1), Ipld::Integer(7)] => Ok(Self::IntGte),
        [Ipld::Integer(1), Ipld::Integer(8)] => Ok(Self::IntAdd),
        [Ipld::Integer(1), Ipld::Integer(9)] => Ok(Self::IntSub),
        [Ipld::Integer(1), Ipld::Integer(10)] => Ok(Self::IntMul),
        [Ipld::Integer(1), Ipld::Integer(11)] => Ok(Self::IntDiv),
        [Ipld::Integer(1), Ipld::Integer(12)] => Ok(Self::IntMod),
        [Ipld::Integer(3), Ipld::Integer(0)] => Ok(Self::TextCons),
        xs => Err(IpldError::PrimOp(Ipld::List(xs.to_owned()))),
      },
      xs => Err(IpldError::PrimOp(xs.to_owned())),
    }
  }

  pub fn arity(self) -> u64 {
    match self {
      Self::NatEql => 2,
      Self::NatLth => 2,
      Self::NatLte => 2,
      Self::NatGth => 2,
      Self::NatGte => 2,
      Self::NatSuc => 1,
      Self::NatPre => 1,
      Self::NatAdd => 2,
      Self::NatSub => 2,
      Self::NatMul => 2,
      Self::NatDiv => 2,
      Self::NatMod => 2,
      Self::IntNew => 2,
      Self::IntSgn => 1,
      Self::IntAbs => 1,
      Self::IntEql => 2,
      Self::IntLth => 2,
      Self::IntLte => 2,
      Self::IntGth => 2,
      Self::IntGte => 2,
      Self::IntAdd => 2,
      Self::IntSub => 2,
      Self::IntMul => 2,
      Self::IntDiv => 2,
      Self::IntMod => 2,
      Self::TextCons => 2,
    }
  }
}

pub fn apply_una_op(opr: PrimOp, x: Literal) -> Option<Literal> {
  use Literal::*;
  use PrimOp::*;
  match (opr, x) {
    (NatSuc, Nat(x)) => Some(Nat(x + BigUint::from(1u64))),
    (NatPre, Nat(x)) if x != 0u64.into() => Some(Nat(x - BigUint::from(1u64))),
    (IntSgn, Int(x)) => match x.sign() {
      Sign::NoSign => Some(Int(BigInt::from(0i64))),
      Sign::Plus => Some(Int(BigInt::from(1i64))),
      Sign::Minus => Some(Int(BigInt::from(-1i64))),
    },
    (IntAbs, Int(x)) => Some(Nat(x.into_parts().1)),
    _ => None,
  }
}

pub fn apply_bin_op(opr: PrimOp, x: Literal, y: Literal) -> Option<Literal> {
  use Literal::*;
  use PrimOp::*;
  let tt = Bool(true);
  let ff = Bool(false);
  let ite = |c| if c { tt } else { ff };
  match (opr, x, y) {
    // Construction
    (IntNew, Bool(x), Nat(y)) => {
      if y == 0u64.into() {
        Some(Int(BigInt::from_biguint(Sign::NoSign, y)))
      }
      else if x {
        Some(Int(BigInt::from_biguint(Sign::Plus, y)))
      }
      else {
        Some(Int(BigInt::from_biguint(Sign::Minus, y)))
      }
    }
    // Comparison
    (NatEql, Nat(x), Nat(y)) => Some(ite(x == y)),
    (IntEql, Int(x), Int(y)) => Some(ite(x == y)),
    (NatLth, Nat(x), Nat(y)) => Some(ite(x < y)),
    (IntLth, Int(x), Int(y)) => Some(ite(x < y)),
    (NatLte, Nat(x), Nat(y)) => Some(ite(x <= y)),
    (IntLte, Int(x), Int(y)) => Some(ite(x <= y)),
    (NatGth, Nat(x), Nat(y)) => Some(ite(x > y)),
    (IntGth, Int(x), Int(y)) => Some(ite(x > y)),
    // Arithmetic
    (NatAdd, Nat(x), Nat(y)) => Some(Nat(x + y)),
    (IntAdd, Int(x), Int(y)) => Some(Int(x + y)),
    (NatSub, Nat(x), Nat(y)) if x >= y => Some(Nat(x - y)),
    (IntSub, Int(x), Int(y)) => Some(Int(x - y)),
    (NatMul, Nat(x), Nat(y)) => Some(Nat(x * y)),
    (IntMul, Int(x), Int(y)) => Some(Int(x * y)),
    (NatDiv, Nat(x), Nat(y)) if y != (0u64).into() => Some(Nat(x * y)),
    (IntDiv, Int(x), Int(y)) if y != 0.into() => Some(Int(x / y)),
    (NatMod, Nat(x), Nat(y)) if y != (0u64).into() => Some(Nat(x * y)),
    (IntMod, Int(x), Int(y)) if y != 0.into() => Some(Int(x % y)),
    (TextCons, Char(c), Text(cs)) => {
      let mut txt: String = c.into();
      txt.push_str(&cs);
      Some(Text(txt))
    }
    _ => None,
  }
}

impl fmt::Display for PrimOp {
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
  impl Arbitrary for PrimOp {
    fn arbitrary(_g: &mut Gen) -> Self {
      let mut rng = rand::thread_rng();
      let gen: u32 = rng.gen_range(0..24);
      match gen {
        0 => Self::NatEql,
        1 => Self::NatLth,
        2 => Self::NatLte,
        3 => Self::NatGth,
        4 => Self::NatGte,
        5 => Self::NatSuc,
        6 => Self::NatPre,
        7 => Self::NatAdd,
        8 => Self::NatSub,
        9 => Self::NatMul,
        10 => Self::NatDiv,
        11 => Self::NatMod,
        12 => Self::IntNew,
        13 => Self::IntSgn,
        14 => Self::IntAbs,
        15 => Self::IntEql,
        16 => Self::IntLth,
        17 => Self::IntLte,
        18 => Self::IntGth,
        19 => Self::IntGte,
        20 => Self::IntAdd,
        21 => Self::IntSub,
        22 => Self::IntMul,
        23 => Self::IntDiv,
        _ => Self::IntMod,
      }
    }
  }

  #[quickcheck]
  fn primop_ipld(x: PrimOp) -> bool {
    match PrimOp::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}
