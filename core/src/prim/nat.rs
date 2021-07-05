use num_bigint::BigUint;
use sp_ipld::Ipld;

use sp_std::{
  fmt,
  borrow::ToOwned,
};

use alloc::string::String;

use crate::{
  ipld_error::IpldError,
  literal::Literal,
  term::Term,
  yatima,
};

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum NatOp {
  Suc,
  Pre,
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

impl NatOp {
  pub fn symbol(self) -> String {
    match self {
      Self::Suc => "suc".to_owned(),
      Self::Pre => "pre".to_owned(),
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
      "suc" => Some(Self::Suc),
      "pre" => Some(Self::Pre),
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
      Self::Suc => yatima!("∀ #Nat -> #Nat"),
      Self::Pre => yatima!("∀ #Nat -> #Nat"),
      Self::Eql => yatima!("∀ #Nat #Nat -> #Bool"),
      Self::Lte => yatima!("∀ #Nat #Nat -> #Bool"),
      Self::Lth => yatima!("∀ #Nat #Nat -> #Bool"),
      Self::Gte => yatima!("∀ #Nat #Nat -> #Bool"),
      Self::Gth => yatima!("∀ #Nat #Nat -> #Bool"),
      Self::Add => yatima!("∀ #Nat #Nat -> #Nat"),
      Self::Sub => yatima!("∀ #Nat #Nat -> #Nat"),
      Self::Mul => yatima!("∀ #Nat #Nat -> #Nat"),
      Self::Div => yatima!("∀ #Nat #Nat -> #Nat"),
      Self::Mod => yatima!("∀ #Nat #Nat -> #Nat"),
    }
  }

  pub fn to_ipld(self) -> Ipld {
    match self {
      Self::Suc => Ipld::Integer(0),
      Self::Pre => Ipld::Integer(1),
      Self::Eql => Ipld::Integer(2),
      Self::Lte => Ipld::Integer(3),
      Self::Lth => Ipld::Integer(4),
      Self::Gte => Ipld::Integer(5),
      Self::Gth => Ipld::Integer(6),
      Self::Add => Ipld::Integer(7),
      Self::Sub => Ipld::Integer(8),
      Self::Mul => Ipld::Integer(9),
      Self::Div => Ipld::Integer(10),
      Self::Mod => Ipld::Integer(11),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Integer(0) => Ok(Self::Suc),
      Ipld::Integer(1) => Ok(Self::Pre),
      Ipld::Integer(2) => Ok(Self::Eql),
      Ipld::Integer(3) => Ok(Self::Lte),
      Ipld::Integer(4) => Ok(Self::Lth),
      Ipld::Integer(5) => Ok(Self::Gte),
      Ipld::Integer(6) => Ok(Self::Gth),
      Ipld::Integer(7) => Ok(Self::Add),
      Ipld::Integer(8) => Ok(Self::Sub),
      Ipld::Integer(9) => Ok(Self::Mul),
      Ipld::Integer(10) => Ok(Self::Div),
      Ipld::Integer(11) => Ok(Self::Mod),
      xs => Err(IpldError::NatOp(xs.to_owned())),
    }
  }

  pub fn arity(self) -> u64 {
    match self {
      Self::Eql => 2,
      Self::Lth => 2,
      Self::Lte => 2,
      Self::Gth => 2,
      Self::Gte => 2,
      Self::Suc => 1,
      Self::Pre => 1,
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
      (Self::Suc, Nat(x)) => Some(Nat(x + BigUint::from(1u64))),
      (Self::Pre, Nat(x)) => {
        if *x != 0u64.into() {
          Some(Nat(x - BigUint::from(1u64)))
        }
        else {
          Some(Nat(BigUint::from(0u64)))
        }
      }
      _ => None,
    }
  }

  pub fn apply2(self, x: &Literal, y: &Literal) -> Option<Literal> {
    use Literal::*;
    let tt = Bool(true);
    let ff = Bool(false);
    let ite = |c| if c { tt } else { ff };
    match (self, x, y) {
      (Self::Eql, Nat(x), Nat(y)) => Some(ite(x == y)),
      (Self::Lte, Nat(x), Nat(y)) => Some(ite(x <= y)),
      (Self::Lth, Nat(x), Nat(y)) => Some(ite(x < y)),
      (Self::Gte, Nat(x), Nat(y)) => Some(ite(x >= y)),
      (Self::Gth, Nat(x), Nat(y)) => Some(ite(x > y)),
      (Self::Add, Nat(x), Nat(y)) => Some(Nat(x + y)),
      (Self::Sub, Nat(x), Nat(y)) if x >= y => Some(Nat(x - y)),
      (Self::Mul, Nat(x), Nat(y)) => Some(Nat(x * y)),
      (Self::Div, Nat(x), Nat(y)) if *y != (0u64).into() => Some(Nat(x / y)),
      (Self::Mod, Nat(x), Nat(y)) if *y != (0u64).into() => Some(Nat(x % y)),
      _ => None,
    }
  }
}

impl fmt::Display for NatOp {
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
    TestResult
  };
  use rand::Rng;
  use Literal::{
    Nat,
    Bool
  };
  use num_bigint::BigUint;
  use sp_std::mem;
  impl Arbitrary for NatOp {
    fn arbitrary(_g: &mut Gen) -> Self {
      let mut rng = rand::thread_rng();
      let gen: u32 = rng.gen_range(0..=11);
      match gen {
        0 => Self::Suc,
        1 => Self::Pre,
        2 => Self::Eql,
        3 => Self::Lte,
        4 => Self::Lth,
        5 => Self::Gte,
        6 => Self::Gth,
        7 => Self::Add,
        8 => Self::Sub,
        9 => Self::Mul,
        10 => Self::Div,
        _ => Self::Mod,
      }
    }
  }

  #[quickcheck]
  fn nat_op_ipld(x: NatOp) -> bool {
    match NatOp::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }

  #[quickcheck]
  fn test_apply(
    op: NatOp,
    a: u64,
    b: u64
  ) -> TestResult {
    let big = BigUint::from;
    let apply1_nat = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        NatOp::apply1(
          op,
          &Nat(big(a))
        ) ==
        expected
      )
    };

    let apply2_nat_nat = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        NatOp::apply2(
          op,
          &Nat(big(a)),
          &Nat(big(b))
        ) ==
        expected
      )
    };

    match op {
      NatOp::Suc => apply1_nat(Some(Nat(big(a) + big(1)))),
      NatOp::Pre => apply1_nat(Some(Nat(big(if a == 0 {
        a
      } else {
        a - 1
      })))),
      NatOp::Eql => apply2_nat_nat(Some(Bool(a == b))),
      NatOp::Lte => apply2_nat_nat(Some(Bool(a <= b))),
      NatOp::Lth => apply2_nat_nat(Some(Bool(a < b))),
      NatOp::Gte => apply2_nat_nat(Some(Bool(a >= b))),
      NatOp::Gth => apply2_nat_nat(Some(Bool(a > b))),
      NatOp::Add => apply2_nat_nat(Some(Nat(big(a) + big(b)))),
      NatOp::Sub => apply2_nat_nat(
          if a >= b {
          Some(Nat(big(a - b)))
        } else {
          None
        }
      ),
      NatOp::Mul => apply2_nat_nat(Some(Nat(big(a) * big(b)))),
      NatOp::Div => apply2_nat_nat(
          if b != 0 {
          Some(Nat(big(a / b)))
        } else {
          None
        }
      ),
      NatOp::Mod => apply2_nat_nat(
          if b != 0 {
          Some(Nat(big(a % b)))
        } else {
          None
        }
      ),
    }
  }

  #[quickcheck]
  fn test_apply_none_on_invalid(
    op: NatOp,
    a: Literal,
    b: u64,
    test_arg_2: bool,
  ) -> TestResult {
    let big = BigUint::from;
    let test_apply1_none_on_invalid = |
      valid_arg: Literal
    | -> TestResult {
      if mem::discriminant(&valid_arg) == mem::discriminant(&a) {
        TestResult::discard()
      } else {
        TestResult::from_bool(
          NatOp::apply1(
            op,
            &a
          ) ==
          None
        )
      }
    };

    let test_apply2_none_on_invalid = |
      valid_arg: Literal,
      a_: Literal,
      b_: Literal
    | -> TestResult {
      let go = || TestResult::from_bool(
        NatOp::apply2(
          op,
          &a_,
          &b_
        ) ==
        None
      );
      if test_arg_2 {
        if mem::discriminant(&valid_arg) == mem::discriminant(&a_) {
          TestResult::discard()
        } else {
          go()
        }
      } else {
        if mem::discriminant(&valid_arg) == mem::discriminant(&b_) {
          TestResult::discard()
        } else {
          go()
        }
      }
    };

    match op {
      // Arity 1, valid is Nat.
      NatOp::Suc | 
      NatOp::Pre => test_apply1_none_on_invalid(Nat(big(b))),
      // Arity 2, valid are Nat on a and b.
      NatOp::Eql |
      NatOp::Lte |
      NatOp::Lth |
      NatOp::Gte |
      NatOp::Gth |
      NatOp::Add |
      NatOp::Sub |
      NatOp::Mul |
      NatOp::Div |
      NatOp::Mod => if test_arg_2 {
        test_apply2_none_on_invalid(
          Nat(big(b)),
          a,
          Nat(big(b))
        )
      } else {
        test_apply2_none_on_invalid(
          Nat(big(b)),
          Nat(big(b)),
          a
        )
      },
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
