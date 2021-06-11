use num_bigint::{
  BigInt,
  Sign,
};
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
      (Self::Sgn, Int(x)) => Some(Bool(matches!(x.sign(), Sign::Plus))),
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
      (Self::New, Bool(x), Nat(y)) => Some(Int(if *x {
        BigInt::from(y.clone())
      } else {
        BigInt::from(y.clone()) * -1
      })),
      (Self::Eql, Int(x), Int(y)) => Some(ite(x == y)),
      (Self::Lte, Int(x), Int(y)) => Some(ite(x <= y)),
      (Self::Lth, Int(x), Int(y)) => Some(ite(x < y)),
      (Self::Gte, Int(x), Int(y)) => Some(ite(x >= y)),
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
    TestResult
  };
  use rand::Rng;
  use Literal::{
    Int,
    Bool,
    Nat
  };
  use num_bigint::{
    BigInt,
    BigUint
  };
  use std::mem;
  impl Arbitrary for IntOp {
    fn arbitrary(_g: &mut Gen) -> Self {
      let mut rng = rand::thread_rng();
      let gen: u32 = rng.gen_range(0..=12);
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

  #[quickcheck]
  fn test_apply(
    op: IntOp,
    a: i64,
    b: i64,
    c: bool,
    d: u64
  ) -> TestResult {
    let big_int = BigInt::from;
    let big_uint = BigUint::from;
    let apply1_int = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        IntOp::apply1(
          op,
          &Int(big_int(a))
        ) ==
        expected
      )
    };

    let apply2_bool_nat = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        IntOp::apply2(
          op,
          &Bool(c),
          &Nat(big_uint(d))
        ) ==
        expected
      )
    };

    let apply2_int_int = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        IntOp::apply2(
          op,
          &Int(big_int(a)),
          &Int(big_int(b))
        ) ==
        expected
      )
    };

    match op {
      IntOp::New => apply2_bool_nat(Some(Int(if c {
        BigInt::from(d)
      } else {
        BigInt::from(d) * -1
      }))),
      IntOp::Sgn => apply1_int(Some(Bool(a.is_positive()))),
      IntOp::Abs => apply1_int(Some(Nat(BigUint::from(a.unsigned_abs())))),
      IntOp::Eql => apply2_int_int(Some(Bool(a == b))),
      IntOp::Lte => apply2_int_int(Some(Bool(a <= b))),
      IntOp::Lth => apply2_int_int(Some(Bool(a < b))),
      IntOp::Gte => apply2_int_int(Some(Bool(a >= b))),
      IntOp::Gth => apply2_int_int(Some(Bool(a > b))),
      IntOp::Add => apply2_int_int(Some(Int(big_int(a) + big_int(b)))),
      IntOp::Sub => apply2_int_int(Some(Int(big_int(a) - big_int(b)))),
      IntOp::Mul => apply2_int_int(Some(Int(big_int(a) * big_int(b)))),
      IntOp::Div => apply2_int_int(
          if b != 0 {
          Some(Int(big_int(a) / big_int(b)))
        } else {
          None
        }
      ),
      IntOp::Mod => apply2_int_int(
          if b != 0 {
          Some(Int(big_int(a) % big_int(b)))
        } else {
          None
        }
      ),
    }
  }

  #[quickcheck]
  fn test_apply_none_on_invalid(
    op: IntOp,
    a: Literal,
    b: i64,
    test_arg_2: bool,
  ) -> TestResult {
    let big = BigInt::from;
    let test_apply1_none_on_invalid = |
      valid_arg: Literal
    | -> TestResult {
      if mem::discriminant(&valid_arg) == mem::discriminant(&a) {
        TestResult::discard()
      } else {
        TestResult::from_bool(
          IntOp::apply1(
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
        IntOp::apply2(
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
      // Arity 1, valid is Int.
      IntOp::Sgn | 
      IntOp::Abs => test_apply1_none_on_invalid(Int(big(b))),
      // Arity 2, valid are Int on a and b.
      IntOp::New |
      IntOp::Eql |
      IntOp::Lte |
      IntOp::Lth |
      IntOp::Gte |
      IntOp::Gth |
      IntOp::Add |
      IntOp::Sub |
      IntOp::Mul |
      IntOp::Div |
      IntOp::Mod => if test_arg_2 {
        test_apply2_none_on_invalid(
          Int(big(b)),
          a,
          Int(big(b))
        )
      } else {
        test_apply2_none_on_invalid(
          Int(big(b)),
          Int(big(b)),
          a,
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
