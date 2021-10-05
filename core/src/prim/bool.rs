use sp_ipld::Ipld;

use sp_std::{
  borrow::ToOwned,
  fmt,
};

use alloc::string::String;

use crate::{
  defs,
  ipld_error::IpldError,
  literal::Literal,
  parse,
  term::Term,
  yatima,
};

/// Primitive boolean operations
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
  /// Gets the syntax string of a boolean operation
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

  /// Gets a Bool operation from a syntax string
  pub fn from_symbol(x: &str) -> Option<Self> {
    match x {
      "eql" => Some(Self::Eql),
      "lte" => Some(Self::Lte),
      "lth" => Some(Self::Lth),
      "gte" => Some(Self::Gte),
      "gth" => Some(Self::Gth),
      "and" => Some(Self::And),
      "or" => Some(Self::Or),
      "xor" => Some(Self::Xor),
      "not" => Some(Self::Not),
      _ => None,
    }
  }

  /// Returns the type of a Bool operation
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

  /// Converts a Bool operation into an IPLD object
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

  /// Converts an IPLD object into a Bool operation
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

  /// Returns the number of parameters used in the operation
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

  /// Applies a unary operation to a literal and returns it if successful
  pub fn apply1(self, x: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x) {
      (Self::Not, Bool(x)) => Some(Bool(!x)),
      _ => None,
    }
  }

  /// Applies a binary operation to a literal and returns it if successful
  pub fn apply2(self, x: &Literal, y: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x, y) {
      (Self::Eql, Bool(x), Bool(y)) => Some(Bool(x == y)),
      (Self::Lte, Bool(x), Bool(y)) => Some(Bool(x <= y)),
      (Self::Lth, Bool(x), Bool(y)) => Some(Bool(x < y)),
      (Self::Gte, Bool(x), Bool(y)) => Some(Bool(x >= y)),
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
    TestResult,
  };
  use rand::Rng;
  use sp_std::mem;
  use Literal::Bool;
  impl Arbitrary for BoolOp {
    fn arbitrary(_g: &mut Gen) -> Self {
      let mut rng = rand::thread_rng();
      let gen: u32 = rng.gen_range(0..=8);
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

  #[quickcheck]
  fn test_apply(op: BoolOp, a: bool, b: bool) -> TestResult {
    let apply1_bool = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(BoolOp::apply1(op, &Bool(a)) == expected)
    };

    let apply2_bool_bool = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(BoolOp::apply2(op, &Bool(a), &Bool(b)) == expected)
    };

    match op {
      BoolOp::Eql => apply2_bool_bool(Some(Bool(a == b))),
      BoolOp::Lte => apply2_bool_bool(Some(Bool(a <= b))),
      BoolOp::Lth => apply2_bool_bool(Some(Bool(a < b))),
      BoolOp::Gte => apply2_bool_bool(Some(Bool(a >= b))),
      BoolOp::Gth => apply2_bool_bool(Some(Bool(a > b))),
      BoolOp::And => apply2_bool_bool(Some(Bool(a && b))),
      BoolOp::Or => apply2_bool_bool(Some(Bool(a || b))),
      BoolOp::Xor => apply2_bool_bool(Some(Bool(a ^ b))),
      BoolOp::Not => apply1_bool(Some(Bool(!a))),
    }
  }

  #[quickcheck]
  fn test_apply_none_on_invalid(
    op: BoolOp,
    a: Literal,
    b: bool,
    test_arg_2: bool,
  ) -> TestResult {
    let test_apply1_none_on_invalid = |valid_arg: Literal| -> TestResult {
      if mem::discriminant(&valid_arg) == mem::discriminant(&a) {
        TestResult::discard()
      }
      else {
        TestResult::from_bool(BoolOp::apply1(op, &a) == None)
      }
    };

    let test_apply2_none_on_invalid =
      |valid_arg: Literal, a_: Literal, b_: Literal| -> TestResult {
        let go = || TestResult::from_bool(BoolOp::apply2(op, &a_, &b_) == None);
        if test_arg_2 {
          if mem::discriminant(&valid_arg) == mem::discriminant(&a_) {
            TestResult::discard()
          }
          else {
            go()
          }
        }
        else {
          if mem::discriminant(&valid_arg) == mem::discriminant(&b_) {
            TestResult::discard()
          }
          else {
            go()
          }
        }
      };

    match op {
      // Arity 1, valid is Bool.
      BoolOp::Not => test_apply1_none_on_invalid(Bool(b)),
      // Arity 2, valid are Bool on a and b.
      BoolOp::Eql
      | BoolOp::Lte
      | BoolOp::Lth
      | BoolOp::Gte
      | BoolOp::Gth
      | BoolOp::And
      | BoolOp::Or
      | BoolOp::Xor => {
        if test_arg_2 {
          test_apply2_none_on_invalid(Bool(b), a, Bool(b))
        }
        else {
          test_apply2_none_on_invalid(Bool(b), Bool(b), a)
        }
      }
    }
  }

  #[quickcheck]
  fn test_associativity(op: BoolOp, a: bool, b: bool, c: bool) -> TestResult {
    match op {
      BoolOp::Or | BoolOp::And => TestResult::from_bool(
        BoolOp::apply2(
          op,
          &Bool(a),
          &BoolOp::apply2(op, &Bool(b), &Bool(c)).unwrap(),
        ) == BoolOp::apply2(
          op,
          &BoolOp::apply2(op, &Bool(a), &Bool(b)).unwrap(),
          &Bool(c),
        ),
      ),
      _ => TestResult::discard(),
    }
  }

  #[quickcheck]
  fn test_commutativity(op: BoolOp, left: bool, right: bool) -> TestResult {
    match op {
      BoolOp::Or | BoolOp::And => TestResult::from_bool(
        BoolOp::apply2(op, &Bool(left), &Bool(right))
          == BoolOp::apply2(op, &Bool(left), &Bool(right)),
      ),
      _ => TestResult::discard(),
    }
  }

  #[quickcheck]
  fn test_distributivity_of_and_over_or(a: bool, b: bool, c: bool) -> bool {
    BoolOp::apply2(
      BoolOp::And,
      &Bool(a),
      &BoolOp::apply2(BoolOp::Or, &Bool(b), &Bool(c)).unwrap(),
    ) == BoolOp::apply2(
      BoolOp::Or,
      &BoolOp::apply2(BoolOp::And, &Bool(a), &Bool(b)).unwrap(),
      &BoolOp::apply2(BoolOp::And, &Bool(a), &Bool(c)).unwrap(),
    )
  }

  #[quickcheck]
  fn test_idempotence(op: BoolOp, input: bool) -> TestResult {
    match op {
      BoolOp::Or | BoolOp::And => TestResult::from_bool(
        BoolOp::apply2(op, &Bool(input), &Bool(input)) == Some(Bool(input)),
      ),
      _ => TestResult::discard(),
    }
  }

  #[quickcheck]
  fn test_absorption_1(a: bool, b: bool) -> bool {
    BoolOp::apply2(
      BoolOp::And,
      &Bool(a),
      &BoolOp::apply2(BoolOp::Or, &Bool(a), &Bool(b)).unwrap(),
    ) == Some(Bool(a))
  }

  #[quickcheck]
  fn test_absorption_2(a: bool, b: bool) -> bool {
    BoolOp::apply2(
      BoolOp::Or,
      &Bool(a),
      &BoolOp::apply2(BoolOp::And, &Bool(a), &Bool(b)).unwrap(),
    ) == Some(Bool(a))
  }

  #[quickcheck]
  fn test_distributivity_of_or_over_and(a: bool, b: bool, c: bool) -> bool {
    BoolOp::apply2(
      BoolOp::Or,
      &Bool(a),
      &BoolOp::apply2(BoolOp::And, &Bool(b), &Bool(c)).unwrap(),
    ) == BoolOp::apply2(
      BoolOp::And,
      &BoolOp::apply2(BoolOp::Or, &Bool(a), &Bool(b)).unwrap(),
      &BoolOp::apply2(BoolOp::Or, &Bool(a), &Bool(c)).unwrap(),
    )
  }

  #[quickcheck]
  fn test_double_negation(input: bool) -> bool {
    BoolOp::apply1(
      BoolOp::Not,
      &BoolOp::apply1(BoolOp::Not, &Bool(input)).unwrap(),
    ) == Some(Bool(input))
  }

  #[quickcheck]
  fn test_de_morgan_1(a: bool, b: bool) -> bool {
    BoolOp::apply2(
      BoolOp::And,
      &BoolOp::apply1(BoolOp::Not, &Bool(a)).unwrap(),
      &BoolOp::apply1(BoolOp::Not, &Bool(b)).unwrap(),
    ) == BoolOp::apply1(
      BoolOp::Not,
      &BoolOp::apply2(BoolOp::Or, &Bool(a), &Bool(b)).unwrap(),
    )
  }

  #[quickcheck]
  fn test_de_morgan_2(a: bool, b: bool) -> bool {
    BoolOp::apply2(
      BoolOp::Or,
      &BoolOp::apply1(BoolOp::Not, &Bool(a)).unwrap(),
      &BoolOp::apply1(BoolOp::Not, &Bool(b)).unwrap(),
    ) == BoolOp::apply1(
      BoolOp::Not,
      &BoolOp::apply2(BoolOp::And, &Bool(a), &Bool(b)).unwrap(),
    )
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
