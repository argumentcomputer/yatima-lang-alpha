use sp_ipld::Ipld;

use sp_std::{
  borrow::ToOwned,
  convert::TryFrom,
  fmt,
};

use alloc::string::String;

use crate::{
  ipld_error::IpldError,
  literal::Literal,
  prim::bits,
};

use num_bigint::BigUint;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum I16Op {
  Abs,
  Sgn,
  Max,
  Min,
  Eql,
  Lte,
  Lth,
  Gth,
  Gte,
  Not,
  And,
  Or,
  Xor,
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  Pow,
  Shl,
  Shr,
  Rol,
  Ror,
  CountZeros,
  CountOnes,
  ToU8,
  ToU16,
  ToU32,
  ToU64,
  ToNat,
  ToI8,
  ToI32,
  ToI64,
  ToInt,
  ToBits,
  ToBytes,
}

impl I16Op {
  pub fn symbol(self) -> String {
    match self {
      Self::Abs => "abs".to_owned(),
      Self::Sgn => "sgn".to_owned(),
      Self::Max => "max".to_owned(),
      Self::Min => "min".to_owned(),
      Self::Eql => "eql".to_owned(),
      Self::Lte => "lte".to_owned(),
      Self::Lth => "lth".to_owned(),
      Self::Gth => "gth".to_owned(),
      Self::Gte => "gte".to_owned(),
      Self::Not => "not".to_owned(),
      Self::And => "and".to_owned(),
      Self::Or => "or".to_owned(),
      Self::Xor => "xor".to_owned(),
      Self::Add => "add".to_owned(),
      Self::Sub => "sub".to_owned(),
      Self::Mul => "mul".to_owned(),
      Self::Div => "div".to_owned(),
      Self::Mod => "mod".to_owned(),
      Self::Pow => "pow".to_owned(),
      Self::Shl => "shl".to_owned(),
      Self::Shr => "shr".to_owned(),
      Self::Rol => "rol".to_owned(),
      Self::Ror => "ror".to_owned(),
      Self::CountZeros => "count_zeros".to_owned(),
      Self::CountOnes => "count_ones".to_owned(),
      Self::ToU8 => "to_U8".to_owned(),
      Self::ToU16 => "to_U16".to_owned(),
      Self::ToU32 => "to_U32".to_owned(),
      Self::ToU64 => "to_U64".to_owned(),
      Self::ToNat => "to_Nat".to_owned(),
      Self::ToI8 => "to_I8".to_owned(),
      Self::ToI32 => "to_I32".to_owned(),
      Self::ToI64 => "to_I64".to_owned(),
      Self::ToInt => "to_Int".to_owned(),
      Self::ToBits => "to_Bits".to_owned(),
      Self::ToBytes => "to_Bytes".to_owned(),
    }
  }

  pub fn from_symbol(x: &str) -> Option<Self> {
    match x {
      "abs" => Some(Self::Abs),
      "sgn" => Some(Self::Sgn),
      "max" => Some(Self::Max),
      "min" => Some(Self::Min),
      "eql" => Some(Self::Eql),
      "lte" => Some(Self::Lte),
      "lth" => Some(Self::Lth),
      "gth" => Some(Self::Gth),
      "gte" => Some(Self::Gte),
      "not" => Some(Self::Not),
      "and" => Some(Self::And),
      "or" => Some(Self::Or),
      "xor" => Some(Self::Xor),
      "add" => Some(Self::Add),
      "sub" => Some(Self::Sub),
      "mul" => Some(Self::Mul),
      "div" => Some(Self::Div),
      "mod" => Some(Self::Mod),
      "pow" => Some(Self::Pow),
      "shl" => Some(Self::Shl),
      "shr" => Some(Self::Shr),
      "rol" => Some(Self::Rol),
      "ror" => Some(Self::Ror),
      "count_zeros" => Some(Self::CountZeros),
      "count_ones" => Some(Self::CountOnes),
      "to_U8" => Some(Self::ToU8),
      "to_U16" => Some(Self::ToU16),
      "to_U32" => Some(Self::ToU32),
      "to_U64" => Some(Self::ToU64),
      "to_Nat" => Some(Self::ToNat),
      "to_I8" => Some(Self::ToI8),
      "to_I32" => Some(Self::ToI32),
      "to_I64" => Some(Self::ToI64),
      "to_Int" => Some(Self::ToInt),
      "to_Bits" => Some(Self::ToBits),
      "to_Bytes" => Some(Self::ToBytes),
      _ => None,
    }
  }

  pub fn to_ipld(self) -> Ipld {
    match self {
      Self::Abs => Ipld::Integer(0),
      Self::Sgn => Ipld::Integer(1),
      Self::Max => Ipld::Integer(2),
      Self::Min => Ipld::Integer(3),
      Self::Eql => Ipld::Integer(4),
      Self::Lte => Ipld::Integer(5),
      Self::Lth => Ipld::Integer(6),
      Self::Gth => Ipld::Integer(7),
      Self::Gte => Ipld::Integer(8),
      Self::Not => Ipld::Integer(9),
      Self::And => Ipld::Integer(10),
      Self::Or => Ipld::Integer(11),
      Self::Xor => Ipld::Integer(12),
      Self::Add => Ipld::Integer(13),
      Self::Sub => Ipld::Integer(14),
      Self::Mul => Ipld::Integer(15),
      Self::Div => Ipld::Integer(16),
      Self::Mod => Ipld::Integer(17),
      Self::Pow => Ipld::Integer(18),
      Self::Shl => Ipld::Integer(19),
      Self::Shr => Ipld::Integer(20),
      Self::Rol => Ipld::Integer(21),
      Self::Ror => Ipld::Integer(22),
      Self::CountZeros => Ipld::Integer(23),
      Self::CountOnes => Ipld::Integer(24),
      Self::ToU8 => Ipld::Integer(25),
      Self::ToU16 => Ipld::Integer(26),
      Self::ToU32 => Ipld::Integer(27),
      Self::ToU64 => Ipld::Integer(28),
      // Self::ToU128 => Ipld::Integer(29),
      Self::ToNat => Ipld::Integer(30),
      Self::ToI8 => Ipld::Integer(31),
      Self::ToI32 => Ipld::Integer(32),
      Self::ToI64 => Ipld::Integer(33),
      // Self::ToI128 => Ipld::Integer(34),
      Self::ToInt => Ipld::Integer(35),
      Self::ToBits => Ipld::Integer(36),
      Self::ToBytes => Ipld::Integer(37),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Integer(0) => Ok(Self::Abs),
      Ipld::Integer(1) => Ok(Self::Sgn),
      Ipld::Integer(2) => Ok(Self::Max),
      Ipld::Integer(3) => Ok(Self::Min),
      Ipld::Integer(4) => Ok(Self::Eql),
      Ipld::Integer(5) => Ok(Self::Lte),
      Ipld::Integer(6) => Ok(Self::Lth),
      Ipld::Integer(7) => Ok(Self::Gth),
      Ipld::Integer(8) => Ok(Self::Gte),
      Ipld::Integer(9) => Ok(Self::Not),
      Ipld::Integer(10) => Ok(Self::And),
      Ipld::Integer(11) => Ok(Self::Or),
      Ipld::Integer(12) => Ok(Self::Xor),
      Ipld::Integer(13) => Ok(Self::Add),
      Ipld::Integer(14) => Ok(Self::Sub),
      Ipld::Integer(15) => Ok(Self::Mul),
      Ipld::Integer(16) => Ok(Self::Div),
      Ipld::Integer(17) => Ok(Self::Mod),
      Ipld::Integer(18) => Ok(Self::Pow),
      Ipld::Integer(19) => Ok(Self::Shl),
      Ipld::Integer(20) => Ok(Self::Shr),
      Ipld::Integer(21) => Ok(Self::Rol),
      Ipld::Integer(22) => Ok(Self::Ror),
      Ipld::Integer(23) => Ok(Self::CountZeros),
      Ipld::Integer(24) => Ok(Self::CountOnes),
      Ipld::Integer(25) => Ok(Self::ToU8),
      Ipld::Integer(26) => Ok(Self::ToU16),
      Ipld::Integer(27) => Ok(Self::ToU32),
      Ipld::Integer(28) => Ok(Self::ToU64),
      // Ipld::Integer(29) => Ok(Self::ToU128),
      Ipld::Integer(30) => Ok(Self::ToNat),
      Ipld::Integer(31) => Ok(Self::ToI8),
      Ipld::Integer(32) => Ok(Self::ToI32),
      Ipld::Integer(33) => Ok(Self::ToI64),
      // Ipld::Integer(34) => Ok(Self::ToI128),
      Ipld::Integer(35) => Ok(Self::ToInt),
      Ipld::Integer(36) => Ok(Self::ToBits),
      Ipld::Integer(37) => Ok(Self::ToBytes),
      xs => Err(IpldError::NatOp(xs.to_owned())),
    }
  }

  pub fn arity(self) -> u64 {
    match self {
      Self::Abs => 1,
      Self::Sgn => 1,
      Self::Max => 0,
      Self::Min => 0,
      Self::Eql => 2,
      Self::Lte => 2,
      Self::Lth => 2,
      Self::Gth => 2,
      Self::Gte => 2,
      Self::Not => 1,
      Self::And => 2,
      Self::Or => 2,
      Self::Xor => 2,
      Self::Add => 2,
      Self::Sub => 2,
      Self::Mul => 2,
      Self::Div => 2,
      Self::Mod => 2,
      Self::Pow => 2,
      Self::Shl => 2,
      Self::Shr => 2,
      Self::Rol => 2,
      Self::Ror => 2,
      Self::CountZeros => 1,
      Self::CountOnes => 1,
      Self::ToU8 => 1,
      Self::ToU16 => 1,
      Self::ToU32 => 1,
      Self::ToU64 => 1,
      Self::ToNat => 1,
      Self::ToI8 => 1,
      Self::ToI32 => 1,
      Self::ToI64 => 1,
      Self::ToInt => 1,
      Self::ToBits => 1,
      Self::ToBytes => 1,
    }
  }

  pub fn apply0(self) -> Option<Literal> {
    use Literal::*;
    match self {
      Self::Max => Some(I16(i16::MAX)),
      Self::Min => Some(I16(i16::MIN)),
      _ => None,
    }
  }

  pub fn apply1(self, x: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x) {
      (Self::Abs, I16(x)) => Some(U16(x.unsigned_abs())),
      (Self::Sgn, I16(x)) => Some(Bool(x.is_positive())),
      (Self::CountZeros, I16(x)) => Some(U32(x.count_zeros())),
      (Self::CountOnes, I16(x)) => Some(U32(x.count_ones())),
      (Self::ToU8, I16(x)) => u8::try_from(*x).ok().map(U8),
      (Self::ToU16, I16(x)) => u16::try_from(*x).ok().map(U16),
      (Self::ToU32, I16(x)) => u32::try_from(*x).ok().map(U32),
      (Self::ToU64, I16(x)) => u64::try_from(*x).ok().map(U64),
      (Self::ToNat, I16(x)) => {
        if x.is_negative() {
          None
        }
        else {
          Some(Nat(BigUint::from(u64::try_from(*x).unwrap())))
        }
      }
      (Self::ToI8, I16(x)) => i8::try_from(*x).ok().map(I8),
      (Self::ToI32, I16(x)) => Some(I32((*x).into())),
      (Self::ToI64, I16(x)) => Some(I64((*x).into())),
      (Self::Not, I16(x)) => Some(I16(!x)),
      (Self::ToInt, I16(x)) => Some(Int((*x).into())),
      (Self::ToBits, I16(x)) => {
        Some(Bits(bits::bytes_to_bits(16, &x.to_be_bytes().into())))
      }
      (Self::ToBytes, I16(x)) => Some(Bytes(x.to_be_bytes().into())),
      _ => None,
    }
  }

  pub fn apply2(self, x: &Literal, y: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x, y) {
      (Self::Eql, I16(x), I16(y)) => Some(Bool(x == y)),
      (Self::Lte, I16(x), I16(y)) => Some(Bool(x <= y)),
      (Self::Lth, I16(x), I16(y)) => Some(Bool(x < y)),
      (Self::Gth, I16(x), I16(y)) => Some(Bool(x > y)),
      (Self::Gte, I16(x), I16(y)) => Some(Bool(x >= y)),
      (Self::And, I16(x), I16(y)) => Some(I16(x & y)),
      (Self::Or, I16(x), I16(y)) => Some(I16(x | y)),
      (Self::Xor, I16(x), I16(y)) => Some(I16(x ^ y)),
      (Self::Add, I16(x), I16(y)) => Some(I16(x.wrapping_add(*y))),
      (Self::Sub, I16(x), I16(y)) => Some(I16(x.wrapping_sub(*y))),
      (Self::Mul, I16(x), I16(y)) => Some(I16(x.wrapping_mul(*y))),
      (Self::Div, I16(x), I16(y)) => {
        if *y == 0 {
          None
        }
        else {
          Some(I16(x.wrapping_div(*y)))
        }
      }
      (Self::Mod, I16(x), I16(y)) => {
        if *y == 0 {
          None
        }
        else {
          Some(I16(x.wrapping_rem(*y)))
        }
      }
      (Self::Pow, I16(x), U32(y)) => Some(I16(x.wrapping_pow(*y))),
      (Self::Shl, U32(x), I16(y)) => Some(I16(y.wrapping_shl(*x))),
      (Self::Shr, U32(x), I16(y)) => Some(I16(y.wrapping_shr(*x))),
      (Self::Rol, U32(x), I16(y)) => Some(I16(y.rotate_left(*x))),
      (Self::Ror, U32(x), I16(y)) => Some(I16(y.rotate_right(*x))),
      _ => None,
    }
  }
}

impl fmt::Display for I16Op {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.symbol())
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use crate::prim::{
    I32Op,
    I64Op,
    I8Op,
    U16Op,
    U32Op,
    U64Op,
    U8Op,
  };
  use num_bigint::BigUint;
  use quickcheck::{
    Arbitrary,
    Gen,
    TestResult,
  };
  use rand::Rng;
  use sp_std::{
    convert::TryInto,
    mem,
  };
  use Literal::{
    Bits,
    Bool,
    Bytes,
    Int,
    Nat,
    I16,
    U16,
    U32,
  };
  impl Arbitrary for I16Op {
    fn arbitrary(_g: &mut Gen) -> Self {
      let mut rng = rand::thread_rng();
      let gen: u32 = rng.gen_range(0..=35);
      match gen {
        0 => Self::Abs,
        1 => Self::Sgn,
        2 => Self::Max,
        3 => Self::Min,
        4 => Self::Eql,
        5 => Self::Lte,
        6 => Self::Lth,
        7 => Self::Gth,
        8 => Self::Gte,
        9 => Self::Not,
        10 => Self::And,
        11 => Self::Or,
        12 => Self::Xor,
        13 => Self::Add,
        14 => Self::Sub,
        15 => Self::Mul,
        16 => Self::Div,
        17 => Self::Mod,
        18 => Self::Pow,
        19 => Self::Shl,
        20 => Self::Shr,
        21 => Self::Rol,
        22 => Self::Ror,
        23 => Self::CountZeros,
        24 => Self::CountOnes,
        25 => Self::ToU8,
        26 => Self::ToU16,
        27 => Self::ToU32,
        28 => Self::ToU64,
        29 => Self::ToNat,
        30 => Self::ToI8,
        31 => Self::ToI32,
        32 => Self::ToI64,
        33 => Self::ToInt,
        34 => Self::ToBits,
        _ => Self::ToBytes,
      }
    }
  }

  #[quickcheck]
  fn i16_op_ipld(x: I16Op) -> bool {
    match I16Op::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }

  #[quickcheck]
  fn test_apply(op: I16Op, a: i16, b: i16, c: u32) -> TestResult {
    let apply0_go = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(I16Op::apply0(op) == expected)
    };

    let apply1_i16 = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(I16Op::apply1(op, &I16(a)) == expected)
    };

    let apply2_i16_i16 = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(I16Op::apply2(op, &I16(a), &I16(b)) == expected)
    };

    let apply2_i16_u32 = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(I16Op::apply2(op, &I16(a), &U32(c)) == expected)
    };

    let apply2_u32_i16 = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(I16Op::apply2(op, &U32(c), &I16(a)) == expected)
    };

    let from_bool = TestResult::from_bool;

    match op {
      I16Op::Abs => apply1_i16(Some(U16(a.unsigned_abs()))),
      I16Op::Sgn => apply1_i16(Some(Bool(a.is_positive()))),
      I16Op::Max => apply0_go(Some(I16(i16::MAX))),
      I16Op::Min => apply0_go(Some(I16(i16::MIN))),
      I16Op::Eql => apply2_i16_i16(Some(Bool(a == b))),
      I16Op::Lte => apply2_i16_i16(Some(Bool(a <= b))),
      I16Op::Lth => apply2_i16_i16(Some(Bool(a < b))),
      I16Op::Gth => apply2_i16_i16(Some(Bool(a > b))),
      I16Op::Gte => apply2_i16_i16(Some(Bool(a >= b))),
      I16Op::Not => apply1_i16(Some(I16(!a))),
      I16Op::And => apply2_i16_i16(Some(I16(a & b))),
      I16Op::Or => apply2_i16_i16(Some(I16(a | b))),
      I16Op::Xor => apply2_i16_i16(Some(I16(a ^ b))),
      I16Op::Add => apply2_i16_i16(Some(I16(a.wrapping_add(b)))),
      I16Op::Sub => apply2_i16_i16(Some(I16(a.wrapping_sub(b)))),
      I16Op::Mul => apply2_i16_i16(Some(I16(a.wrapping_mul(b)))),
      I16Op::Div => {
        apply2_i16_i16(if b == 0 { None } else { Some(I16(a.wrapping_div(b))) })
      }
      I16Op::Mod => {
        apply2_i16_i16(if b == 0 { None } else { Some(I16(a.wrapping_rem(b))) })
      }
      I16Op::Pow => apply2_i16_u32(Some(I16(a.wrapping_pow(c)))),
      I16Op::Shl => apply2_u32_i16(Some(I16(a.wrapping_shl(c)))),
      I16Op::Shr => apply2_u32_i16(Some(I16(a.wrapping_shr(c)))),
      I16Op::Rol => apply2_u32_i16(Some(I16(a.rotate_left(c)))),
      I16Op::Ror => apply2_u32_i16(Some(I16(a.rotate_right(c)))),
      I16Op::CountZeros => apply1_i16(Some(U32(a.count_zeros()))),
      I16Op::CountOnes => apply1_i16(Some(U32(a.count_ones()))),
      I16Op::ToU8 => from_bool(if a < u8::MIN.into() || a > u8::MAX.into() {
        I16Op::apply1(op, &I16(a)) == None
      }
      else {
        U8Op::apply1(U8Op::ToI16, &I16Op::apply1(op, &I16(a)).unwrap())
          == Some(I16(a))
      }),
      I16Op::ToU16 => from_bool(if a < u16::MIN.try_into().unwrap() {
        I16Op::apply1(op, &I16(a)) == None
      }
      else {
        U16Op::apply1(U16Op::ToI16, &I16Op::apply1(op, &I16(a)).unwrap())
          == Some(I16(a))
      }),
      I16Op::ToU32 => from_bool(if a < u32::MIN.try_into().unwrap() {
        I16Op::apply1(op, &I16(a)) == None
      }
      else {
        U32Op::apply1(U32Op::ToI16, &I16Op::apply1(op, &I16(a)).unwrap())
          == Some(I16(a))
      }),
      I16Op::ToU64 => from_bool(if a < u64::MIN.try_into().unwrap() {
        I16Op::apply1(op, &I16(a)) == None
      }
      else {
        U64Op::apply1(U64Op::ToI16, &I16Op::apply1(op, &I16(a)).unwrap())
          == Some(I16(a))
      }),
      I16Op::ToNat => {
        if a.is_negative() {
          apply1_i16(None)
        }
        else {
          apply1_i16(Some(Nat(BigUint::from(u64::try_from(a).unwrap()))))
        }
      }
      I16Op::ToI8 => from_bool(if a < i8::MIN.into() || a > i8::MAX.into() {
        I16Op::apply1(op, &I16(a)) == None
      }
      else {
        I8Op::apply1(I8Op::ToI16, &I16Op::apply1(op, &I16(a)).unwrap())
          == Some(I16(a))
      }),
      I16Op::ToI32 => from_bool(
        I32Op::apply1(I32Op::ToI16, &I16Op::apply1(op, &I16(a)).unwrap())
          == Some(I16(a)),
      ),
      I16Op::ToI64 => from_bool(
        I64Op::apply1(I64Op::ToI16, &I16Op::apply1(op, &I16(a)).unwrap())
          == Some(I16(a)),
      ),
      I16Op::ToInt => apply1_i16(Some(Int(a.into()))),
      I16Op::ToBits => {
        apply1_i16(Some(Bits(bits::bytes_to_bits(16, &a.to_be_bytes().into()))))
      }
      I16Op::ToBytes => apply1_i16(Some(Bytes(a.to_be_bytes().into()))),
    }
  }

  #[quickcheck]
  fn test_apply_none_on_invalid(
    op: I16Op,
    a: Literal,
    b: i16,
    c: u32,
    test_arg_2: bool,
  ) -> TestResult {
    let test_apply1_none_on_invalid = |valid_arg: Literal| -> TestResult {
      if mem::discriminant(&valid_arg) == mem::discriminant(&a) {
        TestResult::discard()
      }
      else {
        TestResult::from_bool(I16Op::apply1(op, &a) == None)
      }
    };

    let test_apply2_none_on_invalid =
      |valid_arg: Literal, a_: Literal, b_: Literal| -> TestResult {
        let go = || TestResult::from_bool(I16Op::apply2(op, &a_, &b_) == None);
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
      // Arity 0.
      I16Op::Max | I16Op::Min => TestResult::discard(),
      // Arity 1, valid is I16.
      I16Op::Abs
      | I16Op::Sgn
      | I16Op::Not
      | I16Op::CountZeros
      | I16Op::CountOnes
      | I16Op::ToU8
      | I16Op::ToU16
      | I16Op::ToU32
      | I16Op::ToU64
      | I16Op::ToNat
      | I16Op::ToI8
      | I16Op::ToI32
      | I16Op::ToI64
      | I16Op::ToInt
      | I16Op::ToBytes
      | I16Op::ToBits => test_apply1_none_on_invalid(I16(b)),
      // Arity 2, valid are I16 on a and b.
      I16Op::Eql
      | I16Op::Lte
      | I16Op::Lth
      | I16Op::Gth
      | I16Op::Gte
      | I16Op::And
      | I16Op::Or
      | I16Op::Xor
      | I16Op::Add
      | I16Op::Sub
      | I16Op::Mul
      | I16Op::Div
      | I16Op::Mod => {
        if test_arg_2 {
          test_apply2_none_on_invalid(I16(b), a, I16(b))
        }
        else {
          test_apply2_none_on_invalid(I16(b), I16(b), a)
        }
      }
      // Arity 2, valid are I16 on a and U32 on b.
      I16Op::Pow => {
        if test_arg_2 {
          test_apply2_none_on_invalid(I16(b), a, U32(c))
        }
        else {
          test_apply2_none_on_invalid(U32(c), I16(b), a)
        }
      }
      // Arity 2, valid are U32 on a and I16 on b.
      I16Op::Shl | I16Op::Shr | I16Op::Rol | I16Op::Ror => {
        if test_arg_2 {
          test_apply2_none_on_invalid(U32(c), a, I16(b))
        }
        else {
          test_apply2_none_on_invalid(I16(b), U32(c), a)
        }
      }
    }
  }
}
