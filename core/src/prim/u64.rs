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
pub enum U64Op {
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
  ToU128,
  ToNat,
  ToI8,
  ToI16,
  ToI32,
  ToI64,
  ToI128,
  ToInt,
  ToBits,
  ToBytes,
}

impl U64Op {
  pub fn symbol(self) -> String {
    match self {
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
      Self::ToU128 => "to_U128".to_owned(),
      Self::ToNat => "to_Nat".to_owned(),
      Self::ToI8 => "to_I8".to_owned(),
      Self::ToI16 => "to_I16".to_owned(),
      Self::ToI32 => "to_I32".to_owned(),
      Self::ToI64 => "to_I64".to_owned(),
      Self::ToI128 => "to_I128".to_owned(),
      Self::ToInt => "to_Int".to_owned(),
      Self::ToBits => "to_Bits".to_owned(),
      Self::ToBytes => "to_Bytes".to_owned(),
    }
  }

  pub fn from_symbol(x: &str) -> Option<Self> {
    match x {
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
      "to_U128" => Some(Self::ToU128),
      "to_Nat" => Some(Self::ToNat),
      "to_I8" => Some(Self::ToI8),
      "to_I16" => Some(Self::ToI16),
      "to_I32" => Some(Self::ToI32),
      "to_I64" => Some(Self::ToI64),
      "to_I128" => Some(Self::ToI128),
      "to_Int" => Some(Self::ToInt),
      "to_Bits" => Some(Self::ToBits),
      "to_Bytes" => Some(Self::ToBytes),
      _ => None,
    }
  }

  pub fn to_ipld(self) -> Ipld {
    match self {
      Self::Max => Ipld::Integer(0),
      Self::Min => Ipld::Integer(1),
      Self::Eql => Ipld::Integer(2),
      Self::Lte => Ipld::Integer(3),
      Self::Lth => Ipld::Integer(4),
      Self::Gth => Ipld::Integer(5),
      Self::Gte => Ipld::Integer(6),
      Self::Not => Ipld::Integer(7),
      Self::And => Ipld::Integer(8),
      Self::Or => Ipld::Integer(9),
      Self::Xor => Ipld::Integer(10),
      Self::Add => Ipld::Integer(11),
      Self::Sub => Ipld::Integer(12),
      Self::Mul => Ipld::Integer(13),
      Self::Div => Ipld::Integer(14),
      Self::Mod => Ipld::Integer(15),
      Self::Pow => Ipld::Integer(16),
      Self::Shl => Ipld::Integer(17),
      Self::Shr => Ipld::Integer(18),
      Self::Rol => Ipld::Integer(19),
      Self::Ror => Ipld::Integer(20),
      Self::CountZeros => Ipld::Integer(21),
      Self::CountOnes => Ipld::Integer(22),
      Self::ToU8 => Ipld::Integer(23),
      Self::ToU16 => Ipld::Integer(24),
      Self::ToU32 => Ipld::Integer(25),
      Self::ToU128 => Ipld::Integer(26),
      Self::ToNat => Ipld::Integer(27),
      Self::ToI8 => Ipld::Integer(28),
      Self::ToI16 => Ipld::Integer(29),
      Self::ToI32 => Ipld::Integer(30),
      Self::ToI64 => Ipld::Integer(31),
      Self::ToI128 => Ipld::Integer(32),
      Self::ToInt => Ipld::Integer(33),
      Self::ToBits => Ipld::Integer(34),
      Self::ToBytes => Ipld::Integer(35),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Integer(0) => Ok(Self::Max),
      Ipld::Integer(1) => Ok(Self::Min),
      Ipld::Integer(2) => Ok(Self::Eql),
      Ipld::Integer(3) => Ok(Self::Lte),
      Ipld::Integer(4) => Ok(Self::Lth),
      Ipld::Integer(5) => Ok(Self::Gth),
      Ipld::Integer(6) => Ok(Self::Gte),
      Ipld::Integer(7) => Ok(Self::Not),
      Ipld::Integer(8) => Ok(Self::And),
      Ipld::Integer(9) => Ok(Self::Or),
      Ipld::Integer(10) => Ok(Self::Xor),
      Ipld::Integer(11) => Ok(Self::Add),
      Ipld::Integer(12) => Ok(Self::Sub),
      Ipld::Integer(13) => Ok(Self::Mul),
      Ipld::Integer(14) => Ok(Self::Div),
      Ipld::Integer(15) => Ok(Self::Mod),
      Ipld::Integer(16) => Ok(Self::Pow),
      Ipld::Integer(17) => Ok(Self::Shl),
      Ipld::Integer(18) => Ok(Self::Shr),
      Ipld::Integer(19) => Ok(Self::Rol),
      Ipld::Integer(20) => Ok(Self::Ror),
      Ipld::Integer(21) => Ok(Self::CountZeros),
      Ipld::Integer(22) => Ok(Self::CountOnes),
      Ipld::Integer(23) => Ok(Self::ToU8),
      Ipld::Integer(24) => Ok(Self::ToU16),
      Ipld::Integer(25) => Ok(Self::ToU32),
      Ipld::Integer(26) => Ok(Self::ToU128),
      Ipld::Integer(27) => Ok(Self::ToNat),
      Ipld::Integer(28) => Ok(Self::ToI8),
      Ipld::Integer(29) => Ok(Self::ToI16),
      Ipld::Integer(30) => Ok(Self::ToI32),
      Ipld::Integer(31) => Ok(Self::ToI64),
      Ipld::Integer(32) => Ok(Self::ToI128),
      Ipld::Integer(33) => Ok(Self::ToInt),
      Ipld::Integer(34) => Ok(Self::ToBits),
      Ipld::Integer(35) => Ok(Self::ToBytes),
      xs => Err(IpldError::NatOp(xs.to_owned())),
    }
  }

  pub fn arity(self) -> u64 {
    match self {
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
      Self::ToU128 => 1,
      Self::ToNat => 1,
      Self::ToI8 => 1,
      Self::ToI16 => 1,
      Self::ToI32 => 1,
      Self::ToI64 => 1,
      Self::ToI128 => 1,
      Self::ToInt => 1,
      Self::ToBits => 1,
      Self::ToBytes => 1,
    }
  }

  pub fn apply0(self) -> Option<Literal> {
    use Literal::*;
    match self {
      Self::Max => Some(U64(u64::MAX)),
      Self::Min => Some(U64(u64::MIN)),
      _ => None,
    }
  }

  pub fn apply1(self, x: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x) {
      (Self::CountZeros, U64(x)) => Some(U32(x.count_zeros())),
      (Self::CountOnes, U64(x)) => Some(U32(x.count_ones())),
      (Self::ToU8, U64(x)) => u8::try_from(*x).ok().map(U8),
      (Self::ToU16, U64(x)) => u16::try_from(*x).ok().map(U16),
      (Self::ToU32, U64(x)) => u32::try_from(*x).ok().map(U32),
      (Self::ToU128, U64(x)) => Some(U128((*x).into())),
      (Self::ToNat, U64(x)) => Some(Nat(BigUint::from(*x))),
      (Self::ToI8, U64(x)) => i8::try_from(*x).ok().map(I8),
      (Self::ToI16, U64(x)) => i16::try_from(*x).ok().map(I16),
      (Self::ToI32, U64(x)) => i32::try_from(*x).ok().map(I32),
      (Self::ToI64, U64(x)) => i64::try_from(*x).ok().map(I64),
      (Self::ToI128, U64(x)) => Some(I128((*x).into())),
      (Self::Not, U64(x)) => Some(U64(!x)),
      (Self::ToInt, U64(x)) => Some(Int((*x).into())),
      (Self::ToBits, U64(x)) => {
        Some(Bits(bits::bytes_to_bits(64, &x.to_be_bytes().into())))
      }
      (Self::ToBytes, U64(x)) => Some(Bytes(x.to_be_bytes().into())),
      _ => None,
    }
  }

  pub fn apply2(self, x: &Literal, y: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x, y) {
      (Self::Eql, U64(x), U64(y)) => Some(Bool(x == y)),
      (Self::Lte, U64(x), U64(y)) => Some(Bool(x <= y)),
      (Self::Lth, U64(x), U64(y)) => Some(Bool(x < y)),
      (Self::Gth, U64(x), U64(y)) => Some(Bool(x > y)),
      (Self::Gte, U64(x), U64(y)) => Some(Bool(x >= y)),
      (Self::And, U64(x), U64(y)) => Some(U64(x & y)),
      (Self::Or, U64(x), U64(y)) => Some(U64(x | y)),
      (Self::Xor, U64(x), U64(y)) => Some(U64(x ^ y)),
      (Self::Add, U64(x), U64(y)) => Some(U64(x.wrapping_add(*y))),
      (Self::Sub, U64(x), U64(y)) => Some(U64(x.wrapping_sub(*y))),
      (Self::Mul, U64(x), U64(y)) => Some(U64(x.wrapping_mul(*y))),
      (Self::Div, U64(x), U64(y)) => {
        if *y == 0 {
          None
        }
        else {
          Some(U64(x.wrapping_div(*y)))
        }
      }
      (Self::Mod, U64(x), U64(y)) => {
        if *y == 0 {
          None
        }
        else {
          Some(U64(x.wrapping_rem(*y)))
        }
      }
      (Self::Pow, U64(x), U32(y)) => Some(U64(x.wrapping_pow(*y))),
      (Self::Shl, U32(x), U64(y)) => Some(U64(y.wrapping_shl(*x))),
      (Self::Shr, U32(x), U64(y)) => Some(U64(y.wrapping_shr(*x))),
      (Self::Rol, U32(x), U64(y)) => Some(U64(y.rotate_left(*x))),
      (Self::Ror, U32(x), U64(y)) => Some(U64(y.rotate_right(*x))),
      _ => None,
    }
  }
}

impl fmt::Display for U64Op {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.symbol())
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use crate::prim::{
    I16Op,
    I32Op,
    I64Op,
    I8Op,
    U16Op,
    U32Op,
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
    U32,
    U64,
  };
  impl Arbitrary for U64Op {
    fn arbitrary(_g: &mut Gen) -> Self {
      let mut rng = rand::thread_rng();
      let gen: u32 = rng.gen_range(0..=33);
      match gen {
        0 => Self::Max,
        1 => Self::Min,
        2 => Self::Eql,
        3 => Self::Lte,
        4 => Self::Lth,
        5 => Self::Gth,
        6 => Self::Gte,
        7 => Self::Not,
        8 => Self::And,
        9 => Self::Or,
        10 => Self::Xor,
        11 => Self::Add,
        12 => Self::Sub,
        13 => Self::Mul,
        14 => Self::Div,
        15 => Self::Mod,
        16 => Self::Pow,
        17 => Self::Shl,
        18 => Self::Shr,
        19 => Self::Rol,
        20 => Self::Ror,
        21 => Self::CountZeros,
        22 => Self::CountOnes,
        23 => Self::ToU8,
        24 => Self::ToU16,
        25 => Self::ToU32,
        26 => Self::ToNat,
        27 => Self::ToI8,
        28 => Self::ToI16,
        29 => Self::ToI32,
        30 => Self::ToI64,
        31 => Self::ToInt,
        32 => Self::ToBytes,
        _ => Self::ToBits,
        /* 26 => Self::ToU128,
         * 32 => Self::ToI128, */
      }
    }
  }

  #[quickcheck]
  fn u64_op_ipld(x: U64Op) -> bool {
    match U64Op::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }

  #[quickcheck]
  fn test_apply(op: U64Op, a: u64, b: u64, c: u32) -> TestResult {
    let apply0_go = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(U64Op::apply0(op) == expected)
    };

    let apply1_u64 = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(U64Op::apply1(op, &U64(a)) == expected)
    };

    let apply2_u64_u64 = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(U64Op::apply2(op, &U64(a), &U64(b)) == expected)
    };

    let apply2_u64_u32 = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(U64Op::apply2(op, &U64(a), &U32(c)) == expected)
    };

    let apply2_u32_u64 = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(U64Op::apply2(op, &U32(c), &U64(a)) == expected)
    };

    let from_bool = TestResult::from_bool;

    match op {
      U64Op::Max => apply0_go(Some(U64(u64::MAX))),
      U64Op::Min => apply0_go(Some(U64(u64::MIN))),
      U64Op::Eql => apply2_u64_u64(Some(Bool(a == b))),
      U64Op::Lte => apply2_u64_u64(Some(Bool(a <= b))),
      U64Op::Lth => apply2_u64_u64(Some(Bool(a < b))),
      U64Op::Gth => apply2_u64_u64(Some(Bool(a > b))),
      U64Op::Gte => apply2_u64_u64(Some(Bool(a >= b))),
      U64Op::Not => apply1_u64(Some(U64(!a))),
      U64Op::And => apply2_u64_u64(Some(U64(a & b))),
      U64Op::Or => apply2_u64_u64(Some(U64(a | b))),
      U64Op::Xor => apply2_u64_u64(Some(U64(a ^ b))),
      U64Op::Add => apply2_u64_u64(Some(U64(a.wrapping_add(b)))),
      U64Op::Sub => apply2_u64_u64(Some(U64(a.wrapping_sub(b)))),
      U64Op::Mul => apply2_u64_u64(Some(U64(a.wrapping_mul(b)))),
      U64Op::Div => {
        apply2_u64_u64(if b == 0 { None } else { Some(U64(a.wrapping_div(b))) })
      }
      U64Op::Mod => {
        apply2_u64_u64(if b == 0 { None } else { Some(U64(a.wrapping_rem(b))) })
      }
      U64Op::Pow => apply2_u64_u32(Some(U64(a.wrapping_pow(c)))),
      U64Op::Shl => apply2_u32_u64(Some(U64(a.wrapping_shl(c)))),
      U64Op::Shr => apply2_u32_u64(Some(U64(a.wrapping_shr(c)))),
      U64Op::Rol => apply2_u32_u64(Some(U64(a.rotate_left(c)))),
      U64Op::Ror => apply2_u32_u64(Some(U64(a.rotate_right(c)))),
      U64Op::CountZeros => apply1_u64(Some(U32(a.count_zeros()))),
      U64Op::CountOnes => apply1_u64(Some(U32(a.count_ones()))),
      U64Op::ToU8 => from_bool(if a > u8::MAX.into() {
        U64Op::apply1(op, &U64(a)) == None
      }
      else {
        U8Op::apply1(U8Op::ToU64, &U64Op::apply1(op, &U64(a)).unwrap())
          == Some(U64(a))
      }),
      U64Op::ToU16 => from_bool(if a > u16::MAX.into() {
        U64Op::apply1(op, &U64(a)) == None
      }
      else {
        U16Op::apply1(U16Op::ToU64, &U64Op::apply1(op, &U64(a)).unwrap())
          == Some(U64(a))
      }),
      U64Op::ToU32 => from_bool(if a > u32::MAX.into() {
        U64Op::apply1(op, &U64(a)) == None
      }
      else {
        U32Op::apply1(U32Op::ToU64, &U64Op::apply1(op, &U64(a)).unwrap())
          == Some(U64(a))
      }),
      U64Op::ToU128 => TestResult::discard(),
      U64Op::ToNat => apply1_u64(Some(Nat(BigUint::from(a)))),
      U64Op::ToI8 => from_bool(if a > i8::MAX.try_into().unwrap() {
        U64Op::apply1(op, &U64(a)) == None
      }
      else {
        I8Op::apply1(I8Op::ToU64, &U64Op::apply1(op, &U64(a)).unwrap())
          == Some(U64(a))
      }),
      U64Op::ToI16 => from_bool(if a > i16::MAX.try_into().unwrap() {
        U64Op::apply1(op, &U64(a)) == None
      }
      else {
        I16Op::apply1(I16Op::ToU64, &U64Op::apply1(op, &U64(a)).unwrap())
          == Some(U64(a))
      }),
      U64Op::ToI32 => from_bool(if a > i32::MAX.try_into().unwrap() {
        U64Op::apply1(op, &U64(a)) == None
      }
      else {
        I32Op::apply1(I32Op::ToU64, &U64Op::apply1(op, &U64(a)).unwrap())
          == Some(U64(a))
      }),
      U64Op::ToI64 => from_bool(if a > i64::MAX.try_into().unwrap() {
        U64Op::apply1(op, &U64(a)) == None
      }
      else {
        I64Op::apply1(I64Op::ToU64, &U64Op::apply1(op, &U64(a)).unwrap())
          == Some(U64(a))
      }),
      U64Op::ToI128 => TestResult::discard(),
      U64Op::ToInt => apply1_u64(Some(Int(a.into()))),
      U64Op::ToBits => {
        apply1_u64(Some(Bits(bits::bytes_to_bits(64, &a.to_be_bytes().into()))))
      }
      U64Op::ToBytes => apply1_u64(Some(Bytes(a.to_be_bytes().into()))),
    }
  }

  #[quickcheck]
  fn test_apply_none_on_invalid(
    op: U64Op,
    a: Literal,
    b: u64,
    c: u32,
    test_arg_2: bool,
  ) -> TestResult {
    let test_apply1_none_on_invalid = |valid_arg: Literal| -> TestResult {
      if mem::discriminant(&valid_arg) == mem::discriminant(&a) {
        TestResult::discard()
      }
      else {
        TestResult::from_bool(U64Op::apply1(op, &a) == None)
      }
    };

    let test_apply2_none_on_invalid =
      |valid_arg: Literal, a_: Literal, b_: Literal| -> TestResult {
        let go = || TestResult::from_bool(U64Op::apply2(op, &a_, &b_) == None);
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
      U64Op::Max | U64Op::Min => TestResult::discard(),
      // Arity 1, valid is U64.
      U64Op::Not
      | U64Op::CountZeros
      | U64Op::CountOnes
      | U64Op::ToU8
      | U64Op::ToU16
      | U64Op::ToU32
      | U64Op::ToU128
      | U64Op::ToNat
      | U64Op::ToI8
      | U64Op::ToI16
      | U64Op::ToI32
      | U64Op::ToI64
      | U64Op::ToI128
      | U64Op::ToInt
      | U64Op::ToBytes
      | U64Op::ToBits => test_apply1_none_on_invalid(U64(b)),
      // Arity 2, valid are U64 on a and b.
      U64Op::Eql
      | U64Op::Lte
      | U64Op::Lth
      | U64Op::Gth
      | U64Op::Gte
      | U64Op::And
      | U64Op::Or
      | U64Op::Xor
      | U64Op::Add
      | U64Op::Sub
      | U64Op::Mul
      | U64Op::Div
      | U64Op::Mod => {
        if test_arg_2 {
          test_apply2_none_on_invalid(U64(b), a, U64(b))
        }
        else {
          test_apply2_none_on_invalid(U64(b), U64(b), a)
        }
      }
      // Arity 2, valid are U64 on a and U32 on b.
      U64Op::Pow => {
        if test_arg_2 {
          test_apply2_none_on_invalid(U64(b), a, U32(c))
        }
        else {
          test_apply2_none_on_invalid(U32(c), U64(b), a)
        }
      }
      // Arity 2, valid are U32 on a and U64 on b.
      U64Op::Shl | U64Op::Shr | U64Op::Rol | U64Op::Ror => {
        if test_arg_2 {
          test_apply2_none_on_invalid(U32(c), a, U64(b))
        }
        else {
          test_apply2_none_on_invalid(U64(b), U32(c), a)
        }
      }
    }
  }
}
