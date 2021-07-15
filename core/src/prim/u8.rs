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
pub enum U8Op {
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
  ToU16,
  ToU32,
  ToU64,
  ToNat,
  ToI8,
  ToI16,
  ToI32,
  ToI64,
  ToInt,
  ToBits,
  ToBytes,
  ToChar,
}

impl U8Op {
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
      Self::ToU16 => "to_U16".to_owned(),
      Self::ToU32 => "to_U32".to_owned(),
      Self::ToU64 => "to_U64".to_owned(),
      Self::ToNat => "to_Nat".to_owned(),
      Self::ToI8 => "to_I8".to_owned(),
      Self::ToI16 => "to_I16".to_owned(),
      Self::ToI32 => "to_I32".to_owned(),
      Self::ToI64 => "to_I64".to_owned(),
      Self::ToInt => "to_Int".to_owned(),
      Self::ToBits => "to_Bits".to_owned(),
      Self::ToBytes => "to_Bytes".to_owned(),
      Self::ToChar => "to_Char".to_owned(),
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
      "to_U16" => Some(Self::ToU16),
      "to_U32" => Some(Self::ToU32),
      "to_U64" => Some(Self::ToU64),
      "to_Nat" => Some(Self::ToNat),
      "to_I8" => Some(Self::ToI8),
      "to_I16" => Some(Self::ToI16),
      "to_I32" => Some(Self::ToI32),
      "to_I64" => Some(Self::ToI64),
      "to_Int" => Some(Self::ToInt),
      "to_Bits" => Some(Self::ToBits),
      "to_Bytes" => Some(Self::ToBytes),
      "to_Char" => Some(Self::ToChar),
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
      Self::ToU16 => Ipld::Integer(23),
      Self::ToU32 => Ipld::Integer(24),
      Self::ToU64 => Ipld::Integer(25),
      // Self::ToU128 => Ipld::Integer(26),
      Self::ToNat => Ipld::Integer(27),
      Self::ToI8 => Ipld::Integer(28),
      Self::ToI16 => Ipld::Integer(29),
      Self::ToI32 => Ipld::Integer(30),
      Self::ToI64 => Ipld::Integer(31),
      // Self::ToI128 => Ipld::Integer(32),
      Self::ToInt => Ipld::Integer(33),
      Self::ToBits => Ipld::Integer(34),
      Self::ToBytes => Ipld::Integer(35),
      Self::ToChar => Ipld::Integer(36),
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
      Ipld::Integer(23) => Ok(Self::ToU16),
      Ipld::Integer(24) => Ok(Self::ToU32),
      Ipld::Integer(25) => Ok(Self::ToU64),
      // Ipld::Integer(26) => Ok(Self::ToU128),
      Ipld::Integer(27) => Ok(Self::ToNat),
      Ipld::Integer(28) => Ok(Self::ToI8),
      Ipld::Integer(29) => Ok(Self::ToI16),
      Ipld::Integer(30) => Ok(Self::ToI32),
      Ipld::Integer(31) => Ok(Self::ToI64),
      // Ipld::Integer(32) => Ok(Self::ToI128),
      Ipld::Integer(33) => Ok(Self::ToInt),
      Ipld::Integer(34) => Ok(Self::ToBits),
      Ipld::Integer(35) => Ok(Self::ToBytes),
      Ipld::Integer(36) => Ok(Self::ToChar),
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
      Self::ToChar => 1,
      Self::ToU16 => 1,
      Self::ToU32 => 1,
      Self::ToU64 => 1,
      Self::ToNat => 1,
      Self::ToI8 => 1,
      Self::ToI16 => 1,
      Self::ToI32 => 1,
      Self::ToI64 => 1,
      Self::ToInt => 1,
      Self::ToBytes => 1,
      Self::ToBits => 1,
    }
  }

  pub fn apply0(self) -> Option<Literal> {
    use Literal::*;
    match self {
      Self::Max => Some(U8(u8::MAX)),
      Self::Min => Some(U8(u8::MIN)),
      _ => None,
    }
  }

  pub fn apply1(self, x: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x) {
      (Self::CountZeros, U8(x)) => Some(U32(x.count_zeros())),
      (Self::CountOnes, U8(x)) => Some(U32(x.count_ones())),
      (Self::ToChar, U8(x)) => Some(Char((*x).into())),
      (Self::ToU16, U8(x)) => Some(U16((*x).into())),
      (Self::ToU32, U8(x)) => Some(U32((*x).into())),
      (Self::ToU64, U8(x)) => Some(U64((*x).into())),
      (Self::ToNat, U8(x)) => {
        Some(Nat(BigUint::from(u64::try_from(*x).unwrap())))
      }
      (Self::ToI8, U8(x)) => i8::try_from(*x).ok().map(I8),
      (Self::ToI16, U8(x)) => Some(I16((*x).into())),
      (Self::ToI32, U8(x)) => Some(I32((*x).into())),
      (Self::ToI64, U8(x)) => Some(I64((*x).into())),
      (Self::Not, U8(x)) => Some(U8(!x)),
      (Self::ToInt, U8(x)) => Some(Int((*x).into())),
      (Self::ToBits, U8(x)) => {
        Some(Bits(bits::bytes_to_bits(8, &x.to_be_bytes().into())))
      }
      (Self::ToBytes, U8(x)) => Some(Bytes(x.to_be_bytes().into())),
      _ => None,
    }
  }

  pub fn apply2(self, x: &Literal, y: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x, y) {
      (Self::Eql, U8(x), U8(y)) => Some(Bool(x == y)),
      (Self::Lte, U8(x), U8(y)) => Some(Bool(x <= y)),
      (Self::Lth, U8(x), U8(y)) => Some(Bool(x < y)),
      (Self::Gth, U8(x), U8(y)) => Some(Bool(x > y)),
      (Self::Gte, U8(x), U8(y)) => Some(Bool(x >= y)),
      (Self::And, U8(x), U8(y)) => Some(U8(x & y)),
      (Self::Or, U8(x), U8(y)) => Some(U8(x | y)),
      (Self::Xor, U8(x), U8(y)) => Some(U8(x ^ y)),
      (Self::Add, U8(x), U8(y)) => Some(U8(x.wrapping_add(*y))),
      (Self::Sub, U8(x), U8(y)) => Some(U8(x.wrapping_sub(*y))),
      (Self::Mul, U8(x), U8(y)) => Some(U8(x.wrapping_mul(*y))),
      (Self::Div, U8(x), U8(y)) => {
        if *y == 0 {
          None
        }
        else {
          Some(U8(x.wrapping_div(*y)))
        }
      }
      (Self::Mod, U8(x), U8(y)) => {
        if *y == 0 {
          None
        }
        else {
          Some(U8(x.wrapping_rem(*y)))
        }
      }
      (Self::Pow, U8(x), U32(y)) => Some(U8(x.wrapping_pow(*y))),
      (Self::Shl, U32(x), U8(y)) => Some(U8(y.wrapping_shl(*x))),
      (Self::Shr, U32(x), U8(y)) => Some(U8(y.wrapping_shr(*x))),
      (Self::Rol, U32(x), U8(y)) => Some(U8(y.rotate_left(*x))),
      (Self::Ror, U32(x), U8(y)) => Some(U8(y.rotate_right(*x))),
      _ => None,
    }
  }
}

impl fmt::Display for U8Op {
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
    U64Op,
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
    Char,
    Int,
    Nat,
    U32,
    U8,
  };
  impl Arbitrary for U8Op {
    fn arbitrary(_g: &mut Gen) -> Self {
      let mut rng = rand::thread_rng();
      let gen: u32 = rng.gen_range(0..=34);
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
        23 => Self::ToChar,
        24 => Self::ToU16,
        25 => Self::ToU32,
        26 => Self::ToU64,
        27 => Self::ToNat,
        28 => Self::ToI8,
        29 => Self::ToI16,
        30 => Self::ToI32,
        31 => Self::ToI64,
        32 => Self::ToInt,
        33 => Self::ToBytes,
        _ => Self::ToBits,
      }
    }
  }

  #[quickcheck]
  fn u8_op_ipld(x: U8Op) -> bool {
    match U8Op::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }

  #[quickcheck]
  fn test_apply(op: U8Op, a: u8, b: u8, c: u32) -> TestResult {
    let apply0_go = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(U8Op::apply0(op) == expected)
    };

    let apply1_u8 = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(U8Op::apply1(op, &U8(a)) == expected)
    };

    let apply2_u8_u8 = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(U8Op::apply2(op, &U8(a), &U8(b)) == expected)
    };

    let apply2_u8_u32 = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(U8Op::apply2(op, &U8(a), &U32(c)) == expected)
    };

    let apply2_u32_u8 = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(U8Op::apply2(op, &U32(c), &U8(a)) == expected)
    };

    let from_bool = TestResult::from_bool;

    match op {
      U8Op::Max => apply0_go(Some(U8(u8::MAX))),
      U8Op::Min => apply0_go(Some(U8(u8::MIN))),
      U8Op::Eql => apply2_u8_u8(Some(Bool(a == b))),
      U8Op::Lte => apply2_u8_u8(Some(Bool(a <= b))),
      U8Op::Lth => apply2_u8_u8(Some(Bool(a < b))),
      U8Op::Gth => apply2_u8_u8(Some(Bool(a > b))),
      U8Op::Gte => apply2_u8_u8(Some(Bool(a >= b))),
      U8Op::Not => apply1_u8(Some(U8(!a))),
      U8Op::And => apply2_u8_u8(Some(U8(a & b))),
      U8Op::Or => apply2_u8_u8(Some(U8(a | b))),
      U8Op::Xor => apply2_u8_u8(Some(U8(a ^ b))),
      U8Op::Add => apply2_u8_u8(Some(U8(a.wrapping_add(b)))),
      U8Op::Sub => apply2_u8_u8(Some(U8(a.wrapping_sub(b)))),
      U8Op::Mul => apply2_u8_u8(Some(U8(a.wrapping_mul(b)))),
      U8Op::Div => {
        apply2_u8_u8(if b == 0 { None } else { Some(U8(a.wrapping_div(b))) })
      }
      U8Op::Mod => {
        apply2_u8_u8(if b == 0 { None } else { Some(U8(a.wrapping_rem(b))) })
      }
      U8Op::Pow => apply2_u8_u32(Some(U8(a.wrapping_pow(c)))),
      U8Op::Shl => apply2_u32_u8(Some(U8(a.wrapping_shl(c)))),
      U8Op::Shr => apply2_u32_u8(Some(U8(a.wrapping_shr(c)))),
      U8Op::Rol => apply2_u32_u8(Some(U8(a.rotate_left(c)))),
      U8Op::Ror => apply2_u32_u8(Some(U8(a.rotate_right(c)))),
      U8Op::CountZeros => apply1_u8(Some(U32(a.count_zeros()))),
      U8Op::CountOnes => apply1_u8(Some(U32(a.count_ones()))),
      U8Op::ToU16 => from_bool(
        U16Op::apply1(U16Op::ToU8, &U8Op::apply1(op, &U8(a)).unwrap())
          == Some(U8(a)),
      ),
      U8Op::ToU32 => from_bool(
        U32Op::apply1(U32Op::ToU8, &U8Op::apply1(op, &U8(a)).unwrap())
          == Some(U8(a)),
      ),
      U8Op::ToU64 => from_bool(
        U64Op::apply1(U64Op::ToU8, &U8Op::apply1(op, &U8(a)).unwrap())
          == Some(U8(a)),
      ),
      U8Op::ToNat => {
        apply1_u8(Some(Nat(BigUint::from(u64::try_from(a).unwrap()))))
      }
      U8Op::ToI8 => from_bool(if a > i8::MAX.try_into().unwrap() {
        U8Op::apply1(op, &U8(a)) == None
      }
      else {
        I8Op::apply1(I8Op::ToU8, &U8Op::apply1(op, &U8(a)).unwrap())
          == Some(U8(a))
      }),
      U8Op::ToI16 => from_bool(
        I16Op::apply1(I16Op::ToU8, &U8Op::apply1(op, &U8(a)).unwrap())
          == Some(U8(a)),
      ),
      U8Op::ToI32 => from_bool(
        I32Op::apply1(I32Op::ToU8, &U8Op::apply1(op, &U8(a)).unwrap())
          == Some(U8(a)),
      ),
      U8Op::ToI64 => from_bool(
        I64Op::apply1(I64Op::ToU8, &U8Op::apply1(op, &U8(a)).unwrap())
          == Some(U8(a)),
      ),
      U8Op::ToInt => apply1_u8(Some(Int(a.into()))),
      U8Op::ToBits => {
        apply1_u8(Some(Bits(bits::bytes_to_bits(8, &a.to_be_bytes().into()))))
      }
      U8Op::ToBytes => apply1_u8(Some(Bytes(a.to_be_bytes().into()))),
      U8Op::ToChar => apply1_u8(Some(Char((a).into()))),
    }
  }

  #[quickcheck]
  fn test_apply_none_on_invalid(
    op: U8Op,
    a: Literal,
    b: u8,
    c: u32,
    test_arg_2: bool,
  ) -> TestResult {
    let test_apply1_none_on_invalid = |valid_arg: Literal| -> TestResult {
      if mem::discriminant(&valid_arg) == mem::discriminant(&a) {
        TestResult::discard()
      }
      else {
        TestResult::from_bool(U8Op::apply1(op, &a) == None)
      }
    };

    let test_apply2_none_on_invalid =
      |valid_arg: Literal, a_: Literal, b_: Literal| -> TestResult {
        let go = || TestResult::from_bool(U8Op::apply2(op, &a_, &b_) == None);
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
      U8Op::Max | U8Op::Min => TestResult::discard(),
      // Arity 1, valid is U8.
      U8Op::Not
      | U8Op::CountZeros
      | U8Op::CountOnes
      | U8Op::ToU16
      | U8Op::ToU32
      | U8Op::ToU64
      | U8Op::ToNat
      | U8Op::ToI8
      | U8Op::ToI16
      | U8Op::ToI32
      | U8Op::ToI64
      | U8Op::ToInt
      | U8Op::ToBytes
      | U8Op::ToBits
      | U8Op::ToChar => test_apply1_none_on_invalid(U8(b)),
      // Arity 2, valid are U8 on a and b.
      U8Op::Eql
      | U8Op::Lte
      | U8Op::Lth
      | U8Op::Gth
      | U8Op::Gte
      | U8Op::And
      | U8Op::Or
      | U8Op::Xor
      | U8Op::Add
      | U8Op::Sub
      | U8Op::Mul
      | U8Op::Div
      | U8Op::Mod => {
        if test_arg_2 {
          test_apply2_none_on_invalid(U8(b), a, U8(b))
        }
        else {
          test_apply2_none_on_invalid(U8(b), U8(b), a)
        }
      }
      // Arity 2, valid are U8 on a and U32 on b.
      U8Op::Pow => {
        if test_arg_2 {
          test_apply2_none_on_invalid(U8(b), a, U32(c))
        }
        else {
          test_apply2_none_on_invalid(U32(c), U8(b), a)
        }
      }
      // Arity 2, valid are U32 on a and U8 on b.
      U8Op::Shl | U8Op::Shr | U8Op::Rol | U8Op::Ror => {
        if test_arg_2 {
          test_apply2_none_on_invalid(U32(c), a, U8(b))
        }
        else {
          test_apply2_none_on_invalid(U8(b), U32(c), a)
        }
      }
    }
  }
}
