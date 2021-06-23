use sp_ipld::Ipld;

use sp_std::{
  convert::TryFrom,
  fmt,
  borrow::ToOwned,
};

use alloc::string::String;

use crate::{
  ipld_error::IpldError,
  literal::Literal,
  prim::bits,
  term::Term,
  yatima,
};

use num_bigint::BigUint;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum U32Op {
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
  ToU64,
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
  ToChar,
}

impl U32Op {
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
      Self::ToU64 => "to_U64".to_owned(),
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
      "to_U8" => Some(Self::ToU8),
      "to_U16" => Some(Self::ToU16),
      "to_U64" => Some(Self::ToU64),
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
      "to_Char" => Some(Self::ToChar),
      _ => None,
    }
  }

  pub fn type_of(self) -> Term {
    match self {
      Self::Max => yatima!("#U32"),
      Self::Min => yatima!("#U32"),
      Self::Eql => yatima!("∀ #U32 #U32 -> #Bool"),
      Self::Lte => yatima!("∀ #U32 #U32 -> #Bool"),
      Self::Lth => yatima!("∀ #U32 #U32 -> #Bool"),
      Self::Gth => yatima!("∀ #U32 #U32 -> #Bool"),
      Self::Gte => yatima!("∀ #U32 #U32 -> #Bool"),
      Self::Not => yatima!("∀ #U32 #U32 -> #Bool"),
      Self::And => yatima!("∀ #U32 #U32 -> #Bool"),
      Self::Or => yatima!("∀ #U32 #U32 -> #Bool"),
      Self::Xor => yatima!("∀ #U32 #U32 -> #Bool"),
      Self::Add => yatima!("∀ #U32 #U32 -> #U32"),
      Self::Sub => yatima!("∀ #U32 #U32 -> #U32"),
      Self::Mul => yatima!("∀ #U32 #U32 -> #U32"),
      Self::Div => yatima!("∀ #U32 #U32 -> #U32"),
      Self::Mod => yatima!("∀ #U32 #U32 -> #U32"),
      Self::Pow => yatima!("∀ #U32 #U32 -> #U32"),
      Self::Shl => yatima!("∀ #U32 #U32 -> #U32"),
      Self::Shr => yatima!("∀ #U32 #U32 -> #U32"),
      Self::Rol => yatima!("∀ #U32 #U32 -> #U32"),
      Self::Ror => yatima!("∀ #U32 #U32 -> #U32"),
      Self::CountZeros => yatima!("∀ #U32 -> #U32"),
      Self::CountOnes => yatima!("∀ #U32 -> #U32"),
      Self::ToU8 => yatima!("∀ #U32 -> #U8"),
      Self::ToU16 => yatima!("∀ #U32 -> #U16"),
      Self::ToU64 => yatima!("∀ #U32 -> #U64"),
      Self::ToU128 => yatima!("∀ #U32 -> #U128"),
      Self::ToNat => yatima!("∀ #U32 -> #Nat"),
      Self::ToI8 => yatima!("∀ #U32 -> #I8"),
      Self::ToI16 => yatima!("∀ #U32 -> #I16"),
      Self::ToI32 => yatima!("∀ #U32 -> #I32"),
      Self::ToI64 => yatima!("∀ #U32 -> #I64"),
      Self::ToI128 => yatima!("∀ #U32 -> #I128"),
      Self::ToInt => yatima!("∀ #U32 -> #Int"),
      Self::ToBits => yatima!("∀ #U32 -> #Bits"),
      Self::ToBytes => yatima!("∀ #U32 -> #Bytes"),
      Self::ToChar => yatima!("∀ #U32 -> #Char"),
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
      Self::ToU64 => Ipld::Integer(25),
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
      Ipld::Integer(23) => Ok(Self::ToU8),
      Ipld::Integer(24) => Ok(Self::ToU16),
      Ipld::Integer(25) => Ok(Self::ToU64),
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
      Self::ToU8 => 1,
      Self::ToU16 => 1,
      Self::ToU64 => 1,
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
      Self::ToChar => 1,
    }
  }

  pub fn apply0(self) -> Option<Literal> {
    use Literal::*;
    match self {
      Self::Max => Some(U32(u32::MAX)),
      Self::Min => Some(U32(u32::MIN)),
      _ => None,
    }
  }

  pub fn apply1(self, x: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x) {
      (Self::CountZeros, U32(x)) => Some(U32(x.count_zeros())),
      (Self::CountOnes, U32(x)) => Some(U32(x.count_ones())),
      (Self::ToU8, U32(x)) => u8::try_from(*x).ok().map(U8),
      (Self::ToU16, U32(x)) => u16::try_from(*x).ok().map(U16),
      (Self::ToU64, U32(x)) => Some(U64((*x).into())),
      (Self::ToU128, U32(x)) => Some(U128((*x).into())),
      (Self::ToNat, U32(x)) => Some(Nat(BigUint::from(u64::try_from(*x).unwrap()))),
      (Self::ToI8, U32(x)) => i8::try_from(*x).ok().map(I8),
      (Self::ToI16, U32(x)) => i16::try_from(*x).ok().map(I16),
      (Self::ToI32, U32(x)) => i32::try_from(*x).ok().map(I32),
      (Self::ToI64, U32(x)) => Some(I64((*x).into())),
      (Self::ToI128, U32(x)) => Some(I128((*x).into())),
      (Self::Not, U32(x)) => Some(U32(!x)),
      (Self::ToInt, U32(x)) => Some(Int((*x).into())),
      (Self::ToBits, U32(x)) => Some(Bits(bits::bytes_to_bits(32, &x.to_be_bytes().into()))),
      (Self::ToBytes, U32(x)) => Some(Bytes(x.to_be_bytes().into())),
      (Self::ToChar, U32(x)) => char::from_u32(*x).map(Char),
      _ => None,
    }
  }

  pub fn apply2(self, x: &Literal, y: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x, y) {
      (Self::Eql, U32(x), U32(y)) => Some(Bool(x == y)),
      (Self::Lte, U32(x), U32(y)) => Some(Bool(x <= y)),
      (Self::Lth, U32(x), U32(y)) => Some(Bool(x < y)),
      (Self::Gth, U32(x), U32(y)) => Some(Bool(x > y)),
      (Self::Gte, U32(x), U32(y)) => Some(Bool(x >= y)),
      (Self::And, U32(x), U32(y)) => Some(U32(x & y)),
      (Self::Or, U32(x), U32(y)) => Some(U32(x | y)),
      (Self::Xor, U32(x), U32(y)) => Some(U32(x ^ y)),
      (Self::Add, U32(x), U32(y)) => Some(U32(x.wrapping_add(*y))),
      (Self::Sub, U32(x), U32(y)) => Some(U32(x.wrapping_sub(*y))),
      (Self::Mul, U32(x), U32(y)) => Some(U32(x.wrapping_mul(*y))),
      (Self::Div, U32(x), U32(y)) => if *y == 0 {
        None
      } else {
        Some(U32(x.wrapping_div(*y)))
      },
      (Self::Mod, U32(x), U32(y)) => if *y == 0 {
        None
      } else {
        Some(U32(x.wrapping_rem(*y)))
      },
      (Self::Pow, U32(x), U32(y)) => Some(U32(x.wrapping_pow(*y))),
      (Self::Shl, U32(x), U32(y)) => Some(U32(y.wrapping_shl(*x))),
      (Self::Shr, U32(x), U32(y)) => Some(U32(y.wrapping_shr(*x))),
      (Self::Rol, U32(x), U32(y)) => Some(U32(y.rotate_left(*x))),
      (Self::Ror, U32(x), U32(y)) => Some(U32(y.rotate_right(*x))),
      _ => None,
    }
  }
}

impl fmt::Display for U32Op {
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
    U32,
    Bool,
    Nat,
    Int,
    Bits,
    Bytes,
    Char
  };
  use crate::prim::{
    U8Op,
    U16Op,
    U64Op,
    I8Op,
    I16Op,
    I32Op,
    I64Op,
  };
  use sp_std::{
    convert::TryInto,
    mem
  };
  use num_bigint::BigUint;
  impl Arbitrary for U32Op {
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
        24 => Self::ToU8,
        25 => Self::ToU16,
        26 => Self::ToU64,
        27 => Self::ToNat,
        28 => Self::ToI8,
        29 => Self::ToI16,
        30 => Self::ToI32,
        31 => Self::ToI64,
        32 => Self::ToInt,
        33 => Self::ToBytes,
        _ => Self::ToBits,
        // 27 => Self::ToU128,
        // 33 => Self::ToI128,
      }
    }
  }

  #[quickcheck]
  fn u32_op_ipld(x: U32Op) -> bool {
    match U32Op::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }

  #[quickcheck]
  fn test_apply(
    op: U32Op,
    a: u32,
    b: u32,
    c: u32
  ) -> TestResult {
    let apply0_go = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        U32Op::apply0(op) ==
        expected
      )
    };

    let apply1_u32 = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        U32Op::apply1(
          op,
          &U32(a)
        ) ==
        expected
      )
    };

    let apply2_u32_a_u32_b = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        U32Op::apply2(
          op,
          &U32(a),
          &U32(b)
        ) ==
        expected
      )
    };

    let apply2_u32_a_u32_c = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        U32Op::apply2(
          op,
          &U32(a),
          &U32(c)
        ) ==
        expected
      )
    };

    let apply2_u32_c_u32_a = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(
        U32Op::apply2(
          op,
          &U32(c),
          &U32(a)
        ) ==
        expected
      )
    };

    let from_bool = TestResult::from_bool;

    match op {
      U32Op::Max => apply0_go(Some(U32(u32::MAX))),
      U32Op::Min => apply0_go(Some(U32(u32::MIN))),
      U32Op::Eql => apply2_u32_a_u32_b(Some(Bool(a == b))),
      U32Op::Lte => apply2_u32_a_u32_b(Some(Bool(a <= b))),
      U32Op::Lth => apply2_u32_a_u32_b(Some(Bool(a < b))),
      U32Op::Gth => apply2_u32_a_u32_b(Some(Bool(a > b))),
      U32Op::Gte => apply2_u32_a_u32_b(Some(Bool(a >= b))),
      U32Op::Not => apply1_u32(Some(U32(!a))),
      U32Op::And => apply2_u32_a_u32_b(Some(U32(a & b))),
      U32Op::Or => apply2_u32_a_u32_b(Some(U32(a | b))),
      U32Op::Xor => apply2_u32_a_u32_b(Some(U32(a ^ b))),
      U32Op::Add => apply2_u32_a_u32_b(Some(U32(a.wrapping_add(b)))),
      U32Op::Sub => apply2_u32_a_u32_b(Some(U32(a.wrapping_sub(b)))),
      U32Op::Mul => apply2_u32_a_u32_b(Some(U32(a.wrapping_mul(b)))),
      U32Op::Div => apply2_u32_a_u32_b(
        if b == 0 {
          None
        } else {
          Some(U32(a.wrapping_div(b)))
        }
      ),
      U32Op::Mod => apply2_u32_a_u32_b(
        if b == 0 {
          None
        } else {
          Some(U32(a.wrapping_rem(b)))
        }
      ),
      U32Op::Pow => apply2_u32_a_u32_c(Some(U32(a.wrapping_pow(c)))),
      U32Op::Shl => apply2_u32_c_u32_a(Some(U32(a.wrapping_shl(c)))),
      U32Op::Shr => apply2_u32_c_u32_a(Some(U32(a.wrapping_shr(c)))),
      U32Op::Rol => apply2_u32_c_u32_a(Some(U32(a.rotate_left(c)))),
      U32Op::Ror => apply2_u32_c_u32_a(Some(U32(a.rotate_right(c)))),
      U32Op::CountZeros => apply1_u32(Some(U32(a.count_zeros()))),
      U32Op::CountOnes => apply1_u32(Some(U32(a.count_ones()))),
      U32Op::ToU8 => from_bool(
        if a > u8::MAX.into() {
          U32Op::apply1(op, &U32(a)) == None
        } else {
          U8Op::apply1(
            U8Op::ToU32,
            &U32Op::apply1(op, &U32(a)).unwrap()
          ) == Some(U32(a))
        }
      ),
      U32Op::ToU16 => from_bool(
        if a > u16::MAX.into() {
          U32Op::apply1(op, &U32(a)) == None
        } else {
          U16Op::apply1(
            U16Op::ToU32,
            &U32Op::apply1(op, &U32(a)).unwrap()
          ) == Some(U32(a))
        }
      ),
      U32Op::ToU64 => from_bool(
        U64Op::apply1(
          U64Op::ToU32,
          &U32Op::apply1(op, &U32(a)).unwrap()
        ) == Some(U32(a))
      ),
      U32Op::ToU128 => TestResult::discard(),
      U32Op::ToNat => apply1_u32(Some(Nat(BigUint::from(u64::try_from(a).unwrap())))),
      U32Op::ToI8 => from_bool(
        if a > i8::MAX.try_into().unwrap() {
          U32Op::apply1(op, &U32(a)) == None
        } else {
          I8Op::apply1(
            I8Op::ToU32,
            &U32Op::apply1(op, &U32(a)).unwrap()
          ) == Some(U32(a))
        }
      ),
      U32Op::ToI16 => from_bool(
        if a > i16::MAX.try_into().unwrap() {
          U32Op::apply1(op, &U32(a)) == None
        } else {
          I16Op::apply1(
            I16Op::ToU32,
            &U32Op::apply1(op, &U32(a)).unwrap()
          ) == Some(U32(a))
        }
      ),
      U32Op::ToI32 => from_bool(
        if a > i32::MAX.try_into().unwrap() {
          U32Op::apply1(op, &U32(a)) == None
        } else {
          I32Op::apply1(
            I32Op::ToU32,
            &U32Op::apply1(op, &U32(a)).unwrap()
          ) == Some(U32(a))
        }
      ),
      U32Op::ToI64 => from_bool(
        I64Op::apply1(
          I64Op::ToU32,
          &U32Op::apply1(op, &U32(a)).unwrap()
        ) == Some(U32(a))
      ),
      U32Op::ToI128 => TestResult::discard(),
      U32Op::ToInt => apply1_u32(Some(Int(a.into()))),
      U32Op::ToBits => apply1_u32(Some(Bits(bits::bytes_to_bits(32, &a.to_be_bytes().into())))),
      U32Op::ToBytes => apply1_u32(Some(Bytes(a.to_be_bytes().into()))),
      U32Op::ToChar => apply1_u32(char::from_u32(a).map(Char)),
    }
  }

  #[quickcheck]
  fn test_apply_none_on_invalid(
    op: U32Op,
    a: Literal,
    b: u32,
    c: u32,
    test_arg_2: bool,
  ) -> TestResult {
    let test_apply1_none_on_invalid = |
      valid_arg: Literal
    | -> TestResult {
      if mem::discriminant(&valid_arg) == mem::discriminant(&a) {
        TestResult::discard()
      } else {
        TestResult::from_bool(
          U32Op::apply1(
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
        U32Op::apply2(
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
      // Arity 0.
      U32Op::Max |
      U32Op::Min => TestResult::discard(),
      // Arity 1, valid is U32.
      U32Op::Not |
      U32Op::CountZeros |
      U32Op::CountOnes |
      U32Op::ToU8 |
      U32Op::ToU16 |
      U32Op::ToU64 |
      U32Op::ToU128 |
      U32Op::ToNat |
      U32Op::ToI8 |
      U32Op::ToI16 |
      U32Op::ToI32 |
      U32Op::ToI64 |
      U32Op::ToI128 |
      U32Op::ToInt |
      U32Op::ToBytes |
      U32Op::ToBits |
      U32Op::ToChar => test_apply1_none_on_invalid(U32(b)),
      // Arity 2, valid are U32 on a and b.
      U32Op::Eql |
      U32Op::Lte |
      U32Op::Lth |
      U32Op::Gth |
      U32Op::Gte |
      U32Op::And |
      U32Op::Or |
      U32Op::Xor |
      U32Op::Add |
      U32Op::Sub |
      U32Op::Mul |
      U32Op::Div |
      U32Op::Mod => if test_arg_2 {
        test_apply2_none_on_invalid(
          U32(b),
          a,
          U32(b)
        )
      } else {
        test_apply2_none_on_invalid(
          U32(b),
          U32(b),
          a
        )
      },
      // Arity 2, valid are U32 on a and U32 on b.
      U32Op::Pow => if test_arg_2 {
        test_apply2_none_on_invalid(
          U32(b),
          a,
          U32(c)
        )
      } else {
        test_apply2_none_on_invalid(
          U32(c),
          U32(b),
          a
        )
      },
      // Arity 2, valid are U32 on a and U32 on b.
      U32Op::Shl |
      U32Op::Shr |
      U32Op::Rol |
      U32Op::Ror => if test_arg_2 {
        test_apply2_none_on_invalid(
          U32(c),
          a,
          U32(b)
        )
      } else {
        test_apply2_none_on_invalid(
          U32(b),
          U32(c),
          a
        )
      }
    }
  }
}
