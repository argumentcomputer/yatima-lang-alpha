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
  term::Term,
  yatima,
};

use num_bigint::BigUint;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum I64Op {
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
  ToU128,
  ToNat,
  ToI8,
  ToI16,
  ToI32,
  ToI128,
  ToInt,
  ToBits,
  ToBytes,
}

impl I64Op {
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
      Self::ToU128 => "to_U128".to_owned(),
      Self::ToNat => "to_Nat".to_owned(),
      Self::ToI8 => "to_I8".to_owned(),
      Self::ToI16 => "to_I16".to_owned(),
      Self::ToI32 => "to_I32".to_owned(),
      Self::ToI128 => "to_I128".to_owned(),
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
      "to_U128" => Some(Self::ToU128),
      "to_Nat" => Some(Self::ToNat),
      "to_I8" => Some(Self::ToI8),
      "to_I16" => Some(Self::ToI16),
      "to_I32" => Some(Self::ToI32),
      "to_I128" => Some(Self::ToI128),
      "to_Int" => Some(Self::ToInt),
      "to_Bits" => Some(Self::ToBits),
      "to_Bytes" => Some(Self::ToBytes),
      _ => None,
    }
  }

  pub fn type_of(self) -> Term {
    match self {
      Self::Abs => yatima!("∀ #I64 -> #U64"),
      Self::Sgn => yatima!("∀ #I64 -> #Bool"),
      Self::Max => yatima!("#I64"),
      Self::Min => yatima!("#I64"),
      Self::Eql => yatima!("∀ #I64 #I64 -> #Bool"),
      Self::Lte => yatima!("∀ #I64 #I64 -> #Bool"),
      Self::Lth => yatima!("∀ #I64 #I64 -> #Bool"),
      Self::Gth => yatima!("∀ #I64 #I64 -> #Bool"),
      Self::Gte => yatima!("∀ #I64 #I64 -> #Bool"),
      Self::Not => yatima!("∀ #I64 #I64 -> #Bool"),
      Self::And => yatima!("∀ #I64 #I64 -> #Bool"),
      Self::Or => yatima!("∀ #I64 #I64 -> #Bool"),
      Self::Xor => yatima!("∀ #I64 #I64 -> #Bool"),
      Self::Add => yatima!("∀ #I64 #I64 -> #I64"),
      Self::Sub => yatima!("∀ #I64 #I64 -> #I64"),
      Self::Mul => yatima!("∀ #I64 #I64 -> #I64"),
      Self::Div => yatima!("∀ #I64 #I64 -> #I64"),
      Self::Mod => yatima!("∀ #I64 #I64 -> #I64"),
      Self::Pow => yatima!("∀ #I64 #U32 -> #I64"),
      Self::Shl => yatima!("∀ #U32 #I64 -> #I64"),
      Self::Shr => yatima!("∀ #U32 #I64 -> #I64"),
      Self::Rol => yatima!("∀ #U32 #I64 -> #I64"),
      Self::Ror => yatima!("∀ #U32 #I64 -> #I64"),
      Self::CountZeros => yatima!("∀ #I64 -> #U32"),
      Self::CountOnes => yatima!("∀ #I64 -> #U32"),
      Self::ToU8 => yatima!("∀ #I64 -> #U8"),
      Self::ToU16 => yatima!("∀ #I64 -> #U16"),
      Self::ToU32 => yatima!("∀ #I64 -> #U32"),
      Self::ToU64 => yatima!("∀ #I64 -> #U64"),
      Self::ToU128 => yatima!("∀ #I64 -> #U128"),
      Self::ToNat => yatima!("∀ #I64 -> #Nat"),
      Self::ToI8 => yatima!("∀ #I64 -> #I8"),
      Self::ToI16 => yatima!("∀ #I64 -> #I16"),
      Self::ToI32 => yatima!("∀ #I64 -> #I32"),
      Self::ToI128 => yatima!("∀ #I64 -> #I128"),
      Self::ToInt => yatima!("∀ #I64 -> #Int"),
      Self::ToBytes => yatima!("∀ #I64 -> #Bytes"),
      Self::ToBits => yatima!("∀ #I64 -> #Bits"),
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
      Self::ToU128 => Ipld::Integer(29),
      Self::ToNat => Ipld::Integer(30),
      Self::ToI8 => Ipld::Integer(31),
      Self::ToI16 => Ipld::Integer(32),
      Self::ToI32 => Ipld::Integer(33),
      Self::ToI128 => Ipld::Integer(34),
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
      Ipld::Integer(29) => Ok(Self::ToU128),
      Ipld::Integer(30) => Ok(Self::ToNat),
      Ipld::Integer(31) => Ok(Self::ToI8),
      Ipld::Integer(32) => Ok(Self::ToI16),
      Ipld::Integer(33) => Ok(Self::ToI32),
      Ipld::Integer(34) => Ok(Self::ToI128),
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
      Self::ToU128 => 1,
      Self::ToNat => 1,
      Self::ToI8 => 1,
      Self::ToI16 => 1,
      Self::ToI32 => 1,
      Self::ToI128 => 1,
      Self::ToInt => 1,
      Self::ToBits => 1,
      Self::ToBytes => 1,
    }
  }

  pub fn apply0(self) -> Option<Literal> {
    use Literal::*;
    match self {
      Self::Max => Some(I64(i64::MAX)),
      Self::Min => Some(I64(i64::MIN)),
      _ => None,
    }
  }

  pub fn apply1(self, x: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x) {
      (Self::Abs, I64(x)) => Some(U64(x.unsigned_abs())),
      (Self::Sgn, I64(x)) => Some(Bool(x.is_positive())),
      (Self::CountZeros, I64(x)) => Some(U32(x.count_zeros())),
      (Self::CountOnes, I64(x)) => Some(U32(x.count_ones())),
      (Self::ToU8, I64(x)) => u8::try_from(*x).ok().map(U8),
      (Self::ToU16, I64(x)) => u16::try_from(*x).ok().map(U16),
      (Self::ToU32, I64(x)) => u32::try_from(*x).ok().map(U32),
      (Self::ToU64, I64(x)) => u64::try_from(*x).ok().map(U64),
      (Self::ToU128, I64(x)) => u128::try_from(*x).ok().map(U128),
      (Self::ToNat, I64(x)) => {
        if x.is_negative() {
          None
        }
        else {
          Some(Nat(BigUint::from(u64::try_from(*x).unwrap())))
        }
      }
      (Self::ToI8, I64(x)) => i8::try_from(*x).ok().map(I8),
      (Self::ToI16, I64(x)) => i16::try_from(*x).ok().map(I16),
      (Self::ToI32, I64(x)) => i32::try_from(*x).ok().map(I32),
      (Self::ToI128, I64(x)) => Some(I128((*x).into())),
      (Self::Not, I64(x)) => Some(I64(!x)),
      (Self::ToInt, I64(x)) => Some(Int((*x).into())),
      (Self::ToBits, I64(x)) => {
        Some(Bits(bits::bytes_to_bits(64, &x.to_be_bytes().into())))
      }
      (Self::ToBytes, I64(x)) => Some(Bytes(x.to_be_bytes().into())),
      _ => None,
    }
  }

  #[cfg(target = "wasm32-unknown-unknown")]
  pub fn apply2(self, x: &Literal, y: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x, y) {
      (Self::Eql, I64(x), I64(y)) => {
        let result;
        asm!("local.get {}\nlocal.get {}\ni64.eq\nlocal.set {}", in(local) x, in(local) y, out(local) result);
        Some(Bool(result == 1))
      }
      (Self::Lte, I64(x), I64(y)) => {
        let result;
        asm!("local.get {}\nlocal.get {}\ni64.le_s\nlocal.set {}", in(local) x, in(local) y, out(local) result);
        Some(Bool(result == 1))
      }
      (Self::Lth, I64(x), I64(y)) => {
        let result;
        asm!("local.get {}\nlocal.get {}\ni64.lt_s\nlocal.set {}", in(local) x, in(local) y, out(local) result);
        Some(Bool(result == 1))
      }
      (Self::Gth, I64(x), I64(y)) => {
        let result;
        asm!("local.get {}\nlocal.get {}\ni64.gt_s\nlocal.set {}", in(local) x, in(local) y, out(local) result);
        Some(Bool(result == 1))
      }
      (Self::Gte, I64(x), I64(y)) => {
        let result;
        asm!("local.get {}\nlocal.get {}\ni64.ge_s\nlocal.set {}", in(local) x, in(local) y, out(local) result);
        Some(Bool(result == 1))
      }
      (Self::And, I64(x), I64(y)) => {
        let result;
        asm!("local.get {}\nlocal.get {}\ni64.and\nlocal.set {}", in(local) x, in(local) y, out(local) result);
        Some(I64(result))
      }
      (Self::Or, I64(x), I64(y)) => {
        let result;
        asm!("local.get {}\nlocal.get {}\ni64.or\nlocal.set {}", in(local) x, in(local) y, out(local) result);
        Some(I64(result))
      }
      (Self::Xor, I64(x), I64(y)) => {
        let result;
        asm!("local.get {}\nlocal.get {}\ni64.xor\nlocal.set {}", in(local) x, in(local) y, out(local) result);
        Some(I64(result))
      }
      (Self::Add, I64(x), I64(y)) => {
        let result;
        asm!("local.get {}\nlocal.get {}\ni64.add\nlocal.set {}", in(local) x, in(local) y, out(local) result);
        Some(I64(result))
      }
      (Self::Sub, I64(x), I64(y)) => {
        let result;
        asm!("local.get {}\nlocal.get {}\ni64.sub\nlocal.set {}", in(local) x, in(local) y, out(local) result);
        Some(I64(result))
      }
      (Self::Mul, I64(x), I64(y)) => {
        let result;
        asm!("local.get {}\nlocal.get {}\ni64.mul\nlocal.set {}", in(local) x, in(local) y, out(local) result);
        Some(I64(result))
      }
      (Self::Div, I64(x), I64(y)) => {
        let result;
        asm!("local.get {}\nlocal.get {}\ni64.div_s\nlocal.set {}", in(local) x, in(local) y, out(local) result);
        Some(I64(result))
      }
      (Self::Mod, I64(x), I64(y)) => {
        let result;
        asm!("local.get {}\nlocal.get {}\ni64.rem_s\nlocal.set {}", in(local) x, in(local) y, out(local) result);
        Some(I64(result))
      }
      (Self::Pow, I64(x), U32(y)) => Some(I64(x.wrapping_pow(*y))),
      (Self::Shl, U32(x), I64(y)) => {
        let result;
        asm!("local.get {}\nlocal.get {}\ni64.shl\nlocal.set {}", in(local) x, in(local) y, out(local) result);
        Some(I64(result))
      }
      (Self::Shr, U32(x), I64(y)) => {
        let result;
        asm!("local.get {}\nlocal.get {}\ni64.shr_s\nlocal.set {}", in(local) x, in(local) y, out(local) result);
        Some(I64(result))
      }
      (Self::Rol, U32(x), I64(y)) => {
        let result;
        asm!("local.get {}\nlocal.get {}\ni64.rotl\nlocal.set {}", in(local) x, in(local) y, out(local) result);
        Some(I64(result))
      }
      (Self::Ror, U32(x), I64(y)) => {
        let result;
        asm!("local.get {}\nlocal.get {}\ni64.rotr\nlocal.set {}", in(local) x, in(local) y, out(local) result);
        Some(I64(result))
      }
      _ => None,
    }
  }

  #[cfg(not(target = "wasm32-unknown-unknown"))]
  pub fn apply2(self, x: &Literal, y: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x, y) {
      (Self::Eql, I64(x), I64(y)) => Some(Bool(x == y)),
      (Self::Lte, I64(x), I64(y)) => Some(Bool(x <= y)),
      (Self::Lth, I64(x), I64(y)) => Some(Bool(x < y)),
      (Self::Gth, I64(x), I64(y)) => Some(Bool(x > y)),
      (Self::Gte, I64(x), I64(y)) => Some(Bool(x >= y)),
      (Self::And, I64(x), I64(y)) => Some(I64(x & y)),
      (Self::Or, I64(x), I64(y)) => Some(I64(x | y)),
      (Self::Xor, I64(x), I64(y)) => Some(I64(x ^ y)),
      (Self::Add, I64(x), I64(y)) => Some(I64(x.wrapping_add(*y))),
      (Self::Sub, I64(x), I64(y)) => Some(I64(x.wrapping_sub(*y))),
      (Self::Mul, I64(x), I64(y)) => Some(I64(x.wrapping_mul(*y))),
      (Self::Div, I64(x), I64(y)) => {
        if *y == 0 {
          None
        }
        else {
          Some(I64(x.wrapping_div(*y)))
        }
      }
      (Self::Mod, I64(x), I64(y)) => {
        if *y == 0 {
          None
        }
        else {
          Some(I64(x.wrapping_rem(*y)))
        }
      }
      (Self::Pow, I64(x), U32(y)) => Some(I64(x.wrapping_pow(*y))),
      (Self::Shl, U32(x), I64(y)) => Some(I64(y.wrapping_shl(*x))),
      (Self::Shr, U32(x), I64(y)) => Some(I64(y.wrapping_shr(*x))),
      (Self::Rol, U32(x), I64(y)) => Some(I64(y.rotate_left(*x))),
      (Self::Ror, U32(x), I64(y)) => Some(I64(y.rotate_right(*x))),
      _ => None,
    }
  }
}

impl fmt::Display for I64Op {
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
    I64,
    U32,
    U64,
  };
  impl Arbitrary for I64Op {
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
        31 => Self::ToI16,
        32 => Self::ToI32,
        33 => Self::ToInt,
        34 => Self::ToBytes,
        _ => Self::ToBits,
        /* 29 => Self::ToU128,
         * 34 => Self::ToI128, */
      }
    }
  }

  #[quickcheck]
  fn i64_op_ipld(x: I64Op) -> bool {
    match I64Op::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }

  #[quickcheck]
  fn test_apply(op: I64Op, a: i64, b: i64, c: u32) -> TestResult {
    let apply0_go = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(I64Op::apply0(op) == expected)
    };

    let apply1_i64 = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(I64Op::apply1(op, &I64(a)) == expected)
    };

    let apply2_i64_i64 = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(I64Op::apply2(op, &I64(a), &I64(b)) == expected)
    };

    let apply2_i64_u32 = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(I64Op::apply2(op, &I64(a), &U32(c)) == expected)
    };

    let apply2_u32_i64 = |expected: Option<Literal>| -> TestResult {
      TestResult::from_bool(I64Op::apply2(op, &U32(c), &I64(a)) == expected)
    };

    let from_bool = TestResult::from_bool;

    match op {
      I64Op::Abs => apply1_i64(Some(U64(a.unsigned_abs()))),
      I64Op::Sgn => apply1_i64(Some(Bool(a.is_positive()))),
      I64Op::Max => apply0_go(Some(I64(i64::MAX))),
      I64Op::Min => apply0_go(Some(I64(i64::MIN))),
      I64Op::Eql => apply2_i64_i64(Some(Bool(a == b))),
      I64Op::Lte => apply2_i64_i64(Some(Bool(a <= b))),
      I64Op::Lth => apply2_i64_i64(Some(Bool(a < b))),
      I64Op::Gth => apply2_i64_i64(Some(Bool(a > b))),
      I64Op::Gte => apply2_i64_i64(Some(Bool(a >= b))),
      I64Op::Not => apply1_i64(Some(I64(!a))),
      I64Op::And => apply2_i64_i64(Some(I64(a & b))),
      I64Op::Or => apply2_i64_i64(Some(I64(a | b))),
      I64Op::Xor => apply2_i64_i64(Some(I64(a ^ b))),
      I64Op::Add => apply2_i64_i64(Some(I64(a.wrapping_add(b)))),
      I64Op::Sub => apply2_i64_i64(Some(I64(a.wrapping_sub(b)))),
      I64Op::Mul => apply2_i64_i64(Some(I64(a.wrapping_mul(b)))),
      I64Op::Div => {
        apply2_i64_i64(if b == 0 { None } else { Some(I64(a.wrapping_div(b))) })
      }
      I64Op::Mod => {
        apply2_i64_i64(if b == 0 { None } else { Some(I64(a.wrapping_rem(b))) })
      }
      I64Op::Pow => apply2_i64_u32(Some(I64(a.wrapping_pow(c)))),
      I64Op::Shl => apply2_u32_i64(Some(I64(a.wrapping_shl(c)))),
      I64Op::Shr => apply2_u32_i64(Some(I64(a.wrapping_shr(c)))),
      I64Op::Rol => apply2_u32_i64(Some(I64(a.rotate_left(c)))),
      I64Op::Ror => apply2_u32_i64(Some(I64(a.rotate_right(c)))),
      I64Op::CountZeros => apply1_i64(Some(U32(a.count_zeros()))),
      I64Op::CountOnes => apply1_i64(Some(U32(a.count_ones()))),
      I64Op::ToU8 => from_bool(if a < u8::MIN.into() || a > u8::MAX.into() {
        I64Op::apply1(op, &I64(a)) == None
      }
      else {
        U8Op::apply1(U8Op::ToI64, &I64Op::apply1(op, &I64(a)).unwrap())
          == Some(I64(a))
      }),
      I64Op::ToU16 => {
        from_bool(if a < u16::MIN.into() || a > u16::MAX.into() {
          I64Op::apply1(op, &I64(a)) == None
        }
        else {
          U16Op::apply1(U16Op::ToI64, &I64Op::apply1(op, &I64(a)).unwrap())
            == Some(I64(a))
        })
      }
      I64Op::ToU32 => {
        from_bool(if a < u32::MIN.into() || a > u32::MAX.into() {
          I64Op::apply1(op, &I64(a)) == None
        }
        else {
          U32Op::apply1(U32Op::ToI64, &I64Op::apply1(op, &I64(a)).unwrap())
            == Some(I64(a))
        })
      }
      I64Op::ToU64 => from_bool(if a < u64::MIN.try_into().unwrap() {
        I64Op::apply1(op, &I64(a)) == None
      }
      else {
        U64Op::apply1(U64Op::ToI64, &I64Op::apply1(op, &I64(a)).unwrap())
          == Some(I64(a))
      }),
      I64Op::ToU128 => TestResult::discard(),
      I64Op::ToNat => {
        if a.is_negative() {
          apply1_i64(None)
        }
        else {
          apply1_i64(Some(Nat(BigUint::from(u64::try_from(a).unwrap()))))
        }
      }
      I64Op::ToI8 => from_bool(if a < i8::MIN.into() || a > i8::MAX.into() {
        I64Op::apply1(op, &I64(a)) == None
      }
      else {
        I8Op::apply1(I8Op::ToI64, &I64Op::apply1(op, &I64(a)).unwrap())
          == Some(I64(a))
      }),
      I64Op::ToI16 => {
        from_bool(if a < i16::MIN.into() || a > i16::MAX.into() {
          I64Op::apply1(op, &I64(a)) == None
        }
        else {
          I16Op::apply1(I16Op::ToI64, &I64Op::apply1(op, &I64(a)).unwrap())
            == Some(I64(a))
        })
      }
      I64Op::ToI32 => {
        from_bool(if a < i32::MIN.into() || a > i32::MAX.into() {
          I64Op::apply1(op, &I64(a)) == None
        }
        else {
          I32Op::apply1(I32Op::ToI64, &I64Op::apply1(op, &I64(a)).unwrap())
            == Some(I64(a))
        })
      }
      I64Op::ToI128 => TestResult::discard(),
      I64Op::ToInt => apply1_i64(Some(Int(a.into()))),
      I64Op::ToBits => {
        apply1_i64(Some(Bits(bits::bytes_to_bits(64, &a.to_be_bytes().into()))))
      }
      I64Op::ToBytes => apply1_i64(Some(Bytes(a.to_be_bytes().into()))),
    }
  }

  #[quickcheck]
  fn test_apply_none_on_invalid(
    op: I64Op,
    a: Literal,
    b: i64,
    c: u32,
    test_arg_2: bool,
  ) -> TestResult {
    let test_apply1_none_on_invalid = |valid_arg: Literal| -> TestResult {
      if mem::discriminant(&valid_arg) == mem::discriminant(&a) {
        TestResult::discard()
      }
      else {
        TestResult::from_bool(I64Op::apply1(op, &a) == None)
      }
    };

    let test_apply2_none_on_invalid =
      |valid_arg: Literal, a_: Literal, b_: Literal| -> TestResult {
        let go = || TestResult::from_bool(I64Op::apply2(op, &a_, &b_) == None);
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
      I64Op::Max | I64Op::Min => TestResult::discard(),
      // Arity 1, valid is I64.
      I64Op::Abs
      | I64Op::Sgn
      | I64Op::Not
      | I64Op::CountZeros
      | I64Op::CountOnes
      | I64Op::ToU8
      | I64Op::ToU16
      | I64Op::ToU32
      | I64Op::ToU64
      | I64Op::ToU128
      | I64Op::ToNat
      | I64Op::ToI8
      | I64Op::ToI16
      | I64Op::ToI32
      | I64Op::ToI128
      | I64Op::ToInt
      | I64Op::ToBytes
      | I64Op::ToBits => test_apply1_none_on_invalid(I64(b)),
      // Arity 2, valid are I64 on a and b.
      I64Op::Eql
      | I64Op::Lte
      | I64Op::Lth
      | I64Op::Gth
      | I64Op::Gte
      | I64Op::And
      | I64Op::Or
      | I64Op::Xor
      | I64Op::Add
      | I64Op::Sub
      | I64Op::Mul
      | I64Op::Div
      | I64Op::Mod => {
        if test_arg_2 {
          test_apply2_none_on_invalid(I64(b), a, I64(b))
        }
        else {
          test_apply2_none_on_invalid(I64(b), I64(b), a)
        }
      }
      // Arity 2, valid are I64 on a and U32 on b.
      I64Op::Pow => {
        if test_arg_2 {
          test_apply2_none_on_invalid(I64(b), a, U32(c))
        }
        else {
          test_apply2_none_on_invalid(U32(c), I64(b), a)
        }
      }
      // Arity 2, valid are U32 on a and I64 on b.
      I64Op::Shl | I64Op::Shr | I64Op::Rol | I64Op::Ror => {
        if test_arg_2 {
          test_apply2_none_on_invalid(U32(c), a, I64(b))
        }
        else {
          test_apply2_none_on_invalid(I64(b), U32(c), a)
        }
      }
    }
  }
}
