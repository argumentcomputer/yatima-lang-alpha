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
  parse,
  defs,
};

/// Primitive 128-bit signed integer operations
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum I128Op {
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
  ToI64,
  ToInt,
  ToBits,
  ToBytes,
}

impl I128Op {
  /// Gets the syntax string of an i128 operation
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
      Self::ToI64 => "to_I64".to_owned(),
      Self::ToInt => "to_Int".to_owned(),
      Self::ToBits => "to_Bits".to_owned(),
      Self::ToBytes => "to_Bytes".to_owned(),
    }
  }

  /// Gets an i128 operation from a syntax string
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
      "to_I64" => Some(Self::ToI64),
      "to_Int" => Some(Self::ToInt),
      "to_Bits" => Some(Self::ToBits),
      "to_Bytes" => Some(Self::ToBytes),
      _ => None,
    }
  }

  /// Returns the type of an i128 operation
  pub fn type_of(self) -> Term {
    match self {
      Self::Abs => yatima!("#I128 -> #U8"),
      Self::Sgn => yatima!("#I128 -> #Bool"),
      Self::Max => yatima!("#I128"),
      Self::Min => yatima!("#I128"),
      Self::Eql => yatima!("∀ #I128 #I128 -> #Bool"),
      Self::Lte => yatima!("∀ #I128 #I128 -> #Bool"),
      Self::Lth => yatima!("∀ #I128 #I128 -> #Bool"),
      Self::Gth => yatima!("∀ #I128 #I128 -> #Bool"),
      Self::Gte => yatima!("∀ #I128 #I128 -> #Bool"),
      Self::Not => yatima!("∀ #I128 #I128 -> #Bool"),
      Self::And => yatima!("∀ #I128 #I128 -> #Bool"),
      Self::Or => yatima!("∀ #I128 #I128 -> #Bool"),
      Self::Xor => yatima!("∀ #I128 #I128 -> #Bool"),
      Self::Add => yatima!("∀ #I128 #I128 -> #I128"),
      Self::Sub => yatima!("∀ #I128 #I128 -> #I128"),
      Self::Mul => yatima!("∀ #I128 #I128 -> #I128"),
      Self::Div => yatima!("∀ #I128 #I128 -> #I128"),
      Self::Mod => yatima!("∀ #I128 #I128 -> #I128"),
      Self::Pow => yatima!("∀ #I128 #U32 -> #I128"),
      Self::Shl => yatima!("∀ #U32 #I128 -> #I128"),
      Self::Shr => yatima!("∀ #U32 #I128 -> #I128"),
      Self::Rol => yatima!("∀ #U32 #I128 -> #I128"),
      Self::Ror => yatima!("∀ #U32 #I128 -> #I128"),
      Self::CountZeros => yatima!("∀ #I128 -> #U32"),
      Self::CountOnes => yatima!("∀ #I128 -> #U32"),
      Self::ToU8 => yatima!("∀ #I128 -> #U8"),
      Self::ToU16 => yatima!("∀ #I128 -> #U16"),
      Self::ToU32 => yatima!("∀ #I128 -> #U32"),
      Self::ToU64 => yatima!("∀ #I128 -> #U64"),
      Self::ToU128 => yatima!("∀ #I128 -> #U128"),
      Self::ToNat => yatima!("∀ #I128 -> #Nat"),
      Self::ToI8 => yatima!("∀ #I128 -> #I8"),
      Self::ToI16 => yatima!("∀ #I128 -> #I16"),
      Self::ToI32 => yatima!("∀ #I128 -> #I32"),
      Self::ToI64 => yatima!("∀ #I128 -> #I64"),
      Self::ToInt => yatima!("∀ #I128 -> #Int"),
      Self::ToBits => yatima!("∀ #I128 -> #Bits"),
      Self::ToBytes => yatima!("∀ #I128 -> #Bytes"),
    }
  }

  /// Converts an i128 operation into an IPLD object
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
      Self::ToI64 => Ipld::Integer(34),
      Self::ToInt => Ipld::Integer(35),
      Self::ToBits => Ipld::Integer(36),
      Self::ToBytes => Ipld::Integer(37),
    }
  }

  /// Converts an IPLD object into an i128 operation
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
      Ipld::Integer(34) => Ok(Self::ToI64),
      Ipld::Integer(35) => Ok(Self::ToInt),
      Ipld::Integer(36) => Ok(Self::ToBits),
      Ipld::Integer(37) => Ok(Self::ToBytes),
      xs => Err(IpldError::NatOp(xs.to_owned())),
    }
  }

  /// Returns the number of parameters used in the operation
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
      Self::ToI64 => 1,
      Self::ToInt => 1,
      Self::ToBytes => 1,
      Self::ToBits => 1,
    }
  }

  /// Applies a nullary operation to a literal and returns it if successful
  pub fn apply0(self) -> Option<Literal> {
    use Literal::*;
    match self {
      Self::Max => Some(I16(i16::MAX)),
      Self::Min => Some(I16(i16::MIN)),
      _ => None,
    }
  }

  /// Applies a unary operation to a literal and returns it if successful
  pub fn apply1(self, x: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x) {
      (Self::Abs, I128(x)) => Some(U128(x.unsigned_abs())),
      (Self::Sgn, I128(x)) => Some(Bool(x.is_positive())),
      (Self::CountZeros, I128(x)) => Some(U32(x.count_zeros())),
      (Self::CountOnes, I128(x)) => Some(U32(x.count_ones())),
      (Self::ToU8, I128(x)) => u8::try_from(*x).ok().map(U8),
      (Self::ToU16, I128(x)) => u16::try_from(*x).ok().map(U16),
      (Self::ToU32, I128(x)) => u32::try_from(*x).ok().map(U32),
      (Self::ToU64, I128(x)) => u64::try_from(*x).ok().map(U64),
      (Self::ToU128, I128(x)) => u128::try_from(*x).ok().map(U128),
      (Self::ToI8, I128(x)) => i8::try_from(*x).ok().map(I8),
      (Self::ToI16, I128(x)) => i16::try_from(*x).ok().map(I16),
      (Self::ToI32, I128(x)) => i32::try_from(*x).ok().map(I32),
      (Self::ToI64, I128(x)) => i64::try_from(*x).ok().map(I64),
      (Self::Not, I128(x)) => Some(I128(!x)),
      (Self::ToInt, I128(x)) => Some(Int((*x).into())),
      (Self::ToBytes, I128(x)) => Some(Bytes(x.to_be_bytes().into())),
      (Self::ToBits, I128(x)) => {
        Some(Bits(bits::bytes_to_bits(128, &x.to_be_bytes().into())))
      }
      _ => None,
    }
  }

  /// Applies a binary operation to a literal and returns it if successful
  pub fn apply2(self, x: &Literal, y: &Literal) -> Option<Literal> {
    use Literal::*;
    match (self, x, y) {
      (Self::Eql, I128(x), I128(y)) => Some(Bool(x == y)),
      (Self::Lte, I128(x), I128(y)) => Some(Bool(x <= y)),
      (Self::Lth, I128(x), I128(y)) => Some(Bool(x < y)),
      (Self::Gth, I128(x), I128(y)) => Some(Bool(x > y)),
      (Self::Gte, I128(x), I128(y)) => Some(Bool(x >= y)),
      (Self::And, I128(x), I128(y)) => Some(I128(x & y)),
      (Self::Or, I128(x), I128(y)) => Some(I128(x | y)),
      (Self::Xor, I128(x), I128(y)) => Some(I128(x ^ y)),
      (Self::Add, I128(x), I128(y)) => Some(I128(x.wrapping_add(*y))),
      (Self::Sub, I128(x), I128(y)) => Some(I128(x.wrapping_sub(*y))),
      (Self::Mul, I128(x), I128(y)) => Some(I128(x.wrapping_mul(*y))),
      (Self::Div, I128(x), I128(y)) => Some(I128(x.wrapping_div(*y))),
      (Self::Mod, I128(x), I128(y)) => Some(I128(x.wrapping_rem(*y))),
      (Self::Pow, I128(x), U32(y)) => Some(I128(x.wrapping_pow(*y))),
      (Self::Shl, U32(x), I128(y)) => Some(I128(y.wrapping_shl(*x))),
      (Self::Shr, U32(x), I128(y)) => Some(I128(y.wrapping_shr(*x))),
      (Self::Rol, U32(x), I128(y)) => Some(I128(y.rotate_left(*x))),
      (Self::Ror, U32(x), I128(y)) => Some(I128(y.rotate_right(*x))),
      _ => None,
    }
  }
}

impl fmt::Display for I128Op {
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
  impl Arbitrary for I128Op {
    fn arbitrary(_g: &mut Gen) -> Self {
      let mut rng = rand::thread_rng();
      let gen: u32 = rng.gen_range(0..37);
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
        29 => Self::ToU128,
        30 => Self::ToNat,
        31 => Self::ToI8,
        32 => Self::ToI16,
        33 => Self::ToI32,
        34 => Self::ToI64,
        35 => Self::ToInt,
        36 => Self::ToBytes,
        _ => Self::ToBits,
      }
    }
  }

  #[quickcheck]
  fn i128_op_ipld(x: I128Op) -> bool {
    match I128Op::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}
