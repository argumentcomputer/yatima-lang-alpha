use std::convert::TryInto;

use num_bigint::{
  BigInt,
  BigUint,
  Sign,
};

use crate::valus::literal::{
  LitType,
  Literal,
};

use crate::decode_error::{
  DecodeError,
  Expected,
};
use hashexpr::{
  atom::Atom::Symbol,
  Expr,
  Expr::Atom,
};
use std::fmt;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum PrimOp {
  /// equality
  Eql,
  /// less-than
  Lth,
  /// less-than-or-equal
  Lte, //
  /// greater-than
  Gth,
  /// greater-than-or-equal
  Gte,
  /// bitwise or
  Bor,
  /// bitwise and
  And,
  /// bitwise xor
  Xor,
  /// bitwise negation
  Not,
  /// successor, increment
  Suc,
  /// predecessor, decrement
  Pre,
  /// addition
  Add,
  /// subtraction
  Sub,
  /// multiplication
  Mul,
  /// division
  Div,
  /// modulus
  Mod,
  /// shift left
  Shl,
  /// shift right
  Shr,
  /// rotate left
  Rol,
  /// rotate right
  Ror,
  /// count leading zeros
  Clz,
  /// count trailing zeros
  Ctz,
  /// count set bits
  Cnt,
  /// length
  Len,
  /// concatenate
  Cat,
  /// cast
  Cst,
}

impl PrimOp {
  pub fn symbol(self) -> String {
    match self {
      Self::Eql => String::from("#eql"),
      Self::Lth => String::from("#lth"),
      Self::Lte => String::from("#lte"),
      Self::Gth => String::from("#gth"),
      Self::Gte => String::from("#gte"),
      Self::Bor => String::from("#bor"),
      Self::And => String::from("#and"),
      Self::Xor => String::from("#xor"),
      Self::Not => String::from("#not"),
      Self::Suc => String::from("#suc"),
      Self::Pre => String::from("#pre"),
      Self::Add => String::from("#add"),
      Self::Sub => String::from("#sub"),
      Self::Mul => String::from("#mul"),
      Self::Div => String::from("#div"),
      Self::Mod => String::from("#mod"),
      Self::Shl => String::from("#shl"),
      Self::Shr => String::from("#shr"),
      Self::Rol => String::from("#rol"),
      Self::Ror => String::from("#ror"),
      Self::Clz => String::from("#clz"),
      Self::Ctz => String::from("#ctz"),
      Self::Cnt => String::from("#cnt"),
      Self::Len => String::from("#len"),
      Self::Cat => String::from("#cat"),
      Self::Cst => String::from("#cst"),
    }
  }

  pub fn from_symbol(s: String) -> Option<Self> {
    match s.as_str() {
      "#eql" => Some(Self::Eql),
      "#lth" => Some(Self::Lth),
      "#lte" => Some(Self::Lte),
      "#gth" => Some(Self::Gth),
      "#gte" => Some(Self::Gte),
      "#bor" => Some(Self::Bor),
      "#and" => Some(Self::And),
      "#xor" => Some(Self::Xor),
      "#not" => Some(Self::Not),
      "#suc" => Some(Self::Suc),
      "#pre" => Some(Self::Pre),
      "#add" => Some(Self::Add),
      "#sub" => Some(Self::Sub),
      "#mul" => Some(Self::Mul),
      "#div" => Some(Self::Div),
      "#mod" => Some(Self::Mod),
      "#shl" => Some(Self::Shl),
      "#shr" => Some(Self::Shr),
      "#rol" => Some(Self::Rol),
      "#ror" => Some(Self::Ror),
      "#clz" => Some(Self::Clz),
      "#ctz" => Some(Self::Ctz),
      "#cnt" => Some(Self::Cnt),
      "#len" => Some(Self::Len),
      "#cat" => Some(Self::Cat),
      "#cst" => Some(Self::Cst),
      _ => None,
    }
  }

  pub fn encode(self) -> Expr { Atom(None, Symbol(self.symbol())) }

  pub fn decode(x: Expr) -> Result<Self, DecodeError> {
    let err = |pos| DecodeError::new(pos, vec![Expected::PrimOp]);
    match x {
      Atom(pos, Symbol(n)) => Self::from_symbol(n).ok_or(err(pos)),
      x => Err(err(x.position())),
    }
  }
}

impl fmt::Display for PrimOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.clone().symbol())
  }
}

pub fn apply_bin_op(opr: PrimOp, x: Literal, y: Literal) -> Option<Literal> {
  use Literal::*;
  use PrimOp::*;
  let tt = BitVector(1, vec![1]);
  let ff = BitVector(1, vec![0]);
  let ite = |c| if c { tt } else { ff };
  match (opr, x, y) {
    // Eql
    (Eql, Nat(l1, x), Nat(l2, y)) if l1 == l2 => Some(ite(x == y)),
    (Eql, Natural(x), Natural(y)) => Some(ite(x == y)),
    (Eql, Int(l1, x), Int(l2, y)) if l1 == l2 => Some(ite(x == y)),
    (Eql, Integer(x), Integer(y)) => Some(ite(x == y)),
    (Eql, BitVector(l1, x), BitVector(l2, y)) if l1 == l2 => Some(ite(x == y)),
    (Eql, BitString(x), BitString(y)) => Some(ite(x == y)),
    (Eql, Text(x), Text(y)) => Some(ite(x == y)),
    (Eql, Char(x), Char(y)) => Some(ite(x == y)),
    (Eql, Exception(x), Exception(y)) => Some(ite(x == y)),
    // Lth
    (Lth, Nat(l1, x), Nat(l2, y)) if l1 == l2 => Some(ite(x < y)),
    (Lth, Natural(x), Natural(y)) => Some(ite(x < y)),
    (Lth, Int(l1, x), Int(l2, y)) if l1 == l2 => Some(ite(x < y)),
    (Lth, Integer(x), Integer(y)) => Some(ite(x < y)),
    (Lth, BitVector(l1, x), BitVector(l2, y)) if l1 == l2 => Some(ite(x < y)),
    (Lth, BitString(x), BitString(y)) => Some(ite(x < y)),
    (Lth, Text(x), Text(y)) => Some(ite(x < y)),
    (Lth, Char(x), Char(y)) => Some(ite(x < y)),
    // Lte
    (Lte, Nat(l1, x), Nat(l2, y)) if l1 == l2 => Some(ite(x <= y)),
    (Lte, Natural(x), Natural(y)) => Some(ite(x <= y)),
    (Lte, Int(l1, x), Int(l2, y)) if l1 == l2 => Some(ite(x <= y)),
    (Lte, Integer(x), Integer(y)) => Some(ite(x <= y)),
    (Lte, BitVector(l1, x), BitVector(l2, y)) if l1 == l2 => Some(ite(x <= y)),
    (Lte, BitString(x), BitString(y)) => Some(ite(x <= y)),
    (Lte, Text(x), Text(y)) => Some(ite(x <= y)),
    (Lte, Char(x), Char(y)) => Some(ite(x <= y)),
    // Gth
    (Gth, Nat(l1, x), Nat(l2, y)) if l1 == l2 => Some(ite(x > y)),
    (Gth, Natural(x), Natural(y)) => Some(ite(x > y)),
    (Gth, Int(l1, x), Int(l2, y)) if l1 == l2 => Some(ite(x > y)),
    (Gth, Integer(x), Integer(y)) => Some(ite(x > y)),
    (Gth, BitVector(l1, x), BitVector(l2, y)) if l1 == l2 => Some(ite(x > y)),
    (Gth, BitString(x), BitString(y)) => Some(ite(x > y)),
    (Gth, Text(x), Text(y)) => Some(ite(x > y)),
    (Gth, Char(x), Char(y)) => Some(ite(x > y)),
    // Gte
    (Gte, Nat(l1, x), Nat(l2, y)) if l1 == l2 => Some(ite(x >= y)),
    (Gte, Natural(x), Natural(y)) => Some(ite(x >= y)),
    (Gte, Int(l1, x), Int(l2, y)) if l1 == l2 => Some(ite(x >= y)),
    (Gte, Integer(x), Integer(y)) => Some(ite(x >= y)),
    (Gte, BitVector(l1, x), BitVector(l2, y)) if l1 == l2 => Some(ite(x >= y)),
    (Gte, BitString(x), BitString(y)) => Some(ite(x >= y)),
    (Gte, Text(x), Text(y)) => Some(ite(x >= y)),
    (Gte, Char(x), Char(y)) => Some(ite(x >= y)),
    // Bor
    (Bor, BitVector(l1, x), BitVector(l2, y)) if l1 == l2 => {
      let z = x.iter().zip(y.iter()).map(|(a, b)| a | b).collect();
      Some(BitVector(l1, z))
    }
    (Bor, BitString(x), BitString(y)) => {
      let z = x.iter().zip(y.iter()).map(|(a, b)| a | b).collect();
      Some(BitString(z))
    }
    // And
    (And, BitVector(l1, x), BitVector(l2, y)) if l1 == l2 => {
      let z = x.iter().zip(y.iter()).map(|(a, b)| a & b).collect();
      Some(BitVector(l1, z))
    }
    (And, BitString(x), BitString(y)) => {
      let z = x.iter().zip(y.iter()).map(|(a, b)| a ^ b).collect();
      Some(BitString(z))
    }
    // Xor
    (Xor, BitVector(l1, x), BitVector(l2, y)) if l1 == l2 => {
      let z = x.iter().zip(y.iter()).map(|(a, b)| a ^ b).collect();
      Some(BitVector(l1, z))
    }
    (Xor, BitString(x), BitString(y)) => {
      let z = x.iter().zip(y.iter()).map(|(a, b)| a ^ b).collect();
      Some(BitString(z))
    }
    // Add
    (Add, Nat(l1, x), Nat(l2, y)) if l1 == l2 => Some(Nat(l1, x + y)),
    (Add, Natural(x), Natural(y)) => Some(Natural(x + y)),
    (Add, Int(l1, x), Int(l2, y)) if l1 == l2 => Some(Int(l1, x + y)),
    (Add, Integer(x), Integer(y)) => Some(Integer(x + y)),
    // Sub
    (Sub, Nat(l1, x), Nat(l2, y)) if l1 == l2 => Some(Nat(l1, x - y)),
    (Sub, Natural(x), Natural(y)) => Some(Natural(x - y)),
    (Sub, Int(l1, x), Int(l2, y)) if l1 == l2 => Some(Int(l1, x - y)),
    (Sub, Integer(x), Integer(y)) => Some(Integer(x - y)),
    // Mul
    (Mul, Nat(l1, x), Nat(l2, y)) if l1 == l2 => Some(Nat(l1, x * y)),
    (Mul, Natural(x), Natural(y)) => Some(Natural(x * y)),
    (Mul, Int(l1, x), Int(l2, y)) if l1 == l2 => Some(Int(l1, x * y)),
    (Mul, Integer(x), Integer(y)) => Some(Integer(x * y)),
    // Div
    (Div, Nat(l1, x), Nat(l2, y)) if l1 == l2 && y != (0 as u64).into() => {
      Some(Nat(l1, x / y))
    }
    (Div, Natural(x), Natural(y)) if y != (0 as u64).into() => {
      Some(Natural(x * y))
    }
    (Div, Int(l1, x), Int(l2, y)) if l1 == l2 && y != 0.into() => {
      Some(Int(l1, x / y))
    }
    (Div, Integer(x), Integer(y)) if y != 0.into() => Some(Integer(x / y)),
    // Mod
    (Mod, Nat(l1, x), Nat(l2, y)) if l1 == l2 && y != (0 as u64).into() => {
      Some(Nat(l1, x % y))
    }
    (Mod, Natural(x), Natural(y)) if y != (0 as u64).into() => {
      Some(Natural(x * y))
    }
    (Mod, Int(l1, x), Int(l2, y)) if l1 == l2 && y != 0.into() => {
      Some(Int(l1, x % y))
    }
    (Mod, Integer(x), Integer(y)) if y != 0.into() => Some(Integer(x % y)),

    // Shl
    (Shl, Nat(l1, x), Nat(64, y)) => {
      let y: u64 = y.try_into().ok()?;
      let l2 = u64::checked_add(l1, y)?;
      Some(Nat(l2, x << y))
    }
    (Shl, Natural(x), Nat(64, y)) => {
      let y: u64 = y.try_into().ok()?;
      Some(Natural(x << y))
    }
    (Shl, Int(l1, x), Nat(64, y)) => {
      let y: u64 = y.try_into().ok()?;
      let l2 = u64::checked_add(l1, y)?;
      Some(Int(l2, x << y))
    }
    (Shl, Integer(x), Nat(64, y)) => {
      let y: u64 = y.try_into().ok()?;
      Some(Integer(x << y))
    }
    _ => None,
  }
}

// pub fn cast_op(
//  x: Literal,
//  typ_: LitType,
//  size: Option<BigUint>,
//) -> Option<Literal> {
//}

#[cfg(test)]
pub mod tests {
  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  use rand::Rng;
  impl Arbitrary for PrimOp {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      let gen = g.gen_range(0, 26);
      match gen {
        0 => Self::Eql,
        1 => Self::Lth,
        2 => Self::Lte,
        3 => Self::Gth,
        4 => Self::Gte,
        5 => Self::Bor,
        6 => Self::And,
        7 => Self::Xor,
        8 => Self::Not,
        9 => Self::Suc,
        10 => Self::Pre,
        11 => Self::Add,
        12 => Self::Sub,
        13 => Self::Mul,
        14 => Self::Div,
        15 => Self::Mod,
        16 => Self::Shl,
        17 => Self::Shr,
        18 => Self::Rol,
        19 => Self::Ror,
        20 => Self::Clz,
        21 => Self::Ctz,
        22 => Self::Cnt,
        23 => Self::Len,
        24 => Self::Cat,
        _ => Self::Cst,
      }
    }
  }
  #[quickcheck]
  fn primop_type_encode_decode(x: PrimOp) -> bool {
    match PrimOp::decode(x.clone().encode()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}
