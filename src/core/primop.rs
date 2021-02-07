use bit_vec::BitVec;
use std::convert::TryInto;

use num_bigint::{
  BigInt,
  BigUint,
};

use crate::core::literal::Literal;

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
  /// length
  Len,
  /// concatenate
  Cat,
  // Cst
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
      Self::Len => String::from("#len"),
      Self::Cat => String::from("#cat"),
      // Self::Cst => String::from("#cst"),
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
      "#len" => Some(Self::Len),
      "#cat" => Some(Self::Cat),
      //"#cst" => Some(Self::Cst),
      _ => None,
    }
  }

  pub fn arity(self) -> u64 {
    match self {
      Self::Not => 1,
      Self::Len => 1,
      Self::Suc => 1,
      Self::Pre => 1,
      _ => 2,
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

pub fn apply_una_op(opr: PrimOp, x: Literal) -> Option<Literal> {
  use Literal::*;
  use PrimOp::*;
  let one: BigUint = (1 as u64).into();
  let one_i: BigInt = (1 as u64).into();
  match (opr, x) {
    (Not, BitVector(l, x)) => {
      let mut bs = BitVec::from_bytes(&x);
      bs.negate();
      Some(BitVector(l, bs.to_bytes()))
    }
    (Not, BitString(x)) => {
      let mut bs = BitVec::from_bytes(&x);
      bs.negate();
      Some(BitString(bs.to_bytes()))
    }
    (Suc, Nat(l1, x)) => {
      let y: BigUint = x + one;
      if y.bits() == l1 { Some(Nat(l1, y)) } else { None }
    }
    (Suc, Natural(x)) => Some(Natural(x + one)),
    (Suc, Integer(x)) => Some(Integer(x + one_i)),
    (Suc, Int(l1, x)) => {
      let y: BigInt = x + one_i;
      if y.bits() == l1 { Some(Int(l1, y)) } else { None }
    }
    (Pre, Natural(x)) if x != (0 as u64).into() => Some(Natural(x - one)),
    (Pre, Nat(l1, x)) if x != (0 as u64).into() => Some(Nat(l1, x - one)),
    (Pre, Integer(x)) => Some(Integer(x - one_i)),
    (Pre, Int(l1, x)) => {
      let y: BigInt = x.clone() - one_i;
      if y < x { Some(Int(l1, y)) } else { None }
    }
    (Len, Nat(l1, _)) => Some(Nat(64, l1.into())),
    (Len, Natural(x)) => Some(Nat(64, x.bits().into())),
    (Len, Int(l1, _)) => Some(Nat(64, l1.into())),
    (Len, Integer(x)) => Some(Nat(64, x.bits().into())),
    (Len, BitVector(l1, _)) => Some(Nat(64, l1.into())),
    (Len, BitString(x)) => Some(Nat(
      64,
      x.len().checked_mul(8).expect("impossible length overflow").into(),
    )),
    (Len, Text(x)) => Some(Nat(64, x.chars().count().into())),
    (Len, Char(_)) => Some(Nat(64, (32 as u64).into())),
    _ => None,
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
    (Sub, Nat(l1, x), Nat(l2, y)) if l1 == l2 && x >= y => Some(Nat(l1, x - y)),
    (Sub, Natural(x), Natural(y)) if x >= y => Some(Natural(x - y)),
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
    (Shl, BitVector(l1, x), Nat(64, y)) => {
      let y: u64 = y.try_into().ok()?;
      let l2 = u64::checked_add(l1, y)?;
      let mut bs = BitVec::from_bytes(&x);
      let mut pad = BitVec::from_elem(y as usize, false);
      bs.append(&mut pad);
      Some(BitVector(l2, bs.to_bytes()))
    }
    (Shl, BitString(x), Nat(64, y)) => {
      let y: u64 = y.try_into().ok()?;
      let mut bs = BitVec::from_bytes(&x);
      let mut pad = BitVec::from_elem(y as usize, false);
      bs.append(&mut pad);
      Some(BitString(bs.to_bytes()))
    }
    // Shr
    (Shr, Nat(l1, x), Nat(64, y)) => {
      let y: u64 = y.try_into().ok()?;
      let l2 = u64::checked_add(l1, y)?;
      Some(Nat(l2, x >> y))
    }
    (Shr, Natural(x), Nat(64, y)) => {
      let y: u64 = y.try_into().ok()?;
      Some(Natural(x >> y))
    }
    (Shr, Int(l1, x), Nat(64, y)) => {
      let y: u64 = y.try_into().ok()?;
      let l2 = u64::checked_add(l1, y)?;
      Some(Int(l2, x >> y))
    }
    (Shr, Integer(x), Nat(64, y)) => {
      let y: u64 = y.try_into().ok()?;
      Some(Integer(x >> y))
    }
    (Shr, BitVector(l1, x), Nat(64, y)) => {
      let y: u64 = y.try_into().ok()?;
      let l2 = u64::checked_add(l1, y)?;
      let mut bs = BitVec::from_bytes(&x);
      bs.truncate(bs.len() - (y as usize));
      Some(BitVector(l2, bs.to_bytes()))
    }
    (Shr, BitString(x), Nat(64, y)) => {
      let y: u64 = y.try_into().ok()?;
      let mut bs = BitVec::from_bytes(&x);
      bs.truncate(bs.len() - (y as usize));
      Some(BitString(bs.to_bytes()))
    }
    // Cat
    (Cat, BitVector(l1, x), BitVector(l2, y)) if l1 == l2 => {
      let mut xs = BitVec::from_bytes(&x);
      let mut ys = BitVec::from_bytes(&y);
      xs.append(&mut ys);
      Some(BitVector(l2, xs.to_bytes()))
    }
    (Cat, BitString(x), BitString(y)) => {
      let mut xs = BitVec::from_bytes(&x);
      let mut ys = BitVec::from_bytes(&y);
      xs.append(&mut ys);
      Some(BitString(xs.to_bytes()))
    }
    (Cat, Text(x), Text(y)) => Some(Text([x, y].join(""))),
    _ => None,
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
  impl Arbitrary for PrimOp {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      let gen = g.gen_range(0, 19);
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
        18 => Self::Len,
        _ => Self::Cat,
        //_ => Self::Cst,
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
