use num_bigint::{
  BigInt,
  BigUint,
  Sign,
};

use crate::valus::literal::Literal;

use crate::decode_error::{
  DecodeError,
  Expected,
};
use hashexpr::{
  atom::Atom::*,
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
      Self::Eql => String::from("=="),
      Self::Lth => String::from("<"),
      Self::Lte => String::from("<="),
      Self::Gth => String::from(">"),
      Self::Gte => String::from(">="),
      Self::Bor => String::from("||"),
      Self::And => String::from("&&"),
      Self::Xor => String::from("^"),
      Self::Not => String::from("!"),
      Self::Suc => String::from("++"),
      Self::Pre => String::from("--"),
      Self::Add => String::from("+"),
      Self::Sub => String::from("-"),
      Self::Mul => String::from("*"),
      Self::Div => String::from("/"),
      Self::Mod => String::from("%"),
      Self::Shl => String::from("<<"),
      Self::Shr => String::from(">>"),
      Self::Rol => String::from("<<<"),
      Self::Ror => String::from(">>>"),
      Self::Clz => String::from("#clz"),
      Self::Ctz => String::from("#ctz"),
      Self::Cnt => String::from("#cnt"),
      Self::Len => String::from("#len"),
      Self::Cat => String::from("<>"),
      Self::Cst => String::from("@@"),
    }
  }

  pub fn from_symbol(s: String) -> Option<Self> {
    match s.as_str() {
      "==" => Some(Self::Eql),
      "<" => Some(Self::Lth),
      "<=" => Some(Self::Lte),
      ">" => Some(Self::Gth),
      ">=" => Some(Self::Gte),
      "||" => Some(Self::Bor),
      "&&" => Some(Self::And),
      "^" => Some(Self::Xor),
      "!" => Some(Self::Not),
      "++" => Some(Self::Suc),
      "--" => Some(Self::Pre),
      "+" => Some(Self::Add),
      "-" => Some(Self::Sub),
      "*" => Some(Self::Mul),
      "/" => Some(Self::Div),
      "%" => Some(Self::Mod),
      "<<" => Some(Self::Shl),
      ">>" => Some(Self::Shr),
      "<<<" => Some(Self::Rol),
      ">>>" => Some(Self::Ror),
      "#clz" => Some(Self::Clz),
      "#ctz" => Some(Self::Ctz),
      "#cnt" => Some(Self::Cnt),
      "#len" => Some(Self::Len),
      "<>" => Some(Self::Cat),
      "@@" => Some(Self::Cst),
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

pub fn apply_op(opr: PrimOp, x: Literal, y: Literal) -> Option<Literal> {
  use Literal::*;
  use PrimOp::*;
  match (opr, x, y) {
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
    //
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
