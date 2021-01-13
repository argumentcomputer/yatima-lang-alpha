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

#[derive(PartialEq, Eq, Clone, Debug)]
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
      Self::Eql => String::from("%eql"),
      Self::Lth => String::from("%lth"),
      Self::Lte => String::from("%lte"),
      Self::Gth => String::from("%gth"),
      Self::Gte => String::from("%gte"),
      Self::Bor => String::from("%bor"),
      Self::And => String::from("%and"),
      Self::Xor => String::from("%xor"),
      Self::Not => String::from("%not"),
      Self::Suc => String::from("%suc"),
      Self::Pre => String::from("%pre"),
      Self::Add => String::from("%add"),
      Self::Sub => String::from("%sub"),
      Self::Mul => String::from("%mul"),
      Self::Div => String::from("%div"),
      Self::Mod => String::from("%mod"),
      Self::Shl => String::from("%shl"),
      Self::Shr => String::from("%shr"),
      Self::Rol => String::from("%rol"),
      Self::Ror => String::from("%ror"),
      Self::Clz => String::from("%clz"),
      Self::Ctz => String::from("%ctz"),
      Self::Cnt => String::from("%cnt"),
      Self::Len => String::from("%len"),
      Self::Cat => String::from("%cat"),
      Self::Cst => String::from("%cst"),
    }
  }

  pub fn from_symbol(s: String) -> Option<Self> {
    match s.as_str() {
      "%eql" => Some(Self::Eql),
      "%lth" => Some(Self::Lth),
      "%lte" => Some(Self::Lte),
      "%gth" => Some(Self::Gth),
      "%gte" => Some(Self::Gte),
      "%bor" => Some(Self::Bor),
      "%and" => Some(Self::And),
      "%xor" => Some(Self::Xor),
      "%not" => Some(Self::Not),
      "%suc" => Some(Self::Suc),
      "%pre" => Some(Self::Pre),
      "%add" => Some(Self::Add),
      "%sub" => Some(Self::Sub),
      "%mul" => Some(Self::Mul),
      "%div" => Some(Self::Div),
      "%mod" => Some(Self::Mod),
      "%shl" => Some(Self::Shl),
      "%shr" => Some(Self::Shr),
      "%rol" => Some(Self::Rol),
      "%ror" => Some(Self::Ror),
      "%clz" => Some(Self::Clz),
      "%ctz" => Some(Self::Ctz),
      "%cnt" => Some(Self::Cnt),
      "%len" => Some(Self::Len),
      "%cat" => Some(Self::Cat),
      "%cst" => Some(Self::Cst),
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
