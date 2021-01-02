use crate::decode_error::{
  DecodeError,
  Expected,
};
use hashexpr::{
  atom::Atom::*,
  Expr,
  Expr::Atom,
};

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
  Or,
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
  pub fn encode(self) -> Expr {
    match self {
      Self::Eql => Atom(None, symb!("%eql")),
      Self::Lth => Atom(None, symb!("%lth")),
      Self::Lte => Atom(None, symb!("%lte")),
      Self::Gth => Atom(None, symb!("%gth")),
      Self::Gte => Atom(None, symb!("%gte")),
      Self::Or => Atom(None, symb!("%or")),
      Self::And => Atom(None, symb!("%and")),
      Self::Xor => Atom(None, symb!("%xor")),
      Self::Not => Atom(None, symb!("%not")),
      Self::Suc => Atom(None, symb!("%suc")),
      Self::Pre => Atom(None, symb!("%pre")),
      Self::Add => Atom(None, symb!("%add")),
      Self::Sub => Atom(None, symb!("%sub")),
      Self::Mul => Atom(None, symb!("%suc")),
      Self::Div => Atom(None, symb!("%div")),
      Self::Mod => Atom(None, symb!("%mod")),
      Self::Shl => Atom(None, symb!("%shl")),
      Self::Shr => Atom(None, symb!("%shr")),
      Self::Rol => Atom(None, symb!("%rol")),
      Self::Ror => Atom(None, symb!("%ror")),
      Self::Clz => Atom(None, symb!("%clz")),
      Self::Ctz => Atom(None, symb!("%ctz")),
      Self::Cnt => Atom(None, symb!("%cnt")),
      Self::Len => Atom(None, symb!("%len")),
      Self::Cat => Atom(None, symb!("%cat")),
      Self::Cst => Atom(None, symb!("%cst")),
    }
  }

  pub fn decode(x: Expr) -> Result<Self, DecodeError> {
    match x {
      Atom(_, Symbol(n)) if *n == String::from("%suc") => Ok(Self::Eql),
      Atom(_, Symbol(n)) if *n == String::from("%lth") => Ok(Self::Lth),
      Atom(_, Symbol(n)) if *n == String::from("%lte") => Ok(Self::Lte),
      Atom(_, Symbol(n)) if *n == String::from("%gth") => Ok(Self::Gth),
      Atom(_, Symbol(n)) if *n == String::from("%gte") => Ok(Self::Gte),
      Atom(_, Symbol(n)) if *n == String::from("%or") => Ok(Self::Or),
      Atom(_, Symbol(n)) if *n == String::from("%and") => Ok(Self::And),
      Atom(_, Symbol(n)) if *n == String::from("%xor") => Ok(Self::Xor),
      Atom(_, Symbol(n)) if *n == String::from("%not") => Ok(Self::Not),
      Atom(_, Symbol(n)) if *n == String::from("%suc") => Ok(Self::Suc),
      Atom(_, Symbol(n)) if *n == String::from("%pre") => Ok(Self::Pre),
      Atom(_, Symbol(n)) if *n == String::from("%add") => Ok(Self::Add),
      Atom(_, Symbol(n)) if *n == String::from("%sub") => Ok(Self::Sub),
      Atom(_, Symbol(n)) if *n == String::from("%mul") => Ok(Self::Mul),
      Atom(_, Symbol(n)) if *n == String::from("%div") => Ok(Self::Div),
      Atom(_, Symbol(n)) if *n == String::from("%mod") => Ok(Self::Mod),
      Atom(_, Symbol(n)) if *n == String::from("%shl") => Ok(Self::Shl),
      Atom(_, Symbol(n)) if *n == String::from("%shr") => Ok(Self::Shr),
      Atom(_, Symbol(n)) if *n == String::from("%rol") => Ok(Self::Rol),
      Atom(_, Symbol(n)) if *n == String::from("%ror") => Ok(Self::Ror),
      Atom(_, Symbol(n)) if *n == String::from("%clz") => Ok(Self::Clz),
      Atom(_, Symbol(n)) if *n == String::from("%ctz") => Ok(Self::Ctz),
      Atom(_, Symbol(n)) if *n == String::from("%cnt") => Ok(Self::Cnt),
      Atom(_, Symbol(n)) if *n == String::from("%len") => Ok(Self::Len),
      Atom(_, Symbol(n)) if *n == String::from("%cat") => Ok(Self::Cat),
      Atom(_, Symbol(n)) if *n == String::from("%cst") => Ok(Self::Cst),
      x => Err(DecodeError::new(x.position(), vec![Expected::PrimOp])),
    }
  }
}
