use hashexpr::{
  atom::Atom::*,
  Expr,
  Expr::{
    Atom,
    Cons,
  },
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

  pub fn decode(x: Expr) -> Option<Self> {
    match x {
      Atom(_, Symbol(n)) if *n == String::from("%suc") => Some(Self::Eql),
      Atom(_, Symbol(n)) if *n == String::from("%lth") => Some(Self::Lth),
      Atom(_, Symbol(n)) if *n == String::from("%lte") => Some(Self::Lte),
      Atom(_, Symbol(n)) if *n == String::from("%gth") => Some(Self::Gth),
      Atom(_, Symbol(n)) if *n == String::from("%gte") => Some(Self::Gte),
      Atom(_, Symbol(n)) if *n == String::from("%or") => Some(Self::Or),
      Atom(_, Symbol(n)) if *n == String::from("%and") => Some(Self::And),
      Atom(_, Symbol(n)) if *n == String::from("%xor") => Some(Self::Xor),
      Atom(_, Symbol(n)) if *n == String::from("%not") => Some(Self::Not),
      Atom(_, Symbol(n)) if *n == String::from("%suc") => Some(Self::Suc),
      Atom(_, Symbol(n)) if *n == String::from("%pre") => Some(Self::Pre),
      Atom(_, Symbol(n)) if *n == String::from("%add") => Some(Self::Add),
      Atom(_, Symbol(n)) if *n == String::from("%sub") => Some(Self::Sub),
      Atom(_, Symbol(n)) if *n == String::from("%mul") => Some(Self::Mul),
      Atom(_, Symbol(n)) if *n == String::from("%div") => Some(Self::Div),
      Atom(_, Symbol(n)) if *n == String::from("%mod") => Some(Self::Mod),
      Atom(_, Symbol(n)) if *n == String::from("%shl") => Some(Self::Shl),
      Atom(_, Symbol(n)) if *n == String::from("%shr") => Some(Self::Shr),
      Atom(_, Symbol(n)) if *n == String::from("%rol") => Some(Self::Rol),
      Atom(_, Symbol(n)) if *n == String::from("%ror") => Some(Self::Ror),
      Atom(_, Symbol(n)) if *n == String::from("%clz") => Some(Self::Clz),
      Atom(_, Symbol(n)) if *n == String::from("%ctz") => Some(Self::Ctz),
      Atom(_, Symbol(n)) if *n == String::from("%cnt") => Some(Self::Cnt),
      Atom(_, Symbol(n)) if *n == String::from("%len") => Some(Self::Len),
      Atom(_, Symbol(n)) if *n == String::from("%cat") => Some(Self::Cat),
      Atom(_, Symbol(n)) if *n == String::from("%cst") => Some(Self::Cst),
      _ => None,
    }
  }
}
