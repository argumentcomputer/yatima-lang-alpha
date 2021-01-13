use im::Vector;
use nom::{
  error::ErrorKind,
  AsBytes,
  InputLength,
};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ParseError<I: AsBytes> {
  UndefinedReference(I, String, Vector<String>),
  TopLevelRedefinition(I, String),
  UnknownLiteralType(I, String),
  UnexpectedLiteral(I, hashexpr::Expr),
  LiteralError(I, hashexpr::error::ParseError<I>),
  ReservedSymbol(I, String),
  NomErr(I, ErrorKind),
}
impl<I: AsBytes> ParseError<I> {
  pub fn rest(self) -> I {
    match self {
      Self::UndefinedReference(i, ..) => i,
      Self::ReservedSymbol(i, ..) => i,
      Self::TopLevelRedefinition(i, ..) => i,
      Self::UnknownLiteralType(i, ..) => i,
      Self::UnexpectedLiteral(i, ..) => i,
      Self::LiteralError(i, ..) => i,
      Self::NomErr(i, _) => i,
    }
  }
}

impl<I: AsBytes> nom::error::ParseError<I> for ParseError<I>
where
  I: InputLength,
  I: Clone,
{
  fn from_error_kind(input: I, kind: ErrorKind) -> Self {
    ParseError::NomErr(input, kind)
  }

  fn append(i: I, k: ErrorKind, other: Self) -> Self {
    if i.clone().input_len() < other.clone().rest().input_len() {
      ParseError::NomErr(i, k)
    }
    else {
      other
    }
  }

  fn or(self, other: Self) -> Self {
    if self.clone().rest().input_len() < other.clone().rest().input_len() {
      self
    }
    else {
      other
    }
  }
}

impl<I: Clone + AsBytes> From<hashexpr::error::ParseError<I>>
  for ParseError<I>
{
  fn from(x: hashexpr::error::ParseError<I>) -> Self {
    match x {
      hashexpr::error::ParseError::NomErr(i, e) => {
        ParseError::NomErr(i.into(), e)
      }
      e => ParseError::LiteralError(e.clone().rest().into(), e),
    }
  }
}
