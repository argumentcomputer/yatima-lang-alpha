use im::Vector;
use nom::{
  error::ErrorKind,
  InputLength,
};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ParseError<I> {
  FreeVariable(I, String, Vector<String>),
  NomErr(I, ErrorKind),
}
impl<I> ParseError<I> {
  pub fn rest(self) -> I {
    match self {
      Self::FreeVariable(i, ..) => i,
      Self::NomErr(i, _) => i,
    }
  }
}

impl<I> nom::error::ParseError<I> for ParseError<I>
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
