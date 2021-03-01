use crate::{
  term::Link,
  unembed_error::UnembedError,
};
use im::Vector;
use nom::{
  error::ErrorKind,
  AsBytes,
  Err,
  InputLength,
};
use std::{
  cmp::Ordering,
  path::PathBuf,
};

#[derive(PartialEq, Debug, Clone)]
pub enum ParseErrorKind<I: AsBytes> {
  UndefinedReference(String, Vector<String>),
  TopLevelRedefinition(String),
  UnknownLiteralType(String),
  UnexpectedLiteral(hashexpr::Expr),
  LiteralError(hashexpr::error::ParseError<I>),
  ReservedSymbol(String),
  ExpectedImportLink(hashexpr::Expr),
  UnknownImportLink(Link),
  MisnamedPackage(String),
  MisnamedImport(String, String),
  MalformedPath,
  ImportCycle(PathBuf),
  EmbeddingError(UnembedError),
  Context(&'static str),
  Nom(ErrorKind),
}

#[derive(PartialEq, Debug, Clone)]
pub struct ParseError<I: AsBytes> {
  pub input: I,
  pub expected: Vec<&'static str>,
  pub errors: Vec<ParseErrorKind<I>>,
}

impl<I: AsBytes> ParseError<I> {
  pub fn new(input: I, error: ParseErrorKind<I>) -> Self {
    ParseError { input, expected: vec![], errors: vec![error] }
  }
}

impl<I: AsBytes> nom::error::ParseError<I> for ParseError<I>
where
  I: InputLength,
  I: Clone,
{
  fn from_error_kind(input: I, kind: ErrorKind) -> Self {
    ParseError::new(input, ParseErrorKind::Nom(kind))
  }

  fn append(input: I, kind: ErrorKind, mut other: Self) -> Self {
    match input.input_len().cmp(&other.input.input_len()) {
      Ordering::Less => ParseError::new(input, ParseErrorKind::Nom(kind)),
      Ordering::Equal => {
        other.errors.push(ParseErrorKind::Nom(kind));
        other
      }
      Ordering::Greater => other,
    }
  }

  fn or(self, mut other: Self) -> Self {
    match self.input.input_len().cmp(&other.input.input_len()) {
      Ordering::Less => self,
      Ordering::Equal => {
        for x in self.errors {
          other.errors.push(x);
        }
        for x in self.expected {
          other.expected.push(x);
        }
        other
      }
      Ordering::Greater => other,
    }
  }
}

impl<I: AsBytes> nom::error::ContextError<I> for ParseError<I>
where
  I: InputLength,
  I: Clone,
{
  fn add_context(input: I, ctx: &'static str, mut other: Self) -> Self {
    match input.input_len().cmp(&other.input.input_len()) {
      Ordering::Less => {
        ParseError { input, expected: vec![ctx], errors: vec![] }
      }
      Ordering::Equal => {
        other.expected.push(ctx);
        other
      }
      Ordering::Greater => other,
    }
  }
}

pub fn convert<I: AsBytes>(
  from: I,
  x: Err<hashexpr::error::ParseError<I>>,
) -> Err<ParseError<I>> {
  match x {
    Err::Incomplete(n) => Err::Incomplete(n),
    Err::Error(e) => {
      Err::Error(ParseError::new(from, ParseErrorKind::LiteralError(e)))
    }
    Err::Failure(e) => {
      Err::Failure(ParseError::new(from, ParseErrorKind::LiteralError(e)))
    }
  }
}
