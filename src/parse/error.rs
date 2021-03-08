use crate::{
  term::Link,
  unembed_error::UnembedError,
};

use hashexpr::{
  base,
  bytevec::ByteVec,
  error::DeserialError,
};

use im::Vector;
use nom::{
  error::ErrorKind,
  AsBytes,
  Err,
  IResult,
  InputLength,
};
use std::{
  cmp::Ordering,
  num::ParseIntError,
  path::PathBuf,
};

#[derive(PartialEq, Debug, Clone)]
pub enum ParseErrorKind {
  UndefinedReference(String, Vector<String>),
  TopLevelRedefinition(String),
  UnknownLiteralType(String),
  UnexpectedLiteral(hashexpr::Expr),
  InvalidBaseEncoding(base::Base),
  UnknownBaseCode,
  ExpectedSingleChar(Vec<char>),
  InvalidBase16EscapeSequence(String),
  DeserialErr(DeserialError<ByteVec>),
  ParseIntErr(ParseIntError),
  ReservedKeyword(String),
  HashExprSyntax(String),
  NumericSyntax(String),
  InvalidSymbol(String),
  ExpectedImportLink(hashexpr::Expr),
  UnknownImportLink(Link),
  MisnamedPackage(String),
  MisnamedImport(String, String),
  MalformedPath,
  ImportCycle(PathBuf),
  EmbeddingError(UnembedError),
  Nom(ErrorKind),
}

impl ParseErrorKind {
  pub fn from_hashexpr_error(x: hashexpr::error::ParseErrorKind) -> Self {
    use hashexpr::error::ParseErrorKind::*;
    match x {
      InvalidBaseEncoding(base) => Self::InvalidBaseEncoding(base),
      ExpectedSingleChar(chars) => Self::ExpectedSingleChar(chars),
      InvalidBase16EscapeSequence(seq) => {
        Self::InvalidBase16EscapeSequence(seq)
      }
      DeserialErr(err) => Self::DeserialErr(err),
      ParseIntErr(err) => Self::ParseIntErr(err),
      Nom(e) => Self::Nom(e),
    }
  }
}

#[derive(PartialEq, Debug, Clone)]
pub struct ParseError<I: AsBytes> {
  pub input: I,
  pub expected: Option<&'static str>,
  pub errors: Vec<ParseErrorKind>,
}

impl<I: AsBytes> ParseError<I> {
  pub fn new(input: I, error: ParseErrorKind) -> Self {
    ParseError { input, expected: None, errors: vec![error] }
  }

  pub fn from_hashexpr_error(
    from: I,
    x: hashexpr::error::ParseError<I>,
  ) -> Self {
    ParseError {
      input: from,
      expected: None,
      errors: vec![ParseErrorKind::from_hashexpr_error(x.error)],
    }
  }

  pub fn pretty(self, from: I) -> String {
    let mut res = String::new();
    // for (i, kind) in e.errors.iter().enumerate() {
    //  let offset = from
    //}
    res
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
  fn add_context(input: I, ctx: &'static str, other: Self) -> Self {
    match input.input_len().cmp(&other.input.input_len()) {
      Ordering::Less => {
        ParseError { input, expected: Some(ctx), errors: vec![] }
      }
      _ => other,
    }
  }
}

pub fn convert<I: AsBytes>(
  from: I,
  x: Err<hashexpr::error::ParseError<I>>,
) -> Err<ParseError<I>> {
  match x {
    Err::Incomplete(n) => Err::Incomplete(n),
    Err::Error(e) => Err::Error(ParseError::from_hashexpr_error(from, e)),
    Err::Failure(e) => Err::Failure(ParseError::from_hashexpr_error(from, e)),
  }
}
pub fn throw_err<I: AsBytes, A, F: Fn(ParseError<I>) -> ParseError<I>>(
  x: IResult<I, A, ParseError<I>>,
  f: F,
) -> IResult<I, A, ParseError<I>> {
  match x {
    Ok(res) => Ok(res),
    Err(Err::Incomplete(n)) => Err(Err::Incomplete(n)),
    Err(Err::Error(e)) => Err(Err::Error(f(e))),
    Err(Err::Failure(e)) => Err(Err::Failure(f(e))),
  }
}
