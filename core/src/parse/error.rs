use crate::{
  name::Name,
  parse::{
    base,
    span::Span,
  },
  term::{
    LitType,
    Literal,
  },
};

use nom::{
  error::ErrorKind,
  AsBytes,
  Err,
  IResult,
  InputLength,
};

#[cfg(not(feature = "std"))]
use sp_std::{
  cmp::Ordering,
  fmt,
  fmt::Write,
  num::ParseIntError,
  vec::Vec,
};
#[cfg(feature = "std")]
use std::{
  cmp::Ordering,
  fmt,
  fmt::Write,
  num::ParseIntError,
  vec::Vec,
};

use sp_im::conslist::ConsList;

use alloc::string::String;

#[derive(PartialEq, Debug, Clone)]
pub enum ParseErrorKind {
  UndefinedReference(Name, ConsList<Name>),
  TopLevelRedefinition(Name),
  UnknownLiteralType(String),
  InvalidBaseEncoding(base::LitBase),
  UnknownBaseCode,
  ExpectedSingleChar(Vec<char>),
  InvalidBase16EscapeSequence(String),
  MultibaseError(multibase::Error),
  CidError,
  ParseIntErr(ParseIntError),
  ReservedKeyword(String),
  NumericSyntax(String),
  ReservedSyntax(String),
  LiteralLacksWhitespaceTermination(Literal),
  LitTypeLacksWhitespaceTermination(LitType),
  UnknownNatOp(Name),
  UnknownIntOp(Name),
  UnknownBitsOp(Name),
  UnknownBytesOp(Name),
  UnknownBoolOp(Name),
  UnknownTextOp(Name),
  UnknownCharOp(Name),
  UnknownU8Op(Name),
  UnknownU16Op(Name),
  UnknownU32Op(Name),
  UnknownU64Op(Name),
  UnknownU128Op(Name),
  UnknownI8Op(Name),
  UnknownI16Op(Name),
  UnknownI32Op(Name),
  UnknownI64Op(Name),
  UnknownI128Op(Name),
  TypeDefConstructorMustReturnItsType,
  InvalidSymbol(String),
  Nom(ErrorKind),
}

impl<'a> fmt::Display for ParseErrorKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::UndefinedReference(name, _) => {
        write!(f, "Undefined reference {}", name)
      }
      Self::TopLevelRedefinition(name) => {
        write!(
          f,
          "Overlapping definition names, \"{}\" already defined or imported",
          name
        )
      }
      Self::ExpectedSingleChar(chrs) => {
        write!(
          f,
          "Character literal syntax must contain one and only one character, \
           but parsed {:?}",
          chrs
        )
      }
      Self::InvalidBase16EscapeSequence(seq) => {
        write!(f, "Unknown base 16 string escape sequence {}.", seq)
      }
      Self::ParseIntErr(e) => {
        write!(f, "Error parsing number: {}", e)
      }
      Self::ReservedKeyword(name) => {
        write!(f, "{}` is a reserved language keyword", name)
      }
      Self::ReservedSyntax(_) => {
        write!(f, "Symbols beginning with '#' are reserved")
      }
      Self::NumericSyntax(_) => {
        write!(f, "Symbols beginning with digits are reserved")
      }
      Self::InvalidSymbol(name) => {
        write!(
          f,
          "The symbol {} contains a reserved character ':', '(', ')', ',', or \
           whitespace or control character.",
          name
        )
      }
      Self::LiteralLacksWhitespaceTermination(x) => {
        write!(f, "Literal {} must be terminated by whitespace or eof", x)
      }
      Self::LitTypeLacksWhitespaceTermination(x) => {
        write!(f, "Literal type {} must be terminated by whitespace or eof", x)
      }
      Self::UnknownNatOp(x) => {
        write!(f, "Unknown primitive Nat operation #Nat.{}", x)
      }
      Self::UnknownIntOp(x) => {
        write!(f, "Unknown primitive Int operation #Int.{}", x)
      }
      Self::UnknownBytesOp(x) => {
        write!(f, "Unknown primitive Bytes operation #Bytes.{}", x)
      }
      Self::UnknownTextOp(x) => {
        write!(f, "Unknown primitive Nat operation #Text.{}", x)
      }
      _ => write!(f, "internal parser error"),
    }
  }
}

impl ParseErrorKind {
  pub fn is_nom_err(&self) -> bool { matches!(self, Self::Nom(_)) }
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
}

impl<'a> fmt::Display for ParseError<Span<'a>> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut res = String::new();

    writeln!(
      &mut res,
      "at line {}:{}",
      self.input.location_line(),
      self.input.get_column()
    )?;
    let line = String::from_utf8_lossy(self.input.get_line_beginning());

    writeln!(&mut res, "{} | {}", self.input.location_line(), line)?;

    let cols = format!("{} | ", self.input.location_line()).len()
      + self.input.get_column();
    for _ in 0..(cols - 1) {
      write!(&mut res, " ")?;
    }
    writeln!(&mut res, "^")?;

    if let Some(exp) = self.expected {
      writeln!(&mut res, "Expected {}", exp)?;
    }

    let mut errs = self.errors.iter().filter(|x| !x.is_nom_err()).peekable();
    if errs.peek() == None {
      // TODO: Nom verbose mode
      writeln!(&mut res, "Internal parser error")?;
    }
    else {
      writeln!(&mut res, "Reported errors:")?;
      for kind in errs {
        writeln!(&mut res, "- {}", kind)?;
      }
    }

    write!(f, "{}", res)
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
      Ordering::Equal => match other.expected {
        None => ParseError { input, expected: Some(ctx), errors: other.errors },
        _ => other,
      },
      Ordering::Greater => other,
    }
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
