use crate::{
  parse::span::Span,
  term::{
    Link,
    LitType,
    Literal,
    PrimOp,
  },
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
  fmt,
  fmt::Write,
  num::ParseIntError,
  path::PathBuf,
  string::String,
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
  LiteralLacksWhitespaceTermination(Literal),
  LitTypeLacksWhitespaceTermination(LitType),
  PrimOpLacksWhitespaceTermination(PrimOp),
  InvalidSymbol(String),
  ExpectedImportLink(hashexpr::Expr),
  UnknownImportLink(Link),
  MisnamedPackage(String),
  MisnamedImport(String, Link, String),
  MalformedPath,
  ImportCycle(PathBuf),
  EmbeddingError(UnembedError),
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
          "Definition {} conflicts with previous or imported declaration",
          name
        )
      }
      Self::InvalidBaseEncoding(base) => {
        write!(f, "Invalid digits for {} encoding", base)
      }
      Self::UnknownBaseCode => {
        write!(f, "Base code must be one of 'b', 'o', 'd', 'x', 'v', 'I', '~'")
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
      // DeserialErr(DeserialError<ByteVec>),
      Self::DeserialErr(e) => {
        write!(f, "Error deserializing hashexpr: {:?}", e)
      }
      Self::ParseIntErr(e) => {
        write!(f, "Error parsing number: {}", e)
      }
      Self::ReservedKeyword(name) => {
        write!(f, "{}` is a reserved language keyword", name)
      }
      Self::HashExprSyntax(_) => {
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
      Self::PrimOpLacksWhitespaceTermination(x) => {
        write!(
          f,
          "Built-in primitive operation {} must be terminated by whitespace \
           or eof",
          x
        )
      }
      Self::ExpectedImportLink(expr) => {
        write!(
          f,
          "Expected the hashexpr encoding of a link, instead found {}",
          expr
        )
      }
      Self::UnknownImportLink(link) => {
        write!(f, "Link {} not found in local hashspace", link)
      }
      Self::MisnamedPackage(name) => {
        write!(
          f,
          "Package {} must be in a file named {}.ya",
          name.clone(),
          name
        )
      }
      Self::MisnamedImport(name, link, pack_name) => {
        write!(
          f,
          "Tried to import a package {} from {}, but package is actually \
           named {}",
          name, link, pack_name
        )
      }
      Self::MalformedPath => {
        write!(f, "malformed path")
      }
      Self::ImportCycle(path) => {
        write!(
          f,
          "An `open` declaration creates an import cycle with file {:?}, \
           which is not allowed",
          path
        )
      }
      Self::EmbeddingError(e) => {
        write!(f, "Error reading package from hashspace: {:?}", e)
      }
      _ => write!(f, "internal parser error"),
    }
  }
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

  pub fn is_nom_err(&self) -> bool {
    match self {
      Self::Nom(_) => true,
      _ => false,
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
}

impl<'a> fmt::Display for ParseError<Span<'a>> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut res = String::new();

    write!(
      &mut res,
      "at line {}:{} \n",
      self.input.location_line(),
      self.input.get_column()
    )?;
    let line = String::from_utf8_lossy(self.input.get_line_beginning());

    write!(&mut res, "{} | {}\n\n", self.input.location_line(), line)?;

    if let Some(exp) = self.expected {
      write!(&mut res, "Expected {}\n", exp)?;
    }

    let mut errs = self.errors.iter().filter(|x| !x.is_nom_err()).peekable();
    if errs.peek() == None {
      // TODO: Nom verbose mode
      write!(&mut res, "Internal parser error\n")?;
    }
    else {
      write!(&mut res, "Reported errors:\n")?;
      for kind in errs {
        write!(&mut res, "- {}\n", kind)?;
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
