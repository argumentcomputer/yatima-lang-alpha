use crate::{
  base,
  bytevec::ByteVec,
};
use nom::{
  error::{
    ErrorKind,
    FromExternalError,
  },
  AsBytes,
  InputLength,
};
use std::num::ParseIntError;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum DeserialError<I: AsBytes> {
  UnknownTypeCode(I, Vec<u8>, Option<u64>),
  BadLinkLength(I, u64),
  BadCharLength(I, u64),
  ExpectedLink(I),
  InvalidUnicodeCodepoint(I, u32),
  Utf8Error(I, std::string::FromUtf8Error),
  NomErr(I, ErrorKind),
}

impl<'a, I: AsBytes> DeserialError<I> {
  pub fn rest(self) -> I {
    match self {
      Self::UnknownTypeCode(i, ..) => i,
      Self::BadCharLength(i, _) => i,
      Self::BadLinkLength(i, _) => i,
      Self::ExpectedLink(i) => i,
      Self::Utf8Error(i, _) => i,
      Self::InvalidUnicodeCodepoint(i, _) => i,
      Self::NomErr(i, _) => i,
    }
  }

  pub fn input_as_bytes(&'a self) -> DeserialError<ByteVec> {
    match self {
      Self::UnknownTypeCode(i, x, y) => DeserialError::UnknownTypeCode(
        i.as_bytes().to_vec().into(),
        x.clone(),
        *y,
      ),
      Self::BadCharLength(i, x) => {
        DeserialError::BadCharLength(i.as_bytes().to_vec().into(), *x)
      }
      Self::BadLinkLength(i, x) => {
        DeserialError::BadLinkLength(i.as_bytes().to_vec().into(), *x)
      }
      Self::ExpectedLink(i) => {
        DeserialError::ExpectedLink(i.as_bytes().to_vec().into())
      }
      Self::Utf8Error(i, x) => {
        DeserialError::Utf8Error(i.as_bytes().to_vec().into(), x.to_owned())
      }
      Self::InvalidUnicodeCodepoint(i, x) => {
        DeserialError::InvalidUnicodeCodepoint(i.as_bytes().to_vec().into(), *x)
      }
      Self::NomErr(i, x) => {
        DeserialError::NomErr(i.as_bytes().to_vec().into(), *x)
      }
    }
  }
}

impl<I: AsBytes> nom::error::ParseError<I> for DeserialError<I>
where
  I: InputLength,
  I: Clone,
{
  fn from_error_kind(input: I, kind: ErrorKind) -> Self {
    DeserialError::NomErr(input, kind)
  }

  fn append(i: I, k: ErrorKind, other: Self) -> Self {
    if i.clone().input_len() < other.clone().rest().input_len() {
      DeserialError::NomErr(i, k)
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

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ParseError<I: AsBytes> {
  InvalidBaseEncoding(I, base::Base),
  ExpectedSingleChar(I, Vec<char>),
  InvalidBase16EscapeSequence(I, String),
  DeserialErr(I, DeserialError<ByteVec>),
  ParseIntErr(I, ParseIntError),
  LengthTooSmall(I, Vec<u8>, u64),
  NomErr(I, ErrorKind),
}

impl<I: AsBytes> ParseError<I> {
  pub fn rest(self) -> I {
    match self {
      Self::InvalidBaseEncoding(i, _) => i,
      Self::ExpectedSingleChar(i, _) => i,
      Self::DeserialErr(i, _) => i,
      Self::InvalidBase16EscapeSequence(i, _) => i,
      Self::ParseIntErr(i, _) => i,
      Self::LengthTooSmall(i, ..) => i,
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

impl<I: Clone + AsBytes> From<base::ParseError<I>> for ParseError<I> {
  fn from(x: base::ParseError<I>) -> Self {
    match x {
      base::ParseError::InvalidEncoding(i, b) => {
        ParseError::InvalidBaseEncoding(i.into(), b)
      }
      base::ParseError::NomErr(i, e) => ParseError::NomErr(i.into(), e),
    }
  }
}

impl<'a, I: Clone + AsBytes> FromExternalError<I, ParseIntError>
  for ParseError<I>
{
  fn from_external_error(input: I, _: ErrorKind, e: ParseIntError) -> Self {
    ParseError::ParseIntErr(input, e)
  }
}
