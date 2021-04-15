use crate::{base, bytevec::ByteVec};
use nom::{
    error::{ErrorKind, FromExternalError},
    AsBytes, InputLength,
};
use std::{cmp::Ordering, num::ParseIntError};

#[derive(PartialEq, Debug, Clone)]
pub enum DeserialErrorKind {
    UnknownTypeCode(Vec<u8>),
    BadLinkLength(u64),
    BadCharLength(u64),
    ExpectedLink,
    InvalidUnicodeCodepoint(u32),
    Utf8Error(std::string::FromUtf8Error),
    Nom(ErrorKind),
}

#[derive(PartialEq, Debug, Clone)]
pub struct DeserialError<I: AsBytes> {
    pub input: I,
    pub error: DeserialErrorKind,
}

impl<'a, I: AsBytes> DeserialError<I> {
    pub fn new(input: I, error: DeserialErrorKind) -> Self {
        Self { input, error }
    }

    pub fn input_as_bytes(&'a self) -> DeserialError<ByteVec> {
        DeserialError {
            input: self.input.as_bytes().to_vec().into(),
            error: self.error.clone(),
        }
    }
}

impl<I: AsBytes> nom::error::ParseError<I> for DeserialError<I>
where
    I: InputLength + Clone,
{
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        Self::new(input, DeserialErrorKind::Nom(kind))
    }

    fn append(input: I, kind: ErrorKind, other: Self) -> Self {
        match input.input_len().cmp(&other.input.input_len()) {
            Ordering::Less => Self {
                input,
                error: DeserialErrorKind::Nom(kind),
            },
            _ => other,
        }
    }

    fn or(self, other: Self) -> Self {
        match self.input.input_len().cmp(&other.input.input_len()) {
            Ordering::Less => self,
            _ => other,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum ParseErrorKind {
    InvalidBaseEncoding(base::Base),
    ExpectedSingleChar(Vec<char>),
    InvalidBase16EscapeSequence(String),
    DeserialErr(DeserialError<ByteVec>),
    ParseIntErr(ParseIntError),
    Nom(ErrorKind),
}

#[derive(PartialEq, Debug, Clone)]
pub struct ParseError<I: AsBytes> {
    pub input: I,
    pub error: ParseErrorKind,
}

impl<I: AsBytes> ParseError<I> {
    pub fn new(input: I, error: ParseErrorKind) -> Self {
        Self { input, error }
    }
}

impl<I: AsBytes> nom::error::ParseError<I> for ParseError<I>
where
    I: InputLength + Clone,
{
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        Self::new(input, ParseErrorKind::Nom(kind))
    }

    fn append(input: I, kind: ErrorKind, other: Self) -> Self {
        match input.input_len().cmp(&other.input.input_len()) {
            Ordering::Less => Self::new(input, ParseErrorKind::Nom(kind)),
            _ => other,
        }
    }

    fn or(self, other: Self) -> Self {
        match self.input.input_len().cmp(&other.input.input_len()) {
            Ordering::Less => self,
            _ => other,
        }
    }
}

// impl<I: Clone + AsBytes> From<base::ParseError<I>> for ParseError<I> {
//  fn from(x: base::ParseError<I>) -> Self {
//    match x {
//      base::ParseError::InvalidEncoding(i, b) => ParseError {
//        input: i,
//        context: vec![ParseErrorKind::InvalidBaseEncoding(b)],
//      },
//      base::ParseError::NomErr(i, e) => {
//        ParseError { input: i, context: vec![ParseErrorKind::Nom(e)] }
//      }
//    }
//  }
//}

impl<'a, I: Clone + AsBytes> FromExternalError<I, ParseIntError> for ParseError<I> {
    fn from_external_error(input: I, _: ErrorKind, e: ParseIntError) -> Self {
        Self {
            input,
            error: ParseErrorKind::ParseIntErr(e),
        }
    }
}
