use base_x;

use crate::parse::error::{
  throw_err,
  ParseError,
  ParseErrorKind,
};

use crate::parse::span::Span;

use hashexpr::base::Base;

use nom::{
  branch::alt,
  bytes::complete::{
    tag,
    take_till1,
  },
  combinator::value,
  error::context,
  IResult,
};

pub fn parse_base_code()
-> impl Fn(Span) -> IResult<Span, Base, ParseError<Span>> {
  move |from: Span| {
    throw_err(
      alt((
        value(Base::_2, tag("b")),
        value(Base::_8, tag("o")),
        value(Base::_10, tag("d")),
        value(Base::_16, tag("x")),
        value(Base::_32, tag("v")),
        value(Base::_58, tag("I")),
        value(Base::_64, tag("~")),
      ))(from),
      |_| ParseError::new(from, ParseErrorKind::UnknownBaseCode),
    )
  }
}

pub fn parse_base2_digits(from: Span) -> IResult<Span, Span, ParseError<Span>> {
  context("base 2 digits", take_till1(|x| !Base::_2.is_digit(x)))(from)
}

pub fn parse_base8_digits(from: Span) -> IResult<Span, Span, ParseError<Span>> {
  context("base 8 digits", take_till1(|x| !Base::_8.is_digit(x)))(from)
}

pub fn parse_base10_digits(
  from: Span,
) -> IResult<Span, Span, ParseError<Span>> {
  context("base 10 digits", take_till1(|x| !Base::_10.is_digit(x)))(from)
}
pub fn parse_base16_digits(
  from: Span,
) -> IResult<Span, Span, ParseError<Span>> {
  context("base 16 digits", take_till1(|x| !Base::_16.is_digit(x)))(from)
}

pub fn parse_base32_digits(
  from: Span,
) -> IResult<Span, Span, ParseError<Span>> {
  context("base 32 digits", take_till1(|x| !Base::_32.is_digit(x)))(from)
}

pub fn parse_base58_digits(
  from: Span,
) -> IResult<Span, Span, ParseError<Span>> {
  context("base 58 digits", take_till1(|x| !Base::_58.is_digit(x)))(from)
}
pub fn parse_base64_digits(
  from: Span,
) -> IResult<Span, Span, ParseError<Span>> {
  context("base 64 digits", take_till1(|x| !Base::_64.is_digit(x)))(from)
}

pub fn parse_base_digits(
  base: Base,
) -> Box<dyn Fn(Span) -> IResult<Span, Span, ParseError<Span>>> {
  Box::new(move |from: Span| match base {
    Base::_2 => parse_base2_digits(from),
    Base::_8 => parse_base8_digits(from),
    Base::_10 => parse_base10_digits(from),
    Base::_16 => parse_base16_digits(from),
    Base::_32 => parse_base32_digits(from),
    Base::_58 => parse_base58_digits(from),
    Base::_64 => parse_base64_digits(from),
  })
}

pub fn parse_base_bytes(
  base: Base,
) -> impl Fn(Span) -> IResult<Span, Vec<u8>, ParseError<Span>> {
  move |from: Span| {
    let (i, o) = parse_base_digits(base)(from)?;
    match base_x::decode(base.base_digits(), o.fragment()) {
      Ok(bytes) => Ok((i, bytes)),
      Err(_) => Err(nom::Err::Error(ParseError::new(
        i,
        ParseErrorKind::InvalidBaseEncoding(base),
      ))),
    }
  }
}
