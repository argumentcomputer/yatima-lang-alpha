use num_bigint::{
  BigInt,
  BigUint,
  Sign,
};

use crate::core::literal::{
  Literal,
};

use crate::parse::{
  base::*,
  span::Span,
  string::*,
};

use crate::parse::error::{
  ParseError,
  ParseErrorKind,
};

use hashexpr::base::Base;

use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::digit1,
  combinator::{
    opt,
    peek,
    value,
  },
  error::context,
  sequence::{
    delimited,
    preceded,
    terminated,
  },
  Err,
  IResult,
};

pub fn parse_nat(from: Span) -> IResult<Span, Literal, ParseError<Span>> {
  let (i, base) = opt(preceded(tag("0"), parse_base_code()))(from)?;
  let base = base.unwrap_or(Base::_10);
  let (upto, bytes) = parse_base_bytes(base)(i)?;
  Ok((upto, Literal::Natural(BigUint::from_bytes_be(&bytes))))
}

pub fn parse_int_sign(from: Span) -> IResult<Span, Sign, ParseError<Span>> {
  let (i, sign) = alt((
    value(Sign::Minus, terminated(tag("-"), peek(digit1))),
    value(Sign::Plus, terminated(tag("+"), peek(digit1))),
  ))(from)?;
  Ok((i, sign))
}

pub fn parse_int(from: Span) -> IResult<Span, Literal, ParseError<Span>> {
  let (i, s) = parse_int_sign(from)?;
  let (i, base) = opt(preceded(tag("0"), parse_base_code()))(i)?;
  let base = base.unwrap_or(Base::_10);
  let (upto, bytes) = parse_base_bytes(base)(i)?;
  Ok((upto, Literal::Integer(BigInt::from_bytes_be(s, &bytes))))
}

pub fn parse_text(from: Span) -> IResult<Span, Literal, ParseError<Span>> {
  let (i, _) = context("open quotes", tag("\""))(from)?;
  let (i, s) = parse_string("\"")(i)?;
  let (upto, _) = tag("\"")(i)?;
  Ok((upto, Literal::Text(s)))
}
pub fn parse_char(from: Span) -> IResult<Span, Literal, ParseError<Span>> {
  let (upto, c) = delimited(tag("'"), parse_string("'"), tag("'"))(from)?;
  let s: Vec<char> = c.chars().collect();
  if s.len() != 1 {
    Err(Err::Error(ParseError::new(
      upto,
      ParseErrorKind::ExpectedSingleChar(s),
    )))
  }
  else {
    Ok((upto, Literal::Char(s[0])))
  }
}

pub fn parse_bits(from: Span) -> IResult<Span, Literal, ParseError<Span>> {
  let (i, base) = terminated(parse_base_code(), tag("\""))(from)?;
  let (i, bytes) = parse_base_bytes(base)(i)?;
  let (upto, _) = context("close quotes", tag("\""))(i)?;
  Ok((upto, Literal::BitString(bytes)))
}
