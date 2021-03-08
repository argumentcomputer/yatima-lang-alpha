use num_bigint::{
  BigInt,
  BigUint,
  Sign,
};

use crate::core::literal::{
  LitType,
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
  combinator::{
    opt,
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

pub fn parse_int(from: Span) -> IResult<Span, Literal, ParseError<Span>> {
  let (i, s) =
    alt((value(Sign::Minus, tag("-")), value(Sign::Plus, tag("+"))))(from)?;
  let (i, base) = opt(preceded(tag("0"), parse_base_code()))(i)?;
  let base = base.unwrap_or(Base::_10);
  let (upto, bytes) = parse_base_bytes(base)(i)?;
  Ok((upto, Literal::Integer(BigInt::from_bytes_be(s, &bytes))))
}

pub fn parse_text(from: Span) -> IResult<Span, Literal, ParseError<Span>> {
  let (upto, val) = delimited(tag("\""), parse_string("\""), tag("\""))(from)?;
  Ok((upto, Literal::Text(val)))
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
  let (upto, _) = tag("\"")(i)?;
  Ok((upto, Literal::BitString(bytes)))
}
