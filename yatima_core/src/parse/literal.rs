use num_bigint::{
  BigInt,
  BigUint,
  Sign,
};

use crate::{
  literal::Literal,
  text,
};

use crate::parse::{
  base,
  span::Span,
  string::*,
};

use crate::parse::error::{
  ParseError,
  ParseErrorKind,
};

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
  let (i, base) = opt(preceded(tag("0"), base::parse_litbase_code()))(from)?;
  let base = base.unwrap_or(base::LitBase::Dec);
  let (upto, bytes) = base::parse_litbase_bytes(base)(i)?;
  Ok((upto, Literal::Nat(BigUint::from_bytes_be(&bytes))))
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
  let (i, base) = opt(preceded(tag("0"), base::parse_litbase_code()))(i)?;
  let base = base.unwrap_or(base::LitBase::Dec);
  let (upto, bytes) = base::parse_litbase_bytes(base)(i)?;
  Ok((upto, Literal::Int(BigInt::from_bytes_be(s, &bytes))))
}

pub fn parse_text(from: Span) -> IResult<Span, Literal, ParseError<Span>> {
  let (i, _) = context("open quotes", tag("\""))(from)?;
  let (i, s) = parse_string("\"")(i)?;
  let (upto, _) = tag("\"")(i)?;
  Ok((upto, Literal::Text(text::from_string(s))))
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

pub fn parse_bytes(from: Span) -> IResult<Span, Literal, ParseError<Span>> {
  let (i, base) = terminated(base::parse_litbase_code(), tag("\""))(from)?;
  let (i, bytes) = opt(base::parse_litbase_bytes(base))(i)?;
  let (upto, _) = context("close quotes", tag("\""))(i)?;
  match bytes {
    Some(bytes) => Ok((upto, Literal::Bytes(bytes))),
    None => Ok((upto, Literal::Bytes(vec![]))),
  }
}
#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn test_parse_nat() {
    let res = parse_nat(Span::new("0b0"));
    assert_eq!(res.unwrap().1, Literal::Nat(0u64.into()));
    let res = parse_nat(Span::new("0b1110"));
    assert_eq!(res.unwrap().1, Literal::Nat(0b1110u64.into()));
    let res = parse_nat(Span::new("0b001110"));
    assert_eq!(res.unwrap().1, Literal::Nat(0b1110u64.into()));
    let res = parse_nat(Span::new("0o0"));
    assert_eq!(res.unwrap().1, Literal::Nat(0u64.into()));
    let res = parse_nat(Span::new("0o123"));
    assert_eq!(res.unwrap().1, Literal::Nat(0o123u64.into()));
    let res = parse_nat(Span::new("0d0"));
    assert_eq!(res.unwrap().1, Literal::Nat(0u64.into()));
    let res = parse_nat(Span::new("0d15"));
    assert_eq!(res.unwrap().1, Literal::Nat(15u64.into()));
    let res = parse_nat(Span::new("0"));
    assert_eq!(res.unwrap().1, Literal::Nat(0u64.into()));
    let res = parse_nat(Span::new("15"));
    assert_eq!(res.unwrap().1, Literal::Nat(15u64.into()));
  }
}
