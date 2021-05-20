use num_bigint::{
  BigInt,
  BigUint,
  Sign,
};

use crate::literal::Literal;

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
  let (i, digits) = base::parse_litbase_digits(base)(i)?;
  let (upto, suffix) =
    opt(alt((tag("u8"), tag("u16"), tag("u32"), tag("u64"), tag("u128"))))(i)?;
  match suffix {
    None => match base_x::decode(base.base_digits(), digits.fragment()) {
      Ok(bytes) => Ok((upto, Literal::Nat(BigUint::from_bytes_be(&bytes)))),
      Err(_) => Err(nom::Err::Error(ParseError::new(
        i,
        ParseErrorKind::InvalidBaseEncoding(base),
      ))),
    },
    Some(suffix) => match *suffix.fragment() {
      "u8" => {
        use ParseErrorKind::ParseIntErr;
        let x = u8::from_str_radix(&digits, base.radix()).map_or_else(
          |e| Err(Err::Error(ParseError::new(from, ParseIntErr(e)))),
          Ok,
        )?;
        Ok((upto, Literal::U8(x)))
      }
      "u16" => {
        use ParseErrorKind::ParseIntErr;
        let x = u16::from_str_radix(&digits, base.radix()).map_or_else(
          |e| Err(Err::Error(ParseError::new(from, ParseIntErr(e)))),
          Ok,
        )?;
        Ok((upto, Literal::U16(x)))
      }
      "u32" => {
        use ParseErrorKind::ParseIntErr;
        let x = u32::from_str_radix(&digits, base.radix()).map_or_else(
          |e| Err(Err::Error(ParseError::new(from, ParseIntErr(e)))),
          Ok,
        )?;
        Ok((upto, Literal::U32(x)))
      }
      "u64" => {
        use ParseErrorKind::ParseIntErr;
        let x = u64::from_str_radix(&digits, base.radix()).map_or_else(
          |e| Err(Err::Error(ParseError::new(from, ParseIntErr(e)))),
          Ok,
        )?;
        Ok((upto, Literal::U64(x)))
      }
      "u128" => {
        use ParseErrorKind::ParseIntErr;
        let x = u128::from_str_radix(&digits, base.radix()).map_or_else(
          |e| Err(Err::Error(ParseError::new(from, ParseIntErr(e)))),
          Ok,
        )?;
        Ok((upto, Literal::U128(x)))
      }
      _ => panic!("implementation error in parse_nat"),
    },
  }
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
  let (i, digits) = base::parse_litbase_digits(base)(i)?;
  let (upto, suffix) =
    opt(alt((tag("i8"), tag("i16"), tag("i32"), tag("i64"), tag("i128"))))(i)?;
  match suffix {
    None => match base_x::decode(base.base_digits(), digits.fragment()) {
      Ok(bytes) => Ok((upto, Literal::Int(BigInt::from_bytes_be(s, &bytes)))),
      Err(_) => Err(nom::Err::Error(ParseError::new(
        i,
        ParseErrorKind::InvalidBaseEncoding(base),
      ))),
    },
    Some(suffix) => {
      let digits = match s {
        Sign::Plus => format!("+{}", digits),
        _ => format!("-{}", digits),
      };
      match *suffix.fragment() {
        "i8" => {
          use ParseErrorKind::ParseIntErr;
          let x = i8::from_str_radix(&digits, base.radix()).map_or_else(
            |e| Err(Err::Error(ParseError::new(from, ParseIntErr(e)))),
            Ok,
          )?;
          Ok((upto, Literal::I8(x)))
        }
        "i16" => {
          use ParseErrorKind::ParseIntErr;
          let x = i16::from_str_radix(&digits, base.radix()).map_or_else(
            |e| Err(Err::Error(ParseError::new(from, ParseIntErr(e)))),
            Ok,
          )?;
          Ok((upto, Literal::I16(x)))
        }
        "i32" => {
          use ParseErrorKind::ParseIntErr;
          let x = i32::from_str_radix(&digits, base.radix()).map_or_else(
            |e| Err(Err::Error(ParseError::new(from, ParseIntErr(e)))),
            Ok,
          )?;
          Ok((upto, Literal::I32(x)))
        }
        "i64" => {
          use ParseErrorKind::ParseIntErr;
          let x = i64::from_str_radix(&digits, base.radix()).map_or_else(
            |e| Err(Err::Error(ParseError::new(from, ParseIntErr(e)))),
            Ok,
          )?;
          Ok((upto, Literal::I64(x)))
        }
        "i128" => {
          use ParseErrorKind::ParseIntErr;
          let x = i128::from_str_radix(&digits, base.radix()).map_or_else(
            |e| Err(Err::Error(ParseError::new(from, ParseIntErr(e)))),
            Ok,
          )?;
          Ok((upto, Literal::I128(x)))
        }
        _ => panic!("implementation error in parse_nat"),
      }
    }
  }
}

pub fn parse_text(from: Span) -> IResult<Span, Literal, ParseError<Span>> {
  let (i, _) = context("open quotes", tag("\""))(from)?;
  let (i, s) = parse_string("\"")(i)?;
  let (upto, _) = tag("\"")(i)?;
  Ok((upto, Literal::Text(s.into())))
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
  let (i, base) = terminated(base::parse_litbase_code(), tag("\'"))(from)?;
  let (i, bytes) = opt(base::parse_litbase_bytes(base))(i)?;
  let (upto, _) = context("close quotes", tag("\'"))(i)?;
  match bytes {
    Some(bytes) => Ok((upto, Literal::Bytes(bytes.into()))),
    None => Ok((upto, Literal::Bytes(vec![].into()))),
  }
}

pub fn parse_bool(from: Span) -> IResult<Span, Literal, ParseError<Span>> {
  alt((
    value(Literal::Bool(true), tag("#Bool.true")),
    value(Literal::Bool(false), tag("#Bool.false")),
  ))(from)
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
