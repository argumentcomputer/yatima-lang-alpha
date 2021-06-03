use crate::{
  parse::{
    error::{
      ParseError,
      ParseErrorKind,
    },
    term::parse_name,
  },
  position::Pos,
  prim::{
    bits::BitsOp,
    bool::BoolOp,
    bytes::BytesOp,
    char::CharOp,
    i128::I128Op,
    i16::I16Op,
    i32::I32Op,
    i64::I64Op,
    i8::I8Op,
    int::IntOp,
    nat::NatOp,
    text::TextOp,
    u128::U128Op,
    u16::U16Op,
    u32::U32Op,
    u64::U64Op,
    u8::U8Op,
  },
  term::{
    Op,
    Term,
  },
};

use libipld::Cid;

use crate::parse::span::Span;

use nom::{
  branch::alt,
  bytes::complete::tag,
  sequence::preceded,
  Err,
  IResult,
};

pub fn parse_nat_op() -> impl Fn(Span) -> IResult<Span, Op, ParseError<Span>> {
  move |from: Span| {
    let (i, name) = parse_name(from)?;
    match NatOp::from_symbol(&name) {
      Some(op) => Ok((i, Op::Nat(op))),
      None => {
        Err(Err::Error(ParseError::new(i, ParseErrorKind::UnknownNatOp(name))))
      }
    }
  }
}
pub fn parse_int_op() -> impl Fn(Span) -> IResult<Span, Op, ParseError<Span>> {
  move |from: Span| {
    let (i, name) = parse_name(from)?;
    match IntOp::from_symbol(&name) {
      Some(op) => Ok((i, Op::Int(op))),
      None => {
        Err(Err::Error(ParseError::new(i, ParseErrorKind::UnknownIntOp(name))))
      }
    }
  }
}

pub fn parse_bits_op() -> impl Fn(Span) -> IResult<Span, Op, ParseError<Span>> {
  move |from: Span| {
    let (i, name) = parse_name(from)?;
    match BitsOp::from_symbol(&name) {
      Some(op) => Ok((i, Op::Bits(op))),
      None => {
        Err(Err::Error(ParseError::new(i, ParseErrorKind::UnknownBitsOp(name))))
      }
    }
  }
}

pub fn parse_bytes_op() -> impl Fn(Span) -> IResult<Span, Op, ParseError<Span>>
{
  move |from: Span| {
    let (i, name) = parse_name(from)?;
    match BytesOp::from_symbol(&name) {
      Some(op) => Ok((i, Op::Bytes(op))),
      None => Err(Err::Error(ParseError::new(
        i,
        ParseErrorKind::UnknownBytesOp(name),
      ))),
    }
  }
}

pub fn parse_text_op() -> impl Fn(Span) -> IResult<Span, Op, ParseError<Span>> {
  move |from: Span| {
    let (i, name) = parse_name(from)?;
    match TextOp::from_symbol(&name) {
      Some(op) => Ok((i, Op::Text(op))),
      None => {
        Err(Err::Error(ParseError::new(i, ParseErrorKind::UnknownTextOp(name))))
      }
    }
  }
}

pub fn parse_char_op() -> impl Fn(Span) -> IResult<Span, Op, ParseError<Span>> {
  move |from: Span| {
    let (i, name) = parse_name(from)?;
    match CharOp::from_symbol(&name) {
      Some(op) => Ok((i, Op::Char(op))),
      None => {
        Err(Err::Error(ParseError::new(i, ParseErrorKind::UnknownCharOp(name))))
      }
    }
  }
}
pub fn parse_bool_op() -> impl Fn(Span) -> IResult<Span, Op, ParseError<Span>> {
  move |from: Span| {
    let (i, name) = parse_name(from)?;
    match BoolOp::from_symbol(&name) {
      Some(op) => Ok((i, Op::Bool(op))),
      None => {
        Err(Err::Error(ParseError::new(i, ParseErrorKind::UnknownBoolOp(name))))
      }
    }
  }
}

pub fn parse_u8_op() -> impl Fn(Span) -> IResult<Span, Op, ParseError<Span>> {
  move |from: Span| {
    let (i, name) = parse_name(from)?;
    match U8Op::from_symbol(&name) {
      Some(op) => Ok((i, Op::U8(op))),
      None => {
        Err(Err::Error(ParseError::new(i, ParseErrorKind::UnknownU8Op(name))))
      }
    }
  }
}

pub fn parse_u16_op() -> impl Fn(Span) -> IResult<Span, Op, ParseError<Span>> {
  move |from: Span| {
    let (i, name) = parse_name(from)?;
    match U16Op::from_symbol(&name) {
      Some(op) => Ok((i, Op::U16(op))),
      None => {
        Err(Err::Error(ParseError::new(i, ParseErrorKind::UnknownU16Op(name))))
      }
    }
  }
}

pub fn parse_u32_op() -> impl Fn(Span) -> IResult<Span, Op, ParseError<Span>> {
  move |from: Span| {
    let (i, name) = parse_name(from)?;
    match U32Op::from_symbol(&name) {
      Some(op) => Ok((i, Op::U32(op))),
      None => {
        Err(Err::Error(ParseError::new(i, ParseErrorKind::UnknownU32Op(name))))
      }
    }
  }
}

pub fn parse_u64_op() -> impl Fn(Span) -> IResult<Span, Op, ParseError<Span>> {
  move |from: Span| {
    let (i, name) = parse_name(from)?;
    match U64Op::from_symbol(&name) {
      Some(op) => Ok((i, Op::U64(op))),
      None => {
        Err(Err::Error(ParseError::new(i, ParseErrorKind::UnknownU64Op(name))))
      }
    }
  }
}

pub fn parse_u128_op() -> impl Fn(Span) -> IResult<Span, Op, ParseError<Span>> {
  move |from: Span| {
    let (i, name) = parse_name(from)?;
    match U128Op::from_symbol(&name) {
      Some(op) => Ok((i, Op::U128(op))),
      None => {
        Err(Err::Error(ParseError::new(i, ParseErrorKind::UnknownU128Op(name))))
      }
    }
  }
}
pub fn parse_i8_op() -> impl Fn(Span) -> IResult<Span, Op, ParseError<Span>> {
  move |from: Span| {
    let (i, name) = parse_name(from)?;
    match I8Op::from_symbol(&name) {
      Some(op) => Ok((i, Op::I8(op))),
      None => {
        Err(Err::Error(ParseError::new(i, ParseErrorKind::UnknownI8Op(name))))
      }
    }
  }
}
pub fn parse_i16_op() -> impl Fn(Span) -> IResult<Span, Op, ParseError<Span>> {
  move |from: Span| {
    let (i, name) = parse_name(from)?;
    match I16Op::from_symbol(&name) {
      Some(op) => Ok((i, Op::I16(op))),
      None => {
        Err(Err::Error(ParseError::new(i, ParseErrorKind::UnknownI16Op(name))))
      }
    }
  }
}
pub fn parse_i32_op() -> impl Fn(Span) -> IResult<Span, Op, ParseError<Span>> {
  move |from: Span| {
    let (i, name) = parse_name(from)?;
    match I32Op::from_symbol(&name) {
      Some(op) => Ok((i, Op::I32(op))),
      None => {
        Err(Err::Error(ParseError::new(i, ParseErrorKind::UnknownI32Op(name))))
      }
    }
  }
}
pub fn parse_i64_op() -> impl Fn(Span) -> IResult<Span, Op, ParseError<Span>> {
  move |from: Span| {
    let (i, name) = parse_name(from)?;
    match I64Op::from_symbol(&name) {
      Some(op) => Ok((i, Op::I64(op))),
      None => {
        Err(Err::Error(ParseError::new(i, ParseErrorKind::UnknownI64Op(name))))
      }
    }
  }
}
pub fn parse_i128_op() -> impl Fn(Span) -> IResult<Span, Op, ParseError<Span>> {
  move |from: Span| {
    let (i, name) = parse_name(from)?;
    match I128Op::from_symbol(&name) {
      Some(op) => Ok((i, Op::I128(op))),
      None => {
        Err(Err::Error(ParseError::new(i, ParseErrorKind::UnknownI128Op(name))))
      }
    }
  }
}

pub fn parse_opr(
  input: Cid,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (upto, op) = alt((
      preceded(tag("#Nat."), parse_nat_op()),
      preceded(tag("#Int."), parse_int_op()),
      preceded(tag("#Bool."), parse_bool_op()),
      preceded(tag("#Bits."), parse_bits_op()),
      preceded(tag("#Bytes."), parse_bytes_op()),
      preceded(tag("#Text."), parse_text_op()),
      preceded(tag("#Char."), parse_char_op()),
      preceded(tag("#U8."), parse_u8_op()),
      preceded(tag("#U16."), parse_u16_op()),
      preceded(tag("#U32."), parse_u32_op()),
      preceded(tag("#U64."), parse_u64_op()),
      preceded(tag("#U128."), parse_u128_op()),
      preceded(tag("#I8."), parse_i8_op()),
      preceded(tag("#I16."), parse_i16_op()),
      preceded(tag("#I32."), parse_i32_op()),
      preceded(tag("#I64."), parse_i64_op()),
      preceded(tag("#I128."), parse_i128_op()),
    ))(from)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Term::Opr(pos, op)))
  }
}
