use crate::{
  parse::{
    error::{
      throw_err,
      ParseError,
      ParseErrorKind,
    },
    term::{
      parse_builtin_symbol_end,
      parse_name,
    },
  },
  position::Pos,
  prim::{
    bytes::BytesOp,
    char::CharOp,
    int::IntOp,
    nat::NatOp,
    text::TextOp,
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

pub fn parse_opr(
  input: Cid,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (upto, op) = alt((
      preceded(tag("#Nat."), parse_nat_op()),
      preceded(tag("#Int."), parse_int_op()),
      preceded(tag("#Bytes."), parse_bytes_op()),
      preceded(tag("#Text."), parse_text_op()),
      preceded(tag("#Char."), parse_char_op()),
      preceded(tag("#U8."), parse_u8_op()),
    ))(from)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Term::Opr(pos, op)))
  }
}
