use yatima_core::{
  defs::{
    Def,
    Defs,
  },
  package::Entry,
  parse::{
    package::parse_entry,
    span::Span,
    term::{
      input_cid,
      parse_expression,
    },
  },
  term::Term,
};

use crate::file::{
  error,
  error::FileError,
};

use im::Vector;

use libipld::Cid;
use nom::{
  self,
  branch::alt,
  bytes::complete::tag,
  combinator::{
    eof,
    map,
    opt,
    peek,
    success,
    value,
  },
  error::context,
  multi::{
    many0,
    many1,
    separated_list1,
  },
  sequence::{
    delimited,
    preceded,
    terminated,
  },
  Err,
  IResult,
};

pub enum Command {
  Eval(Box<Term>),
  Type(Box<Term>),
  Browse,
  // Help,
  Define(Box<(String, Def, Entry)>),
  // Type,
  // Load,
  // Import,
  Quit,
}

pub fn parse_eval(
  input: Cid,
  defs: Defs,
) -> impl Fn(Span) -> IResult<Span, Command, FileError<Span>> {
  move |i: Span| {
    let (i, trm) =
      parse_expression(input, defs.clone(), None, Vector::new())(i)
        .map_err(error::convert)?;
    Ok((i, Command::Eval(Box::new(trm))))
  }
}

pub fn parse_type(
  input: Cid,
  defs: Defs,
) -> impl Fn(Span) -> IResult<Span, Command, FileError<Span>> {
  move |i: Span| {
    let (i, _) = alt((tag(":t"), tag(":type")))(i)?;
    let (i, trm) =
      parse_expression(input, defs.clone(), None, Vector::new())(i)
        .map_err(error::convert)?;
    Ok((i, Command::Type(Box::new(trm))))
  }
}

pub fn parse_define(
  input: Cid,
  defs: Defs,
) -> impl Fn(Span) -> IResult<Span, Command, FileError<Span>> {
  move |i: Span| {
    let (i, res) =
      parse_entry(input, defs.clone())(i).map_err(error::convert)?;
    Ok((i, Command::Define(Box::new(res))))
  }
}

pub fn parse_quit() -> impl Fn(Span) -> IResult<Span, Command, FileError<Span>>
{
  move |i: Span| {
    let (i, _) = alt((tag(":q"), tag(":quit")))(i)?;
    Ok((i, Command::Quit))
  }
}

pub fn parse_browse() -> impl Fn(Span) -> IResult<Span, Command, FileError<Span>>
{
  move |i: Span| {
    let (i, _) = alt((tag(":b"), tag(":browse")))(i)?;
    Ok((i, Command::Browse))
  }
}

pub fn parse_command(
  input: Cid,
  defs: Defs,
) -> impl Fn(Span) -> IResult<Span, Command, FileError<Span>> {
  move |i: Span| {
    alt((
      parse_quit(),
      parse_browse(),
      parse_type(input, defs.clone()),
      parse_define(input, defs.clone()),
      parse_eval(input, defs.clone()),
    ))(i)
  }
}
