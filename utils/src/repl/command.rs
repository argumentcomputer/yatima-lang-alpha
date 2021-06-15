use sp_im::ConsList;
use yatima_core::{
  defs::{
    Def,
    Defs,
  },
  name::Name,
  package::Entry,
  parse::{
    package::parse_entry,
    span::Span,
    term::{
      parse_expression,
      parse_name,
      parse_space1,
    },
  },
  term::Term,
};

use crate::file::{
  error,
  error::FileError,
};

use sp_std::{
  cell::RefCell,
  collections::vec_deque::VecDeque,
  rc::Rc,
};

use cid::Cid;
use nom::{
  self,
  branch::alt,
  bytes::complete::tag,
  combinator::value,
  IResult,
};

pub enum Command {
  Eval(Box<Term>),
  Type(Box<Term>),
  Set(String, bool),
  Browse,
  // Help,
  Define(Box<(Name, Def, Entry)>),
  Load(Name),
  // Import,
  Quit,
}

pub fn parse_eval(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
) -> impl Fn(Span) -> IResult<Span, Command, FileError<Span>> {
  move |i: Span| {
    let (i, trm) = parse_expression(
      input,
      defs.clone(),
      None,
      ConsList::new(),
      Rc::new(VecDeque::new()),
    )(i)
    .map_err(error::convert)?;
    Ok((i, Command::Eval(Box::new(trm))))
  }
}

pub fn parse_set() -> impl Fn(Span) -> IResult<Span, Command, FileError<Span>> {
  move |i: Span| {
    let (i, _) = alt((tag(":set"), tag(":s")))(i)?;
    let (i, _) = parse_space1(i).map_err(error::convert)?;
    // let (i, _) = alt((tag("type-system")))(i)?;
    let (i, s) = parse_name(i).map_err(error::convert)?;
    let (i, _) = parse_space1(i).map_err(error::convert)?;
    let (i, b) = alt((
      value(true, tag("on")),
      value(true, tag("true")),
      value(true, tag("yes")),
      value(false, tag("off")),
      value(false, tag("false")),
      value(false, tag("no")),
    ))(i)?;
    Ok((i, Command::Set(s.to_string(), b)))
  }
}

pub fn parse_type(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
) -> impl Fn(Span) -> IResult<Span, Command, FileError<Span>> {
  move |i: Span| {
    let (i, _) = alt((tag(":type"), tag(":t")))(i)?;
    let (i, trm) = parse_expression(
      input,
      defs.clone(),
      None,
      ConsList::new(),
      Rc::new(VecDeque::new()),
    )(i)
    .map_err(error::convert)?;
    Ok((i, Command::Type(Box::new(trm))))
  }
}

pub fn parse_define(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
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
    let (i, _) = alt((tag(":quit"), tag(":q")))(i)?;
    Ok((i, Command::Quit))
  }
}

pub fn parse_browse() -> impl Fn(Span) -> IResult<Span, Command, FileError<Span>>
{
  move |i: Span| {
    let (i, _) = alt((tag(":browse"), tag(":b")))(i)?;
    Ok((i, Command::Browse))
  }
}

pub fn parse_load() -> impl Fn(Span) -> IResult<Span, Command, FileError<Span>>
{
  move |i: Span| {
    let (i, _) = alt((tag(":load"), tag(":l")))(i)?;
    let (i, _) = parse_space1(i).map_err(error::convert)?;
    let (i, name) = parse_name(i).map_err(error::convert)?;
    Ok((i, Command::Load(name)))
  }
}

pub fn parse_command(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
) -> impl Fn(Span) -> IResult<Span, Command, FileError<Span>> {
  move |i: Span| {
    alt((
      parse_quit(),
      parse_browse(),
      parse_set(),
      parse_load(),
      parse_type(input, defs.clone()),
      parse_define(input, defs.clone()),
      parse_eval(input, defs.clone()),
    ))(i)
  }
}
