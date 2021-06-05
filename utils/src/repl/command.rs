use yatima_core::{
  defs::{
    Def,
    Defs,
  },
  name::Name,
  package::Entry,
  parse::{
    package::{
      parse_entry,
      parse_link,
    },
    error::{
      ParseError,
      ParseErrorKind,
    },
    span::Span,
    term::{
      is_valid_symbol_string,
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

use std::{
  collections::VecDeque,
  rc::Rc,
};

use libipld::Cid;
use nom::{
  self,
  Err,
  branch::alt,
  bytes::complete::{
    tag,
    take_till1,
  },
  IResult,
};

pub enum Command {
  Eval(Box<Term>),
  Type(Box<Term>),
  Browse,
  // Help,
  Define(Box<(Name, Def, Entry)>),
  Show { typ_ : String, link: Cid },
  // Type,
  Load(Name),
  // Import,
  Quit,
}

pub fn parse_eval(
  input: Cid,
  defs: Defs,
) -> impl Fn(Span) -> IResult<Span, Command, FileError<Span>> {
  move |i: Span| {
    let (i, trm) = parse_expression(
      input,
      defs.clone(),
      None,
      Rc::new(VecDeque::new()),
      VecDeque::new(),
    )(i)
    .map_err(error::convert)?;
    Ok((i, Command::Eval(Box::new(trm))))
  }
}

pub fn parse_type(
  input: Cid,
  defs: Defs,
) -> impl Fn(Span) -> IResult<Span, Command, FileError<Span>> {
  move |i: Span| {
    let (i, _) = alt((tag(":type"), tag(":t")))(i)?;
    let (i, trm) = parse_expression(
      input,
      defs.clone(),
      None,
      Rc::new(VecDeque::new()),
      VecDeque::new(),
    )(i)
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

pub fn parse_word(from: Span) -> IResult<Span, String, ParseError<Span>>
{
  let (i, s) = take_till1(|x| {
    char::is_whitespace(x)
  })(from)?;
  let s: String = String::from(s.fragment().to_owned());

  if !is_valid_symbol_string(&s) {
    Err(Err::Error(ParseError::new(from, ParseErrorKind::InvalidSymbol(s))))
  } else {
    Ok((i, s))
  }
}


pub fn parse_show() -> impl Fn(Span) -> IResult<Span, Command, FileError<Span>>
{
  move |i: Span| {
    let (i, _) = alt((tag(":show"), tag(":s")))(i)?;
    let (i, _) = parse_space1(i).map_err(error::convert)?;
    let (i, typ_) = parse_word(i).map_err(error::convert)?;
    let (i, _) = parse_space1(i).map_err(error::convert)?;
    let (i, link) = parse_link(i).map_err(error::convert)?;
    Ok((i, Command::Show { link, typ_ }))
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
      parse_load(),
      parse_show(),
      parse_type(input, defs.clone()),
      parse_define(input, defs.clone()),
      parse_eval(input, defs.clone()),
    ))(i)
  }
}
