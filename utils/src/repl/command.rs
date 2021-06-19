use sp_im::ConsList;
use yatima_core::{
  defs::{
    Def,
    Defs,
  },
  name::Name,
  package::Entry,
  parse::{
    error::{
      ParseError,
      ParseErrorKind,
    },
    package::{
      parse_entry,
      parse_link,
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

use multiaddr::Multiaddr;
use sp_std::{
  cell::RefCell,
  collections::vec_deque::VecDeque,
  rc::Rc,
  convert::TryFrom,
};

use cid::Cid;
use nom::{
  self,
  branch::alt,
  bytes::complete::{
    tag,
    take_till1,
  },
  Err,
  combinator::value,
  IResult,
};

/// A reference for something that can be loaded
#[derive(Debug, Clone)]
pub enum Reference {
  Cid(Cid),
  FileName(Name),
  Multiaddr(Multiaddr),
}

pub enum Command {
  Eval(Box<Term>),
  Type(Box<Term>),
  Set(String, bool),
  Browse,
  // Help,
  Define(Box<(Name, Def, Entry)>),
  Show { typ_: String, link: Cid },
  // Type,
  Load(Reference),
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
    let (i, mut res) =
      parse_entry(input, defs.clone())(i).map_err(error::convert)?;
    let res = res.pop().unwrap();
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

pub fn parse_multiaddr(
  from: Span,
) -> IResult<Span, Reference, ParseError<Span>> {
  let (i, s) = take_till1(|x| char::is_whitespace(x))(from)?;
  let s: String = String::from(s.fragment().to_owned());
  match Multiaddr::try_from(s) {
    Ok(addr) => Ok((i, Reference::Multiaddr(addr))),
    Err(e) => {
      Err(Err::Error(ParseError::new(from, ParseErrorKind::InvalidSymbol(e.to_string()))))
    }
  }
}

pub fn parse_cid_reference(from: Span) -> IResult<Span, Reference, ParseError<Span>>
{
  let (i, cid) = parse_link(from)?;
  Ok((i, Reference::Cid(cid)))
}

pub fn parse_name_reference(from: Span) -> IResult<Span, Reference, ParseError<Span>>
{
  let (i, name) = parse_name(from)?;
  Ok((i, Reference::FileName(name)))
}

/// Parse the :load command
pub fn parse_load() -> impl Fn(Span) -> IResult<Span, Command, FileError<Span>>
{
  move |i: Span| {
    let (i, _) = alt((tag(":load"), tag(":l")))(i)?;
    let (i, _) = parse_space1(i).map_err(error::convert)?;
    let (i, reference) = alt((parse_multiaddr, parse_name_reference, parse_cid_reference))(i).map_err(error::convert)?;
    Ok((i, Command::Load(reference)))
  }
}

pub fn parse_word(from: Span) -> IResult<Span, String, ParseError<Span>> {
  let (i, s) = take_till1(|x| char::is_whitespace(x))(from)?;
  let s: String = String::from(s.fragment().to_owned());

  if !is_valid_symbol_string(&s) {
    Err(Err::Error(ParseError::new(from, ParseErrorKind::InvalidSymbol(s))))
  }
  else {
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
  defs: Rc<RefCell<Defs>>,
) -> impl Fn(Span) -> IResult<Span, Command, FileError<Span>> {
  move |i: Span| {
    alt((
      parse_quit(),
      parse_browse(),
      parse_set(),
      parse_load(),
      parse_show(),
      parse_type(input, defs.clone()),
      parse_define(input, defs.clone()),
      parse_eval(input, defs.clone()),
    ))(i)
  }
}
