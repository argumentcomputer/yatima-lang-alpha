use crate::{
  parse::error::ParseError,
  term::Term,
};

use im::Vector;
use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::{
    alphanumeric0,
    multispace0,
    multispace1,
    satisfy,
  },
  multi::{
    separated_list0,
    separated_list1,
  },
  sequence::{
    delimited,
    preceded,
  },
  Err,
  IResult,
};

pub fn parse_nam(i: &str) -> IResult<&str, String, ParseError<&str>> {
  fn init_char(x: char) -> bool { x.is_alphabetic() || x == '_' }
  let (i, init) = satisfy(init_char)(i)?;
  let (i, rest) = alphanumeric0(i)?;
  Ok((i, format!("{}{}", init, rest)))
}

pub fn parse_var(
  ctx: Vector<String>,
) -> impl Fn(&str) -> IResult<&str, Term, ParseError<&str>> {
  move |i: &str| {
    let (i, nam) = parse_nam(i)?;
    let (i, idx) = match ctx.iter().enumerate().find(|(_, x)| **x == nam) {
      Some((idx, _)) => Ok((i, idx)),
      None => {
        Err(Err::Error(ParseError::FreeVariable(i, nam.clone(), ctx.clone())))
      }
    }?;
    Ok((i, Term::Var(None, nam.clone(), idx as u64)))
  }
}
pub fn parse_lam(
  ctx: Vector<String>,
) -> impl Fn(&str) -> IResult<&str, Term, ParseError<&str>> {
  move |i: &str| {
    let (i, _) = nom::character::complete::char('λ')(i)?;
    let (i, _) = multispace0(i)?;
    let (i, ns) = separated_list1(multispace1, parse_nam)(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag("=>")(i)?;
    let (i, _) = multispace0(i)?;
    let mut ctx2 = ctx.clone();
    for n in ns.clone().into_iter() {
      ctx2.push_front(n);
    }
    let (i, bod) = parse_term(ctx2)(i)?;
    let trm = ns
      .iter()
      .rev()
      .fold(bod, |acc, n| Term::Lam(None, n.clone(), Box::new(acc)));
    Ok((i, trm))
  }
}
pub fn parse_term(
  ctx: Vector<String>,
) -> impl Fn(&str) -> IResult<&str, Term, ParseError<&str>> {
  move |i: &str| {
    let (i, fun) = parse_term_inner(ctx.clone())(i)?;
    let (i, _) = multispace0(i)?;
    let (i, args) =
      separated_list0(multispace1, parse_term_inner(ctx.clone()))(i)?;
    let trm = args
      .into_iter()
      .fold(fun, |acc, arg| Term::App(None, Box::new(acc), Box::new(arg)));
    Ok((i, trm))
  }
}
pub fn parse_term_inner(
  ctx: Vector<String>,
) -> impl Fn(&str) -> IResult<&str, Term, ParseError<&str>> {
  move |i: &str| {
    alt((
      delimited(
        preceded(tag("("), multispace0),
        parse_term(ctx.clone()),
        tag(")"),
      ),
      parse_lam(ctx.clone()),
      parse_var(ctx.clone()),
    ))(i)
  }
}
pub fn parse(i: &str) -> IResult<&str, Term, ParseError<&str>> {
  parse_term(Vector::new())(i)
}
