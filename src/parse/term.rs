use crate::{
  parse::error::ParseError,
  term::{
    Refs,
    Term,
    Uses,
  },
};

use hashexpr::{
  position::Pos,
  span::Span,
};
use std::collections::HashMap;

use im::Vector;
use nom::{
  branch::alt,
  bytes::complete::{
    tag,
    take_till1,
  },
  character::complete::{
    alphanumeric0,
    multispace0,
    multispace1,
    satisfy,
  },
  combinator::{
    map,
    success,
    value,
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

pub fn reserved_symbols() -> Vector<String> {
  Vector::from(vec![
    String::from("λ"),
    String::from("=>"),
    String::from("∀"),
    String::from("->"),
    String::from("@"),
    String::from("data"),
    String::from("case"),
    String::from("Type"),
    String::from("%eql"),
    String::from("%lth"),
    String::from("%lte"),
    String::from("%gth"),
    String::from("%gte"),
    String::from("%or"),
    String::from("%and"),
    String::from("%xor"),
    String::from("%not"),
    String::from("%suc"),
    String::from("%pre"),
    String::from("%add"),
    String::from("%sub"),
    String::from("%mul"),
    String::from("%div"),
    String::from("%mod"),
    String::from("%shl"),
    String::from("%shr"),
    String::from("%rol"),
    String::from("%ror"),
    String::from("%clz"),
    String::from("%ctz"),
    String::from("%cnt"),
    String::from("%len"),
    String::from("%cat"),
    String::from("%cst"),
    String::from("%Natural"),
    String::from("%Integer"),
    String::from("%Bits"),
    String::from("%Text"),
    String::from("%Char"),
    String::from("%Link"),
    String::from("%Exception"),
  ])
}

pub fn parse_nam(i: Span) -> IResult<Span, String, ParseError<Span>> {
  let (i, s) = take_till1(|x| {
    char::is_whitespace(x) | (x == ':') | (x == ')') | (x == '(')
  })(i)?;
  let s: String = String::from(s.fragment().to_owned());
  if reserved_symbols().contains(&s) {
    Err(Err::Error(ParseError::ReservedSymbol(i, s)))
  }
  else {
    Ok((i, s))
  }
}

pub fn parse_var(
  refs: Refs,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (upto, nam) = parse_nam(from)?;
    let pos = Some(Pos::from_upto(from, upto));
    match ctx.iter().enumerate().find(|(_, x)| **x == nam) {
      Some((idx, _)) => Ok((upto, Term::Var(pos, nam.clone(), idx as u64))),
      None => match refs.get(&nam) {
        Some((def_link, ast_link)) => {
          Ok((upto, Term::Ref(pos, nam.clone(), *def_link, *ast_link)))
        }
        None => Err(Err::Error(ParseError::UndefinedReference(
          upto,
          nam.clone(),
          ctx.clone(),
        ))),
      },
    }
  }
}

pub fn parse_lam(
  refs: Refs,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = nom::character::complete::char('λ')(from)?;
    let (i, _) = multispace0(i)?;
    let (i, ns) = separated_list1(multispace1, parse_nam)(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag("=>")(i)?;
    let (i, _) = multispace0(i)?;
    let mut ctx2 = ctx.clone();
    for n in ns.clone().into_iter() {
      ctx2.push_front(n);
    }
    let (upto, bod) = parse_term(refs.clone(), ctx2)(i)?;
    let pos = Some(Pos::from_upto(from, upto));
    let trm = ns
      .iter()
      .rev()
      .fold(bod, |acc, n| Term::Lam(pos, n.clone(), Box::new(acc)));
    Ok((upto, trm))
  }
}

pub fn parse_uses(i: Span) -> IResult<Span, Uses, ParseError<Span>> {
  alt((
    value(Uses::None, tag("0")),
    value(Uses::Affi, tag("&")),
    value(Uses::Once, tag("1")),
    success(Uses::Many),
  ))(i)
}

pub fn parse_binder_full(
  refs: Refs,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, (Uses, String, Term), ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("(")(i)?;
    let (i, u) = parse_uses(i)?;
    let (i, _) = multispace1(i)?;
    let (i, n) = parse_nam(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag(":")(i)?;
    let (i, _) = multispace0(i)?;
    let (i, typ) = parse_term(refs.clone(), ctx.clone())(i)?;
    let (i, _) = tag(")")(i)?;
    Ok((i, (u, n, typ)))
  }
}

pub fn parse_binder_short(
  refs: Refs,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, (Uses, String, Term), ParseError<Span>> {
  move |i: Span| {
    map(parse_term(refs.clone(), ctx.clone()), |t| {
      (Uses::Many, String::from(""), t)
    })(i)
  }
}

pub fn parse_binder(
  refs: Refs,
  ctx: Vector<String>,
  nam_opt: bool,
) -> impl Fn(Span) -> IResult<Span, (Uses, String, Term), ParseError<Span>> {
  move |i: Span| {
    if nam_opt {
      alt((
        parse_binder_short(refs.clone(), ctx.clone()),
        parse_binder_full(refs.clone(), ctx.clone()),
      ))(i)
    }
    else {
      parse_binder_full(refs.clone(), ctx.clone())(i)
    }
  }
}

pub fn parse_binders(
  refs: Refs,
  ctx: Vector<String>,
  nam_opt: bool,
) -> impl FnMut(Span) -> IResult<Span, Vec<(Uses, String, Term)>, ParseError<Span>>
{
  move |mut i: Span| {
    let mut ctx = ctx.clone();
    let mut res = Vec::new();

    match parse_binder(refs.clone(), ctx.clone(), nam_opt)(i.clone()) {
      Err(e) => return Err(e),
      Ok((i1, (u, n, t))) => {
        ctx.push_front(n.clone());
        res.push((u, n, t));
        i = i1;
      }
    }

    loop {
      match preceded(
        multispace0,
        parse_binder(refs.clone(), ctx.clone(), nam_opt),
      )(i)
      {
        Err(Err::Error(_)) => return Ok((i, res)),
        Err(e) => return Err(e),
        Ok((i2, (u, n, t))) => {
          ctx.push_front(n.clone());
          res.push((u, n, t));
          i = i2;
        }
      }
    }
  }
}

pub fn parse_all(
  refs: Refs,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = nom::character::complete::char('∀')(from)?;
    let (i, _) = multispace0(i)?;
    let (i, bs) = parse_binders(refs.clone(), ctx.clone(), true)(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag("->")(i)?;
    let (i, _) = multispace0(i)?;
    let mut ctx2 = ctx.clone();
    for (_, n, _) in bs.clone().iter() {
      ctx2.push_front(n.clone());
    }
    let (upto, bod) = parse_term(refs.clone(), ctx2)(i)?;
    let pos = Some(Pos::from_upto(from, upto));
    let trm = bs.into_iter().rev().fold(bod, |acc, (u, n, t)| {
      Term::All(pos, u, n, Box::new(t), Box::new(acc))
    });
    Ok((upto, trm))
  }
}

pub fn parse_type(
  refs: Refs,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (upto, _) = tag("Type")(from)?;
    let pos = Some(Pos::from_upto(from, upto));
    Ok((upto, Term::Typ(pos)))
  }
}

pub fn parse_self(
  refs: Refs,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = nom::character::complete::char('@')(from)?;
    let (i, n) = parse_nam(i)?;
    let (i, _) = multispace1(i)?;
    let mut ctx2 = ctx.clone();
    ctx2.push_front(n.clone());
    let (upto, bod) = parse_term(refs.clone(), ctx2)(i)?;
    let pos = Some(Pos::from_upto(from, upto));
    Ok((upto, Term::Slf(pos, n, Box::new(bod))))
  }
}

pub fn parse_case(
  refs: Refs,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("case")(from)?;
    let (i, _) = multispace1(i)?;
    let (upto, bod) = parse_term(refs.clone(), ctx.clone())(i)?;
    let pos = Some(Pos::from_upto(from, upto));
    Ok((upto, Term::Cse(pos, Box::new(bod))))
  }
}

pub fn parse_data(
  refs: Refs,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("data")(from)?;
    let (i, _) = multispace1(i)?;
    let (upto, bod) = parse_term(refs.clone(), ctx.clone())(i)?;
    let pos = Some(Pos::from_upto(from, upto));
    Ok((upto, Term::Dat(pos, Box::new(bod))))
  }
}

pub fn parse_term(
  refs: Refs,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, fun) = parse_term_inner(refs.clone(), ctx.clone())(from)?;
    let (i, _) = multispace0(i)?;
    let (upto, args) = separated_list0(
      multispace1,
      parse_term_inner(refs.clone(), ctx.clone()),
    )(i)?;
    let pos = Some(Pos::from_upto(from, upto));
    let trm = args
      .into_iter()
      .fold(fun, |acc, arg| Term::App(pos, Box::new(acc), Box::new(arg)));
    Ok((upto, trm))
  }
}
pub fn parse_term_inner(
  refs: Refs,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |i: Span| {
    alt((
      delimited(
        preceded(tag("("), multispace0),
        parse_term(refs.clone(), ctx.clone()),
        tag(")"),
      ),
      parse_self(refs.clone(), ctx.clone()),
      parse_data(refs.clone(), ctx.clone()),
      parse_case(refs.clone(), ctx.clone()),
      parse_all(refs.clone(), ctx.clone()),
      parse_lam(refs.clone(), ctx.clone()),
      parse_type(refs.clone(), ctx.clone()),
      parse_var(refs.clone(), ctx.clone()),
    ))(i)
  }
}
pub fn parse(i: &str) -> IResult<Span, Term, ParseError<Span>> {
  parse_term(HashMap::new(), Vector::new())(Span::new(i))
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use crate::term::tests::test_refs;
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  #[quickcheck]
  fn term_parse_print(x: Term) -> bool {
    match parse_term(test_refs(), Vector::new())(Span::new(&format!("{}", x))) {
      Ok((_, y)) => x == y,
      _ => false,
    }
  }
}
