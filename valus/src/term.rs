use std::fmt;

use im::Vector;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{
  alphanumeric0, multispace0, multispace1, satisfy,
};
use nom::error::ErrorKind;
use nom::error::ParseError;
use nom::multi::separated_list0;
use nom::multi::separated_list1;
use nom::sequence::{delimited, preceded};
use nom::Err;
use nom::IResult;
use nom::InputLength;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Term {
  Var(String, u64),
  Lam(String, Box<Term>),
  App(Box<(Term, Term)>),
}

impl Term {}

impl fmt::Display for Term {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use Term::*;

    fn lams(nam: &String, bod: &Term) -> String {
      match bod {
        Lam(nam2, bod2) => format!("{} {}", nam, lams(nam2, bod2)),
        _ => format!("{} => {}", nam, bod),
      }
    }

    fn apps(fun: &Term, arg: &Term) -> String {
      match (fun, arg) {
        (App(boxed_fun), App(boxed_arg)) => {
          let (ff, fa) = boxed_fun.as_ref();
          let (af, aa) = boxed_arg.as_ref();
          format!("{} ({})", apps(ff, fa), apps(af, aa))
        }
        (App(boxed_fun), Lam(_, _)) => {
          let (ff, fa) = boxed_fun.as_ref();
          format!("{} ({})", apps(ff, fa), arg)
        }
        (App(boxed_fun), Var(_, _)) => {
          let (ff, fa) = boxed_fun.as_ref();
          format!("{} {}", apps(ff, fa), arg)
        }
        (Lam(_, _), App(boxed_arg)) => {
          let (af, aa) = boxed_arg.as_ref();
          format!("({}) ({})", fun, apps(af, aa))
        }
        (Var(_, _), App(boxed_arg)) => {
          let (af, aa) = boxed_arg.as_ref();
          format!("{} ({})", fun, apps(af, aa))
        }
        (Var(_, _), Lam(_, _)) => {
          format!("{} ({})", fun, arg)
        }
        (Lam(_, _), Var(_, _)) => {
          format!("({}) {}", fun, arg)
        }
        (Var(_, _), Var(_, _)) => {
          format!("{} {}", fun, arg)
        }
        (Lam(_, _), Lam(_, _)) => {
          format!("({}) ({})", fun, arg)
        }
      }
    };

    match self {
      Term::Var(nam, _) => {
        write!(f, "{}", nam)
      }
      Term::Lam(nam, term) => {
        write!(f, "λ {}", lams(nam, term))
      }
      Term::App(boxed) => {
        let (fun, arg) = boxed.as_ref();
        write!(f, "{}", apps(fun, arg))
      }
    }
  }
}

#[macro_export]
macro_rules! var {
  ($n:literal,$i:literal) => {
    Term::Var(String::from($n), $i)
  };
}

#[macro_export]
macro_rules! lam {
  ($n:literal,$b:expr) => {
    Term::Lam(String::from($n), Box::new($b))
  };
}

#[macro_export]
macro_rules! app {
  ($i:expr, $j:expr) => {
    Term::App(Box::new(($i, $j)));
  };
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TermParseError<I> {
  FreeVariable(I, String, Vector<String>),
  NomErr(I, ErrorKind),
}

impl<I> TermParseError<I> {
  pub fn rest(self) -> I {
    match self {
      Self::FreeVariable(i, _, _) => i,
      Self::NomErr(i, _) => i,
    }
  }
}

impl<I> ParseError<I> for TermParseError<I>
where
  I: InputLength,
  I: Clone,
{
  fn from_error_kind(input: I, kind: ErrorKind) -> Self {
    TermParseError::NomErr(input, kind)
  }

  fn append(i: I, k: ErrorKind, other: Self) -> Self {
    if i.clone().input_len() < other.clone().rest().input_len() {
      TermParseError::NomErr(i, k)
    } else {
      other
    }
  }

  fn or(self, other: Self) -> Self {
    if self.clone().rest().input_len() < other.clone().rest().input_len() {
      self
    } else {
      other
    }
  }
}
pub fn parse_nam(i: &str) -> IResult<&str, String, TermParseError<&str>> {
  fn init_char(x: char) -> bool {
    x.is_alphabetic() || x == '_'
  }
  let (i, init) = satisfy(init_char)(i)?;
  let (i, rest) = alphanumeric0(i)?;
  Ok((i, format!("{}{}", init, rest)))
}

pub fn parse_var(
  ctx: Vector<String>,
) -> impl Fn(&str) -> IResult<&str, Term, TermParseError<&str>> {
  move |i: &str| {
    let (i, nam) = parse_nam(i)?;
    let (i, idx) = match ctx.iter().enumerate().find(|(_, x)| x.as_ref() == nam)
    {
      Some((idx, _)) => Ok((i, idx)),
      None => Err(Err::Error(TermParseError::FreeVariable(
        i,
        nam.clone(),
        ctx.clone(),
      ))),
    }?;
    Ok((i, Term::Var(nam.clone(), idx as u64)))
  }
}

pub fn parse_lam(
  ctx: Vector<String>,
) -> impl Fn(&str) -> IResult<&str, Term, TermParseError<&str>> {
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
    let trm =
      ns.iter().rev().fold(bod, |acc, n| Term::Lam(n.clone(), Box::new(acc)));
    Ok((i, trm))
  }
}

pub fn parse_term(
  ctx: Vector<String>,
) -> impl Fn(&str) -> IResult<&str, Term, TermParseError<&str>> {
  move |i: &str| {
    let (i, fun) = parse_term_inner(ctx.clone())(i)?;
    let (i, _) = multispace0(i)?;
    let (i, args) =
      separated_list0(multispace1, parse_term_inner(ctx.clone()))(i)?;
    let trm =
      args.into_iter().fold(fun, |acc, arg| Term::App(Box::new((acc, arg))));
    Ok((i, trm))
  }
}

pub fn parse_term_inner(
  ctx: Vector<String>,
) -> impl Fn(&str) -> IResult<&str, Term, TermParseError<&str>> {
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

pub fn parse(i: &str) -> IResult<&str, Term, TermParseError<&str>> {
  parse_term(Vector::new())(i)
}

#[cfg(test)]
mod test {
  use super::*;
  use quickcheck::{Arbitrary, Gen};
  use rand::prelude::IteratorRandom;
  use rand::Rng;

  fn arbitrary_name<G: Gen>(g: &mut G) -> String {
    let alpha = "abcdefghjiklmnopqrstuvwxyz";
    let x = g.gen_range(0, alpha.len());
    format!("{}", alpha.as_bytes()[x] as char)
  }

  fn arbitrary_lam<G: Gen>(g: &mut G, ctx: Vector<String>) -> Term {
    let n = arbitrary_name(g);
    let mut ctx2 = ctx.clone();
    ctx2.push_front(n.clone());
    Term::Lam(n, Box::new(arbitrary_term(g, ctx2)))
  }

  fn arbitrary_var<G: Gen>(g: &mut G, ctx: Vector<String>) -> Term {
    let n = ctx.iter().choose(g).unwrap();
    let (i, _) = ctx.iter().enumerate().find(|(_, x)| *x == n).unwrap();
    Term::Var(n.clone(), i as u64)
  }

  fn arbitrary_term<G: Gen>(g: &mut G, ctx: Vector<String>) -> Term {
    let len = ctx.len();
    if len == 0 {
      arbitrary_lam(g, ctx)
    } else {
      let x: u32 = g.gen();
      match x % 5 {
        0 | 1 | 2 => arbitrary_var(g, ctx),
        3 => arbitrary_lam(g, ctx),
        _ => Term::App(Box::new((
          arbitrary_term(g, ctx.clone()),
          arbitrary_term(g, ctx.clone()),
        ))),
      }
    }
  }

  impl Arbitrary for Term {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      arbitrary_term(g, Vector::new())
    }
  }

  #[test]
  fn display_term() {
    let id = lam!("x", var!("x", 0));
    assert_eq!(format!("{}", id), "λ x => x");
    let x = app!(id.clone(), var!("x", 0));
    assert_eq!(format!("{}", x), "(λ x => x) x");
    assert_eq!(parse("λ x => x"), Ok(("", id)));
    let f = lam!("x", lam!("y", var!("x", 1)));
    assert_eq!(format!("{}", f), "λ x y => x");
    assert_eq!(parse("λ x y => x"), Ok(("", f)));
    let f = lam!("x", lam!("y", lam!("z", var!("x", 2))));
    assert_eq!(format!("{}", f), "λ x y z => x");
    assert_eq!(parse("λ x y z => x"), Ok(("", f)));
    let f = lam!("x", lam!("y", app!(var!("x", 1), var!("y", 0))));
    assert_eq!(format!("{}", f), "λ x y => x y");
    assert_eq!(parse("λ x y => x y"), Ok(("", f)));
    let f = app!(lam!("x", var!("x", 0)), lam!("x", var!("x", 0)));
    assert_eq!(format!("{}", f), "(λ x => x) (λ x => x)");
    assert_eq!(parse("(λ x => x) (λ x => x)"), Ok(("", f)));

    let three_str = "λ s z => s (s (s z))";
    let three_term = lam!(
      "s",
      lam!(
        "z",
        app!(
          var!("s", 1),
          app!(var!("s", 1), app!(var!("s", 1), var!("z", 0)))
        )
      )
    );
    assert_eq!(format!("{}", three_term), three_str);
    assert_eq!(parse(three_str), Ok(("", three_term)));
  }

  #[quickcheck]
  fn term_print_parse(x: Term) -> bool {
    match parse(&format!("{}", x)) {
      Ok((_, y)) => x == y,
      _ => false,
    }
  }
}
