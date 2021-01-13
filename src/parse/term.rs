use crate::{
  parse::error::ParseError,
  term::{
    LitType,
    Literal,
    PrimOp,
    Refs,
    Term,
    Uses,
  },
};

use hashexpr::{
  atom::Atom,
  position::Pos,
  span::Span,
  Expr,
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
    terminated,
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
    String::from(":="),
    String::from(";"),
    String::from("typeann"),
    String::from("type"),
    String::from("data"),
    String::from("case"),
    String::from("Type"),
  ])
}

pub fn parse_name(i: Span) -> IResult<Span, String, ParseError<Span>> {
  let (i, s) = take_till1(|x| {
    char::is_whitespace(x) | (x == ':') | (x == ';') | (x == ')') | (x == '(')
  })(i)?;
  let s: String = String::from(s.fragment().to_owned());
  if reserved_symbols().contains(&s) {
    Err(Err::Error(ParseError::ReservedSymbol(i, s)))
  }
  else if s.starts_with("#") {
    Err(Err::Error(ParseError::ReservedSymbol(i, s)))
  }
  else if !hashexpr::is_valid_symbol_string(&s) {
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
    let (upto, nam) = parse_name(from)?;
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
    let (i, ns) = separated_list1(multispace1, parse_name)(i)?;
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
    value(Uses::None, terminated(tag("0"), multispace1)),
    value(Uses::Affi, terminated(tag("&"), multispace1)),
    value(Uses::Once, terminated(tag("1"), multispace1)),
    success(Uses::Many),
  ))(i)
}

pub fn parse_binder_full(
  refs: Refs,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, (Uses, String, Term), ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("(")(i)?;
    let (i, _) = multispace0(i)?;
    let (i, u) = parse_uses(i)?;
    let (i, n) = parse_name(i)?;
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
        parse_binder_full(refs.clone(), ctx.clone()),
        parse_binder_short(refs.clone(), ctx.clone()),
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
  _refs: Refs,
  _ctx: Vector<String>,
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
    let (i, n) = parse_name(i)?;
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

pub fn parse_decl(
  refs: Refs,
  ctx: Vector<String>,
  rec: bool,
  shadow: bool,
) -> impl Fn(Span) -> IResult<Span, (String, Term, Term), ParseError<Span>> {
  move |from: Span| {
    let (i, nam) = parse_name(from)?;
    if refs.get(&nam).is_some() && !shadow {
      Err(Err::Error(ParseError::TopLevelRedefinition(from, nam.clone())))
    }
    else {
      let (i, _) = multispace0(i)?;
      let (i, bs) = alt((
        terminated(
          parse_binders(refs.clone(), ctx.clone(), false),
          multispace0,
        ),
        success(Vec::new()),
      ))(i)?;
      let (i, _) = tag(":")(i)?;
      let (i, _) = multispace0(i)?;
      let mut ctx2 = ctx.clone();
      for (_, n, _) in bs.clone().iter() {
        ctx2.push_front(n.clone());
      }
      let (i, typ) = parse_term(refs.clone(), ctx2.clone())(i)?;
      if rec {
        ctx2.push_front(nam.clone());
      };
      let (i, _) = multispace0(i)?;
      let (i, _) = tag(":=")(i)?;
      let (i, _) = multispace0(i)?;
      let (upto, trm) = parse_term(refs.clone(), ctx2.clone())(i)?;
      let pos = Some(Pos::from_upto(from, upto));
      let trm = bs
        .iter()
        .rev()
        .fold(trm, |acc, (_, n, _)| Term::Lam(pos, n.clone(), Box::new(acc)));
      let typ = bs.into_iter().rev().fold(typ, |acc, (u, n, t)| {
        Term::All(pos, u, n, Box::new(t), Box::new(acc))
      });
      Ok((upto, (nam, trm, typ)))
    }
  }
}

pub fn parse_let(
  refs: Refs,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, rec) =
      alt((value(true, tag("letrec")), value(false, tag("let"))))(from)?;
    let (i, _) = multispace1(i)?;
    let (i, uses) = parse_uses(i)?;
    let (i, (nam, exp, typ)) =
      parse_decl(refs.clone(), ctx.clone(), rec, true)(i)?;
    let (i, _) = tag(";")(i)?;
    let (i, _) = multispace0(i)?;
    let mut ctx2 = ctx.clone();
    ctx2.push_front(nam.clone());
    let (upto, bod) = parse_term(refs.clone(), ctx2.clone())(i)?;
    let pos = Some(Pos::from_upto(from, upto));
    Ok((
      upto,
      Term::Let(
        pos,
        rec,
        uses,
        nam.clone(),
        Box::new(typ),
        Box::new(exp),
        Box::new(bod),
      ),
    ))
  }
}

pub fn parse_lty(
  _refs: Refs,
  _ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (upto, tag) = alt((
      tag("#Nat"),
      tag("#Int"),
      tag("#Bits"),
      tag("#Text"),
      tag("#Char"),
      tag("#Link"),
      tag("#Exception"),
    ))(from)?;
    let pos = Some(Pos::from_upto(from, upto));
    match tag.fragment().as_ref() {
      "#Nat" => Ok((upto, Term::LTy(pos, LitType::Nat))),
      "#Int" => Ok((upto, Term::LTy(pos, LitType::Int))),
      "#Bits" => Ok((upto, Term::LTy(pos, LitType::Bits))),
      "#Text" => Ok((upto, Term::LTy(pos, LitType::Text))),
      "#Char" => Ok((upto, Term::LTy(pos, LitType::Char))),
      "#Link" => Ok((upto, Term::LTy(pos, LitType::Link))),
      "#Exception" => {
        Ok((upto, Term::LTy(pos, crate::term::LitType::Exception)))
      }
      e => {
        Err(Err::Error(ParseError::UnknownLiteralType(upto, String::from(e))))
      }
    }
  }
}
pub fn parse_exception(
  _refs: Refs,
  _ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("#!")(from)?;
    let p = |i| hashexpr::string::parse_string("\"", i);
    let (upto, val) =
      delimited(tag("\""), p, tag("\""))(i).map_err(|e| Err::convert(e))?;
    let pos = Some(Pos::from_upto(from, upto));
    Ok((upto, Term::Lit(pos, Literal::Exception(val))))
  }
}

pub fn parse_lit(
  _refs: Refs,
  _ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (upto, atom) = alt((
      hashexpr::parse_bits,
      hashexpr::parse_raw,
      hashexpr::parse_nat,
      hashexpr::parse_int,
      hashexpr::parse_text,
      hashexpr::parse_char,
    ))(from)
    .map_err(|e| nom::Err::convert(e))?;
    let pos = Some(Pos::from_upto(from, upto));
    match atom {
      Expr::Atom(_, Atom::Link(x)) => {
        Ok((upto, Term::Lit(pos, Literal::Link(x))))
      }
      Expr::Atom(_, Atom::Bits(x, len)) => {
        Ok((upto, Term::Lit(pos, Literal::Bits(len, x))))
      }
      Expr::Atom(_, Atom::Text(x, len)) => {
        Ok((upto, Term::Lit(pos, Literal::Text(len, x))))
      }
      Expr::Atom(_, Atom::Char(x)) => {
        Ok((upto, Term::Lit(pos, Literal::Char(x))))
      }
      Expr::Atom(_, Atom::Nat(x, len)) => {
        Ok((upto, Term::Lit(pos, Literal::Nat(len, x))))
      }
      Expr::Atom(_, Atom::Int(x, len)) => {
        Ok((upto, Term::Lit(pos, Literal::Int(len, x))))
      }
      e => Err(Err::Error(ParseError::UnexpectedLiteral(upto, e))),
    }
  }
}

pub fn parse_opr(
  _refs: Refs,
  _ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (upto, op) = alt((
      alt((tag("#eql"), tag("#lth"), tag("#lte"), tag("#gth"), tag("#gte"))),
      alt((tag("#bor"), tag("#and"), tag("#xor"), tag("#not"))),
      alt((
        tag("#suc"),
        tag("#pre"),
        tag("#add"),
        tag("#sub"),
        tag("#mul"),
        tag("#div"),
        tag("#mod"),
      )),
      alt((tag("#shl"), tag("#shr"), tag("#rol"), tag("#ror"))),
      alt((tag("#clz"), tag("#ctz"), tag("#cnt"))),
      alt((tag("#len"), tag("#cat"), tag("#cst"))),
    ))(from)?;
    let pos = Some(Pos::from_upto(from, upto));
    let op_str: String = String::from(*op.fragment());
    match PrimOp::from_symbol(op_str) {
      Some(op) => Ok((upto, Term::Opr(pos, op))),
      _ => panic!("impossible"),
    }
  }
}

pub fn parse_ann(
  refs: Refs,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, trm) = parse_term(refs.clone(), ctx.clone())(from)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag("::")(i)?;
    let (i, _) = multispace0(i)?;
    let (upto, typ) = parse_term(refs.clone(), ctx.clone())(i)?;
    let pos = Some(Pos::from_upto(from, upto));
    Ok((upto, Term::Ann(pos, Box::new(typ), Box::new(trm))))
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
        parse_ann(refs.clone(), ctx.clone()),
        tag(")"),
      ),
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
      parse_let(refs.clone(), ctx.clone()),
      parse_type(refs.clone(), ctx.clone()),
      parse_lty(refs.clone(), ctx.clone()),
      parse_opr(refs.clone(), ctx.clone()),
      parse_exception(refs.clone(), ctx.clone()),
      parse_lit(refs.clone(), ctx.clone()),
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

  #[quickcheck]
  fn term_parse_print(x: Term) -> bool {
    match parse_term(test_refs(), Vector::new())(Span::new(&format!("{}", x))) {
      Ok((_, y)) => x == y,
      e => {
        println!("{}", x);
        println!("{:?}", e);
        false
      }
    }
  }
}
