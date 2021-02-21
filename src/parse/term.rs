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
  position::Pos,
  span::Span,
  AVal::*,
  Expr,
};

use im::{
  HashMap,
  Vector,
};
use nom::{
  branch::alt,
  bytes::complete::{
    tag,
    take_till,
    take_till1,
  },
  character::complete::{
    multispace0,
    multispace1,
  },
  combinator::{
    map,
    success,
    value,
  },
  multi::{
    many0,
    many1,
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
    String::from("//"),
    String::from("λ"),
    String::from("lambda"),
    String::from("=>"),
    String::from("{"),
    String::from("}"),
    String::from("∀"),
    String::from("forall"),
    String::from("->"),
    String::from("@"),
    String::from("="),
    String::from(";"),
    String::from("::"),
    String::from("type"),
    String::from("data"),
    String::from("def"),
    String::from("open"),
    String::from("case"),
    String::from("Type"),
  ])
}

pub fn parse_line_comment(i: Span) -> IResult<Span, Span, ParseError<Span>> {
  let (i, _) = tag("//")(i)?;
  let (i, com) = take_till(|c| c == '\n')(i)?;
  Ok((i, com))
}
pub fn parse_space(i: Span) -> IResult<Span, Vec<Span>, ParseError<Span>> {
  let (i, _) = multispace0(i)?;
  let (i, com) = many0(terminated(parse_line_comment, multispace1))(i)?;
  Ok((i, com))
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
    // TODO: more specific error for literal overlap
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
        Some((d, a)) => Ok((upto, Term::Ref(pos, nam.clone(), *d, *a))),
        None => Err(Err::Error(ParseError::UndefinedReference(
          upto,
          nam.clone(),
          ctx.to_owned(),
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
    let (i, _) = alt((tag("λ"), tag("lambda")))(from)?;
    let (i, _) = parse_space(i)?;
    let (i, ns) = separated_list1(multispace1, parse_name)(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = tag("=>")(i)?;
    let (i, _) = parse_space(i)?;
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
) -> impl Fn(Span) -> IResult<Span, (Uses, Vec<String>, Term), ParseError<Span>>
{
  move |i: Span| {
    let (i, _) = tag("(")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, u) = parse_uses(i)?;
    let (i, ns) = many1(terminated(parse_name, parse_space))(i)?;
    let (i, _) = tag(":")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, typ) = parse_term(refs.to_owned(), ctx.to_owned())(i)?;
    let (i, _) = tag(")")(i)?;
    Ok((i, (u, ns, typ)))
  }
}

pub fn parse_binder_short(
  refs: Refs,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, (Uses, Vec<String>, Term), ParseError<Span>>
{
  move |i: Span| {
    map(parse_term(refs.to_owned(), ctx.to_owned()), |t| {
      (Uses::Many, vec![String::from("")], t)
    })(i)
  }
}

pub fn parse_binder(
  refs: Refs,
  ctx: Vector<String>,
  nam_opt: bool,
) -> impl Fn(Span) -> IResult<Span, (Uses, Vec<String>, Term), ParseError<Span>>
{
  move |i: Span| {
    if nam_opt {
      alt((
        parse_binder_full(refs.clone(), ctx.clone()),
        parse_binder_short(refs.to_owned(), ctx.to_owned()),
      ))(i)
    }
    else {
      parse_binder_full(refs.to_owned(), ctx.to_owned())(i)
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
    let mut ctx = ctx.to_owned();
    let mut res = Vec::new();

    match parse_binder(refs.to_owned(), ctx.to_owned(), nam_opt)(i.to_owned()) {
      Err(e) => return Err(e),
      Ok((i1, (u, ns, t))) => {
        for n in ns {
          ctx.push_front(n.to_owned());
          res.push((u, n, t.clone()));
        }
        i = i1;
      }
    }

    loop {
      match preceded(
        parse_space,
        parse_binder(refs.to_owned(), ctx.to_owned(), nam_opt),
      )(i)
      {
        Err(Err::Error(_)) => return Ok((i, res)),
        Err(e) => return Err(e),
        Ok((i2, (u, ns, t))) => {
          for n in ns {
            ctx.push_front(n.to_owned());
            res.push((u, n, t.clone()));
          }
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
    let (i, _) = alt((tag("∀"), tag("forall")))(from)?;
    let (i, _) = parse_space(i)?;
    let (i, bs) = parse_binders(refs.clone(), ctx.clone(), true)(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = tag("->")(i)?;
    let (i, _) = parse_space(i)?;
    let mut ctx2 = ctx.clone();
    for (_, n, _) in bs.clone().iter() {
      ctx2.push_front(n.clone());
    }
    let (upto, bod) = parse_term(refs.to_owned(), ctx2)(i)?;
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
    let (i, _) = parse_space(i)?;
    let mut ctx2 = ctx.clone();
    ctx2.push_front(n.clone());
    let (upto, bod) = parse_term(refs.to_owned(), ctx2)(i)?;
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
    let (i, _) = parse_space(i)?;
    let (upto, bod) = parse_term(refs.to_owned(), ctx.clone())(i)?;
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
    let (i, _) = parse_space(i)?;
    let (upto, bod) = parse_term(refs.to_owned(), ctx.clone())(i)?;
    let pos = Some(Pos::from_upto(from, upto));
    Ok((upto, Term::Dat(pos, Box::new(bod))))
  }
}

pub fn parse_typed_definition(
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
      let (i, _) = parse_space(i)?;
      let (i, bs) = alt((
        terminated(
          parse_binders(refs.clone(), ctx.clone(), false),
          parse_space,
        ),
        success(Vec::new()),
      ))(i)?;
      let (i, _) = tag(":")(i)?;
      let (i, _) = parse_space(i)?;
      let mut type_ctx = ctx.clone();
      for (_, n, _) in bs.clone().iter() {
        type_ctx.push_front(n.clone());
      }
      let (i, typ) = parse_term(refs.clone(), type_ctx)(i)?;
      let mut term_ctx = ctx.to_owned();
      if rec {
        term_ctx.push_front(nam.clone());
      };
      for (_, n, _) in bs.clone().iter() {
        term_ctx.push_front(n.clone());
      }
      let (i, _) = parse_space(i)?;
      let (i, _) = tag("=")(i)?;
      let (i, _) = parse_space(i)?;
      let (upto, trm) = parse_term(refs.clone(), term_ctx)(i)?;
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
    let (i, _) = parse_space(i)?;
    let (i, uses) = parse_uses(i)?;
    let (i, (nam, exp, typ)) =
      parse_typed_definition(refs.clone(), ctx.clone(), rec, true)(i)?;
    let (i, _) = tag(";")(i)?;
    let (i, _) = parse_space(i)?;
    let mut ctx2 = ctx.clone();
    ctx2.push_front(nam.clone());
    let (upto, bod) = parse_term(refs.to_owned(), ctx2)(i)?;
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
      tag("#Natural"),
      tag("#Nat"),
      tag("#Integer"),
      tag("#Int"),
      tag("#BitString"),
      tag("#BitVector"),
      tag("#Text"),
      tag("#Char"),
      tag("#Link"),
      tag("#Exception"),
    ))(from)?;
    let pos = Some(Pos::from_upto(from, upto));
    match tag.fragment().as_ref() {
      "#Natural" => Ok((upto, Term::LTy(pos, LitType::Natural))),
      "#Integer" => Ok((upto, Term::LTy(pos, LitType::Integer))),
      "#BitString" => Ok((upto, Term::LTy(pos, LitType::BitString))),
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
      Expr::Atom(_, Link(x)) => Ok((upto, Term::Lit(pos, Literal::Link(x)))),
      Expr::Atom(_, Bits(x)) => {
        Ok((upto, Term::Lit(pos, Literal::BitString(x))))
      }
      Expr::Atom(_, Text(x)) => Ok((upto, Term::Lit(pos, Literal::Text(x)))),
      Expr::Atom(_, Char(x)) => Ok((upto, Term::Lit(pos, Literal::Char(x)))),
      Expr::Atom(_, Nat(x)) => Ok((upto, Term::Lit(pos, Literal::Natural(x)))),
      Expr::Atom(_, Int(x)) => Ok((upto, Term::Lit(pos, Literal::Integer(x)))),
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
    let (i, _) = parse_space(i)?;
    let (i, _) = tag("::")(i)?;
    let (i, _) = parse_space(i)?;
    let (upto, typ) = parse_term(refs.to_owned(), ctx.to_owned())(i)?;
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
    let (i, _) = parse_space(i)?;
    let (upto, args) = separated_list0(
      multispace1,
      parse_term_inner(refs.to_owned(), ctx.to_owned()),
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
        preceded(tag("("), parse_space),
        parse_ann(refs.clone(), ctx.clone()),
        tag(")"),
      ),
      delimited(
        preceded(tag("("), parse_space),
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
      parse_var(refs.to_owned(), ctx.to_owned()),
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

  #[test]
  fn test_cases() {
    let res = parse_binder_full(HashMap::new(), Vector::new())(Span::new(
      "(a b c: Type)",
    ));
    println!("res2: {:?}", res);
    assert!(res.is_ok())
  }

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
