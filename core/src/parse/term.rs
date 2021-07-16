use crate::{
  defs::Defs,
  name::{
    is_valid_symbol_string,
    Name,
  },
  parse::{
    error::{
      ParseError,
      ParseErrorKind,
    },
    literal::*,
    op::parse_opr,
  },
  position::Pos,
  term::{
    LitType,
    Term,
    Uses,
  },
};

use sp_cid::Cid;
use sp_multihash::{
  Code,
  MultihashDigest,
};

use sp_im::{
  conslist::ConsList,
  vector::Vector,
};

use sp_ipld::{
  dag_cbor::DagCborCodec,
  Codec,
};

use sp_std::{
  borrow::ToOwned,
  boxed::Box,
  vec::Vec,
};

use alloc::string::{
  String,
  ToString,
};

use crate::parse::span::Span;

use nom::{
  branch::alt,
  bytes::complete::{
    tag,
    take_till,
    take_till1,
  },
  character::complete::{
    digit1,
    multispace0,
    multispace1,
    satisfy,
  },
  combinator::{
    eof,
    peek,
    value,
  },
  error::context,
  multi::{
    many0,
    many1,
  },
  sequence::{
    delimited,
    preceded,
    terminated,
  },
  Err,
  IResult,
};
use sp_std::collections::vec_deque::VecDeque;

pub type Ctx = ConsList<Name>;
pub type Quasi = Vector<Term>;

pub fn reserved_symbols() -> VecDeque<String> {
  VecDeque::from(vec![
    String::from("//"),
    String::from("λ"),
    String::from("ω"),
    String::from("&"),
    String::from("lambda"),
    String::from("=>"),
    String::from("∀"),
    String::from("forall"),
    String::from("->"),
    String::from("="),
    String::from(";"),
    String::from("::"),
    String::from("let"),
    String::from("in"),
    String::from("type"),
    String::from("data"),
    String::from("self"),
    String::from("def"),
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

pub fn parse_space1(i: Span) -> IResult<Span, Vec<Span>, ParseError<Span>> {
  let (i, _) = multispace1(i)?;
  let (i, com) = many0(terminated(parse_line_comment, multispace1))(i)?;
  Ok((i, com))
}

pub fn parse_name(from: Span) -> IResult<Span, Name, ParseError<Span>> {
  let (i, s) = take_till1(|x| {
    char::is_whitespace(x)
      | (x == ':')
      | (x == ';')
      | (x == ')')
      | (x == '(')
      | (x == '{')
      | (x == '}')
      | (x == ',')
  })(from)?;
  let s: String = String::from(s.fragment().to_owned());
  if reserved_symbols().contains(&s) {
    Err(Err::Error(ParseError::new(from, ParseErrorKind::ReservedKeyword(s))))
  }
  else if s.starts_with('#') {
    Err(Err::Error(ParseError::new(from, ParseErrorKind::ReservedSyntax(s))))
  }
  else if is_numeric_symbol_string1(&s) | is_numeric_symbol_string2(&s) {
    Err(Err::Error(ParseError::new(from, ParseErrorKind::NumericSyntax(s))))
  }
  else if !is_valid_symbol_string(&s) {
    Err(Err::Error(ParseError::new(from, ParseErrorKind::InvalidSymbol(s))))
  }
  else {
    Ok((i, Name::from(s)))
  }
}

pub fn parse_uses() -> impl Fn(Span) -> IResult<Span, Uses, ParseError<Span>> {
  move |i: Span| {
    alt((
      value(Uses::Many, terminated(tag("ω"), multispace1)),
      value(Uses::None, terminated(tag("0"), multispace1)),
      value(Uses::Affi, terminated(tag("&"), multispace1)),
      value(Uses::Once, terminated(tag("1"), multispace1)),
    ))(i)
  }
}

pub fn is_numeric_symbol_string1(s: &str) -> bool {
  s.starts_with('0')
    || s.starts_with('1')
    || s.starts_with('2')
    || s.starts_with('3')
    || s.starts_with('4')
    || s.starts_with('5')
    || s.starts_with('6')
    || s.starts_with('7')
    || s.starts_with('8')
    || s.starts_with('9')
}
pub fn is_numeric_symbol_string2(s: &str) -> bool {
  s.starts_with("-0")
    || s.starts_with("-1")
    || s.starts_with("-2")
    || s.starts_with("-3")
    || s.starts_with("-4")
    || s.starts_with("-5")
    || s.starts_with("-6")
    || s.starts_with("-7")
    || s.starts_with("-8")
    || s.starts_with("-9")
    || s.starts_with("+0")
    || s.starts_with("+1")
    || s.starts_with("+2")
    || s.starts_with("+3")
    || s.starts_with("+4")
    || s.starts_with("+5")
    || s.starts_with("+6")
    || s.starts_with("+7")
    || s.starts_with("+8")
    || s.starts_with("+9")
}

pub fn parse_antiquote(
  ctx: Ctx,
  quasi: Quasi,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (upto, nam) =
      context("quasiquoted value", preceded(tag("#$"), digit1))(from)?;
    if let Some((_, trm)) = quasi
      .iter()
      .enumerate()
      .find(|(i, _)| format!("{}", i) == nam.to_string())
    {
      Ok((upto, trm.clone()))
    }
    else {
      Err(Err::Error(ParseError::new(
        upto,
        ParseErrorKind::UndefinedReference(
          Name::from(format!("#${}", nam.to_string())),
          ctx.clone(),
        ),
      )))
    }
  }
}

pub fn parse_var(
  input: Cid,
  defs: Defs,
  rec: Option<Name>,
  ctx: Ctx,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (upto, nam) = context("local or global reference", parse_name)(from)?;
    let pos = Pos::from_upto(input, from, upto);
    let is_rec_name = match rec.clone() {
      Some(rec_ref) => nam == rec_ref,
      _ => false,
    };
    if let Some((idx, _)) = ctx.iter().enumerate().find(|(_, x)| **x == nam) {
      Ok((upto, Term::Var(pos, nam.clone(), idx as u64)))
    }
    else if is_rec_name {
      Ok((upto, Term::Rec(pos)))
    }
    else if let Some(def) = defs.get(&nam) {
      Ok((upto, Term::Ref(pos, nam.clone(), def.def_cid, def.ast_cid)))
    }
    else {
      Err(Err::Error(ParseError::new(
        upto,
        ParseErrorKind::UndefinedReference(nam.clone(), ctx.clone()),
      )))
    }
  }
}

pub fn parse_arg(
  input: Cid,
  defs: Defs,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Quasi,
) -> impl Fn(Span) -> IResult<Span, (Uses, Term, Term), ParseError<Span>> {
  move |i: Span| {
    let (i, _) = terminated(tag("("), parse_space)(i)?;
    let (i, arg) = context(
      "app arg",
      parse_telescope(
        input,
        defs.clone(),
        rec.clone(),
        ctx.clone(),
        quasi.clone(),
      ),
    )(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = terminated(tag("::"), parse_space)(i)?;
    let (i, u) = terminated(parse_uses(), parse_space)(i)?;
    let (i, typ) = context(
      "app typ",
      parse_telescope(
        input,
        defs.clone(),
        rec.clone(),
        ctx.clone(),
        quasi.clone(),
      ),
    )(i)?;
    let (i, _) = preceded(parse_space, tag(")"))(i)?;
    Ok((i, (u, typ, arg)))
  }
}

pub fn parse_args(
  input: Cid,
  defs: Defs,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Quasi,
) -> impl FnMut(Span) -> IResult<Span, Vec<(Uses, Term, Term)>, ParseError<Span>>
{
  move |mut i: Span| {
    let mut res = Vec::new();

    loop {
      match preceded(parse_space, peek(parse_tele_end))(i) {
        Ok((i2, _)) => return Ok((i2, res)),
        _ => {}
      }
      match preceded(
        parse_space,
        parse_arg(
          input,
          defs.to_owned(),
          rec.clone(),
          ctx.clone(),
          quasi.to_owned(),
        ),
      )(i)
      {
        Err(e) => return Err(e),
        Ok((i2, x)) => {
          res.push(x);
          i = i2;
        }
      }
    }
  }
}

pub fn parse_tele_end(i: Span) -> IResult<Span, (), ParseError<Span>> {
  let (i, _) = alt((
    peek(tag("def")),
    peek(tag("type")),
    peek(tag("in")),
    peek(tag("::")),
    peek(tag("=")),
    peek(tag("->")),
    peek(tag(";")),
    peek(tag(")")),
    peek(tag("{")),
    peek(tag("}")),
    peek(tag(",")),
    peek(eof),
  ))(i)?;
  Ok((i, ()))
}

pub fn parse_telescope(
  input: Cid,
  defs: Defs,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Quasi,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, fun) = context(
      "app fun",
      parse_term(input, defs.clone(), rec.clone(), ctx.clone(), quasi.clone()),
    )(from)?;
    let (i, _) = parse_space(i)?;
    let (upto, args) =
      parse_args(input, defs.clone(), rec.clone(), ctx.clone(), quasi.clone())(
        i,
      )?;
    let pos = Pos::from_upto(input, from, upto);
    let trm = args.into_iter().fold(fun, |acc, (u, typ, arg)| {
      Term::App(pos, u, Box::new(acc), Box::new(typ), Box::new(arg))
    });
    return Ok((upto, trm));
  }
}

pub fn parse_binder(
  input: Cid,
  defs: Defs,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Quasi,
) -> impl Fn(Span) -> IResult<Span, Vec<(Uses, Name, Term)>, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("(")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, u) = parse_uses()(i)?;
    let (i, ns) = many1(terminated(parse_name, parse_space))(i)?;
    let (i, _) = tag(":")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, typ) = parse_telescope(
      input,
      defs.clone(),
      rec.clone(),
      ctx.clone(),
      quasi.to_owned(),
    )(i)?;
    let (i, _) = tag(")")(i)?;
    let mut res = Vec::new();
    for (i, n) in ns.iter().enumerate() {
      res.push((u, n.to_owned(), typ.clone().shift(i as u64, Some(0))))
    }
    Ok((i, res))
  }
}

pub fn parse_binders(
  input: Cid,
  defs: Defs,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Quasi,
  terminator: Vec<char>,
) -> impl FnMut(Span) -> IResult<Span, Vec<(Uses, Name, Term)>, ParseError<Span>>
{
  move |mut i: Span| {
    let mut ctx = ctx.clone();
    let mut res = Vec::new();

    loop {
      match preceded(parse_space, peek(satisfy(|x| terminator.contains(&x))))(i)
      {
        Ok((i2, _)) => return Ok((i2, res)),
        _ => {}
      }
      match preceded(
        parse_space,
        parse_binder(
          input,
          defs.to_owned(),
          rec.clone(),
          ctx.clone(),
          quasi.to_owned(),
        ),
      )(i)
      {
        Err(e) => return Err(e),
        Ok((i2, bs)) => {
          for (u, n, t) in bs {
            ctx = ctx.cons(n.to_owned());
            res.push((u, n, t));
          }
          i = i2;
        }
      }
    }
  }
}

pub fn parse_binders1(
  input: Cid,
  defs: Defs,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Quasi,
  terminator: Vec<char>,
) -> impl FnMut(Span) -> IResult<Span, Vec<(Uses, Name, Term)>, ParseError<Span>>
{
  move |mut i: Span| {
    let mut ctx = ctx.clone();
    let mut res = Vec::new();

    match parse_binder(
      input,
      defs.to_owned(),
      rec.clone(),
      ctx.clone(),
      quasi.to_owned(),
    )(i.to_owned())
    {
      Err(e) => return Err(e),
      Ok((i1, bs)) => {
        for (u, n, t) in bs {
          ctx = ctx.cons(n.to_owned());
          res.push((u, n, t));
        }
        i = i1;
      }
    }
    let (i, mut res2) = parse_binders(
      input,
      defs.to_owned(),
      rec.clone(),
      ctx.clone(),
      quasi.to_owned(),
      terminator.clone(),
    )(i)?;
    res.append(&mut res2);
    Ok((i, res))
  }
}

pub fn parse_lam(
  input: Cid,
  defs: Defs,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Quasi,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = alt((tag("λ"), tag("lambda")))(from)?;
    let (i, _) = parse_space(i)?;
    let (i, bs) = parse_binders1(
      input,
      defs.clone(),
      rec.clone(),
      ctx.clone(),
      quasi.clone(),
      vec!['='],
    )(i)?;
    let (i, _) = terminated(tag("=>"), parse_space)(i)?;
    let mut ctx2 = ctx.clone();
    for (_, n, _) in bs.iter() {
      ctx2 = ctx2.cons(n.clone());
    }
    let (upto, bod) = parse_telescope(
      input,
      defs.to_owned(),
      rec.clone(),
      ctx2,
      quasi.to_owned(),
    )(i)?;
    let pos = Pos::from_upto(input, from, upto);
    let trm = bs.into_iter().rev().fold(bod, |acc, (u, n, t)| {
      Term::Lam(pos, u, n, Box::new(t), Box::new(acc))
    });
    Ok((upto, trm))
  }
}

pub fn parse_all(
  input: Cid,
  defs: Defs,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Quasi,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = alt((tag("∀"), tag("forall")))(from)?;
    let (i, _) = parse_space(i)?;
    let (i, bs) = parse_binders1(
      input,
      defs.clone(),
      rec.clone(),
      ctx.clone(),
      quasi.clone(),
      vec!['-'],
    )(i)?;
    let (i, _) = tag("->")(i)?;
    let (i, _) = parse_space(i)?;
    let mut ctx2 = ctx.clone();
    for (_, n, _) in bs.iter() {
      ctx2 = ctx2.cons(n.clone());
    }
    let (upto, bod) = parse_telescope(
      input,
      defs.to_owned(),
      rec.clone(),
      ctx2,
      quasi.to_owned(),
    )(i)?;
    let pos = Pos::from_upto(input, from, upto);
    let trm = bs.into_iter().rev().fold(bod, |acc, (u, n, t)| {
      Term::All(pos, u, n, Box::new(t), Box::new(acc))
    });
    Ok((upto, trm))
  }
}

pub fn parse_typ(
  input: Cid,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (upto, _) = tag("Type")(from)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Term::Typ(pos)))
  }
}

pub fn parse_slf(
  input: Cid,
  defs: Defs,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Quasi,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("self")(from)?;
    let (i, _) = parse_space(i)?;
    let (i, n) = parse_name(i)?;
    let (i, _) = parse_space(i)?;
    let mut ctx2 = ctx.clone();
    ctx2 = ctx2.cons(n.clone());
    let (upto, bod) = parse_telescope(
      input,
      defs.to_owned(),
      rec.clone(),
      ctx2.clone(),
      quasi.to_owned(),
    )(i)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Term::Slf(pos, n, Box::new(bod))))
  }
}

pub fn parse_cse(
  input: Cid,
  defs: Defs,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Quasi,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("case")(from)?;
    let (i, _) = parse_space(i)?;
    let (upto, bod) = parse_telescope(
      input,
      defs.to_owned(),
      rec.clone(),
      ctx.clone(),
      quasi.to_owned(),
    )(i)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Term::Cse(pos, Box::new(bod))))
  }
}
pub fn parse_dat(
  input: Cid,
  defs: Defs,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Quasi,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("data")(from)?;
    let (i, _) = parse_space(i)?;
    let (i, typ) =
      parse_term(input, defs.clone(), rec.clone(), ctx.clone(), quasi.clone())(
        i,
      )?;
    let (i, _) = parse_space(i)?;
    let (upto, bod) =
      parse_term(input, defs.clone(), rec.clone(), ctx.clone(), quasi.clone())(
        i,
      )?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Term::Dat(pos, Box::new(typ), Box::new(bod))))
  }
}

pub fn parse_let(
  input: Cid,
  defs: Defs,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Quasi,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, letrec) =
      alt((value(true, tag("letrec")), value(false, tag("let"))))(from)?;
    let (i, _) = parse_space(i)?;
    let (i, uses) = parse_uses()(i)?;
    let (i, _) = parse_space(i)?;
    let (i, nam) = parse_name(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = tag(":")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, typ) = parse_telescope(
      input,
      defs.clone(),
      rec.clone(),
      ctx.clone(),
      quasi.clone(),
    )(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = tag("=")(i)?;
    let (i, _) = parse_space(i)?;
    let ctx2 = ctx.clone().cons(nam.clone());
    let (i, exp) = parse_telescope(
      input,
      defs.clone(),
      rec.clone(),
      if letrec { ctx2.clone() } else { ctx.clone() },
      quasi.clone(),
    )(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = tag("in")(i)?;
    let (i, _) = parse_space(i)?;
    let (upto, bod) =
      parse_telescope(input, defs.clone(), rec.clone(), ctx2, quasi.clone())(
        i,
      )?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((
      upto,
      Term::Let(
        pos,
        letrec,
        uses,
        nam,
        Box::new(typ),
        Box::new(exp),
        Box::new(bod),
      ),
    ))
  }
}

pub fn parse_lty(
  input: Cid,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (upto, lty) = alt((
      value(LitType::Nat, tag("#Nat")),
      value(LitType::Int, tag("#Int")),
      value(LitType::Bits, tag("#Bits")),
      value(LitType::Bytes, tag("#Bytes")),
      value(LitType::Bool, tag("#Bool")),
      value(LitType::Text, tag("#Text")),
      value(LitType::Char, tag("#Char")),
      value(LitType::U8, tag("#U8")),
      value(LitType::U16, tag("#U16")),
      value(LitType::U32, tag("#U32")),
      value(LitType::U64, tag("#U64")),
      value(LitType::U128, tag("#U128")),
      value(LitType::I8, tag("#I8")),
      value(LitType::I16, tag("#I16")),
      value(LitType::I32, tag("#I32")),
      value(LitType::I64, tag("#I64")),
      value(LitType::I128, tag("#I128")),
    ))(from)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Term::LTy(pos, lty)))
  }
}

pub fn parse_lit(
  input: Cid,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (upto, lit) = alt((
      parse_bits,
      parse_bytes,
      parse_bool,
      parse_text,
      parse_char,
      parse_int,
      parse_nat,
    ))(from)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Term::Lit(pos, lit)))
  }
}

pub fn parse_term(
  input: Cid,
  defs: Defs,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Quasi,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |i: Span| {
    alt((
      context(
        "application telescope",
        delimited(
          preceded(tag("("), parse_space),
          parse_telescope(
            input,
            defs.clone(),
            rec.clone(),
            ctx.clone(),
            quasi.clone(),
          ),
          preceded(parse_space, tag(")")),
        ),
      ),
      context(
        "slf",
        parse_slf(input, defs.clone(), rec.clone(), ctx.clone(), quasi.clone()),
      ),
      context(
        "dat",
        parse_dat(input, defs.clone(), rec.clone(), ctx.clone(), quasi.clone()),
      ),
      context(
        "cse",
        parse_cse(input, defs.clone(), rec.clone(), ctx.clone(), quasi.clone()),
      ),
      context(
        "all",
        parse_all(input, defs.clone(), rec.clone(), ctx.clone(), quasi.clone()),
      ),
      context(
        "lam",
        parse_lam(input, defs.clone(), rec.clone(), ctx.clone(), quasi.clone()),
      ),
      context(
        "let",
        parse_let(input, defs.clone(), rec.clone(), ctx.clone(), quasi.clone()),
      ),
      context("type", parse_typ(input)),
      context("opr", parse_opr(input)),
      context("lit", parse_lit(input)),
      context("lty", parse_lty(input)),
      parse_antiquote(ctx.clone(), quasi.clone()),
      context(
        "var",
        parse_var(input, defs.to_owned(), rec.clone(), ctx.clone()),
      ),
    ))(i)
  }
}

pub fn input_cid(i: &str) -> Cid {
  Cid::new_v1(
    0x55,
    Code::Blake2b256.digest(
      DagCborCodec.encode(&i.to_owned()).unwrap().into_inner().as_ref(),
    ),
  )
}

pub fn parse(i: &str, defs: Defs) -> IResult<Span, Term, ParseError<Span>> {
  parse_term(input_cid(i), defs, None, Ctx::new(), Quasi::new())(Span::new(i))
}
pub fn parse_quasi(
  i: &str,
  defs: Defs,
  quasi: Quasi,
) -> IResult<Span, Term, ParseError<Span>> {
  parse_term(input_cid(i), defs, None, Ctx::new(), quasi)(Span::new(i))
}

#[macro_export]
macro_rules! yatima {
  ($i:literal) => {
    crate::parse::term::parse($i, crate::defs::Defs::new()).unwrap().1
  };
  ($i:literal, $($q: expr),*) => {{
    let mut quasi = sp_im::vector::Vector::new();
    $(quasi.push_back($q);)*
    crate::parse::term::parse_quasi($i,
      crate::defs::Defs::new(),
      sp_im::vector::Vector::from(quasi))
      .unwrap().1
  }}
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use crate::{
    literal::{
      LitType,
      Literal,
    },
    term::tests::test_defs,
  };

  #[test]
  fn test_parse_binder() {
    fn test(
      ctx: Vec<Name>,
      i: &str,
    ) -> IResult<Span, Vec<(Uses, Name, Term)>, ParseError<Span>> {
      parse_binder(
        input_cid(i),
        Defs::new(),
        None,
        Ctx::from(ctx),
        Quasi::new(),
      )(Span::new(i))
    }
    let res = test(vec![], "(ω a b c: Type)");
    assert!(res.is_ok());
    let res = res.unwrap().1;
    assert_eq!(res, vec![
      (Uses::Many, Name::from("a"), yatima!("Type")),
      (Uses::Many, Name::from("b"), yatima!("Type")),
      (Uses::Many, Name::from("c"), yatima!("Type"))
    ]);
    let res = test(vec![Name::from("A")], "(ω a b c: A)");
    assert!(res.is_ok());
    let res = res.unwrap().1;
    assert_eq!(res, vec![
      (Uses::Many, Name::from("a"), Term::Var(Pos::None, Name::from("A"), 0)),
      (Uses::Many, Name::from("b"), Term::Var(Pos::None, Name::from("A"), 1)),
      (Uses::Many, Name::from("c"), Term::Var(Pos::None, Name::from("A"), 2)),
    ]);
    let res = test(vec![Name::from("A")], "(ω a: (∀ (ω x: A) -> A))");
    assert!(res.is_ok());
    let res = res.unwrap().1;
    assert_eq!(res, vec![(
      Uses::Many,
      Name::from("a"),
      Term::All(
        Pos::None,
        Uses::Many,
        Name::from("x"),
        Box::new(Term::Var(Pos::None, Name::from("A"), 0)),
        Box::new(Term::Var(Pos::None, Name::from("A"), 1)),
      )
    ),]);
    fn test_binders(
      ctx: Vec<Name>,
      i: &str,
    ) -> IResult<Span, Vec<(Uses, Name, Term)>, ParseError<Span>> {
      parse_binders(
        input_cid(i),
        Defs::new(),
        None,
        Ctx::from(ctx),
        Quasi::new(),
        vec![':'],
      )(Span::new(i))
    }
    let res1 = test(vec![Name::from("A")], "(ω a : (∀ (ω x: A) -> A))");
    let res2 =
      test_binders(vec![Name::from("A")], "(ω a : (∀ (ω x: A) -> A)):");
    assert!(res1.is_ok() && res2.is_ok());
    let (res1, res2) = (res1.unwrap().1, res2.unwrap().1);
    assert_eq!(res1, res2);
    let res1 = test(vec![Name::from("A")], "(ω a b c: (∀ (ω x: A) -> A))");
    let res2 = test_binders(
      vec![Name::from("A")],
      "(ω a : (∀ (ω x: A) -> A))
       (ω b : (∀ (ω x: A) -> A))
       (ω c : (∀ (ω x: A) -> A))
    :",
    );
    assert!(res1.is_ok() && res2.is_ok());
    let (res1, res2) = (res1.unwrap().1, res2.unwrap().1);
    assert_eq!(res1, res2);
    let res1 =
      test(vec![Name::from("A")], "(ω a b c d e f g: (∀ (ω x y z w: A) -> A))");
    let res2 = test_binders(
      vec![Name::from("A")],
      "(ω a: (∀ (ω x y z w: A) -> A))
       (ω b: (∀ (ω x y z w: A) -> A))
       (ω c: (∀ (ω x y z w: A) -> A))
       (ω d: (∀ (ω x y z w: A) -> A))
       (ω e: (∀ (ω x y z w: A) -> A))
       (ω f: (∀ (ω x y z w: A) -> A))
       (ω g: (∀ (ω x y z w: A) -> A))
    :",
    );
    assert!(res1.is_ok() && res2.is_ok());
    let (res1, res2) = (res1.unwrap().1, res2.unwrap().1);
    assert_eq!(res1, res2)
  }

  #[test]
  fn test_parse_binders() {
    use Term::*;
    fn test(
      i: &str,
    ) -> IResult<Span, Vec<(Uses, Name, Term)>, ParseError<Span>> {
      parse_binders(
        input_cid(i),
        Defs::new(),
        None,
        Ctx::new(),
        Quasi::new(),
        vec![':'],
      )(Span::new(i))
    }
    let res = test(":");
    assert!(res.is_ok());
    let res = test("(ω A: Type) (ω a b c: A):");
    assert!(res.is_ok());
    assert!(
      res.unwrap().1
        == vec![
          (Uses::Many, Name::from("A"), Typ(Pos::None)),
          (Uses::Many, Name::from("a"), Var(Pos::None, Name::from("A"), 0)),
          (Uses::Many, Name::from("b"), Var(Pos::None, Name::from("A"), 1)),
          (Uses::Many, Name::from("c"), Var(Pos::None, Name::from("A"), 2)),
        ]
    );
    let res = test("(ω A: Type) (ω a b c: Unknown):");
    assert!(res.is_err());
    match res.unwrap_err() {
      Err::Error(err) => {
        assert!(
          err.errors
            == vec![ParseErrorKind::UndefinedReference(
              Name::from("Unknown"),
              ConsList::from(vec!(Name::from("A")))
            )]
        )
      }
      _ => {
        assert!(false)
      }
    }
    let res = test("(ω x: Unknown):");
    assert!(res.is_err());
    match res.unwrap_err() {
      Err::Error(err) => {
        assert!(
          err.errors
            == vec![ParseErrorKind::UndefinedReference(
              Name::from("Unknown"),
              ConsList::new(),
            )]
        );
      }
      _ => {
        assert!(false)
      }
    }
  }

  #[test]
  fn test_parse_telescope() {
    fn test(i: &str) -> IResult<Span, Term, ParseError<Span>> {
      parse_telescope(
        input_cid(i),
        Defs::new(),
        None,
        ConsList::new(),
        Quasi::new(),
      )(Span::new(i))
    }
    let res = test("(1 (3 :: ω 2))");
    println!("res {:?}", res);
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Term::App(
        Pos::None,
        Uses::Many,
        Box::new(Term::Lit(Pos::None, Literal::Nat(1u64.into()))),
        Box::new(Term::Lit(Pos::None, Literal::Nat(2u64.into()))),
        Box::new(Term::Lit(Pos::None, Literal::Nat(3u64.into()))),
      )
    );
    let res = test("(Type (Type :: 0 Type))");
    assert!(res.is_ok());
    let res = test("(1 (1 :: ω #Nat) (1 :: ω #Nat))");
    println!("res {:?}", res);
    assert!(res.is_ok());
    let res = test("(#Int (1 :: 0 #U16))");
    println!("res {:?}", res);
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Term::App(
        Pos::None,
        Uses::None,
        Box::new(Term::LTy(Pos::None, LitType::Int)),
        Box::new(Term::LTy(Pos::None, LitType::U16)),
        Box::new(Term::Lit(Pos::None, Literal::Nat(1u64.into()))),
      )
    );
  }

  #[test]
  fn test_parse_lam() {
    fn test(i: &str) -> IResult<Span, Term, ParseError<Span>> {
      parse_lam(input_cid(i), Defs::new(), None, Ctx::new(), Quasi::new())(
        Span::new(i),
      )
    }
    let res = test("λ (ω a: Type) => Type");
    println!("res: {:?}", res);
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Term::Lam(
        Pos::None,
        Uses::Many,
        Name::from("a"),
        Box::new(Term::Typ(Pos::None)),
        Box::new(Term::Typ(Pos::None))
      )
    );
    let res = test("λ (ω a: λ (ω x: Type) => Type) => Type");
    // println!("res: {:?}", res);
    assert!(res.is_ok());
    let res = test("λ (ω a: Type (Type :: ω Type)) => Type");
    println!("res: {:?}", res);
    assert!(res.is_ok());
    let res = test("λ (ω a: Type) (ω b: Type) => Type");
    println!("res: {:?}", res);
    assert!(res.is_ok());
  }

  #[test]
  fn test_parse_all() {
    fn test(i: &str) -> IResult<Span, Term, ParseError<Span>> {
      parse_all(input_cid(i), Defs::new(), None, Ctx::new(), Quasi::new())(
        Span::new(i),
      )
    }
    let res = test("∀ (ω a: Type) -> Type");
    // println!("res: {:?}", res);
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Term::All(
        Pos::None,
        Uses::Many,
        Name::from("a"),
        Box::new(Term::Typ(Pos::None)),
        Box::new(Term::Typ(Pos::None))
      )
    );
    let res = test("∀ (ω x: #Nat) -> ∀ (ω y: #Nat) -> #Nat");
    println!("res: {:?}", res);
    assert!(res.is_ok());
    let res = test("∀ (ω x: #Nat) (ω y: #Nat) -> #Nat");
    println!("res: {:?}", res);
    assert!(res.is_ok());
  }

  #[test]
  fn test_parse_cse() {
    fn test(i: &str) -> IResult<Span, Term, ParseError<Span>> {
      parse_cse(input_cid(i), Defs::new(), None, Ctx::new(), Quasi::new())(
        Span::new(i),
      )
    }
    let res = test("case Type");
    assert!(res.is_ok());
    let res = test("case (λ (ω x: Type) => Type)");
    println!("res: {:?}", res);
    assert!(res.is_ok());
  }

  #[test]
  fn test_parse_let() {
    fn test(i: &str) -> IResult<Span, Term, ParseError<Span>> {
      parse_let(input_cid(i), Defs::new(), None, Ctx::new(), Quasi::new())(
        Span::new(i),
      )
    }
    let res = test("let ω x: Type = Type in x");
    println!("res: {:?}", res);
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Term::Let(
        Pos::None,
        false,
        Uses::Many,
        Name::from("x"),
        Box::new(Term::Typ(Pos::None)),
        Box::new(Term::Typ(Pos::None)),
        Box::new(Term::Var(Pos::None, Name::from("x"), 0))
      )
    );
    let res = test("letrec ω x : Type = x in Type");
    println!("res: {:?}", res);
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Term::Let(
        Pos::None,
        true,
        Uses::Many,
        Name::from("x"),
        Box::new(Term::Typ(Pos::None)),
        Box::new(Term::Var(Pos::None, Name::from("x"), 0)),
        Box::new(Term::Typ(Pos::None)),
      )
    )
  }

  //#[test]
  // fn test_parse_term() {
  //  fn test(i: &str) -> IResult<Span, Term, ParseError<Span>> {
  //    parse_term(input_cid(i), Defs::new(), None, Ctx::new(), Quasi::new())(
  //      Span::new(i),
  //    )
  //  }
  //  let res = test("#Nat.eql");
  //  println!("res: {:?}", res);
  //  assert!(res.is_ok());
  //  let res = test("#Bool.true");
  //  println!("res: {:?}", res);
  //  assert!(res.is_ok());
  //}

  #[quickcheck]
  fn term_parse_print(x: Term) -> bool {
    let i = format!("{}", x);
    match parse_term(input_cid(&i), test_defs(), None, Ctx::new(), Quasi::new())(
      Span::new(&i),
    ) {
      Ok((_, y)) => {
        if x == y {
          true
        }
        else {
          println!("{:?}", x);
          println!("{}", x);
          println!("{:?}", y);
          println!("{}", y);
          false
        }
      }
      Err(e) => {
        println!("{}", x);
        println!("{}", e);
        false
      }
    }
  }
}
