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
  },
  combinator::{
    opt,
    value,
  },
  error::context,
  multi::many0,
  sequence::{
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

pub fn parse_app(
  input: Cid,
  defs: Defs,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Quasi,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = terminated(tag("("), parse_space)(from)?;
    let (i, fun) = context(
      "app fun",
      parse_term(input, defs.clone(), rec.clone(), ctx.clone(), quasi.clone()),
    )(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = terminated(tag("("), parse_space)(i)?;
    let (i, arg) = context(
      "app arg",
      parse_term(input, defs.clone(), rec.clone(), ctx.clone(), quasi.clone()),
    )(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = terminated(tag("::"), parse_space)(i)?;
    let (i, u) = terminated(parse_uses(), parse_space)(i)?;
    let (i, typ) = context(
      "app typ",
      parse_term(input, defs.clone(), rec.clone(), ctx.clone(), quasi.clone()),
    )(i)?;
    let (i, _) = preceded(parse_space, tag(")"))(i)?;
    let (upto, _) = preceded(parse_space, tag(")"))(i)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Term::App(pos, u, Box::new((fun, typ, arg)))))
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
    let (i, _) = terminated(tag("("), parse_space)(from)?;
    let (i, _) = alt((tag("λ"), tag("lambda")))(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = terminated(tag("("), parse_space)(i)?;
    let (i, u) = terminated(parse_uses(), parse_space)(i)?;
    let (i, n) = terminated(parse_name, parse_space)(i)?;
    let (i, _) = terminated(tag(":"), parse_space)(i)?;
    let (i, t) = parse_term(
      input,
      defs.clone(),
      rec.clone(),
      ctx.clone(),
      quasi.to_owned(),
    )(i)?;
    let (i, _) = preceded(parse_space, tag(")"))(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = terminated(tag("=>"), parse_space)(i)?;
    let mut ctx2 = ctx.clone();
    ctx2 = ctx2.cons(n.clone());
    let (i, bod) =
      parse_term(input, defs.to_owned(), rec.clone(), ctx2, quasi.to_owned())(
        i,
      )?;
    let (upto, _) = preceded(parse_space, tag(")"))(i)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Term::Lam(pos, u, n, Box::new((t, bod)))))
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
    let (i, _) = preceded(tag("("), parse_space)(from)?;
    let (i, _) = alt((tag("∀"), tag("lambda")))(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = terminated(tag("("), parse_space)(i)?;
    let (i, u) = terminated(parse_uses(), parse_space)(i)?;
    let (i, n) = terminated(parse_name, parse_space)(i)?;
    let (i, _) = terminated(tag(":"), parse_space)(i)?;
    let (i, t) = parse_term(
      input,
      defs.clone(),
      rec.clone(),
      ctx.clone(),
      quasi.to_owned(),
    )(i)?;
    let (i, _) = preceded(parse_space, tag(")"))(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = tag("->")(i)?;
    let (i, _) = parse_space(i)?;
    let mut ctx2 = ctx.clone();
    ctx2 = ctx2.cons(n.clone());
    let (i, bod) =
      parse_term(input, defs.to_owned(), rec.clone(), ctx2, quasi.to_owned())(
        i,
      )?;
    let (upto, _) = preceded(parse_space, tag(")"))(i)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Term::All(pos, u, n, Box::new((t, bod)))))
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
    let (i, _) = preceded(tag("("), parse_space)(from)?;
    let (i, _) = tag("self")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, n) = parse_name(i)?;
    let (i, _) = parse_space(i)?;
    let mut ctx2 = ctx.clone();
    ctx2 = ctx2.cons(n.clone());
    let (i, bod) = parse_term(
      input,
      defs.to_owned(),
      rec.clone(),
      ctx2.clone(),
      quasi.to_owned(),
    )(i)?;
    let (upto, _) = preceded(parse_space, tag(")"))(i)?;
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
    let (i, _) = preceded(tag("("), parse_space)(from)?;
    let (i, _) = tag("case")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, bod) = parse_term(
      input,
      defs.to_owned(),
      rec.clone(),
      ctx.clone(),
      quasi.to_owned(),
    )(i)?;
    let (upto, _) = preceded(parse_space, tag(")"))(i)?;
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
    let (i, _) = preceded(tag("("), parse_space)(from)?;
    let (i, _) = tag("data")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, typ) =
      parse_term(input, defs.clone(), rec.clone(), ctx.clone(), quasi.clone())(
        i,
      )?;
    let (i, _) = parse_space(i)?;
    let (i, bod) =
      parse_term(input, defs.clone(), rec.clone(), ctx.clone(), quasi.clone())(
        i,
      )?;
    let (upto, _) = preceded(parse_space, tag(")"))(i)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Term::Dat(pos, Box::new((typ, bod)))))
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
    let (i, _) = preceded(tag("("), parse_space)(from)?;
    let (i, letrec) =
      alt((value(true, tag("letrec")), value(false, tag("let"))))(i)?;
    let (i, _) = parse_space(i)?;
    let (i, uses) = parse_uses()(i)?;
    let (i, _) = parse_space(i)?;
    let (i, nam) = parse_name(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = tag(":")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, typ) =
      parse_term(input, defs.clone(), rec.clone(), ctx.clone(), quasi.clone())(
        i,
      )?;
    let (i, _) = parse_space(i)?;
    let (i, _) = tag("=")(i)?;
    let (i, _) = parse_space(i)?;
    let ctx2 = ctx.clone().cons(nam.clone());
    let (i, exp) = parse_term(
      input,
      defs.clone(),
      rec.clone(),
      if letrec { ctx2.clone() } else { ctx.clone() },
      quasi.clone(),
    )(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = tag("in")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, bod) =
      parse_term(input, defs.clone(), rec.clone(), ctx2, quasi.clone())(i)?;
    let (upto, _) = preceded(parse_space, tag(")"))(i)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Term::Let(pos, letrec, uses, nam, Box::new((typ, exp, bod)))))
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
      context(
        "app",
        parse_app(input, defs.clone(), rec.clone(), ctx.clone(), quasi.clone()),
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
    let mut quasi = Vec::new();
    $(quasi.push($q);)*
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
  fn test_parse_app() {
    fn test(i: &str) -> IResult<Span, Term, ParseError<Span>> {
      parse_app(input_cid(i), Defs::new(), None, ConsList::new(), Quasi::new())(
        Span::new(i),
      )
    }
    let res = test("(1 (1 :: ω #Nat))");
    println!("res {:?}", res);
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Term::App(
        Pos::None,
        Uses::Many,
        Box::new((
          Term::Lit(Pos::None, Literal::Nat(1u64.into())),
          Term::LTy(Pos::None, LitType::Nat),
          Term::Lit(Pos::None, Literal::Nat(1u64.into())),
        ))
      )
    );
    let res = test("(Type (Type :: 0 Type))");
    assert!(res.is_ok());
  }

  #[test]
  fn test_parse_lam() {
    fn test(i: &str) -> IResult<Span, Term, ParseError<Span>> {
      parse_lam(input_cid(i), Defs::new(), None, Ctx::new(), Quasi::new())(
        Span::new(i),
      )
    }
    let res = test("(λ (ω a: Type) => Type)");
    // println!("res: {:?}", res);
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Term::Lam(
        Pos::None,
        Uses::Many,
        Name::from("a"),
        Box::new((Term::Typ(Pos::None), Term::Typ(Pos::None)))
      )
    );
    let res = test("(λ (ω a: (λ (ω x: Type) => Type)) => Type)");
    // println!("res: {:?}", res);
    assert!(res.is_ok());
    let res = test("(λ (ω a: (Type (Type :: ω Type))) => Type)");
    // println!("res: {:?}", res);
    assert!(res.is_ok());
  }

  #[test]
  fn test_parse_all() {
    fn test(i: &str) -> IResult<Span, Term, ParseError<Span>> {
      parse_all(input_cid(i), Defs::new(), None, Ctx::new(), Quasi::new())(
        Span::new(i),
      )
    }
    let res = test("(∀ (ω a: Type) -> Type)");
    println!("res: {:?}", res);
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Term::All(
        Pos::None,
        Uses::Many,
        Name::from("a"),
        Box::new((Term::Typ(Pos::None), Term::Typ(Pos::None)))
      )
    )
  }

  #[test]
  fn test_parse_let() {
    fn test(i: &str) -> IResult<Span, Term, ParseError<Span>> {
      parse_let(input_cid(i), Defs::new(), None, Ctx::new(), Quasi::new())(
        Span::new(i),
      )
    }
    let res = test("(let ω x : Type = Type in x)");
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Term::Let(
        Pos::None,
        false,
        Uses::Many,
        Name::from("x"),
        Box::new((
          Term::Typ(Pos::None),
          Term::Typ(Pos::None),
          Term::Var(Pos::None, Name::from("x"), 0)
        ))
      )
    );
    let res = test("(letrec ω x : Type = x in Type)");
    println!("res: {:?}", res);
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Term::Let(
        Pos::None,
        true,
        Uses::Many,
        Name::from("x"),
        Box::new((
          Term::Typ(Pos::None),
          Term::Var(Pos::None, Name::from("x"), 0),
          Term::Typ(Pos::None),
        ))
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
          println!("{}", x);
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
