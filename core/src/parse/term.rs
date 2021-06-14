use crate::{
  defs::Defs,
  name::Name,
  parse::{
    error::{
      throw_err,
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

use cid::Cid;
use multihash::{
  Code,
  MultihashDigest,
};

use sp_im::ConsList;
use sp_ipld::{
  dag_cbor::DagCborCodec,
  Codec,
};
use sp_std::{
  cell::RefCell,
  rc::Rc,
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
    eof,
    map,
    opt,
    peek,
    success,
    value,
  },
  error::context,
  multi::{
    many0,
    many1,
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
use std::collections::VecDeque;

type Ctx = ConsList<Name>;

pub fn reserved_symbols() -> VecDeque<String> {
  VecDeque::from(vec![
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
    String::from("let"),
    String::from("in"),
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

pub fn is_valid_symbol_char(c: char) -> bool {
  c != ':'
    && c != ';'
    && c != '('
    && c != ')'
    && c != ','
    && !char::is_whitespace(c)
    && !char::is_control(c)
}

pub fn is_valid_symbol_string(s: &str) -> bool {
  let invalid_chars = s.starts_with('"')
    || s.starts_with('\'')
    || s.starts_with('#')
    || s.chars().any(|x| !is_valid_symbol_char(x));
  !s.is_empty() && !invalid_chars
}

pub fn parse_antiquote(
  ctx: Ctx,
  quasi: Rc<VecDeque<Term>>,
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
  defs: Rc<RefCell<Defs>>,
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
    else if let Some(def) = defs.as_ref().borrow().get(&nam) {
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

pub fn parse_lam(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Rc<VecDeque<Term>>,
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
      ctx2 = ctx2.cons(n);
    }
    let (upto, bod) = parse_expression(
      input,
      defs.clone(),
      rec.clone(),
      ctx2,
      quasi.to_owned(),
    )(i)?;
    let pos = Pos::from_upto(input, from, upto);
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
  input: Cid,
  defs: Rc<RefCell<Defs>>,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Rc<VecDeque<Term>>,
) -> impl Fn(Span) -> IResult<Span, Vec<(Uses, Name, Term)>, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("(")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, u) = parse_uses(i)?;
    let (i, ns) = many1(terminated(parse_name, parse_space))(i)?;
    let (i, _) = tag(":")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, typ) = parse_expression(
      input,
      defs.clone(),
      rec.clone(),
      ctx.clone(),
      quasi.to_owned(),
    )(i)?;
    let (i, _) = tag(")")(i)?;
    let mut res = Vec::new();
    for (i, n) in ns.iter().enumerate() {
      res.push((u, n.to_owned(), typ.clone().shift(i as u64, 0)))
    }
    Ok((i, res))
  }
}

pub fn parse_binder_short(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Rc<VecDeque<Term>>,
) -> impl Fn(Span) -> IResult<Span, Vec<(Uses, Name, Term)>, ParseError<Span>> {
  move |i: Span| {
    map(
      parse_term(
        input,
        defs.clone(),
        rec.clone(),
        ctx.clone(),
        quasi.to_owned(),
      ),
      |t| vec![(Uses::Many, Name::from(""), t)],
    )(i)
  }
}

pub fn parse_binder(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Rc<VecDeque<Term>>,
  nam_opt: bool,
) -> impl Fn(Span) -> IResult<Span, Vec<(Uses, Name, Term)>, ParseError<Span>> {
  move |i: Span| {
    if nam_opt {
      alt((
        parse_binder_full(
          input,
          defs.clone(),
          rec.clone(),
          ctx.clone(),
          quasi.clone(),
        ),
        parse_binder_short(
          input,
          defs.to_owned(),
          rec.clone(),
          ctx.clone(),
          quasi.to_owned(),
        ),
      ))(i)
    }
    else {
      parse_binder_full(
        input,
        defs.to_owned(),
        rec.clone(),
        ctx.clone(),
        quasi.to_owned(),
      )(i)
    }
  }
}

pub fn parse_binders(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Rc<VecDeque<Term>>,
  nam_opt: bool,
  terminator: &'static str,
) -> impl FnMut(Span) -> IResult<Span, Vec<(Uses, Name, Term)>, ParseError<Span>>
{
  move |mut i: Span| {
    let mut ctx = ctx.clone();
    let mut res = Vec::new();

    loop {
      match preceded(parse_space, tag(terminator))(i) {
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
          nam_opt,
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
  defs: Rc<RefCell<Defs>>,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Rc<VecDeque<Term>>,
  nam_opt: bool,
  terminator: &'static str,
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
      nam_opt,
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
      nam_opt,
      terminator,
    )(i)?;
    res.append(&mut res2);
    Ok((i, res))
  }
}

pub fn parse_all(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Rc<VecDeque<Term>>,
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
      true,
      "->",
    )(i)?;
    let (i, _) = parse_space(i)?;
    let mut ctx2 = ctx.clone();
    for (_, n, _) in bs.iter() {
      ctx2 = ctx2.cons(n.clone());
    }
    let (upto, bod) = parse_expression(
      input,
      defs.to_owned(),
      rec.clone(),
      ctx2,
      quasi.to_owned(),
    )(i)?;
    let pos = Pos::from_upto(input, from, upto);
    let trm = bs
      .into_iter()
      .rev()
      .fold(bod, |acc, (u, n, t)| Term::All(pos, u, n, Box::new((t, acc))));
    Ok((upto, trm))
  }
}

pub fn parse_type(
  input: Cid,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (upto, _) = tag("Type")(from)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Term::Typ(pos)))
  }
}

pub fn parse_self(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Rc<VecDeque<Term>>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = nom::character::complete::char('@')(from)?;
    let (i, n) = parse_name(i)?;
    let (i, _) = parse_space(i)?;
    let mut ctx2 = ctx.clone();
    ctx2 = ctx2.cons(n.clone());
    let (upto, bod) = parse_expression(
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

pub fn parse_case(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Rc<VecDeque<Term>>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("case")(from)?;
    let (i, _) = parse_space(i)?;
    let (upto, bod) = parse_expression(
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

pub fn parse_data(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Rc<VecDeque<Term>>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("data")(from)?;
    let (i, _) = parse_space(i)?;
    let (upto, bod) = parse_expression(
      input,
      defs.to_owned(),
      rec.clone(),
      ctx.clone(),
      quasi.to_owned(),
    )(i)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Term::Dat(pos, Box::new(bod))))
  }
}

/// The input `(A: Type) (x: A) : A = x` returns:
///   - type: `∀ (A: Type) (x: A) -> A`
///   - term: `λ A x => x`
/// This is useful for parsing lets and defs
pub fn parse_bound_expression(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Rc<VecDeque<Term>>,
  nam: Name,
  letrec: bool,
) -> impl Fn(Span) -> IResult<Span, (Term, Term), ParseError<Span>> {
  move |from: Span| {
    let (i, bs) = parse_binders(
      input,
      defs.clone(),
      rec.clone(),
      ctx.clone(),
      quasi.clone(),
      false,
      ":",
    )(from)?;
    let (i, _) = parse_space(i)?;
    let mut type_ctx = ctx.clone();
    for (_, n, _) in bs.iter() {
      type_ctx = type_ctx.cons(n.clone());
    }
    let (i, typ) = parse_expression(
      input,
      defs.clone(),
      rec.clone(),
      type_ctx,
      quasi.clone(),
    )(i)?;
    let mut term_ctx = ctx.clone();
    if letrec {
      term_ctx = term_ctx.cons(nam.clone());
    };
    for (_, n, _) in bs.iter() {
      term_ctx = term_ctx.cons(n.clone());
    }
    let (i, _) = parse_space(i)?;
    let (i, _) = tag("=")(i)?;
    let (i, _) = parse_space(i)?;
    let (upto, trm) = parse_expression(
      input,
      defs.clone(),
      rec.clone(),
      term_ctx,
      quasi.clone(),
    )(i)?;
    let pos = Pos::from_upto(input, from, upto);
    let trm = bs
      .iter()
      .rev()
      .fold(trm, |acc, (_, n, _)| Term::Lam(pos, n.clone(), Box::new(acc)));
    let typ = bs
      .into_iter()
      .rev()
      .fold(typ, |acc, (u, n, t)| Term::All(pos, u, n, Box::new((t, acc))));
    Ok((upto, (typ, trm)))
  }
}

pub fn parse_let(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Rc<VecDeque<Term>>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, letrec) =
      alt((value(true, tag("letrec")), value(false, tag("let"))))(from)?;
    let (i, _) = parse_space(i)?;
    let (i, uses) = parse_uses(i)?;
    let (i, _) = parse_space(i)?;
    let (i, nam) = parse_name(i)?;
    let (i, _) = parse_space(i)?;
    let (i, (typ, exp)) = parse_bound_expression(
      input,
      defs.clone(),
      rec.clone(),
      ctx.clone(),
      quasi.clone(),
      nam.clone(),
      letrec,
    )(i)?;
    let (i, _) = alt((tag(";"), tag("in")))(i)?;
    let (i, _) = parse_space(i)?;
    let mut ctx2 = ctx.clone();
    ctx2 = ctx2.cons(nam.clone());
    let (upto, bod) = parse_expression(
      input,
      defs.to_owned(),
      rec.clone(),
      ctx2,
      quasi.to_owned(),
    )(i)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Term::Let(pos, letrec, uses, nam, Box::new((typ, exp, bod)))))
  }
}

pub fn parse_builtin_symbol_end()
-> impl Fn(Span) -> IResult<Span, (), ParseError<Span>> {
  move |from: Span| {
    alt((
      peek(value((), parse_space1)),
      peek(value((), eof)),
      peek(value((), tag("("))),
      peek(value((), tag(")"))),
      peek(value((), tag(";"))),
      peek(value((), tag(":"))),
      peek(value((), tag(","))),
    ))(from)
  }
}

pub fn parse_lty(
  input: Cid,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, lty) = alt((
      value(LitType::Nat, tag("#Nat")),
      value(LitType::Int, tag("#Int")),
      value(LitType::Bytes, tag("#Bytes")),
      value(LitType::Text, tag("#Text")),
      value(LitType::Bool, tag("#Bool")),
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
    let (upto, _) = throw_err(parse_builtin_symbol_end()(i), |_| {
      ParseError::new(
        i,
        ParseErrorKind::LitTypeLacksWhitespaceTermination(lty.to_owned()),
      )
    })?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Term::LTy(pos, lty)))
  }
}
// pub fn parse_exception()
//-> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
//  move |from: Span| {
//    let (i, _) = tag("#!")(from)?;
//    let p = |i| hashexpr::string::parse_string("\"", i);
//    let (upto, val) = delimited(tag("\""), p, tag("\""))(i)
//      .map_err(|e| error::convert(i, e))?;
//    let pos = Some(Pos::from_upto(input,from, upto));
//    Ok((upto, Term::Lit(pos, Literal::Exception(val))))
//  }
//}

pub fn parse_lit(
  input: Cid,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, lit) = alt((
      parse_bits,
      parse_bytes,
      parse_bool,
      parse_text,
      parse_char,
      parse_int,
      parse_nat,
    ))(from)?;
    let (upto, _) = throw_err(parse_builtin_symbol_end()(i), |_| {
      ParseError::new(
        i,
        ParseErrorKind::LiteralLacksWhitespaceTermination(lit.to_owned()),
      )
    })?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Term::Lit(pos, lit)))
  }
}

pub fn parse_expression(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Rc<VecDeque<Term>>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, trm) =
      parse_apps(input, defs.clone(), rec.clone(), ctx.clone(), quasi.clone())(
        from,
      )?;
    let (i, has_ann) = opt(tag("::"))(i)?;
    if has_ann.is_some() {
      let (i, typ) = context(
        "type annotation",
        parse_apps(
          input,
          defs.clone(),
          rec.clone(),
          ctx.clone(),
          quasi.clone(),
        ),
      )(i)?;
      let pos = Pos::from_upto(input, from, i);
      Ok((i, Term::Ann(pos, Box::new((typ, trm)))))
    }
    else {
      Ok((i, trm))
    }
  }
}

pub fn parse_app_end(i: Span) -> IResult<Span, (), ParseError<Span>> {
  let (i, _) = alt((
    peek(tag("def")),
    peek(tag("open")),
    peek(tag("::")),
    peek(tag("=")),
    peek(tag("->")),
    peek(tag(";")),
    peek(tag(")")),
    peek(eof),
  ))(i)?;
  Ok((i, ()))
}

pub fn parse_apps(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Rc<VecDeque<Term>>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i2, _) = parse_space(from)?;
    let (i2, fun) =
      parse_term(input, defs.clone(), rec.clone(), ctx.clone(), quasi.clone())(
        i2,
      )?;
    let mut i = i2;
    let mut args = Vec::new();
    loop {
      let (i2, _) = parse_space(i)?;
      match parse_app_end(i2) {
        Ok((..)) => {
          let pos = Pos::from_upto(input, from, i2);
          let trm = args
            .into_iter()
            .fold(fun, |acc, arg| Term::App(pos, Box::new((acc, arg))));
          return Ok((i2, trm));
        }
        _ => {
          let (i2, arg) = parse_term(
            input,
            defs.clone(),
            rec.clone(),
            ctx.clone(),
            quasi.clone(),
          )(i2)?;
          args.push(arg);
          i = i2
        }
      }
    }
  }
}

pub fn parse_term(
  input: Cid,
  defs: Rc<RefCell<Defs>>,
  rec: Option<Name>,
  ctx: Ctx,
  quasi: Rc<VecDeque<Term>>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |i: Span| {
    context(
      "term",
      alt((
        delimited(
          preceded(tag("("), parse_space),
          context(
            "expression",
            parse_expression(
              input,
              defs.clone(),
              rec.clone(),
              ctx.clone(),
              quasi.clone(),
            ),
          ),
          context(
            "close parenthesis ')' of an expression",
            preceded(parse_space, tag(")")),
          ),
        ),
        parse_self(
          input,
          defs.clone(),
          rec.clone(),
          ctx.clone(),
          quasi.clone(),
        ),
        parse_data(
          input,
          defs.clone(),
          rec.clone(),
          ctx.clone(),
          quasi.clone(),
        ),
        parse_case(
          input,
          defs.clone(),
          rec.clone(),
          ctx.clone(),
          quasi.clone(),
        ),
        parse_all(input, defs.clone(), rec.clone(), ctx.clone(), quasi.clone()),
        parse_lam(input, defs.clone(), rec.clone(), ctx.clone(), quasi.clone()),
        parse_let(input, defs.clone(), rec.clone(), ctx.clone(), quasi.clone()),
        parse_type(input),
        parse_lty(input),
        parse_opr(input),
        parse_lit(input),
        parse_antiquote(ctx.clone(), quasi.clone()),
        parse_var(input, defs.to_owned(), rec.clone(), ctx.clone()),
      )),
    )(i)
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
  parse_expression(
    input_cid(i),
    Rc::new(RefCell::new(defs)),
    None,
    ConsList::new(),
    Rc::new(VecDeque::new()),
  )(Span::new(i))
}
pub fn parse_quasi(
  i: &str,
  defs: Rc<RefCell<Defs>>,
  quasi: Rc<VecDeque<Term>>,
) -> IResult<Span, Term, ParseError<Span>> {
  parse_expression(input_cid(i), defs, None, ConsList::new(), quasi)(Span::new(
    i,
  ))
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
      sp_std::rc::Rc::new(sp_std::cell::RefCell::new(crate::defs::Defs::new())),
      sp_std::rc::Rc::new(sp_std::collections::vec_deque::VecDeque::from(quasi)))
      .unwrap().1
  }}
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use crate::term::tests::test_defs;

  #[test]
  fn test_parse_apps() {
    fn test(i: &str) -> IResult<Span, Term, ParseError<Span>> {
      parse_apps(
        input_cid(i),
        Rc::new(RefCell::new(Defs::new())),
        None,
        ConsList::new(),
        Rc::new(VecDeque::new()),
      )(Span::new(i))
    }
    let res = test("0d1");
    assert!(res.is_ok());
    let res = test("0d1 0d1");
    assert!(res.is_ok());
    let res = test("0d1 0d1 def");
    assert!(res.is_ok());
  }

  #[test]
  fn test_parse_expression() {
    fn test(i: &str) -> IResult<Span, Term, ParseError<Span>> {
      parse_expression(
        input_cid(i),
        Rc::new(RefCell::new(Defs::new())),
        None,
        ConsList::new(),
        Rc::new(VecDeque::new()),
      )(Span::new(i))
    }
    let res = test("(Type :: Type)");
    assert!(res.is_ok());
    let res = test("(Type (Type Type)  )");
    assert!(res.is_ok());
    let res = test(
      "λ x c n => (c x (c x (c x (c x (c x (c x (c x (c x (c x (c x (c x x (c \
       x (c x (c x (c x n)))))))))))))))",
    );
    assert!(res.is_ok());
    let res = test("∀ Type -> Type");
    assert!(res.is_ok());
    let res = test("∀ (_ :Type) -> Type");
    assert!(res.is_ok());
  }
  #[test]
  fn test_parse_binder_full() {
    fn test(
      i: &str,
    ) -> IResult<Span, Vec<(Uses, Name, Term)>, ParseError<Span>> {
      parse_binder_full(
        input_cid(i),
        Rc::new(RefCell::new(Defs::new())),
        None,
        ConsList::new(),
        Rc::new(VecDeque::new()),
      )(Span::new(i))
    }
    let res = test("(a b c: Type)");
    assert!(res.is_ok());
  }

  #[test]
  fn test_parse_alls() {
    fn test(i: &str) -> IResult<Span, Term, ParseError<Span>> {
      parse_all(
        input_cid(i),
        Rc::new(RefCell::new(Defs::new())),
        None,
        ConsList::new(),
        Rc::new(VecDeque::new()),
      )(Span::new(i))
    }
    let res = test("∀ (a b c: Type) -> Type");
    println!("res: {:?}", res);
    assert!(res.is_ok());
  }

  #[test]
  fn test_parse_let() {
    fn test(i: &str) -> IResult<Span, Term, ParseError<Span>> {
      parse_let(
        input_cid(i),
        Rc::new(RefCell::new(Defs::new())),
        None,
        ConsList::new(),
        Rc::new(VecDeque::new()),
      )(Span::new(i))
    }
    let res = test("let 0 x: Type = Type; x");
    assert!(res.is_ok());
    let res = test("let 0 f (x: Unknown) = Type; x");
    println!("res: {:?}", res);
    match res.unwrap_err() {
      Err::Error(err) => {
        assert!(
          err.errors
            == vec![ParseErrorKind::UndefinedReference(
              Name::from("Unknown"),
              ConsList::new(),
            )]
        )
      }
      _ => {
        assert!(false)
      }
    }
  }
  #[test]
  fn test_parse_bound_expression() {
    fn test(i: &str) -> IResult<Span, (Term, Term), ParseError<Span>> {
      parse_bound_expression(
        input_cid(i),
        Rc::new(RefCell::new(Defs::new())),
        None,
        ConsList::new(),
        Rc::new(VecDeque::new()),
        Name::from("test"),
        false,
      )(Span::new(i))
    }
    let res = test(": Type = Type");
    assert!(res.is_ok());
    let res = test("(x: Type): Type = Type");
    assert!(res.is_ok());
    let res = test("(x: Unknown): Type = Type");
    match res.unwrap_err() {
      Err::Error(err) => {
        println!("err: {:?}", err);
        assert!(
          err.errors
            == vec![ParseErrorKind::UndefinedReference(
              Name::from("Unknown"),
              ConsList::new()
            )]
        )
      }
      _ => {
        assert!(false)
      }
    }
  }
  #[test]
  fn test_parse_binders1() {
    use Term::*;
    fn test(
      nam_opt: bool,
      i: &str,
    ) -> IResult<Span, Vec<(Uses, Name, Term)>, ParseError<Span>> {
      parse_binders(
        input_cid(i),
        Rc::new(RefCell::new(Defs::new())),
        None,
        ConsList::new(),
        Rc::new(VecDeque::new()),
        nam_opt,
        ":",
      )(Span::new(i))
    }
    let res = test(true, "Type #Text:");
    assert!(res.is_ok());
    assert!(
      res.unwrap().1
        == vec![
          (Uses::Many, Name::from(""), Typ(Pos::None)),
          (Uses::Many, Name::from(""), LTy(Pos::None, LitType::Text)),
        ]
    );
  }

  #[test]
  fn test_parse_binders() {
    use Term::*;
    fn test(
      nam_opt: bool,
      i: &str,
    ) -> IResult<Span, Vec<(Uses, Name, Term)>, ParseError<Span>> {
      parse_binders(
        input_cid(i),
        Rc::new(RefCell::new(Defs::new())),
        None,
        ConsList::new(),
        Rc::new(VecDeque::new()),
        nam_opt,
        ":",
      )(Span::new(i))
    }
    let res = test(true, ":");
    assert!(res.is_ok());
    let res = test(true, "Type Type:");
    assert!(res.is_ok());
    assert!(
      res.unwrap().1
        == vec![
          (Uses::Many, Name::from(""), Typ(Pos::None)),
          (Uses::Many, Name::from(""), Typ(Pos::None)),
        ]
    );
    let res = test(true, "(A: Type) (a b c: A):");
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
    let res = test(true, "(A: Type) (a b c: Unknown):");
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
    let res = test(false, "(x: Unknown):");
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

  #[quickcheck]
  fn term_parse_print(x: Term) -> bool {
    let i = format!("{}", x);
    match parse_expression(
      input_cid(&i),
      Rc::new(RefCell::new(test_defs())),
      None,
      ConsList::new(),
      Rc::new(VecDeque::new()),
    )(Span::new(&i))
    {
      Ok((_, y)) => x == y,
      Err(e) => {
        println!("{}", x);
        println!("{}", e);
        false
      }
    }
  }
}
