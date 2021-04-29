use crate::{
  defs::Defs,
  parse::{
    error::{
      throw_err,
      ParseError,
      ParseErrorKind,
    },
    literal::*,
  },
  position::Pos,
  term::{
    LitType,
    PrimOp,
    Term,
    Uses,
  },
};

use std::rc::Rc;

use cid::Cid;
use libipld::{
  cbor::DagCborCodec,
  codec::Codec,
};
use multihash::{
  Code,
  MultihashDigest,
};

use crate::parse::span::Span;

use im::Vector;
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

pub fn parse_name(from: Span) -> IResult<Span, String, ParseError<Span>> {
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
    Ok((i, s))
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

pub fn parse_var(
  input: Cid,
  defs: Defs,
  rec: Option<Rc<String>>,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (upto, nam) = context("local or global reference", parse_name)(from)?;
    let pos = Pos::from_upto(input, from, upto);
    let is_rec_name = match rec.clone() {
      Some(rec_ref) => {
        let rec_name = rec_ref.as_ref();
        nam == *rec_name
      }
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
        ParseErrorKind::UndefinedReference(nam.clone(), ctx.to_owned()),
      )))
    }
  }
}

pub fn parse_lam(
  input: Cid,
  defs: Defs,
  rec: Option<Rc<String>>,
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
    let (upto, bod) =
      parse_expression(input, defs.clone(), rec.clone(), ctx2)(i)?;
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
  defs: Defs,
  rec: Option<Rc<String>>,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Vec<(Uses, String, Term)>, ParseError<Span>>
{
  move |i: Span| {
    let (i, _) = tag("(")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, u) = parse_uses(i)?;
    let (i, ns) = many1(terminated(parse_name, parse_space))(i)?;
    let (i, _) = tag(":")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, typ) =
      parse_expression(input, defs.to_owned(), rec.clone(), ctx.to_owned())(i)?;
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
  defs: Defs,
  rec: Option<Rc<String>>,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Vec<(Uses, String, Term)>, ParseError<Span>>
{
  move |i: Span| {
    map(parse_term(input, defs.to_owned(), rec.clone(), ctx.to_owned()), |t| {
      vec![(Uses::Many, String::from(""), t)]
    })(i)
  }
}

pub fn parse_binder(
  input: Cid,
  defs: Defs,
  rec: Option<Rc<String>>,
  ctx: Vector<String>,
  nam_opt: bool,
) -> impl Fn(Span) -> IResult<Span, Vec<(Uses, String, Term)>, ParseError<Span>>
{
  move |i: Span| {
    if nam_opt {
      alt((
        parse_binder_full(input, defs.clone(), rec.clone(), ctx.clone()),
        parse_binder_short(input, defs.to_owned(), rec.clone(), ctx.to_owned()),
      ))(i)
    }
    else {
      parse_binder_full(input, defs.to_owned(), rec.clone(), ctx.to_owned())(i)
    }
  }
}

pub fn parse_binders(
  input: Cid,
  defs: Defs,
  rec: Option<Rc<String>>,
  ctx: Vector<String>,
  nam_opt: bool,
) -> impl FnMut(Span) -> IResult<Span, Vec<(Uses, String, Term)>, ParseError<Span>>
{
  move |mut i: Span| {
    let mut ctx = ctx.to_owned();
    let mut res = Vec::new();

    match parse_binder(
      input,
      defs.to_owned(),
      rec.clone(),
      ctx.to_owned(),
      nam_opt,
    )(i.to_owned())
    {
      Err(e) => return Err(e),
      Ok((i1, bs)) => {
        for (u, n, t) in bs {
          ctx.push_front(n.to_owned());
          res.push((u, n, t));
        }
        i = i1;
      }
    }

    loop {
      match preceded(
        parse_space,
        parse_binder(
          input,
          defs.to_owned(),
          rec.clone(),
          ctx.to_owned(),
          nam_opt,
        ),
      )(i)
      {
        Err(Err::Error(_)) => return Ok((i, res)),
        Err(e) => return Err(e),
        Ok((i2, bs)) => {
          for (u, n, t) in bs {
            ctx.push_front(n.to_owned());
            res.push((u, n, t));
          }
          i = i2;
        }
      }
    }
  }
}

pub fn parse_all(
  input: Cid,
  defs: Defs,
  rec: Option<Rc<String>>,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = alt((tag("∀"), tag("forall")))(from)?;
    let (i, _) = parse_space(i)?;
    let (i, bs) =
      parse_binders(input, defs.clone(), rec.clone(), ctx.clone(), true)(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = tag("->")(i)?;
    let (i, _) = parse_space(i)?;
    let mut ctx2 = ctx.clone();
    for (_, n, _) in bs.iter() {
      ctx2.push_front(n.clone());
    }
    let (upto, bod) =
      parse_expression(input, defs.to_owned(), rec.clone(), ctx2)(i)?;
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
  defs: Defs,
  rec: Option<Rc<String>>,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = nom::character::complete::char('@')(from)?;
    let (i, n) = parse_name(i)?;
    let (i, _) = parse_space(i)?;
    let mut ctx2 = ctx.clone();
    ctx2.push_front(n.clone());
    let (upto, bod) =
      parse_expression(input, defs.to_owned(), rec.clone(), ctx2)(i)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Term::Slf(pos, n, Box::new(bod))))
  }
}

pub fn parse_case(
  input: Cid,
  defs: Defs,
  rec: Option<Rc<String>>,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("case")(from)?;
    let (i, _) = parse_space(i)?;
    let (upto, bod) =
      parse_expression(input, defs.to_owned(), rec.clone(), ctx.clone())(i)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Term::Cse(pos, Box::new(bod))))
  }
}

pub fn parse_data(
  input: Cid,
  defs: Defs,
  rec: Option<Rc<String>>,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("data")(from)?;
    let (i, _) = parse_space(i)?;
    let (upto, bod) =
      parse_expression(input, defs.to_owned(), rec.clone(), ctx.clone())(i)?;
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
  defs: Defs,
  rec: Option<Rc<String>>,
  ctx: Vector<String>,
  nam: String,
  letrec: bool,
) -> impl Fn(Span) -> IResult<Span, (Term, Term), ParseError<Span>> {
  move |from: Span| {
    let (i, bs) = alt((
      terminated(
        parse_binders(input, defs.clone(), rec.clone(), ctx.clone(), false),
        parse_space,
      ),
      success(Vec::new()),
    ))(from)?;
    let (i, _) = tag(":")(i)?;
    let (i, _) = parse_space(i)?;
    let mut type_ctx = ctx.clone();
    for (_, n, _) in bs.iter() {
      type_ctx.push_front(n.clone());
    }
    let (i, typ) =
      parse_expression(input, defs.clone(), rec.clone(), type_ctx)(i)?;
    let mut term_ctx = ctx.to_owned();
    if letrec {
      term_ctx.push_front(nam.clone());
    };
    for (_, n, _) in bs.iter() {
      term_ctx.push_front(n.clone());
    }
    let (i, _) = parse_space(i)?;
    let (i, _) = tag("=")(i)?;
    let (i, _) = parse_space(i)?;
    let (upto, trm) =
      parse_expression(input, defs.clone(), rec.clone(), term_ctx)(i)?;
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
  defs: Defs,
  rec: Option<Rc<String>>,
  ctx: Vector<String>,
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
      nam.clone(),
      letrec,
    )(i)?;
    let (i, _) = alt((tag(";"), tag("in")))(i)?;
    let (i, _) = parse_space(i)?;
    let mut ctx2 = ctx.clone();
    ctx2.push_front(nam.clone());
    let (upto, bod) =
      parse_expression(input, defs.to_owned(), rec.clone(), ctx2)(i)?;
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
      value(LitType::Char, tag("#Char")),
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
    let (i, lit) =
      alt((parse_bytes, parse_text, parse_char, parse_nat, parse_int))(from)?;
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

pub fn parse_opr(
  input: Cid,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, op) = alt((
      alt((
        value(PrimOp::NatEql, tag("#Nat.eql")),
        value(PrimOp::NatLth, tag("#Nat.lth")),
        value(PrimOp::NatLte, tag("#Nat.lte")),
        value(PrimOp::NatGth, tag("#Nat.gth")),
        value(PrimOp::NatGte, tag("#Nat.gte")),
        value(PrimOp::NatSuc, tag("#Nat.suc")),
        value(PrimOp::NatPre, tag("#Nat.pre")),
        value(PrimOp::NatAdd, tag("#Nat.add")),
        value(PrimOp::NatSub, tag("#Nat.sub")),
        value(PrimOp::NatMul, tag("#Nat.mul")),
        value(PrimOp::NatDiv, tag("#Nat.div")),
        value(PrimOp::NatMod, tag("#Nat.mod")),
      )),
      alt((
        value(PrimOp::IntNew, tag("#Int.new")),
        value(PrimOp::IntSgn, tag("#Int.sgn")),
        value(PrimOp::IntAbs, tag("#Int.abs")),
        value(PrimOp::IntEql, tag("#Int.eql")),
        value(PrimOp::IntLth, tag("#Int.lth")),
        value(PrimOp::IntLte, tag("#Int.lte")),
        value(PrimOp::IntGth, tag("#Int.gth")),
        value(PrimOp::IntGte, tag("#Int.gte")),
        value(PrimOp::IntAdd, tag("#Int.add")),
        value(PrimOp::IntSub, tag("#Int.sub")),
        value(PrimOp::IntMul, tag("#Int.mul")),
        value(PrimOp::IntDiv, tag("#Int.div")),
        value(PrimOp::IntMod, tag("#Int.mod")),
      )),
    ))(from)?;
    let (upto, _) = throw_err(parse_builtin_symbol_end()(i), |_| {
      ParseError::new(
        i,
        ParseErrorKind::PrimOpLacksWhitespaceTermination(op.to_owned()),
      )
    })?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Term::Opr(pos, op)))
  }
}

pub fn parse_expression(
  input: Cid,
  defs: Defs,
  rec: Option<Rc<String>>,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, trm) =
      parse_apps(input, defs.clone(), rec.clone(), ctx.clone())(from)?;
    let (i, has_ann) = opt(tag("::"))(i)?;
    if has_ann.is_some() {
      let (i, typ) = context(
        "type annotation",
        parse_apps(input, defs.clone(), rec.clone(), ctx.clone()),
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
  defs: Defs,
  rec: Option<Rc<String>>,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i2, _) = parse_space(from)?;
    let (i2, fun) =
      parse_term(input, defs.clone(), rec.clone(), ctx.clone())(i2)?;
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
          let (i2, arg) =
            parse_term(input, defs.clone(), rec.clone(), ctx.clone())(i2)?;
          args.push(arg);
          i = i2
        }
      }
    }
  }
}

pub fn parse_term(
  input: Cid,
  defs: Defs,
  rec: Option<Rc<String>>,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |i: Span| {
    context(
      "term",
      alt((
        delimited(
          preceded(tag("("), parse_space),
          context(
            "expression",
            parse_expression(input, defs.clone(), rec.clone(), ctx.clone()),
          ),
          context(
            "close parenthesis ')' of an expression",
            preceded(parse_space, tag(")")),
          ),
        ),
        parse_self(input, defs.clone(), rec.clone(), ctx.clone()),
        parse_data(input, defs.clone(), rec.clone(), ctx.clone()),
        parse_case(input, defs.clone(), rec.clone(), ctx.clone()),
        parse_all(input, defs.clone(), rec.clone(), ctx.clone()),
        parse_lam(input, defs.clone(), rec.clone(), ctx.clone()),
        parse_let(input, defs.clone(), rec.clone(), ctx.clone()),
        parse_type(input),
        parse_lty(input),
        parse_opr(input),
        parse_lit(input),
        parse_var(input, defs.to_owned(), rec.clone(), ctx.to_owned()),
      )),
    )(i)
  }
}

pub fn input_cid(i: &str) -> Cid {
  Cid::new_v1(
    0x55,
    Code::Blake2b256.digest(&DagCborCodec.encode(&i.to_owned()).unwrap()),
  )
}

pub fn parse(i: &str) -> IResult<Span, Term, ParseError<Span>> {
  parse_expression(input_cid(i), Defs::new(), None, Vector::new())(Span::new(i))
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use crate::term::tests::test_defs;

  #[test]
  fn test_parse_apps() {
    fn test(i: &str) -> IResult<Span, Term, ParseError<Span>> {
      parse_apps(input_cid(i), Defs::new(), None, Vector::new())(Span::new(i))
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
      parse_expression(input_cid(i), Defs::new(), None, Vector::new())(
        Span::new(i),
      )
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
  fn test_parse_binders() {
    use Term::*;
    fn test(
      nam_opt: bool,
      i: &str,
    ) -> IResult<Span, Vec<(Uses, String, Term)>, ParseError<Span>> {
      parse_binders(input_cid(i), Defs::new(), None, Vector::new(), nam_opt)(
        Span::new(i),
      )
    }
    fn test_full(
      i: &str,
    ) -> IResult<Span, Vec<(Uses, String, Term)>, ParseError<Span>> {
      parse_binder_full(input_cid(i), Defs::new(), None, Vector::new())(
        Span::new(i),
      )
    }
    let res = test_full("(a b c: Type)");
    assert!(res.is_ok());
    let res = test(true, "Type Type");
    assert!(res.is_ok());
    assert!(
      res.unwrap().1
        == vec![
          (Uses::Many, String::from(""), Typ(Pos::None)),
          (Uses::Many, String::from(""), Typ(Pos::None)),
        ]
    );
    let res = test(true, "(A: Type) (a b c: A)");
    assert!(res.is_ok());
    assert!(
      res.unwrap().1
        == vec![
          (Uses::Many, String::from("A"), Typ(Pos::None)),
          (Uses::Many, String::from("a"), Var(Pos::None, String::from("A"), 0)),
          (Uses::Many, String::from("b"), Var(Pos::None, String::from("A"), 1)),
          (Uses::Many, String::from("c"), Var(Pos::None, String::from("A"), 2)),
        ]
    );
  }

  #[quickcheck]
  fn term_parse_print(x: Term) -> bool {
    let i = format!("{}", x);
    match parse_expression(input_cid(&i), test_defs(), None, Vector::new())(
      Span::new(&i),
    ) {
      Ok((_, y)) => x == y,
      Err(e) => {
        println!("{}", x);
        println!("{}", e);
        false
      }
    }
  }
}
