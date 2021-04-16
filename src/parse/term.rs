use crate::{
  parse::{
    error::{
      throw_err,
      ParseError,
      ParseErrorKind,
    },
    literal::*,
  },
  term::{
    LitType,
    PrimOp,
    Refs,
    Term,
    Uses,
  },
};

use crate::parse::span::Span;

use hashexpr::position::Pos;

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

#[must_use]
pub fn reserved_symbols() -> Vector<String> {
  Vector::from(vec![
    String::from("//"),
    String::from("\u{3bb}"),
    String::from("lambda"),
    String::from("=>"),
    String::from("{"),
    String::from("}"),
    String::from("\u{2200}"),
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
  else if s.starts_with('#') | s.starts_with("~\"") {
    Err(Err::Error(ParseError::new(from, ParseErrorKind::HashExprSyntax(s))))
  }
  else if is_numeric_symbol_string1(&s) || is_numeric_symbol_string2(&s) {
    Err(Err::Error(ParseError::new(from, ParseErrorKind::NumericSyntax(s))))
  }
  else if is_valid_symbol_string(&s) {
    Ok((i, s))
  }
  else {
    Err(Err::Error(ParseError::new(from, ParseErrorKind::InvalidSymbol(s))))
  }
}

#[must_use]
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

#[must_use]
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

#[must_use]
pub fn is_valid_symbol_char(c: char) -> bool {
  c != ':'
    && c != ';'
    && c != '('
    && c != ')'
    && c != ','
    && !char::is_whitespace(c)
    && !char::is_control(c)
}

#[must_use]
pub fn is_valid_symbol_string(s: &str) -> bool {
  let zero_length = s.is_empty();
  let invalid_chars = s.starts_with('"')
    || s.starts_with('\'')
    || s.starts_with('#')
    || s.chars().any(|x| !is_valid_symbol_char(x));
  !zero_length && !invalid_chars
}

pub fn parse_var(
  refs: Refs,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (upto, nam) = context("local or global reference", parse_name)(from)?;
    let pos = Some(Pos::from_upto(from, upto));
    match ctx.iter().enumerate().find(|(_, x)| **x == nam) {
      Some((idx, _)) => Ok((upto, Term::Var(pos, nam.clone(), idx as u64))),
      None => match refs.get(&nam) {
        Some((d, a)) => Ok((upto, Term::Ref(pos, nam.clone(), *d, *a))),
        None => Err(Err::Error(ParseError::new(
          upto,
          ParseErrorKind::UndefinedReference(nam.clone(), ctx.clone()),
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
    let (i, _) = alt((tag("\u{3bb}"), tag("lambda")))(from)?;
    let (i, _) = parse_space(i)?;
    let (i, ns) = separated_list1(multispace1, parse_name)(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = tag("=>")(i)?;
    let (i, _) = parse_space(i)?;
    let mut ctx2 = ctx.clone();
    for n in ns.clone() {
      ctx2.push_front(n);
    }
    let (upto, bod) = parse_expression(refs.clone(), ctx2)(i)?;
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
) -> impl Fn(Span) -> IResult<Span, Vec<(Uses, String, Term)>, ParseError<Span>>
{
  move |i: Span| {
    let (i, _) = tag("(")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, u) = parse_uses(i)?;
    let (i, ns) = many1(terminated(parse_name, parse_space))(i)?;
    let (i, _) = tag(":")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, typ) = parse_expression(refs.clone(), ctx.clone())(i)?;
    let (i, _) = tag(")")(i)?;
    let mut res = Vec::new();
    for (i, n) in ns.iter().enumerate() {
      res.push((u, n.clone(), typ.clone().shift(i as u64, 0)))
    }
    Ok((i, res))
  }
}

pub fn parse_binder_short(
  refs: Refs,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Vec<(Uses, String, Term)>, ParseError<Span>>
{
  move |i: Span| {
    map(parse_term(refs.clone(), ctx.clone()), |t| {
      vec![(Uses::Many, String::from(""), t)]
    })(i)
  }
}

pub fn parse_binder(
  refs: Refs,
  ctx: Vector<String>,
  nam_opt: bool,
) -> impl Fn(Span) -> IResult<Span, Vec<(Uses, String, Term)>, ParseError<Span>>
{
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

    match parse_binder(refs.clone(), ctx.clone(), nam_opt)(i) {
      Err(e) => return Err(e),
      Ok((i1, bs)) => {
        for (u, n, t) in bs {
          ctx.push_front(n.clone());
          res.push((u, n, t));
        }
        i = i1;
      }
    }

    loop {
      match preceded(
        parse_space,
        parse_binder(refs.clone(), ctx.clone(), nam_opt),
      )(i)
      {
        Err(Err::Error(_)) => return Ok((i, res)),
        Err(e) => return Err(e),
        Ok((i2, bs)) => {
          for (u, n, t) in bs {
            ctx.push_front(n.clone());
            res.push((u, n, t));
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
    let (i, _) = alt((tag("\u{2200}"), tag("forall")))(from)?;
    let (i, _) = parse_space(i)?;
    let (i, bs) = parse_binders(refs.clone(), ctx.clone(), true)(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = tag("->")(i)?;
    let (i, _) = parse_space(i)?;
    let mut ctx2 = ctx.clone();
    for (_, n, _) in &bs {
      ctx2.push_front(n.clone());
    }
    let (upto, bod) = parse_expression(refs.clone(), ctx2)(i)?;
    let pos = Some(Pos::from_upto(from, upto));
    let trm = bs
      .into_iter()
      .rev()
      .fold(bod, |acc, (u, n, t)| Term::All(pos, u, n, Box::new((t, acc))));
    Ok((upto, trm))
  }
}

pub fn parse_type() -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
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
    let (upto, bod) = parse_expression(refs.clone(), ctx2)(i)?;
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
    let (upto, bod) = parse_expression(refs.clone(), ctx.clone())(i)?;
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
    let (upto, bod) = parse_expression(refs.clone(), ctx.clone())(i)?;
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
      Err(Err::Error(ParseError::new(
        from,
        ParseErrorKind::TopLevelRedefinition(nam),
      )))
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
      for (_, n, _) in &bs {
        type_ctx.push_front(n.clone());
      }
      let (i, typ) = parse_expression(refs.clone(), type_ctx)(i)?;
      let mut term_ctx = ctx.clone();
      if rec {
        term_ctx.push_front(nam.clone());
      };
      for (_, n, _) in &bs {
        term_ctx.push_front(n.clone());
      }
      let (i, _) = parse_space(i)?;
      let (i, _) = tag("=")(i)?;
      let (i, _) = parse_space(i)?;
      let (upto, trm) = parse_expression(refs.clone(), term_ctx)(i)?;
      let pos = Some(Pos::from_upto(from, upto));
      let trm = bs
        .iter()
        .rev()
        .fold(trm, |acc, (_, n, _)| Term::Lam(pos, n.clone(), Box::new(acc)));
      let typ = bs
        .into_iter()
        .rev()
        .fold(typ, |acc, (u, n, t)| Term::All(pos, u, n, Box::new((t, acc))));
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
    let (upto, bod) = parse_expression(refs.clone(), ctx2)(i)?;
    let pos = Some(Pos::from_upto(from, upto));
    Ok((upto, Term::Let(pos, rec, uses, nam, Box::new((typ, exp, bod)))))
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

pub fn parse_lty() -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, lty) = alt((
      value(LitType::Natural, tag("#Natural")),
      value(LitType::Integer, tag("#Integer")),
      value(LitType::BitString, tag("#BitString")),
      value(LitType::Text, tag("#Text")),
      value(LitType::Char, tag("#Char")),
    ))(from)?;
    let (upto, _) = throw_err(parse_builtin_symbol_end()(i), |_| {
      ParseError::new(i, ParseErrorKind::LitTypeLacksWhitespaceTermination(lty))
    })?;
    let pos = Some(Pos::from_upto(from, upto));
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
//    let pos = Some(Pos::from_upto(from, upto));
//    Ok((upto, Term::Lit(pos, Literal::Exception(val))))
//  }
//}

pub fn parse_lit() -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, lit) =
      alt((parse_bits, parse_text, parse_char, parse_nat, parse_int))(from)?;
    let (upto, _) = throw_err(parse_builtin_symbol_end()(i), |_| {
      ParseError::new(
        i,
        ParseErrorKind::LiteralLacksWhitespaceTermination(lit.clone()),
      )
    })?;
    let pos = Some(Pos::from_upto(from, upto));
    Ok((upto, Term::Lit(pos, lit)))
  }
}

pub fn parse_opr() -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, op) = alt((
      alt((
        value(PrimOp::Eql, tag("#eql")),
        value(PrimOp::Lth, tag("#lth")),
        value(PrimOp::Lte, tag("#lte")),
        value(PrimOp::Gth, tag("#gth")),
        value(PrimOp::Gte, tag("#gte")),
      )),
      alt((
        value(PrimOp::Bor, tag("#bor")),
        value(PrimOp::And, tag("#and")),
        value(PrimOp::Xor, tag("#xor")),
        value(PrimOp::Not, tag("#not")),
      )),
      alt((
        value(PrimOp::Suc, tag("#suc")),
        value(PrimOp::Pre, tag("#pre")),
        value(PrimOp::Add, tag("#add")),
        value(PrimOp::Sub, tag("#sub")),
        value(PrimOp::Mul, tag("#mul")),
        value(PrimOp::Div, tag("#div")),
        value(PrimOp::Mod, tag("#mod")),
      )),
      alt((value(PrimOp::Shl, tag("#shl")), value(PrimOp::Shr, tag("#shr")))),
      alt((value(PrimOp::Len, tag("#len")), value(PrimOp::Cat, tag("#cat")))),
    ))(from)?;
    let (upto, _) = throw_err(parse_builtin_symbol_end()(i), |_| {
      ParseError::new(i, ParseErrorKind::PrimOpLacksWhitespaceTermination(op))
    })?;
    let pos = Some(Pos::from_upto(from, upto));
    Ok((upto, Term::Opr(pos, op)))
  }
}

pub fn parse_expression(
  refs: Refs,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i, trm) = parse_apps(refs.clone(), ctx.clone())(from)?;
    let (i, has_ann) = opt(tag("::"))(i)?;
    if has_ann.is_some() {
      let (i, typ) =
        context("type annotation", parse_apps(refs.clone(), ctx.clone()))(i)?;
      let pos = Some(Pos::from_upto(from, i));
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
  refs: Refs,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |from: Span| {
    let (i2, _) = parse_space(from)?;
    let (i2, fun) = parse_term(refs.clone(), ctx.clone())(i2)?;
    let mut i = i2;
    let mut args = Vec::new();
    loop {
      let (i2, _) = parse_space(i)?;
      if let Ok((..)) = parse_app_end(i2) {
        let pos = Some(Pos::from_upto(from, i2));
        let trm = args
          .into_iter()
          .fold(fun, |acc, arg| Term::App(pos, Box::new((acc, arg))));
        return Ok((i2, trm));
      }
      let (i2, arg) = parse_term(refs.clone(), ctx.clone())(i2)?;
      args.push(arg);
      i = i2
    }
  }
}

pub fn parse_term(
  refs: Refs,
  ctx: Vector<String>,
) -> impl Fn(Span) -> IResult<Span, Term, ParseError<Span>> {
  move |i: Span| {
    context(
      "term",
      alt((
        delimited(
          preceded(tag("("), parse_space),
          context("expression", parse_expression(refs.clone(), ctx.clone())),
          context(
            "close parenthesis ')' of an expression",
            preceded(parse_space, tag(")")),
          ),
        ),
        parse_self(refs.clone(), ctx.clone()),
        parse_data(refs.clone(), ctx.clone()),
        parse_case(refs.clone(), ctx.clone()),
        parse_all(refs.clone(), ctx.clone()),
        parse_lam(refs.clone(), ctx.clone()),
        parse_let(refs.clone(), ctx.clone()),
        parse_type(),
        parse_lty(),
        parse_opr(),
        parse_lit(),
        parse_var(refs.clone(), ctx.clone()),
      )),
    )(i)
  }
}
pub fn parse(i: &str) -> IResult<Span, Term, ParseError<Span>> {
  parse_expression(HashMap::new(), Vector::new())(Span::new(i))
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use crate::term::tests::test_refs;

  #[test]
  fn test_apps() {
    let res = parse_apps(HashMap::new(), Vector::new())(Span::new("0d1"));
    println!("res: {:?}", res);
    assert!(res.is_ok());
    let res = parse_apps(HashMap::new(), Vector::new())(Span::new("0d1 0d1"));
    println!("res: {:?}", res);
    assert!(res.is_ok());
    let res =
      parse_apps(HashMap::new(), Vector::new())(Span::new("0d1 0d1 def"));
    println!("res: {:?}", res);
    assert!(res.is_ok());
  }

  #[test]
  fn test_cases() {
    use Term::*;
    let res = parse_expression(HashMap::new(), Vector::new())(Span::new(
      "(Type :: Type)",
    ));
    // println!("res: {:?}", res);
    assert!(res.is_ok());
    let res = parse_expression(HashMap::new(), Vector::new())(Span::new(
      "(Type (Type Type)  )",
    ));
    // println!("res: {:?}", res);
    assert!(res.is_ok());
    let res = parse_expression(HashMap::new(), Vector::new())(Span::new(
      "\u{3bb} x c n => (c x (c x (c x (c x (c x (c x (c x (c x (c x (c x (c \
       x x (c x (c x (c x (c x n)))))))))))))))",
    ));
    // println!("res: {:?}", res);
    assert!(res.is_ok());
    let res = parse_expression(HashMap::new(), Vector::new())(Span::new(
      "\u{3bb} x c n => (c x (c x (c x (c x (c x (c x (c x (c x (c x (c x (c \
       x x (c x (c x (c x (c x n)))))))))))))))",
    ));
    // println!("res: {:?}", res);
    assert!(res.is_ok());
    let res = parse_binder_full(HashMap::new(), Vector::new())(Span::new(
      "(a b c: Type)",
    ));
    // println!("res: {:?}", res);
    assert!(res.is_ok());
    let res = parse_binders(HashMap::new(), Vector::new(), true)(Span::new(
      "Type Type",
    ));
    println!("res: {:?}", res);
    assert!(res.is_ok());
    assert!(
      res.unwrap().1
        == vec![
          (Uses::Many, String::from(""), Typ(None)),
          (Uses::Many, String::from(""), Typ(None)),
        ]
    );
    let res = parse_binders(HashMap::new(), Vector::new(), true)(Span::new(
      "(A: Type) (a b c: A)",
    ));
    // println!("res: {:?}", res);
    assert!(res.is_ok());
    assert!(
      res.unwrap().1
        == vec![
          (Uses::Many, String::from("A"), Typ(None)),
          (Uses::Many, String::from("a"), Var(None, String::from("A"), 0)),
          (Uses::Many, String::from("b"), Var(None, String::from("A"), 1)),
          (Uses::Many, String::from("c"), Var(None, String::from("A"), 2)),
        ]
    );
    let res = parse_expression(HashMap::new(), Vector::new())(Span::new(
      "\u{2200} Type -> Type",
    ));
    // println!("res: {:?}", res);
    assert!(res.is_ok());
    let res = parse_expression(HashMap::new(), Vector::new())(Span::new(
      "\u{2200} (_ :Type) -> Type",
    ));
    // println!("res: {:?}", res);
    assert!(res.is_ok());
  }

  #[quickcheck]
  fn term_parse_print(x: Term) -> bool {
    match parse_expression(test_refs(), Vector::new())(Span::new(&format!(
      "{}",
      x
    ))) {
      Ok((_, y)) => x == y,
      Err(e) => {
        println!("{}", x);
        println!("{}", e);
        false
      }
    }
  }
}
