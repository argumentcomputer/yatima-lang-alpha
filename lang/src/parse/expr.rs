use sp_cid::Cid;
use sp_im::vector::Vector;
use yatima_core::{
  literal::{
    LitType,
    Literal,
  },
  name::{
    is_valid_symbol_string,
    Name,
  },
  parse::{
    string,
    term::{
      is_numeric_symbol_string1,
      is_numeric_symbol_string2,
    },
  },
  position::Pos,
};

use sp_im::ordset::OrdSet;

use sp_std::{
  borrow::ToOwned,
  boxed::Box,
  cell::RefCell,
  collections::{
    btree_map::BTreeMap,
    btree_set::BTreeSet,
  },
  rc::Rc,
  vec::Vec,
};

use alloc::string::{
  String,
  ToString,
};

use crate::{
  decl::Decl,
  expr::{
    Argument,
    Binder,
    Expr,
    LetDef,
  },
  parse::{
    error::{
      throw_err,
      ParseError,
      ParseErrorKind,
    },
    literal::*,
    op::parse_opr,
    span::Span,
  },
  pre_uses::PreUses,
};

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
    many_till,
    separated_list1,
  },
  sequence::{
    delimited,
    preceded,
    terminated,
  },
  Err,
  IResult,
  Parser,
};

pub type Ctx = OrdSet<Name>;

pub struct State {
  expr_hole_count: u64,
  uses_hole_count: u64,
}

pub type RcState = Rc<RefCell<State>>;

impl State {
  pub fn new() -> Self {
    State { expr_hole_count: 0u64, uses_hole_count: 0u64 }
  }

  pub fn init_ref() -> RcState { Rc::new(RefCell::new(State::new())) }
}

pub fn reserved_symbols() -> Vector<String> {
  Vector::from(vec![
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
    String::from("do"),
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

pub fn expr_hole(
  input: Cid,
  state: RcState,
) -> impl Fn(Span) -> IResult<Span, Expr, ParseError<Span>> {
  move |i: Span| {
    let mut state = state.borrow_mut();
    let count = state.expr_hole_count.clone();
    state.expr_hole_count += 1;
    let pos = Pos::from_upto(input, i, i);
    Ok((i, Expr::MetaVariable(pos, Name::from(format!("expr_{}", count)))))
  }
}

pub fn uses_hole(
  state: RcState,
) -> impl Fn(Span) -> IResult<Span, PreUses, ParseError<Span>> {
  move |i: Span| {
    let mut state = state.borrow_mut();
    let count = state.uses_hole_count.clone();
    state.uses_hole_count += 1;
    Ok((i, PreUses::Hol(Name::from(format!("uses_{}", count)))))
  }
}

pub fn parse_uses(
  state: RcState,
) -> impl Fn(Span) -> IResult<Span, PreUses, ParseError<Span>> {
  move |i: Span| {
    alt((
      value(PreUses::Many, terminated(tag("ω"), multispace1)),
      value(PreUses::None, terminated(tag("0"), multispace1)),
      value(PreUses::Affi, terminated(tag("&"), multispace1)),
      value(PreUses::Once, terminated(tag("1"), multispace1)),
      uses_hole(state.clone()),
    ))(i)
  }
}

pub fn parse_typ(
  input: Cid,
) -> impl Fn(Span) -> IResult<Span, Expr, ParseError<Span>> {
  move |from: Span| {
    let (upto, _) = tag("Type")(from)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Expr::Type(pos)))
  }
}

pub fn parse_lit(
  input: Cid,
) -> impl Fn(Span) -> IResult<Span, Expr, ParseError<Span>> {
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
    Ok((upto, Expr::Literal(pos, lit)))
  }
}

pub fn parse_lty(
  input: Cid,
) -> impl Fn(Span) -> IResult<Span, Expr, ParseError<Span>> {
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
      // value(LitType::U128, tag("#U128")),
      value(LitType::I8, tag("#I8")),
      value(LitType::I16, tag("#I16")),
      value(LitType::I32, tag("#I32")),
      value(LitType::I64, tag("#I64")),
      // value(LitType::I128, tag("#I128")),
    ))(from)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Expr::LitType(pos, lty)))
  }
}

pub fn parse_var(
  input: Cid,
  ctx: Ctx,
) -> impl Fn(Span) -> IResult<Span, Expr, ParseError<Span>> {
  move |from: Span| {
    let (upto, name) = context("local or global reference", parse_name)(from)?;
    let pos = Pos::from_upto(input, from, upto);
    if ctx.contains(&name) {
      Ok((upto, Expr::Variable(pos, name.clone())))
    }
    else {
      Ok((upto, Expr::Reference(pos, name.clone())))
    }
  }
}

pub fn parse_hole(
  input: Cid,
) -> impl Fn(Span) -> IResult<Span, Expr, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("?")(from)?;
    let (upto, nam) = context("metavariable", parse_name)(i)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Expr::MetaVariable(pos, nam)))
  }
}

pub fn parse_arg_full(
  input: Cid,
  ctx: Ctx,
  state: RcState,
) -> impl Fn(Span) -> IResult<Span, Argument, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = terminated(tag("("), parse_space)(i)?;
    let (i, term) = context(
      "app arg",
      parse_telescope(input, ctx.clone(), state.clone()),
    )(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = terminated(tag("::"), parse_space)(i)?;
    let (i, uses) = terminated(parse_uses(state.clone()), parse_space)(i)?;
    let (i, typ_) = context(
      "app typ",
      parse_telescope(input, ctx.clone(), state.clone()),
    )(i)?;
    let (i, _) = preceded(parse_space, tag(")"))(i)?;
    Ok((i, Argument { uses, typ_, term }))
  }
}

pub fn parse_arg_short(
  input: Cid,

  ctx: Ctx,
  state: RcState,
) -> impl Fn(Span) -> IResult<Span, Argument, ParseError<Span>> {
  move |i: Span| {
    let (i, term) =
      context("app arg", parse_expr(input, ctx.clone(), state.clone()))(i)?;
    let (i, uses) = uses_hole(state.clone())(i)?;
    let (i, typ_) = expr_hole(input, state.clone())(i)?;
    Ok((i, Argument { uses, typ_, term }))
  }
}

pub fn parse_arg(
  input: Cid,
  ctx: Ctx,
  state: RcState,
) -> impl Fn(Span) -> IResult<Span, Argument, ParseError<Span>> {
  move |i: Span| {
    alt((
      parse_arg_full(input, ctx.clone(), state.clone()),
      parse_arg_short(input, ctx.clone(), state.clone()),
    ))(i)
  }
}

pub fn parse_args(
  input: Cid,

  ctx: Ctx,
  state: RcState,
) -> impl FnMut(Span) -> IResult<Span, Vec<Argument>, ParseError<Span>> {
  move |mut i: Span| {
    let mut res = Vec::new();
    loop {
      match preceded(parse_space, peek(parse_tele_end))(i) {
        Ok((i2, _)) => return Ok((i2, res)),
        _ => {}
      }
      match preceded(parse_space, parse_arg(input, ctx.clone(), state.clone()))(
        i,
      ) {
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
  ctx: Ctx,
  state: RcState,
) -> impl Fn(Span) -> IResult<Span, Expr, ParseError<Span>> {
  move |from: Span| {
    let (i, fun) =
      context("app fun", parse_expr(input, ctx.clone(), state.clone()))(from)?;
    let (i, _) = parse_space(i)?;
    let (upto, args) = parse_args(input, ctx.clone(), state.clone())(i)?;
    let pos = Pos::from_upto(input, from, upto);
    let trm = if args.is_empty() {
      fun
    }
    else {
      Expr::Application(pos, Box::new(fun), args)
    };
    return Ok((upto, trm));
  }
}

pub fn parse_binder_full(
  input: Cid,

  ctx: Ctx,
  state: RcState,
) -> impl Fn(Span) -> IResult<Span, Vec<Binder>, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("(")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, u) = parse_uses(state.clone())(i)?;
    let (i, ns) = many1(terminated(parse_name, parse_space))(i)?;
    let (i, _) = tag(":")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, typ) = parse_telescope(input, ctx.clone(), state.clone())(i)?;
    let (i, _) = tag(")")(i)?;
    let mut res = Vec::new();
    for n in ns.iter() {
      res.push(Binder {
        uses: u.clone(),
        name: n.to_owned(),
        typ_: typ.clone(),
      })
    }
    Ok((i, res))
  }
}

pub fn parse_name_binder(
  input: Cid,
  state: RcState,
) -> impl Fn(Span) -> IResult<Span, Vec<Binder>, ParseError<Span>> {
  move |i: Span| {
    let (i, name) = parse_name(i)?;
    let (i, uses) = uses_hole(state.clone())(i)?;
    let (i, typ_) = expr_hole(input, state.clone())(i)?;
    Ok((i, vec![Binder { uses, name, typ_ }]))
  }
}

pub fn parse_type_binder(
  input: Cid,

  ctx: Ctx,
  state: RcState,
) -> impl Fn(Span) -> IResult<Span, Vec<Binder>, ParseError<Span>> {
  move |i: Span| {
    let (i, uses) = uses_hole(state.clone())(i)?;
    let (i, typ_) = parse_expr(input, ctx.clone(), state.clone())(i)?;
    Ok((i, vec![Binder { uses, name: Name::from("_"), typ_ }]))
  }
}

#[derive(Clone, Copy)]
pub enum BinderOpt {
  NameOrFull,
  TypeOrFull,
}

pub fn parse_binder(
  input: Cid,

  ctx: Ctx,
  state: RcState,
  binder_opt: BinderOpt,
) -> impl Fn(Span) -> IResult<Span, Vec<Binder>, ParseError<Span>> {
  move |i: Span| match binder_opt {
    BinderOpt::NameOrFull => alt((
      parse_binder_full(input, ctx.clone(), state.clone()),
      parse_name_binder(input, state.clone()),
    ))(i),
    BinderOpt::TypeOrFull => alt((
      parse_binder_full(input, ctx.clone(), state.clone()),
      parse_type_binder(input, ctx.clone(), state.clone()),
    ))(i),
  }
}

pub fn parse_binders(
  input: Cid,
  ctx: Ctx,
  state: RcState,
  terminator: Vec<char>,
  binder_opt: BinderOpt,
) -> impl FnMut(Span) -> IResult<Span, Vec<Binder>, ParseError<Span>> {
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
        parse_binder(input, ctx.clone(), state.clone(), binder_opt),
      )(i)
      {
        Err(e) => return Err(e),
        Ok((i2, bs)) => {
          for b in bs {
            ctx.insert(b.name.to_owned());
            res.push(b);
          }
          i = i2;
        }
      }
    }
  }
}

pub fn parse_binders1(
  input: Cid,

  ctx: Ctx,
  state: RcState,
  terminator: Vec<char>,
  binder_opt: BinderOpt,
) -> impl FnMut(Span) -> IResult<Span, Vec<Binder>, ParseError<Span>> {
  move |mut i: Span| {
    let mut ctx = ctx.clone();
    let mut res = Vec::new();

    match parse_binder(input, ctx.clone(), state.clone(), binder_opt)(
      i.to_owned(),
    ) {
      Err(e) => return Err(e),
      Ok((i1, bs)) => {
        for b in bs {
          ctx.insert(b.name.to_owned());
          res.push(b);
        }
        i = i1;
      }
    }
    let (i, mut res2) = parse_binders(
      input,
      ctx.clone(),
      state.clone(),
      terminator.clone(),
      binder_opt,
    )(i)?;
    res.append(&mut res2);
    Ok((i, res))
  }
}

pub fn parse_lambda(
  input: Cid,
  ctx: Ctx,
  state: RcState,
) -> impl Fn(Span) -> IResult<Span, Expr, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = alt((tag("λ"), tag("lambda")))(from)?;
    let (i, _) = parse_space(i)?;
    let (i, bs) = parse_binders1(
      input,
      ctx.clone(),
      state.clone(),
      vec!['='],
      BinderOpt::NameOrFull,
    )(i)?;
    let (i, _) = terminated(tag("=>"), parse_space)(i)?;
    let mut ctx = ctx.clone();
    for b in bs.iter() {
      ctx.insert(b.name.clone());
    }
    let (upto, bod) = parse_telescope(input, ctx, state.clone())(i)?;
    let pos = Pos::from_upto(input, from, upto);
    let trm = Expr::Lambda(pos, bs, Box::new(bod));
    Ok((upto, trm))
  }
}

pub fn parse_forall(
  input: Cid,
  ctx: Ctx,
  state: RcState,
) -> impl Fn(Span) -> IResult<Span, Expr, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = alt((tag("∀"), tag("forall")))(from)?;
    let (i, _) = parse_space(i)?;
    let (i, bs) = parse_binders1(
      input,
      ctx.clone(),
      state.clone(),
      vec!['-'],
      BinderOpt::NameOrFull,
    )(i)?;
    let (i, _) = terminated(tag("=>"), parse_space)(i)?;
    let mut ctx = ctx.clone();
    for b in bs.iter() {
      ctx.insert(b.name.clone());
    }
    let (upto, bod) = parse_telescope(input, ctx, state.clone())(i)?;
    let pos = Pos::from_upto(input, from, upto);
    let trm = Expr::Forall(pos, bs, Box::new(bod));
    Ok((upto, trm))
  }
}

pub fn parse_self_type(
  input: Cid,

  ctx: Ctx,
  state: RcState,
) -> impl Fn(Span) -> IResult<Span, Expr, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("@")(from)?;
    let (i, n) = parse_name(i)?;
    let (i, _) = parse_space(i)?;
    let mut ctx = ctx.clone();
    ctx.insert(n.clone());
    let (upto, bod) = parse_telescope(input, ctx, state.clone())(i)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Expr::SelfType(pos, n, Box::new(bod))))
  }
}

pub fn parse_self_case(
  input: Cid,
  ctx: Ctx,
  state: RcState,
) -> impl Fn(Span) -> IResult<Span, Expr, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("case")(from)?;
    let (i, _) = parse_space(i)?;
    let (upto, bod) = parse_telescope(input, ctx.clone(), state.clone())(i)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Expr::SelfCase(pos, Box::new(bod))))
  }
}

pub fn parse_self_data(
  input: Cid,
  ctx: Ctx,
  state: RcState,
) -> impl Fn(Span) -> IResult<Span, Expr, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("data")(from)?;
    let (i, _) = parse_space(i)?;
    let (i, typ) = parse_expr(input, ctx.clone(), state.clone())(i)?;
    let (i, _) = parse_space(i)?;
    let (upto, bod) = parse_expr(input, ctx.clone(), state.clone())(i)?;
    let pos = Pos::from_upto(input, from, upto);
    Ok((upto, Expr::SelfData(pos, Box::new(typ), Box::new(bod))))
  }
}

pub fn parse_let_def(
  input: Cid,

  ctx: Ctx,
  state: RcState,
) -> impl Fn(Span) -> IResult<Span, LetDef, ParseError<Span>> {
  move |from: Span| {
    let (i, rec) =
      alt((value(true, tag("letrec")), value(false, tag("let"))))(from)?;
    let (i, _) = parse_space(i)?;
    let (i, uses) = parse_uses(state.clone())(i)?;
    let (i, _) = parse_space(i)?;
    let (i, name) = parse_name(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = tag(":")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, typ_) = parse_telescope(input, ctx.clone(), state.clone())(i)?;
    let (i, _) = parse_space(i)?;
    let (i, _) = tag("=")(i)?;
    let (i, _) = parse_space(i)?;
    let mut ctx2 = ctx.clone();
    ctx2.insert(name.clone());
    let (i, term) = parse_telescope(
      input,
      if rec { ctx2.clone() } else { ctx.clone() },
      state.clone(),
    )(i)?;
    Ok((i, LetDef { rec, uses, name, typ_, term }))
  }
}

// pub fn parse_let(
//  input: Cid,
//
//  ctx: Ctx,
//  state: RcState,
//) -> impl Fn(Span) -> IResult<Span, Expr, ParseError<Span>> {
//  move |from: Span| {
//    let (i, let_defs) =
//      many1(parse_let_def(input, ctx.clone(), state.clone()))(from)?;
//    let (i, _) = parse_space(i)?;
//    let (i, _) = tag("in")(i)?;
//    let (i, _) = parse_space(i)?;
//    let (upto, bod) = parse_telescope(input, ctx.clone(), state.clone())(i)?;
//    let pos = Pos::from_upto(input, from, upto);
//    Ok((
//      upto,
//      Term::Let(
//        pos,
//        letrec,
//        uses,
//        nam,
//        Box::new(typ),
//        Box::new(exp),
//        Box::new(bod),
//      ),
//    ))
//  }
//}

pub fn parse_expr(
  input: Cid,
  ctx: Ctx,
  state: RcState,
) -> impl Fn(Span) -> IResult<Span, Expr, ParseError<Span>> {
  move |i: Span| {
    alt((
      context(
        "application telescope",
        delimited(
          preceded(tag("("), parse_space),
          parse_telescope(input, ctx.clone(), state.clone()),
          preceded(parse_space, tag(")")),
        ),
      ),
      context("self-type", parse_self_type(input, ctx.clone(), state.clone())),
      context(
        "self-type data",
        parse_self_data(input, ctx.clone(), state.clone()),
      ),
      context(
        "self-type case",
        parse_self_case(input, ctx.clone(), state.clone()),
      ),
      context("forall", parse_forall(input, ctx.clone(), state.clone())),
      context("lambda", parse_lambda(input, ctx.clone(), state.clone())),
      // context(
      //  "let",
      //  parse_let(input,  rec.clone(), ctx.clone(), quasi.clone()),
      //),
      context("type", parse_typ(input)),
      context("literal operation", parse_opr(input)),
      context("literal", parse_lit(input)),
      context("literal type", parse_lty(input)),
      context("hole", parse_hole(input)),
      context("var", parse_var(input, ctx.clone())),
    ))(i)
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use yatima_core::parse::term::input_cid;

  #[test]
  fn test_parse_telescope() {
    fn test(i: &str) -> IResult<Span, Expr, ParseError<Span>> {
      parse_telescope(input_cid(i), Ctx::new(), State::init_ref())(Span::new(i))
    }
    let res = test("(1 (3 :: ω 2))");
    // println!("res {:?}", res);
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Expr::Application(
        Pos::None,
        Box::new(Expr::Literal(Pos::None, Literal::Nat(1u64.into()))),
        vec![Argument::new(
          PreUses::Many,
          Expr::Literal(Pos::None, Literal::Nat(2u64.into())),
          Expr::Literal(Pos::None, Literal::Nat(3u64.into())),
        )]
      )
    );
    let res = test("(1 2)");
    // println!("res {:?}", res);
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Expr::Application(
        Pos::None,
        Box::new(Expr::Literal(Pos::None, Literal::Nat(1u64.into()))),
        vec![Argument::new(
          PreUses::Hol(Name::from("uses_0")),
          Expr::MetaVariable(Pos::None, Name::from("expr_0")),
          Expr::Literal(Pos::None, Literal::Nat(2u64.into())),
        )]
      )
    );
    let res = test("(Type (Type :: 0 Type))");
    assert!(res.is_ok());
    let res = test("(1 (1 :: ω #Nat) (1 :: ω #Nat))");
    // println!("res {:?}", res);
    assert!(res.is_ok());
    let res = test("(#Int (1 :: 0 #U16))");
    println!("res {:?}", res);
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Expr::Application(
        Pos::None,
        Box::new(Expr::LitType(Pos::None, LitType::Int)),
        vec![Argument::new(
          PreUses::None,
          Expr::LitType(Pos::None, LitType::U16),
          Expr::Literal(Pos::None, Literal::Nat(1u64.into()))
        ),]
      )
    );
  }
  #[test]
  fn test_parse_lambda() {
    fn test(i: &str) -> IResult<Span, Expr, ParseError<Span>> {
      parse_lambda(input_cid(i), Ctx::new(), State::init_ref())(Span::new(i))
    }
    let res = test("λ (ω a: Type) => Type");
    println!("res: {:?}", res);
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Expr::Lambda(
        Pos::None,
        vec![Binder::new(
          PreUses::Many,
          Name::from("a"),
          Expr::Type(Pos::None)
        )],
        Box::new(Expr::Type(Pos::None))
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
    let res = test("λ a => Type");
    println!("res: {:?}", res);
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Expr::Lambda(
        Pos::None,
        vec![Binder::new(
          PreUses::Hol(Name::from("uses_0")),
          Name::from("a"),
          Expr::MetaVariable(Pos::None, Name::from("expr_0"))
        )],
        Box::new(Expr::Type(Pos::None))
      )
    );
    let res = test("λ a => a");
    println!("res: {:?}", res);
    assert!(res.is_ok());
    assert_eq!(
      res.unwrap().1,
      Expr::Lambda(
        Pos::None,
        vec![Binder::new(
          PreUses::Hol(Name::from("uses_0")),
          Name::from("a"),
          Expr::MetaVariable(Pos::None, Name::from("expr_0"))
        )],
        Box::new(Expr::Variable(Pos::None, Name::from("a")))
      )
    );
    let res = test("λ a b c => a");
    println!("res: {:?}", res);
    assert!(res.is_ok());
  }
}
