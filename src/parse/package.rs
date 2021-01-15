use crate::{
  defs::{
    Def,
    Defs,
  },
  hashspace::embed::desaturate_term,
  imports::Imports,
  package::Package,
  parse::{
    error::ParseError,
    term::*,
  },
  term::{
    Literal,
    PrimOp,
    Refs,
    Term,
    Uses,
  },
};

use std::{
  fs,
  path::{
    Path,
    PathBuf,
  },
};

use hashexpr::{
  position::Pos,
  span::Span,
};

use im::{
  HashMap,
  Vector,
};
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
    eof,
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

pub fn parse_def(
  refs: Refs,
) -> impl Fn(Span) -> IResult<Span, Def, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("def")(from)?;
    let (i, _) = multispace1(i)?;
    let (upto, (name, term, typ_)) =
      parse_decl(refs.clone(), Vector::new(), true, false)(i)?;
    let pos = Some(Pos::from_upto(from, upto));
    Ok((upto, Def { pos, name, doc: String::new(), typ_, term }))
  }
}

pub fn parse_defs(
  refs: Refs,
) -> impl Fn(Span) -> IResult<Span, (Refs, Defs), ParseError<Span>> {
  move |i: Span| {
    let mut defs: Vec<Def> = Vec::new();
    let mut i = i;
    let mut refs = refs.clone();
    loop {
      let (i2, _) = multispace0(i)?;
      i = i2;
      let end: IResult<Span, Span, ParseError<Span>> = eof(i);
      if end.is_ok() {
        return Ok((i, (refs, Defs { defs })));
      }
      else {
        let (i2, def) = parse_def(refs.clone())(i)?;
        i = i2;
        let def_link = def.clone().encode().link();
        let ast_link = desaturate_term(def.clone().term).0.encode().link();
        refs.insert(def.clone().name, (def_link, ast_link));
        defs.push(def);
      }
    }
  }
}

pub fn parse<'a>(
  i: &'a str,
) -> IResult<Span<'a>, Package, ParseError<Span<'a>>> {
  let (i, (_, defs)) = parse_defs(HashMap::new())(Span::new(i))?;
  let imports = Imports { imports: Vec::new() };
  Ok((i.to_owned(), Package {
    name: String::from("test"),
    docs: String::from(""),
    imports,
    defs,
  }))
}

pub struct TypeDecl {
  pub nam: String,
  pub typ: Term,
  pub ctors: HashMap<String, Term>,
}

pub fn parse_file<'a>(p: PathBuf) -> Package {
  let txt = fs::read_to_string(&p).expect("file not found");
  match parse(&txt) {
    Ok((_, p)) => p,
    Err(e) => {
      panic!("Error parsing file {}: {}", p.to_string_lossy(), e)
    }
  }
}

// pub fn parse_type_decl(
//  refs: Refs,
//  ctx: Vector<String>,
//) -> impl Fn(Span) -> IResult<Span, Vec<(String, Term)>, ParseError<Span>> {
//  move |i: Span| {
//    let (i, _) = tag("type")?;
//    let (i, _) = multispace1(i)?;
//    let (i, nam) = parse_name(i)?;
//    let (i, _) = multispace0(i)?;
//    let (i, bs) = parse_binders(refs, Vector::new(), false)
//
//  }
//}
