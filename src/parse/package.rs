use crate::{
  defs::{
    Def,
    Defs,
  },
  hashspace::embed::embed_term,
  imports::Imports,
  package::Package,
  parse::{
    error::ParseError,
    term::*,
  },
  term::{
    Link,
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
  HashSet,
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
    opt,
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

pub enum Declaration {
  Def { def: Def },
  OpenFile { name: String, alias: String, path: PathBuf },
  OpenLink { name: String, alias: String, link: Link },
  Data { name: String, typ_: Term, ctors: HashMap<String, Term> },
}

#[derive(Debug, Clone)]
pub struct PackageEnv {
  refs: Refs,
  open: HashSet<PathBuf>,
  done: HashMap<PathBuf, Link>,
}

impl PackageEnv {
  pub fn new() -> Self {
    PackageEnv {
      refs: HashMap::new(),
      open: HashSet::new(),
      done: HashMap::new(),
    }
  }
}

pub fn parse_def(
  env: PackageEnv,
) -> impl Fn(Span) -> IResult<Span, Def, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("def")(from)?;
    let (i, _) = parse_space(i)?;
    let (upto, (name, term, typ_)) =
      parse_decl(env.refs.clone(), Vector::new(), true, false)(i)?;
    let pos = Some(Pos::from_upto(from, upto));
    Ok((upto, Def { pos, name, docs: String::new(), typ_, term }))
  }
}

pub fn parse_link(from: Span) -> IResult<Span, Link, ParseError<Span>> {
  let (upto, link) = hashexpr::parse_raw(from).map_err(|e| Err::convert(e))?;
  match link {
    hashexpr::Expr::Atom(_, hashexpr::Atom::Link(link)) => Ok((upto, link)),
    e => Err(Err::Error(ParseError::ExpectedImportLink(upto, e))),
  }
}

// pub fn parse_open(
//  env: PackageEnv,
//) -> impl Fn(Span) -> IResult<Span, Package, ParseError<Span>> {
//  move |from: Span| {
//    let (i, _) = tag("open")(from)?;
//    let (i, _) = parse_space(i)?;
//    let (i, nam) = parse_name(i)?;
//    let (i, ali) = opt(preceded(parse_space, parse_name))(i)?;
//    let (i, link) = opt(preceded(parse_space, parse_link))(i)?;
//    match link {
//      Some(link) => {
//        let expr = hashspace::get(link)?
//
//      },
//      None => panic!("todo openfile"),
//    }
//  }
//}

pub fn parse_defs(
  env: PackageEnv,
) -> impl Fn(Span) -> IResult<Span, (Refs, Defs), ParseError<Span>> {
  move |i: Span| {
    let mut defs: Vec<Def> = Vec::new();
    let mut i = i;
    let mut refs = env.refs.clone();
    loop {
      let (i2, _) = multispace0(i)?;
      i = i2;
      let end: IResult<Span, Span, ParseError<Span>> = eof(i);
      if end.is_ok() {
        return Ok((i, (refs, Defs { defs })));
      }
      else {
        let (i2, def) = parse_def(env.clone())(i)?;
        i = i2;
        let link = embed_term(def.clone().term).0.encode().link();
        refs.insert(def.clone().name, link);
        defs.push(def);
      }
    }
  }
}

pub fn parse<'a>(
  i: &'a str,
) -> IResult<Span<'a>, Package, ParseError<Span<'a>>> {
  let (i, _) = multispace0(Span::new(i))?;
  // let (i, docs) = parse_doc(
  let (i, _) = tag("package")(i)?;
  let (i, _) = multispace1(i)?;
  let (i, name) = parse_name(i)?;
  let (i, _) = multispace1(i)?;
  let (i, _) = tag("where")(i)?;
  let (i, _) = multispace1(i)?;
  let (i, (_, defs)) = parse_defs(PackageEnv::new())(i)?;
  let imports = Imports { imports: Vec::new() };
  Ok((i.to_owned(), Package { name, docs: String::from(""), imports, defs }))
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

// pub fn parse_data_decl(
//  refs: Refs,
//  ctx: Vector<String>,
//) -> impl Fn(Span) -> IResult<Span, Vec<(String, Term)>, ParseError<Span>> {
//  move |i: Span| {
//    let (i, _) = tag("data")?;
//    let (i, _) = multispace1(i)?;
//    let (i, nam) = parse_name(i)?;
//    let (i, _) = multispace0(i)?;
//    let (i, bs) = parse_binders(refs, Vector::new(), false)
//
//  }
//}
