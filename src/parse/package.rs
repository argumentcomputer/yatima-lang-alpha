use crate::{
  hashspace,
  imports::{
    Import,
    Imports,
  },
  package::{
    Declaration,
    Package,
  },
  parse::{
    error::ParseError,
    term::*,
  },
  term::{
    Def,
    Defs,
    Link,
    Term,
  },
};

use std::{
  fs,
  path::PathBuf,
};

use hashexpr::{
  position::Pos,
  span::Span,
  AVal,
  AVal::*,
  Expr,
};

use im::{
  HashMap,
  HashSet,
  OrdMap,
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

#[derive(Debug, Clone)]
pub struct PackageEnv {
  path: PathBuf,
  open: HashSet<PathBuf>,
  done: HashMap<PathBuf, Link>,
}

impl PackageEnv {
  pub fn new(path: PathBuf) -> Self {
    PackageEnv { path, open: HashSet::new(), done: HashMap::new() }
  }
}

pub fn parse_def(
  defs: Defs,
) -> impl Fn(Span) -> IResult<Span, Def, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("def")(from)?;
    let (i, _) = parse_space(i)?;
    let (upto, (name, term, typ_)) =
      parse_decl(defs.to_owned(), Vector::new(), true, false)(i)?;
    let pos = Some(Pos::from_upto(from, upto));
    Ok((upto, Def { pos, name, docs: String::new(), typ_, term }))
  }
}

pub fn parse_link(from: Span) -> IResult<Span, Link, ParseError<Span>> {
  let (upto, link) = hashexpr::parse_raw(from).map_err(|e| Err::convert(e))?;
  match link {
    Expr::Atom(_, Link(link)) => Ok((upto, link)),
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

// pub fn parse_declaration(
//  env: PackageEnv,
//  defs: Defs,
//) -> impl Fn(Span) -> IResult<Span, Declaration, ParseError<Span>> {
//  move |i: Span| {
//    let (i, def) = parse_def(defs)(i)?;
//    Ok((i, Declaration::Defn { def.name,
//
//  }
//}

pub fn parse_package(
  env: PackageEnv,
  source_link: Link,
) -> impl Fn(Span) -> IResult<Span, (Link, Package), ParseError<Span>> {
  move |i: Span| {
    let (i, _) = multispace0(i)?;
    // let (i, docs) = parse_doc(
    let docs = String::from("");
    let (i, _) = tag("package")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, name) = parse_name(i)?;
    let (i, _) = multispace1(i)?;
    let (i, _) = tag("where")(i)?;
    let mut decls: Vec<Declaration> = Vec::new();
    let mut defs: Defs = OrdMap::new();
    let mut imports: Imports = Imports { imports: Vec::new() };
    let mut i = i;
    loop {
      let (i2, _) = parse_space(i)?;
      i = i2;
      let end: IResult<Span, Span, ParseError<Span>> = eof(i);
      if end.is_ok() {
        let pack = Package { name, docs, source: source_link, imports, decls };
        let pack_link = hashspace::put(pack.clone().encode());
        return Ok((i, (pack_link, pack)));
      }
      else {
        let (i2, def) = parse_def(defs.clone())(i)?;
        i = i2;
        let def_name = def.name.clone();
        let (defn, typ_, term) = def.embed();
        let typ_enc = typ_.encode();
        println!("type {}", typ_enc.clone());
        let type_link = hashspace::put(typ_enc);
        println!("type link {:?} {}", type_link, type_link);
        let trm_enc = term.encode();
        println!("term {}", trm_enc.clone());
        let term_link = hashspace::put(trm_enc);
        println!("term link {:?} {}", term_link, term_link);
        let def_enc = defn.encode();
        println!("def {}", def_enc.clone());
        let def_link = hashspace::put(def_enc);
        println!("def link {:?} {}", def_link, def_link);
        decls.push(Declaration::Defn {
          name: def_name.clone(),
          defn: def_link,
          term: term_link,
        });
        defs.insert(def_name, (def_link, term_link));
      }
    }
  }

  // let imports = Imports { imports: Vec::new() };
  // Ok((i.to_owned(), Package { name, docs: String::from(""), imports, defs }))
}

pub fn parse_file<'a>(env: PackageEnv) -> (Link, Package) {
  let path = env.path.clone();
  let txt = fs::read_to_string(&path).expect("file not found");
  let source_link = hashspace::put(text!(txt.clone()));
  let span = Span::new(&txt);
  match parse_package(env, source_link)(span) {
    Ok((_, p)) => p,
    Err(e) => {
      panic!("Error parsing file {}: {}", path.to_string_lossy(), e)
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
