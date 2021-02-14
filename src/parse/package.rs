use crate::{
  hashspace,
  package::{
    merge_defs,
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
  unembed_error::UnembedError,
};

use std::{
  ffi::OsString,
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
  /* TODO: Cache of completed files so we don't reparse packages we've
   * already parsed
   * done: Rc<HashMap<PathBuf, Link>>, */
}

impl PackageEnv {
  pub fn new(path: PathBuf) -> Self {
    PackageEnv { path, open: HashSet::new() }
  }

  pub fn set_path(self, path: PathBuf) -> Self {
    PackageEnv { path, open: self.open }
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

pub fn parse_open(
  env: PackageEnv,
) -> impl Fn(Span) -> IResult<Span, Declaration, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("open")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, name) = parse_name(i)?;
    let (i, alias) = opt(preceded(parse_space, parse_name))(i)?;
    let alias = alias.unwrap_or(String::from(""));
    let (i, from) = opt(preceded(parse_space, parse_link))(i)?;
    match from {
      Some(from) => Ok((i, Declaration::Open { name, alias, from })),
      None => {
        let mut path = env.path.parent().unwrap().to_path_buf();
        for n in name.split(".") {
          path.push(n);
        }
        if env.open.contains(&path) {
          Err(Err::Error(ParseError::ImportCycle(i, path)))
        }
        else {
          let env = env.clone().set_path(path);
          let (link, ..) = parse_file(env);
          Ok((i, Declaration::Open { name, alias, from: link }))
        }
      }
    }
  }
}

pub fn parse_declaration(
  env: PackageEnv,
  defs: Defs,
) -> impl Fn(Span) -> IResult<Span, Declaration, ParseError<Span>> {
  move |i: Span| {
    let (i, d) =
      alt((parse_defn(defs.to_owned()), parse_open(env.to_owned())))(i)?;
    Ok((i, d))
  }
}

pub fn parse_defn(
  defs: Defs,
) -> impl Fn(Span) -> IResult<Span, Declaration, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("def")(from)?;
    let (i, _) = parse_space(i)?;
    let (upto, (name, term, typ_)) =
      parse_decl(defs.to_owned(), Vector::new(), true, false)(i)?;
    let pos = Some(Pos::from_upto(from, upto));
    let def = Def { pos, name, docs: String::new(), typ_, term };
    let def_name = def.name.clone();
    let (defn, typ_, term) = def.embed();
    let typ_enc = typ_.encode();
    // println!("type {}", typ_enc.clone());
    let _type_link = hashspace::put(typ_enc);
    // println!("type link {:?} {}", _type_link, _type_link);
    let trm_enc = term.encode();
    // println!("term {}", trm_enc.clone());
    let term_link = hashspace::put(trm_enc);
    // println!("term link {:?} {}", term_link, term_link);
    let def_enc = defn.encode();
    // println!("def {}", def_enc.clone());
    let def_link = hashspace::put(def_enc);
    // println!("def link {:?} {}", def_link, def_link);
    let def = Declaration::Defn {
      name: def_name.clone(),
      defn: def_link,
      term: term_link,
    };
    Ok((upto, def))
  }
}

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
    let file_name =
      env.path.file_name().ok_or(Err::Error(ParseError::MalformedPath(i)))?;
    let name_os: OsString = name.clone().into();
    if name_os != file_name {
      return Err(Err::Error(ParseError::MisnamedPackage(i, name.clone())));
    }
    let (i, _) = multispace1(i)?;
    let (i, _) = tag("where")(i)?;
    let mut decls: Vec<Declaration> = Vec::new();
    let mut defs: Defs = OrdMap::new();
    let mut i = i;
    loop {
      let (i2, _) = parse_space(i)?;
      i = i2;
      let end: IResult<Span, Span, ParseError<Span>> = eof(i);
      if end.is_ok() {
        let pack = Package { name, docs, source: source_link, decls };
        let pack_link = hashspace::put(pack.clone().encode());
        return Ok((i, (pack_link, pack)));
      }
      else {
        let (i2, decl) = parse_declaration(env.to_owned(), defs.clone())(i)?;
        decls.push(decl.clone());
        match decl {
          Declaration::Defn { name, defn, term } => {
            defs.insert(name, (defn, term));
          }
          Declaration::Open { name, alias, from } => {
            let pack = hashspace::get(from)
              .ok_or(Err::Error(ParseError::UnknownImportLink(i2, from)))?;
            let pack = Package::decode(pack).map_err(|e| {
              Err::Error(ParseError::EmbeddingError(
                i2,
                UnembedError::DecodeError(e),
              ))
            })?;

            if name != pack.name {
              return Err(Err::Error(ParseError::MisnamedImport(
                i2, name, pack.name,
              )));
            };
            let import_defs: Defs = pack
              .defs()
              .map_err(|e| Err::Error(ParseError::EmbeddingError(i2, e)))?;
            defs = merge_defs(defs, import_defs, alias)
          }
        }
        i = i2;
      }
    }
  }
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
