use crate::{
  hashspace,
  package::{
    merge_defs,
    merge_refs,
    Declaration,
    Package,
  },
  parse::{
    error,
    error::{
      ParseError,
      ParseErrorKind,
    },
    term::*,
  },
  term::{
    Def,
    Defs,
    Link,
    Refs,
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
  HashMap,
  HashSet,
  Vector,
};
use nom::{
  branch::alt,
  bytes::complete::tag,
  character::complete::{
    multispace0,
    multispace1,
  },
  combinator::{
    eof,
    opt,
  },
  multi::separated_list0,
  sequence::{
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

pub fn parse_link(from: Span) -> IResult<Span, Link, ParseError<Span>> {
  let (upto, link) =
    hashexpr::parse_raw(from).map_err(|e| error::convert(from, e))?;
  match link {
    Expr::Atom(_, Link(link)) => Ok((upto, link)),
    e => Err(Err::Error(ParseError::new(
      upto,
      ParseErrorKind::ExpectedImportLink(e),
    ))),
  }
}

fn parse_alias(i: Span) -> IResult<Span, String, ParseError<Span>> {
  let (i, _) = tag("as")(i)?;
  let (i, _) = parse_space(i)?;
  let (i, a) = parse_name(i)?;
  Ok((i, a))
}

fn parse_with(i: Span) -> IResult<Span, Vec<String>, ParseError<Span>> {
  let (i, _) = tag("(")(i)?;
  let (i, ns) = separated_list0(
    terminated(tag(","), parse_space),
    terminated(parse_name, parse_space),
  )(i)?;
  let (i, _) = tag(")")(i)?;
  Ok((i, ns))
}

pub fn parse_open(
  env: PackageEnv,
) -> impl Fn(Span) -> IResult<Span, Declaration, ParseError<Span>> {
  move |i: Span| {
    let (i, _) = tag("open")(i)?;
    let (i, _) = parse_space(i)?;
    let (i, name) = parse_name(i)?;
    let (i, _) = parse_space(i)?;
    let (i, alias) = opt(terminated(parse_alias, parse_space))(i)?;
    let alias = alias.unwrap_or(String::from(""));
    let (i, with) = opt(terminated(parse_with, parse_space))(i)?;
    let (i, from) = opt(terminated(parse_link, parse_space))(i)?;
    match from {
      Some(from) => Ok((i, Declaration::Open { name, alias, with, from })),
      None => {
        let mut path = env.path.parent().unwrap().to_path_buf();
        for n in name.split(".") {
          path.push(n);
        }
        path.set_extension("ya");
        let mut open = env.open.clone();
        let has_path = open.insert(path.clone());
        if has_path.is_some() {
          Err(Err::Error(ParseError::new(i, ParseErrorKind::ImportCycle(path))))
        }
        else {
          let env = PackageEnv { path, open };
          let (link, ..) = parse_file(env);
          Ok((i, Declaration::Open { name, alias, with, from: link }))
        }
      }
    }
  }
}

pub fn parse_defn(
  refs: Refs,
) -> impl Fn(Span) -> IResult<Span, Declaration, ParseError<Span>> {
  move |from: Span| {
    let (i, _) = tag("def")(from)?;
    let (i, _) = parse_space(i)?;
    let (upto, (name, term, typ_)) =
      parse_typed_definition(refs.to_owned(), Vector::new(), true, false)(i)?;
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
) -> impl Fn(Span) -> IResult<Span, (Link, Package, Defs, Refs), ParseError<Span>>
{
  move |i: Span| {
    let (i, _) = parse_space(i)?;
    // let (i, docs) = parse_doc(
    let docs = String::from("");
    let (i, _) = tag("package")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, name) = parse_name(i)?;
    let file_name = env
      .path
      .file_name()
      .ok_or(Err::Error(ParseError::new(i, ParseErrorKind::MalformedPath)))?;
    let name_os: OsString = format!("{}.ya", name.clone()).into();
    if name_os != file_name {
      return Err(Err::Error(ParseError::new(
        i,
        ParseErrorKind::MisnamedPackage(name.clone()),
      )));
    }
    let (i, _) = multispace1(i)?;
    let (i, _) = tag("where")(i)?;
    let mut decls: Vec<Declaration> = Vec::new();
    let mut refs: Refs = HashMap::new();
    let mut defs: Defs = HashMap::new();
    let mut i = i;
    loop {
      let (i2, _) = parse_space(i)?;
      i = i2;
      let end: IResult<Span, Span, ParseError<Span>> = eof(i);
      if end.is_ok() {
        let pack = Package { name, docs, source: source_link, decls };
        let pack_link = hashspace::put(pack.clone().encode());
        return Ok((i, (pack_link, pack, defs, refs)));
      }
      else {
        let (i2, decl) =
          alt((parse_defn(refs.to_owned()), parse_open(env.to_owned())))(i)?;
        decls.push(decl.clone());
        match decl {
          Declaration::Defn { name, defn, term } => {
            let def = Def::get_link(defn).map_err(|e| {
              Err::Error(ParseError::new(i2, ParseErrorKind::EmbeddingError(e)))
            })?;
            refs.insert(name, (defn, term));
            defs.insert(defn, def);
          }
          Declaration::Open { name, alias, with, from } => {
            let pack = Package::get_link(from).map_err(|e| {
              Err::Error(ParseError::new(i2, ParseErrorKind::EmbeddingError(e)))
            })?;
            if name != pack.name {
              return Err(Err::Error(ParseError::new(
                i2,
                ParseErrorKind::MisnamedImport(name, pack.name),
              )));
            };
            let (import_refs, import_defs): (Refs, Defs) =
              pack.refs_defs().map_err(|e| {
                Err::Error(ParseError::new(
                  i2,
                  ParseErrorKind::EmbeddingError(e),
                ))
              })?;
            defs = merge_defs(defs, import_defs);
            refs = merge_refs(refs, import_refs, alias, with);
          }
        }
        i = i2;
      }
    }
  }
}

pub fn parse_file<'a>(env: PackageEnv) -> (Link, Package, Defs, Refs) {
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
//
#[cfg(test)]
pub mod tests {
  use super::*;

  #[test]
  fn test_cases() {
    let res = parse_with(Span::new("()"));
    println!("res: {:?}", res);
    assert!(res.is_ok());
    let res = parse_with(Span::new("(a)"));
    println!("res: {:?}", res);
    assert!(res.is_ok());
    let res = parse_with(Span::new("(a,b)"));
    println!("res: {:?}", res);
    assert!(res.is_ok());
    let res = parse_with(Span::new("(a,b,c)"));
    println!("res: {:?}", res);
    assert!(res.is_ok());
  }
}
