use yatima_core::{
  self,
  anon::Anon,
  defs::{
    Def,
    Defs,
  },
  package::{
    import_alias,
    Entry,
    Import,
    Index,
    Package,
  },
  parse::{
    package::{
      parse_alias,
      parse_defs,
      parse_link,
      parse_with,
    },
    span::Span,
    term::{
      parse_name,
      parse_space,
    },
  },
  position::Pos,
};

use crate::file::{
  error,
  error::{
    FileError,
    FileErrorKind,
  },
  store,
};

use std::{
  ffi::OsString,
  fs,
  path::PathBuf,
};

use cid::Cid;
use libipld::ipld::Ipld;

use im::{
  HashMap,
  HashSet,
};

use nom::{
  bytes::complete::tag,
  combinator::opt,
  sequence::terminated,
  Err,
  IResult,
};

use std::{
  cell::RefCell,
  rc::Rc,
};

#[derive(Debug, Clone)]
pub struct PackageEnv {
  root: PathBuf,
  path: PathBuf,
  open: HashSet<PathBuf>,
  /* sources: Rc<RefCell<HashMap<Cid, PathBuf>>>,
   * done: Rc<RefCell<HashMap<PathBuf, Cid>>>, */
}

impl PackageEnv {
  #[must_use]
  pub fn new(root: PathBuf, path: PathBuf) -> Self {
    Self {
      root,
      path,
      open: HashSet::new(),
      /*    sources: Rc::new(RefCell::new(HashMap::new())),
       *    done: Rc::new(RefCell::new(HashMap::new())), */
    }
  }

  // pub fn insert_source(&self, cid: Cid, path: PathBuf) {
  //  let mut sources = self.sources.borrow_mut();
  //  sources.insert(cid, path);
  //}

  // pub fn insert_done(&self, path: PathBuf, cid: Cid) {
  //  let mut done = self.done.borrow_mut();
  //  done.insert(path, cid);
  //}
}

pub fn parse_file(env: PackageEnv) -> (Cid, Package, Defs) {
  let path = env.path.clone();
  let txt = fs::read_to_string(&path).expect("file not found");
  let input_cid = store::put(Ipld::String(txt.clone()));
  // env.insert_source(input_cid, path.clone());
  match parse_package(input_cid, env)(Span::new(&txt)) {
    Ok((_, p)) => p,
    Err(e) => match e {
      Err::Incomplete(_) => panic!("Incomplete"),
      Err::Failure(e) => {
        println!("Parse Failure in {}:\n{}", path.to_string_lossy(), e);
        panic!()
      }
      Err::Error(e) => {
        println!("Parse Error in {}:\n{}", path.to_string_lossy(), e);
        panic!()
      }
    },
  }
}

pub fn entry_to_def(d: Entry) -> Result<Def, FileErrorKind> {
  use FileErrorKind::*;
  let type_ipld: Ipld = store::get(d.type_anon)
    .map_or_else(|| Err(UnknownLink(d.type_anon)), Ok)?;
  let type_anon: Anon =
    Anon::from_ipld(&type_ipld).map_or_else(|e| Err(IpldError(e)), Ok)?;
  let term_ipld: Ipld = store::get(d.term_anon)
    .map_or_else(|| Err(UnknownLink(d.term_anon)), Ok)?;
  let term_anon =
    Anon::from_ipld(&term_ipld).map_or_else(|e| Err(IpldError(e)), Ok)?;
  Def::unembed(d, type_anon, term_anon)
    .map_or_else(|e| Err(EmbedError(Box::new(e))), Ok)
}

pub fn index_to_defs(i: &Index) -> Result<Defs, FileErrorKind> {
  use FileErrorKind::*;
  let mut defs = Defs::new();
  for (n, cid) in &i.0 {
    let entry_ipld: Ipld =
      store::get(*cid).map_or_else(|| Err(UnknownLink(*cid)), Ok)?;
    let entry: Entry =
      Entry::from_ipld(&entry_ipld).map_or_else(|e| Err(IpldError(e)), Ok)?;
    let def = entry_to_def(entry)?;
    defs.insert(n.clone(), def);
  }
  Ok(defs)
}

pub fn parse_import(
  env: PackageEnv,
) -> impl Fn(Span) -> IResult<Span, (Cid, Import, Defs), FileError<Span>> {
  move |i: Span| {
    let (i, _) = tag("import")(i)?;
    let (i, _) = parse_space(i).map_err(error::convert)?;
    let (i, name) = parse_name(i).map_err(error::convert)?;
    let (i, _) = parse_space(i).map_err(error::convert)?;
    let (i, alias) =
      opt(terminated(parse_alias, parse_space))(i).map_err(error::convert)?;
    let alias = alias.unwrap_or_else(|| String::from(""));
    let (i, with) =
      opt(terminated(parse_with, parse_space))(i).map_err(error::convert)?;
    let (i, from) =
      opt(terminated(parse_link, parse_space))(i).map_err(error::convert)?;

    if let Some(from) = from {
      use FileErrorKind::*;
      let (_, pack) = store::get(from).map_or_else(
        || Err(Err::Error(FileError::new(i, UnknownLink(from)))),
        |v| Ok((i, v)),
      )?;
      let (_, pack) = Package::from_ipld(&pack).map_or_else(
        |e| Err(Err::Error(FileError::new(i, IpldError(e)))),
        |v| Ok((i, v)),
      )?;
      let (_, defs) = index_to_defs(&pack.index).map_or_else(
        |e| Err(Err::Error(FileError::new(i, e))),
        |v| Ok((i, v)),
      )?;
      let with: Vec<String> = with.unwrap_or_else(|| defs.names());
      Ok((i, (from, Import { cid: from, name, alias, with }, defs)))
    }
    else {
      let mut path = env.root.clone();
      for n in name.split('.') {
        path.push(n);
      }
      path.set_extension("ya");
      let mut open = env.open.clone();
      let has_path = open.insert(path.clone());
      if has_path.is_some() {
        Err(Err::Error(FileError::new(i, FileErrorKind::ImportCycle(path))))
      }
      else {
        let env = PackageEnv {
          root: env.root.clone(),
          path,
          open,
          /* sources: env.sources.clone(),
           * done: env.done.clone(), */
        };
        let (from, _, defs) = parse_file(env);
        let with = with.unwrap_or_else(|| defs.names());
        Ok((i, (from, Import { cid: from, name, alias, with }, defs)))
      }
    }
  }
}

pub fn parse_imports(
  env: PackageEnv,
) -> impl Fn(Span) -> IResult<Span, (Vec<Import>, Defs), FileError<Span>> {
  move |i: Span| {
    let mut defs = Defs::new();
    let mut imps: Vec<Import> = Vec::new();
    let mut i = i;
    loop {
      let (i2, _) = parse_space(i).map_err(error::convert)?;
      i = i2;
      let end: IResult<Span, Span, FileError<Span>> = tag("where")(i);
      match end {
        Ok((i_end, _)) => return Ok((i_end, (imps, defs))),
        _ => {
          let (i2, (from, imp, imp_defs)) = parse_import(env.clone())(i)?;
          for (def_name, _) in &imp_defs.names {
            let imp_def = imp_defs.get(def_name).unwrap();
            let def_name = import_alias(def_name.clone(), &imp);
            if let Some(def) = defs.get(&def_name) {
              if imp_def.def_cid != def.def_cid {
                return Err(Err::Error(FileError::new(
                  i,
                  FileErrorKind::ImportCollision(
                    imp.name.clone(),
                    from,
                    def_name.clone(),
                  ),
                )));
              }
            }
          }
          defs = defs.merge(imp_defs, &imp);
          imps.push(imp);
          i = i2;
        }
      }
    }
  }
}

pub fn parse_package(
  input: Cid,
  env: PackageEnv,
) -> impl Fn(Span) -> IResult<Span, (Cid, Package, Defs), FileError<Span>> {
  move |from: Span| {
    let (i, _) = parse_space(from).map_err(error::convert)?;
    let (i, _) = tag("package")(i)?;
    let (i, _) = parse_space(i).map_err(error::convert)?;
    let (i, name) = parse_name(i).map_err(error::convert)?;
    // println!("name {}", name);
    let file_name = env.path.file_name().ok_or_else(|| {
      Err::Error(FileError::new(i, FileErrorKind::MalformedPath))
    })?;
    let name_os: OsString = format!("{}.ya", name).into();
    if name_os != file_name {
      return Err(Err::Error(FileError::new(
        i,
        FileErrorKind::MisnamedPackage(name),
      )));
    }
    let (i, (imports, defs)) = parse_imports(env.clone())(i)?;
    let (i, _) = parse_space(i).map_err(error::convert)?;
    let (upto, (defs, index)) =
      parse_defs(input, defs)(i).map_err(error::convert)?;
    for (_, d) in &defs.defs {
      let (def, typ, trm) = d.embed();
      store::put(typ.to_ipld());
      store::put(trm.to_ipld());
      let def_cid = store::put(def.to_ipld());
      if def_cid != d.def_cid {
        panic!("def cid error, expected {}, got {}", d.def_cid, def_cid)
      }
    }
    let pos = Pos::from_upto(input, from, upto);
    let package = Package { pos, name, imports, index };
    let pack_cid = store::put(package.to_ipld());
    Ok((from, (pack_cid, package, defs)))
  }
}
