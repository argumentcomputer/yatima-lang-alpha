use crate::{
  file::{
    error,
    error::{
      FileError,
      FileErrorKind,
    },
  },
  store::Store,
};
use yatima_core::{
  self,
  anon::Anon,
  defs::{
    Def,
    Defs,
  },
  name::Name,
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

use sp_cid::Cid;
use sp_ipld::Ipld;

use nom::{
  bytes::complete::tag,
  combinator::opt,
  sequence::terminated,
  Err,
  IResult,
};

use std::{
  cell::RefCell,
  collections::{
    HashMap,
    HashSet,
  },
  ffi::OsString,
  fs,
  path::PathBuf,
  rc::Rc,
};

#[derive(Debug, Clone)]
pub struct PackageEnv {
  root: PathBuf,
  path: PathBuf,
  store: Rc<dyn Store>,
  open: Rc<RefCell<HashSet<PathBuf>>>,
  done: Rc<RefCell<HashMap<PathBuf, Cid>>>,
  // sources: Rc<RefCell<HashMap<Cid, PathBuf>>>,
}

impl PackageEnv {
  #[must_use]
  pub fn new(root: PathBuf, path: PathBuf, store: Rc<dyn Store>) -> Self {
    Self {
      root,
      path,
      store: store.clone(),
      open: Rc::new(RefCell::new(HashSet::new())),
      done: Rc::new(RefCell::new(HashMap::new())),
    }
  }

  pub fn insert_open(&self, path: PathBuf) -> bool {
    let mut open = self.open.borrow_mut();
    open.insert(path)
  }

  pub fn remove_open(&self, path: PathBuf) -> bool {
    let mut open = self.open.borrow_mut();
    open.remove(&path)
  }

  pub fn insert_done(&self, path: PathBuf, cid: Cid) -> Option<Cid> {
    let mut done = self.done.borrow_mut();
    done.insert(path, cid)
  }

  pub fn get_done_cid(&self, path: &PathBuf) -> Option<Cid> {
    let done = self.done.borrow();
    let cid = done.get(path);
    cid.cloned()
  }
}

pub fn parse_file(env: PackageEnv) -> Result<(Cid, Package, Defs), String> {
  let mut path = env.root.clone();
  path.push(env.path.clone());
  let txt = fs::read_to_string(&path)
    .map_err(|e| format!("file {:?} not found {:?}", &path, e))?;
  parse_text(txt.as_str(), env)
}

pub fn parse_text(
  txt: &str,
  env: PackageEnv,
) -> Result<(Cid, Package, Defs), String> {
  let path = env.path.clone();
  let input_cid = env.store.put(Ipld::String(txt.to_owned()));
  match parse_package(input_cid, env)(Span::new(&txt)) {
    Ok((_, p)) => Ok(p),
    Err(e) => match e {
      Err::Incomplete(_) => Err("Incomplete".to_owned()),
      Err::Failure(e) => {
        Err(format!("Parse Failure in {}:\n{}", path.to_string_lossy(), e))
      }
      Err::Error(e) => {
        Err(format!("Parse Error in {}:\n{}", path.to_string_lossy(), e))
      }
    },
  }
}

pub fn entry_to_def(
  d: Entry,
  store: Rc<dyn Store>,
) -> Result<Def, FileErrorKind> {
  use FileErrorKind::*;
  let type_ipld: Ipld =
    store.get(d.type_anon).map_or_else(|| Err(UnknownLink(d.type_anon)), Ok)?;
  let type_anon: Anon =
    Anon::from_ipld(&type_ipld).map_or_else(|e| Err(IpldError(e)), Ok)?;
  let term_ipld: Ipld =
    store.get(d.term_anon).map_or_else(|| Err(UnknownLink(d.term_anon)), Ok)?;
  let term_anon =
    Anon::from_ipld(&term_ipld).map_or_else(|e| Err(IpldError(e)), Ok)?;
  Def::unembed(d, type_anon, term_anon)
    .map_or_else(|e| Err(EmbedError(Box::new(e))), Ok)
}

pub fn index_to_defs(
  i: &Index,
  env: PackageEnv,
) -> Result<Defs, FileErrorKind> {
  use FileErrorKind::*;
  let mut defs = Defs::new();
  for (n, cid) in &i.0 {
    let entry_ipld: Ipld =
      env.store.get(*cid).map_or_else(|| Err(UnknownLink(*cid)), Ok)?;
    let entry: Entry =
      Entry::from_ipld(&entry_ipld).map_or_else(|e| Err(IpldError(e)), Ok)?;
    let def = entry_to_def(entry, env.store.clone())?;
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
    let alias = alias.unwrap_or_else(|| Name::from(""));
    let (i, with) =
      opt(terminated(parse_with, parse_space))(i).map_err(error::convert)?;
    let (i, from) =
      opt(terminated(parse_link, parse_space))(i).map_err(error::convert)?;

    let mut import_path = env.root.clone();
    for n in name.split('.') {
      import_path.push(n);
    }
    import_path.set_extension("ya");

    let from = match from {
      Some(cid) => Some(cid),
      None => env.get_done_cid(&import_path.clone()),
    };
    if let Some(from) = from {
      use FileErrorKind::*;
      let (_, pack) = env.store.get(from).map_or_else(
        || Err(Err::Error(FileError::new(i, UnknownLink(from)))),
        |v| Ok((i, v)),
      )?;
      let (_, pack) = Package::from_ipld(&pack).map_or_else(
        |e| Err(Err::Error(FileError::new(i, IpldError(e)))),
        |v| Ok((i, v)),
      )?;
      let (_, defs) = index_to_defs(&pack.index, env.clone()).map_or_else(
        |e| Err(Err::Error(FileError::new(i, e))),
        |v| Ok((i, v)),
      )?;
      let with: Vec<Name> = with.unwrap_or_else(|| defs.names());
      Ok((i, (from, Import { cid: from, name, alias, with }, defs)))
    }
    else {
      let has_path = env.insert_open(import_path.clone());
      if !has_path {
        Err(Err::Error(FileError::new(
          i,
          FileErrorKind::ImportCycle(import_path.clone()),
        )))
      }
      else {
        let env = PackageEnv {
          root: env.root.clone(),
          path: import_path
            .clone()
            .strip_prefix(env.root.clone())
            .unwrap()
            .to_path_buf(),
          open: env.open.clone(),
          done: env.done.clone(),
          store: env.store.clone(),
        };
        let (from, pack, defs) = parse_file(env.clone()).map_err(|e| {
          nom::Err::Error(error::FileError::new(
            i,
            error::FileErrorKind::SystemError(e),
          ))
        })?;
        env.remove_open(import_path.clone());
        env.insert_done(import_path, from);
        let names = pack.index.keys();
        let with = with.unwrap_or_else(|| names);
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
                    imp.name.to_string(),
                    from,
                    def_name.to_string(),
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
    for (n, _) in index.0.iter() {
      let d = defs.get(n).unwrap();
      let (entry, typ, trm) = d.clone().embed();
      env.store.put(typ.to_ipld());
      env.store.put(trm.to_ipld());
      let entry_cid = env.store.put(entry.to_ipld());
      if entry_cid != d.def_cid {
        panic!("def cid error, expected {}, got {}", d.def_cid, entry_cid)
      }
    }
    let pos = Pos::from_upto(input, from, upto);
    let package = Package { pos, name, imports, index };
    let pack_cid = env.store.put(package.to_ipld());
    Ok((from, (pack_cid, package, defs)))
  }
}
