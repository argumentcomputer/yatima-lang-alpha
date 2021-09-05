use sp_ipld::Ipld;
use std::{
  io::{
    self,
    Error,
    ErrorKind,
  },
  path::PathBuf,
  rc::Rc,
};
use yatima_core::{
  check::error::CheckError,
  defs::Defs,
  package::Package,
  position::Pos,
};

use crate::{
  debug,
  log,
  store::{
    self,
    Store,
    CallbackResult,
  },
};

pub mod error;
pub mod parse;

pub fn check_all_in_file(
  root: PathBuf,
  path: PathBuf,
  store: Rc<dyn Store>,
) -> io::Result<Rc<Defs>> {
  let env = parse::PackageEnv::new(root, path, store.clone());
  let (_, p, ds) = parse::parse_file(env).map_err(|e| Error::new(ErrorKind::Other, e))?;
  let cid = store.put(p.to_ipld());
  debug!("Checking package {} at {}", p.name, cid);
  check_all(Rc::new(p), Rc::new(ds), store).map_err(|e| Error::new(ErrorKind::Other, e))
}

/// Type check all in an IPLD representation of a package
pub fn check_all_in_ipld(
  ipld: Ipld,
  store: Rc<dyn Store>,
) -> Result<(Rc<Package>, Rc<Defs>), String> {
  let p = Rc::new(Package::from_ipld(&ipld)?);

  if let CallbackResult::Sync(ds) =
    store::load_package_defs(store.clone(), p.clone(), None)? {
  debug!("Checking package {} at {}", p.name, p.cid());
  check_all(p.clone(), Rc::new(ds), store).map(|defs| (p, defs))
  }
  else {
    panic!("Illegal state")
  }
}

pub fn check_all(p: Rc<Package>, ds: Rc<Defs>, store: Rc<dyn Store>) -> Result<Rc<Defs>, String> {
  for i in &p.imports {
    debug!("Checking import {} at {}", i.name, i.cid);
    for n in &i.with {
      match yatima_core::check::check_def(
        ds.clone(),
        &yatima_core::package::import_alias(n.to_owned(), &i),
        false,
      ) {
        Ok(ty) => {
          log!("✓ {}: {}", n, ty.pretty(Some(&n.to_string()), false))
        }
        Err(e @ CheckError::UndefinedReference(Pos::None, _)) => {
          log!("✕ {}: {}", n, e);
        }
        Err(err) => {
          let def = ds.get(n).unwrap();
          log!("✕ {}: {}", n, def.typ_.pretty(Some(&n.to_string()), false));
          if let Pos::Some(pos) = err.pos() {
            if let Some(Ipld::String(input)) = store.get(pos.input) {
              log!("{}", pos.range(input))
            }
          }
          log!("Error: {}", err);
        }
      }
    }
  }
  log!("Checking definitions:");
  for (n, _) in &p.index.0 {
    match yatima_core::check::check_def(ds.clone(), n, false) {
      Ok(ty) => log!("✓ {}: {}", n, ty.pretty(Some(&n.to_string()), false)),
      Err(e @ CheckError::UndefinedReference(Pos::None, _)) => {
        log!("✕ {}: {}", n, e);
      }
      Err(err) => {
        let def = ds.get(n).unwrap();
        log!("✕ {}: {}", n, def.typ_.pretty(Some(&n.to_string()), false));
        if let Pos::Some(pos) = err.pos() {
          if let Some(Ipld::String(input)) = store.get(pos.input) {
            log!("{}", pos.range(input))
          }
        }
        log!("Error: {}", err);
      }
    }
  }
  Ok(ds)
}
