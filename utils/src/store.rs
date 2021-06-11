use crate::file;
use libipld::{
  cid::Cid,
  ipld::Ipld,
};
use multiaddr::Multiaddr;
use std::rc::Rc;
use yatima_core::{
  anon,
  package::{Package, Entry, Index},
  defs::{
    Def,
    Defs
  },
};

/// This trait describes the interactions with
/// externally stored IPLD structures.
pub trait Store: std::fmt::Debug {
  /// Get an IPLD from and Multiaddr 
  fn get_by_multiaddr(&self, multiaddr: Multiaddr) -> Result<Ipld, String>;

  /// Load a package in a environment agnostic way
  fn load_by_name(&self, path: Vec<&str>) -> Option<Ipld>;

  /// Put an IPLD expression into the store
  fn put(&self, expr: Ipld) -> Cid;

  /// Get an IPLD expression from the store
  fn get(&self, link: Cid) -> Option<Ipld>;
}

/// Load all the package defs from the store
pub fn load_package_defs(store: Rc<dyn Store>, package: Package) -> Result<Defs, String> {
  let Index(def_refs) = &package.index;
  let mut defs = Defs::new();
  for (name, cid) in def_refs {
    if let Some(entry_ipld) = store.get(cid.clone()) { 
      let entry = Entry::from_ipld(&entry_ipld).map_err(|e| format!("{:?}", e))?;
      if let Some(type_anon_ipld) = store.get(entry.type_anon) {
        let type_anon = anon::Anon::from_ipld(&type_anon_ipld).map_err(|e| format!("{:?}", e))?;
        if let Some(term_anon_ipld) = store.get(entry.term_anon) {
          let term_anon = anon::Anon::from_ipld(&term_anon_ipld).map_err(|e| format!("{:?}", e))?; 
          defs.insert(name.clone(), Def::unembed(entry, type_anon, term_anon).map_err(|e| format!("{:?}", e))?);
        } else {
          return Err(format!("Failed to term_anon at {}", &entry.term_anon));
        }
      } else {
        return Err(format!("Failed to type_anon at {}", &entry.type_anon));
      }
    } else {
      return Err(format!("Failed to load {} at {}", name, package.cid()));
    }
  }
  Ok(defs)
}

/// Show the contents of a link
pub fn show(
  store: Rc<dyn Store>,
  cid: Cid,
  typ_: String,
) -> Result<String, String> {
  if let Some(ipld) = store.get(cid) {
    match typ_.as_str() {
      "package" => {
        let pack = Package::from_ipld(&ipld)?;
        Ok(format!("{:?}", pack))
      }
      "entry" => {
        let entry = Entry::from_ipld(&ipld)?;
        let mut s = format!("{:?}\n", entry);
        let def = file::parse::entry_to_def(entry, store).expect("valid def");
        s += format!("{}", def).as_str();

        Ok(s)
      }
      "anon" => {
        let pack = anon::Anon::from_ipld(&ipld)?;
        Ok(format!("{:?}", pack))
      }
      _ => Ok(format!("{:?}", ipld)),
    }
  }
  else {
    Err(format!("cannot find {}", cid))
  }
}
