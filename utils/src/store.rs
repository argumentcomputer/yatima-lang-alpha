use std::{
  rc::Rc,
};
use libipld::{
  ipld::Ipld,
  cid::Cid,
};
use yatima_core::{
  package,
  anon,
};
use crate::file;

/// This trait describes the interations with
/// externaly stored IPLD structures. 
pub trait Store: std::fmt::Debug {
  // fn load_file(&self, path: Vec<u8>);

  /// Put an Ipld expression into the store
  fn put(&self, expr: Ipld) -> Cid;

  /// Get an Ipld expression from the store
  fn get(&self, link: Cid) -> Option<Ipld>;
}

/// Show the contents of a link
pub fn show(store: Rc<dyn Store>, cid: Cid, typ_: String) -> Result<String, String> {
  if let Some(ipld) = store.get(cid) {
    match typ_.as_str() {
      "package" => {
        let pack = package::Package::from_ipld(&ipld)?;
        Ok(format!("{:?}", pack))
      }
      "entry" => {
        let entry =
          package::Entry::from_ipld(&ipld)?;
        let mut s = format!("{:?}\n", entry);
        let def = file::parse::entry_to_def(entry, store).expect("valid def");
        s += format!("{}", def).as_str();

        Ok(s)
      }
      "anon" => {
        let pack =
          anon::Anon::from_ipld(&ipld)?;
        Ok(format!("{:?}", pack))
      }
      _ => {
        Ok(format!("{:?}", ipld))
      }
    }
  } else {
    Err(format!("cannot find {}", cid))
  } 
}
