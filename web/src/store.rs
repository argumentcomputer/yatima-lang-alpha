use libipld::{
  cid::Cid,
  cbor::DagCborCodec,
  codec::Codec,
  ipld::Ipld,
};
use yatima_utils::{
  store::{Store, cid},
};


#[derive(Debug, Clone)]
pub struct WebStore {}

impl WebStore {
  pub fn new() -> Self {
    WebStore {}
  }
}

impl Store for WebStore {
  fn get(&self, link: Cid) -> Option<Ipld> {
    // TODO
    None
  }

  fn put(&self, expr: Ipld) -> Cid {
    // TODO
    cid(&Ipld::Null)
  }
}

