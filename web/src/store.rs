use cid::Cid;
use sp_ipld::{
  Codec,
  DagCborCodec,
  Ipld,
};
use yatima_core::cid::cid;
use yatima_utils::store::Store;

#[derive(Debug, Clone)]
pub struct WebStore {}

impl WebStore {
  pub fn new() -> Self { WebStore {} }
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
