use cid::Cid;
use sp_ipld::{
  dag_cbor,
  Codec,
  Ipld,
};
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
    dag_cbor::cid(&Ipld::Null)
  }
}
