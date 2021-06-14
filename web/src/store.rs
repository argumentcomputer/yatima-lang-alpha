use cid::Cid;
use sp_ipld::{
  dag_cbor,
  Ipld,
};
use yatima_utils::store::Store;

#[derive(Debug, Clone)]
pub struct WebStore {}

impl WebStore {
  pub fn new() -> Self { WebStore {} }
}

impl Store for WebStore {
  fn get(&self, _link: Cid) -> Option<Ipld> {
    // TODO
    None
  }

  fn put(&self, _expr: Ipld) -> Cid {
    // TODO
    dag_cbor::cid(&Ipld::Null)
  }
}
