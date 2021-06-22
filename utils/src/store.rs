use cid::Cid;
use ipld::Ipld;

/// This trait describes the interations with
/// externaly stored IPLD structures.
pub trait Store: std::fmt::Debug {
  fn put(&self, expr: Ipld) -> Cid;
  fn get(&self, link: Cid) -> Option<Ipld>;
}
