use cid::Cid;
use libipld::{
  cbor::DagCborCodec,
  codec::Codec,
  ipld::Ipld,
};
use multihash::{
  Code,
  MultihashDigest,
};

/// This trait describes the interations with
/// externaly stored IPLD structures.
pub trait Store: std::fmt::Debug {
    fn put(&self, expr: Ipld) -> Cid;
    fn get(&self, link: Cid) -> Option<Ipld>;
}
