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

pub fn cid(x: &Ipld) -> Cid {
  Cid::new_v1(0x71, Code::Blake2b256.digest(&DagCborCodec.encode(x).unwrap()))
}
