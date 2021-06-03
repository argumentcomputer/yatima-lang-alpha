
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

pub fn cid(x: &Ipld) -> Cid {
  Cid::new_v1(
    0x71,
    Code::Blake2b256.digest(&DagCborCodec.encode(x).unwrap()),
  )
}
