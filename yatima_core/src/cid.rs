
use cid::Cid;
use sp_ipld::{
  DagCborCodec,
  Codec,
  Ipld,
};
use multihash::{
  Code,
  MultihashDigest,
};

pub fn cid(x: &Ipld) -> Cid {
  Cid::new_v1(
    0x71,
    Code::Blake2b256.digest(DagCborCodec.encode(x).unwrap().into_inner().as_ref()),
  )
}
