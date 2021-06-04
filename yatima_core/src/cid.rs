use cid::Cid;
use multihash::{
  Code,
  MultihashDigest,
};
use sp_ipld::{
  Codec,
  DagCborCodec,
  Ipld,
};

pub fn cid(x: &Ipld) -> Cid {
  Cid::new_v1(
    0x71,
    Code::Blake2b256
      .digest(DagCborCodec.encode(x).unwrap().into_inner().as_ref()),
  )
}
