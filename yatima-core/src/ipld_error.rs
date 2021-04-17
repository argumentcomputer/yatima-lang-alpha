use libipld::ipld::Ipld;
use std::num::TryFromIntError;

pub enum IpldError {
  Utf8(Vec<u8>, std::string::FromUtf8Error),
  ByteCount(Vec<u8>, u64),
  UnicodeChar(u32),
  U64(TryFromIntError),
  Uses(Ipld),
  Bool(Ipld),
  Position(Ipld),
  Literal(Ipld),
  LitType(Ipld),
  PrimOp(Ipld),
  Anon(Ipld),
  Meta(Ipld),
  Definition(Ipld),
  Index(Ipld),
  IndexEntry(Ipld),
}
