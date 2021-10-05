use alloc::string::String;
use sp_ipld::Ipld;
use sp_std::{
  num::TryFromIntError,
  vec::Vec,
};

/// IPLD objects that fail during conversion
#[derive(PartialEq, Debug, Clone)]
pub enum IpldError {
  Utf8(Vec<u8>, alloc::string::FromUtf8Error),
  ByteCount(Vec<u8>, u64),
  UnicodeChar(u32),
  U64(TryFromIntError),
  Uses(Ipld),
  Bool(Ipld),
  Position(Ipld),
  Literal(Ipld),
  LitType(Ipld),
  PrimOp(Ipld),
  NatOp(Ipld),
  IntOp(Ipld),
  BytesOp(Ipld),
  BitsOp(Ipld),
  TextOp(Ipld),
  CharOp(Ipld),
  BoolOp(Ipld),
  U8Op(Ipld),
  U16Op(Ipld),
  U32Op(Ipld),
  U64Op(Ipld),
  U128Op(Ipld),
  I8Op(Ipld),
  I16Op(Ipld),
  I32Op(Ipld),
  I64Op(Ipld),
  I128Op(Ipld),
  IoOp(Ipld),
  Anon(Ipld),
  Meta(Ipld),
  Entry(Ipld),
  Index(Ipld),
  IndexEntry(Ipld),
  Import(Ipld),
  ImportEntry(Ipld),
  Package(Ipld),
}

impl From<IpldError> for String {
  fn from(e: IpldError) -> String { format!("{:?}", e) }
}
