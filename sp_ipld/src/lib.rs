extern crate alloc;
extern crate sp_std;

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;
#[cfg(test)]
extern crate libipld;
#[cfg(test)]
extern crate rand;

use core::{
  any::type_name,
  cmp,
  convert::TryInto,
};

use alloc::{
  borrow::ToOwned,
  boxed::Box,
  format,
  string::{
    String,
    ToString,
  },
  sync::Arc,
  vec,
};
use byteorder::{
  BigEndian,
  ByteOrder,
};
use cid::Cid;
use sp_std::{
  collections::btree_map::BTreeMap,
  convert::TryFrom,
  ops::Deref,
  vec::Vec,
};

#[derive(Clone, PartialEq)]
pub enum Ipld {
  /// Represents the absence of a value or the value undefined.
  Null,
  /// Represents a boolean value.
  Bool(bool),
  /// Represents an integer.
  Integer(i128),
  /// Represents a floating point value.
  Float(f64),
  /// Represents an UTF-8 string.
  String(String),
  /// Represents a sequence of bytes.
  Bytes(Vec<u8>),
  /// Represents a list.
  List(Vec<Ipld>),
  /// Represents a map of strings.
  StringMap(BTreeMap<String, Ipld>),
  /// Represents a link to an Ipld node.
  Link(Cid),
}

impl sp_std::fmt::Debug for Ipld {
  fn fmt(&self, f: &mut sp_std::fmt::Formatter) -> sp_std::fmt::Result {
    use Ipld::*;
    match self {
      Null => write!(f, "null"),
      Bool(b) => write!(f, "{:?}", b),
      Integer(i) => write!(f, "{:?}", i),
      Float(i) => write!(f, "{:?}", i),
      String(s) => write!(f, "{:?}", s),
      Bytes(b) => write!(f, "{:?}", b),
      List(l) => write!(f, "{:?}", l),
      StringMap(m) => write!(f, "{:?}", m),
      Link(cid) => write!(f, "{}", cid),
    }
  }
}

pub struct UnsupportedCodec(pub u64);

pub enum Error {
  UnsupportedCodec(u64),
}

pub trait Codec:
  Copy
  + Unpin
  + Send
  + Sync
  + 'static
  + Sized
  + TryFrom<u64, Error = UnsupportedCodec>
  + Into<u64> {
  fn encode<T: Encode<Self> + ?Sized>(
    &self,
    obj: &T,
  ) -> Result<ByteCursor, String> {
    let mut buf = ByteCursor::new(Vec::with_capacity(u16::MAX as usize));
    obj.encode(*self, &mut buf)?;
    Ok(buf)
  }

  fn decode<T: Decode<Self>>(
    &self,
    mut bytes: ByteCursor,
  ) -> Result<T, String> {
    T::decode(*self, &mut bytes)
  }

  fn references<T: References<Self>, E: Extend<Cid>>(
    &self,
    mut bytes: ByteCursor,
    set: &mut E,
  ) -> Result<(), String> {
    T::references(*self, &mut bytes, set)
  }
}

pub trait Encode<C: Codec> {
  fn encode(&self, c: C, w: &mut ByteCursor) -> Result<(), String>;
}

impl<C: Codec, T: Encode<C>> Encode<C> for &T {
  fn encode(&self, c: C, w: &mut ByteCursor) -> Result<(), String> {
    self.deref().encode(c, w)
  }
}

pub trait Decode<C: Codec>: Sized {
  fn decode(c: C, r: &mut ByteCursor) -> Result<Self, String>;
}

pub trait References<C: Codec>: Sized {
  fn references<E: Extend<Cid>>(
    c: C,
    r: &mut ByteCursor,
    set: &mut E,
  ) -> Result<(), String>;
}

pub trait SkipOne: Codec {
  fn skip(&self, r: &mut ByteCursor) -> Result<(), String>;
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct DagCborCodec;

impl Codec for DagCborCodec {}

impl From<DagCborCodec> for u64 {
  fn from(_: DagCborCodec) -> Self { 0x71 }
}

impl TryFrom<u64> for DagCborCodec {
  type Error = UnsupportedCodec;

  fn try_from(_: u64) -> core::result::Result<Self, Self::Error> { Ok(Self) }
}

pub trait DagCbor: Encode<DagCborCodec> + Decode<DagCborCodec> {}

impl<T: Encode<DagCborCodec> + Decode<DagCborCodec>> DagCbor for T {}

pub enum SeekFrom {
  Start(u64),
  End(i64),
  Current(i64),
}

#[derive(Clone, Debug)]
pub struct ByteCursor {
  inner: Vec<u8>,
  pos: u64,
}

impl ByteCursor {
  pub const fn new(inner: Vec<u8>) -> ByteCursor {
    ByteCursor { pos: 0, inner }
  }

  pub fn into_inner(self) -> Vec<u8> { self.inner }

  pub const fn get_ref(&self) -> &Vec<u8> { &self.inner }

  pub fn get_mut(&mut self) -> &mut Vec<u8> { &mut self.inner }

  pub const fn position(&self) -> u64 { self.pos }

  pub fn set_position(&mut self, pos: u64) { self.pos = pos }

  pub fn read(&mut self, buf: &mut [u8]) -> usize {
    let from = &mut self.fill_buf();
    let amt = cmp::min(buf.len(), from.len());
    let (a, b) = from.split_at(amt);
    if amt == 1 {
      buf[0] = a[0];
    }
    else {
      buf[..amt].copy_from_slice(a);
    }
    *from = b;
    self.pos += amt as u64;
    amt
  }

  pub fn read_exact(&mut self, buf: &mut [u8]) -> Result<(), String> {
    let n = buf.len();
    let from = &mut self.fill_buf();
    if buf.len() > from.len() {
      return Err("failed to fill whole buffer".to_owned());
    }
    let (a, b) = from.split_at(buf.len());

    if buf.len() == 1 {
      buf[0] = a[0];
    }
    else {
      buf.copy_from_slice(a);
    }

    *from = b;
    self.pos += n as u64;
    Ok(())
  }

  pub fn fill_buf(&mut self) -> &[u8] {
    let amt = cmp::min(self.pos, self.inner.len() as u64);
    &self.inner[(amt as usize)..]
  }

  fn seek(&mut self, style: SeekFrom) -> Result<u64, String> {
    let (base_pos, offset) = match style {
      SeekFrom::Start(n) => {
        self.pos = n;
        return Ok(n);
      }
      SeekFrom::End(n) => {
        let x: &[u8] = self.inner.as_ref();
        (x.len() as u64, n)
      }
      SeekFrom::Current(n) => (self.pos, n),
    };
    let new_pos = if offset >= 0 {
      base_pos.checked_add(offset as u64)
    }
    else {
      base_pos.checked_sub((offset.wrapping_neg()) as u64)
    };
    match new_pos {
      Some(n) => {
        self.pos = n;
        Ok(self.pos)
      }
      None => {
        Err("invalid seek to a negative or overflowing position".to_owned())
      }
    }
  }

  fn write(&mut self, buf: &[u8]) -> Result<usize, String> {
    let vec = &mut self.inner;
    let pos: usize = self.pos.try_into().map_err(|_| {
      "cursor position exceeds maximum possible vector length".to_owned()
    })?;
    let len = vec.len();
    if len < pos {
      vec.resize(pos, 0);
    }
    {
      let space = vec.len() - pos;
      let (left, right) = buf.split_at(cmp::min(space, buf.len()));
      vec[pos..pos + left.len()].copy_from_slice(left);
      vec.extend_from_slice(right);
    }
    self.pos = (pos + buf.len()) as u64;
    Ok(buf.len())
  }

  fn write_all(&mut self, mut buf: &[u8]) -> Result<(), String> {
    while !buf.is_empty() {
      match self.write(buf) {
        Ok(0) => {
          return Err("failed to write whole buffer".to_owned());
        }
        Ok(n) => buf = &buf[n..],
        Err(e) => return Err(e),
      }
    }
    Ok(())
  }
}

pub fn read_u8(r: &mut ByteCursor) -> Result<u8, String> {
  let mut buf = [0; 1];
  r.read_exact(&mut buf)?;
  Ok(buf[0])
}

pub fn read_u16(r: &mut ByteCursor) -> Result<u16, String> {
  let mut buf = [0; 2];
  r.read_exact(&mut buf)?;
  Ok(BigEndian::read_u16(&buf))
}

pub fn read_u32(r: &mut ByteCursor) -> Result<u32, String> {
  let mut buf = [0; 4];
  r.read_exact(&mut buf)?;
  Ok(BigEndian::read_u32(&buf))
}

pub fn read_u64(r: &mut ByteCursor) -> Result<u64, String> {
  let mut buf = [0; 8];
  r.read_exact(&mut buf)?;
  Ok(BigEndian::read_u64(&buf))
}

pub fn read_f32(r: &mut ByteCursor) -> Result<f32, String> {
  let mut buf = [0; 4];
  r.read_exact(&mut buf)?;
  Ok(BigEndian::read_f32(&buf))
}

pub fn read_f64(r: &mut ByteCursor) -> Result<f64, String> {
  let mut buf = [0; 8];
  r.read_exact(&mut buf)?;
  Ok(BigEndian::read_f64(&buf))
}

pub fn read_bytes(r: &mut ByteCursor, len: usize) -> Result<Vec<u8>, String> {
  let mut buf = vec![0; len];
  r.read_exact(&mut buf)?;
  Ok(buf)
}

pub fn read_str(r: &mut ByteCursor, len: usize) -> Result<String, String> {
  let bytes = read_bytes(r, len)?;
  Ok(
    String::from_utf8(bytes)
      .map_err(|_| "Error converting to UTF-8".to_owned())?,
  )
}

pub fn read_list<T: Decode<DagCborCodec>>(
  r: &mut ByteCursor,
  len: usize,
) -> Result<Vec<T>, String> {
  let mut list: Vec<T> = Vec::with_capacity(len);
  for _ in 0..len {
    list.push(T::decode(DagCborCodec, r)?);
  }
  Ok(list)
}

pub fn read_list_il<T: Decode<DagCborCodec>>(
  r: &mut ByteCursor,
) -> Result<Vec<T>, String> {
  let mut list: Vec<T> = Vec::new();
  loop {
    let major = read_u8(r)?;
    if major == 0xff {
      break;
    }
    r.seek(SeekFrom::Current(-1))?;
    let value = T::decode(DagCborCodec, r)?;
    list.push(value);
  }
  Ok(list)
}

pub fn read_map<K: Decode<DagCborCodec> + Ord, T: Decode<DagCborCodec>>(
  r: &mut ByteCursor,
  len: usize,
) -> Result<BTreeMap<K, T>, String> {
  let mut map: BTreeMap<K, T> = BTreeMap::new();
  for _ in 0..len {
    let key = K::decode(DagCborCodec, r)?;
    let value = T::decode(DagCborCodec, r)?;
    map.insert(key, value);
  }
  Ok(map)
}

pub fn read_map_il<K: Decode<DagCborCodec> + Ord, T: Decode<DagCborCodec>>(
  r: &mut ByteCursor,
) -> Result<BTreeMap<K, T>, String> {
  let mut map: BTreeMap<K, T> = BTreeMap::new();
  loop {
    let major = read_u8(r)?;
    if major == 0xff {
      break;
    }
    r.seek(SeekFrom::Current(-1))?;
    let key = K::decode(DagCborCodec, r)?;
    let value = T::decode(DagCborCodec, r)?;
    map.insert(key, value);
  }
  Ok(map)
}

pub fn read_link(r: &mut ByteCursor) -> Result<Cid, String> {
  let ty = read_u8(r)?;
  if ty != 0x58 {
    return Err(format!("Unknown cbor tag `{}`", ty));
  }
  let len = read_u8(r)?;
  if len == 0 {
    return Err("Length out of range when decoding Cid.".to_owned());
  }
  let bytes = read_bytes(r, len as usize)?;
  if bytes[0] != 0 {
    return Err(format!("Invalid Cid prefix: {}", bytes[0]));
  }

  // skip the first byte per
  // https://github.com/ipld/specs/blob/master/block-layer/codecs/dag-cbor.md#links
  Ok(Cid::try_from(&bytes[1..]).map_err(|x| x.to_string())?)
}

pub fn read_len(r: &mut ByteCursor, major: u8) -> Result<usize, String> {
  Ok(match major {
    0x00..=0x17 => major as usize,
    0x18 => read_u8(r)? as usize,
    0x19 => read_u16(r)? as usize,
    0x1a => read_u32(r)? as usize,
    0x1b => {
      let len = read_u64(r)?;
      if len > usize::max_value() as u64 {
        return Err("Length out of range when decoding usize.".to_owned());
      }
      len as usize
    }
    major => {
      return Err(format!(
        "Unexpected cbor code `0x{}` when decoding usize.",
        major
      ));
    }
  })
}

impl Decode<DagCborCodec> for bool {
  fn decode(_: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    let result = match major {
      0xf4 => false,
      0xf5 => true,
      _ => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding bool.",
          major
        ));
      }
    };
    Ok(result)
  }
}

impl Decode<DagCborCodec> for u8 {
  fn decode(_: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    let result = match major {
      0x00..=0x17 => major,
      0x18 => read_u8(r)?,
      _ => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding u8.",
          major
        ));
      }
    };
    Ok(result)
  }
}

impl Decode<DagCborCodec> for u16 {
  fn decode(_: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    let result = match major {
      0x00..=0x17 => major as u16,
      0x18 => read_u8(r)? as u16,
      0x19 => read_u16(r)?,
      _ => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding u16.",
          major
        ));
      }
    };
    Ok(result)
  }
}

impl Decode<DagCborCodec> for u32 {
  fn decode(_: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    let result = match major {
      0x00..=0x17 => major as u32,
      0x18 => read_u8(r)? as u32,
      0x19 => read_u16(r)? as u32,
      0x1a => read_u32(r)?,
      _ => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding u32.",
          major
        ));
      }
    };
    Ok(result)
  }
}

impl Decode<DagCborCodec> for u64 {
  fn decode(_: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    let result = match major {
      0x00..=0x17 => major as u64,
      0x18 => read_u8(r)? as u64,
      0x19 => read_u16(r)? as u64,
      0x1a => read_u32(r)? as u64,
      0x1b => read_u64(r)?,
      _ => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding u64.",
          major
        ));
      }
    };
    Ok(result)
  }
}

impl Decode<DagCborCodec> for i8 {
  fn decode(_: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    let result = match major {
      0x20..=0x37 => -1 - (major - 0x20) as i8,
      0x38 => -1 - read_u8(r)? as i8,
      _ => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding i8.",
          major
        ));
      }
    };
    Ok(result)
  }
}

impl Decode<DagCborCodec> for i16 {
  fn decode(_: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    let result = match major {
      0x20..=0x37 => -1 - (major - 0x20) as i16,
      0x38 => -1 - read_u8(r)? as i16,
      0x39 => -1 - read_u16(r)? as i16,
      _ => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding i16.",
          major
        ));
      }
    };
    Ok(result)
  }
}

impl Decode<DagCborCodec> for i32 {
  fn decode(_: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    let result = match major {
      0x20..=0x37 => -1 - (major - 0x20) as i32,
      0x38 => -1 - read_u8(r)? as i32,
      0x39 => -1 - read_u16(r)? as i32,
      0x3a => -1 - read_u32(r)? as i32,
      _ => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding i32.",
          major
        ));
      }
    };
    Ok(result)
  }
}

impl Decode<DagCborCodec> for i64 {
  fn decode(_: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    let result = match major {
      0x20..=0x37 => -1 - (major - 0x20) as i64,
      0x38 => -1 - read_u8(r)? as i64,
      0x39 => -1 - read_u16(r)? as i64,
      0x3a => -1 - read_u32(r)? as i64,
      0x3b => -1 - read_u64(r)? as i64,
      _ => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding i64.",
          major
        ));
      }
    };
    Ok(result)
  }
}

impl Decode<DagCborCodec> for f32 {
  fn decode(_: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    let result = match major {
      0xfa => read_f32(r)?,
      _ => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding f32.",
          major
        ));
      }
    };
    Ok(result)
  }
}

impl Decode<DagCborCodec> for f64 {
  fn decode(_: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    let result = match major {
      0xfa => read_f32(r)? as f64,
      0xfb => read_f64(r)?,
      _ => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding f64.",
          major
        ));
      }
    };
    Ok(result)
  }
}

impl Decode<DagCborCodec> for String {
  fn decode(_: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    let result = match major {
      0x60..=0x7b => {
        let len = read_len(r, major - 0x60)?;
        read_str(r, len)?
      }
      _ => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding String.",
          major
        ));
      }
    };
    Ok(result)
  }
}

impl Decode<DagCborCodec> for Cid {
  fn decode(_: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    if major == 0xd8 {
      if let Ok(tag) = read_u8(r) {
        if tag == 42 {
          return read_link(r);
        }
      }
    }
    Err(format!("Unexpected cbor code `0x{}` when decoding Cid.", major))
  }
}

impl Decode<DagCborCodec> for Box<[u8]> {
  fn decode(_: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    let result = match major {
      0x40..=0x5b => {
        let len = read_len(r, major - 0x40)?;
        read_bytes(r, len)?.into_boxed_slice()
      }
      _ => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding Box<[u8]>.",
          major
        ));
      }
    };
    Ok(result)
  }
}

impl<T: Decode<DagCborCodec>> Decode<DagCborCodec> for Option<T> {
  fn decode(c: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    let result = match major {
      0xf6 => None,
      0xf7 => None,
      _ => {
        r.seek(SeekFrom::Current(-1))?;
        Some(T::decode(c, r)?)
      }
    };
    Ok(result)
  }
}

impl<T: Decode<DagCborCodec>> Decode<DagCborCodec> for Vec<T> {
  fn decode(_: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    let result = match major {
      0x80..=0x9b => {
        let len = read_len(r, major - 0x80)?;
        read_list(r, len)?
      }
      0x9f => read_list_il(r)?,
      _ => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding Vec<{}>.",
          major,
          type_name::<T>()
        ));
      }
    };
    Ok(result)
  }
}

impl<K: Decode<DagCborCodec> + Ord, T: Decode<DagCborCodec>>
  Decode<DagCborCodec> for BTreeMap<K, T>
{
  fn decode(_: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    let result = match major {
      0xa0..=0xbb => {
        let len = read_len(r, major - 0xa0)?;
        read_map(r, len)?
      }
      0xbf => read_map_il(r)?,
      _ => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding BTreeMap<{}, {}>.",
          major,
          type_name::<K>(),
          type_name::<T>()
        ));
      }
    };
    Ok(result)
  }
}

impl Decode<DagCborCodec> for Ipld {
  fn decode(_: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    let ipld = match major {
      // Major type 0: an unsigned integer
      0x00..=0x17 => Self::Integer(major as i128),
      0x18 => Self::Integer(read_u8(r)? as i128),
      0x19 => Self::Integer(read_u16(r)? as i128),
      0x1a => Self::Integer(read_u32(r)? as i128),
      0x1b => Self::Integer(read_u64(r)? as i128),

      // Major type 1: a negative integer
      0x20..=0x37 => Self::Integer(-1 - (major - 0x20) as i128),
      0x38 => Self::Integer(-1 - read_u8(r)? as i128),
      0x39 => Self::Integer(-1 - read_u16(r)? as i128),
      0x3a => Self::Integer(-1 - read_u32(r)? as i128),
      0x3b => Self::Integer(-1 - read_u64(r)? as i128),

      // Major type 2: a byte string
      0x40..=0x5b => {
        let len = read_len(r, major - 0x40)?;
        let bytes = read_bytes(r, len as usize)?;
        Self::Bytes(bytes)
      }

      // Major type 3: a text string
      0x60..=0x7b => {
        let len = read_len(r, major - 0x60)?;
        let string = read_str(r, len as usize)?;
        Self::String(string)
      }

      // Major type 4: an array of data items
      0x80..=0x9b => {
        let len = read_len(r, major - 0x80)?;
        let list = read_list(r, len as usize)?;
        Self::List(list)
      }

      // Major type 4: an array of data items (indefinite length)
      0x9f => {
        let list = read_list_il(r)?;
        Self::List(list)
      }

      // Major type 5: a map of pairs of data items
      0xa0..=0xbb => {
        let len = read_len(r, major - 0xa0)?;
        Self::StringMap(read_map(r, len as usize)?)
      }

      // Major type 5: a map of pairs of data items (indefinite length)
      0xbf => {
        let pos = r.seek(SeekFrom::Current(0))?;
        r.seek(SeekFrom::Start(pos))?;
        Self::StringMap(read_map_il(r)?)
      }

      // Major type 6: optional semantic tagging of other major types
      0xd8 => {
        let tag = read_u8(r)?;
        if tag == 42 {
          Self::Link(read_link(r)?)
        }
        else {
          return Err(format!("Unknown cbor tag `{}`", tag));
        }
      }

      // Major type 7: floating-point numbers and other simple data types that
      // need no content
      0xf4 => Self::Bool(false),
      0xf5 => Self::Bool(true),
      0xf6 => Self::Null,
      0xf7 => Self::Null,
      0xfa => Self::Float(read_f32(r)? as f64),
      0xfb => Self::Float(read_f64(r)?),
      _ => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding Ipld.",
          major,
        ));
      }
    };
    Ok(ipld)
  }
}

impl References<DagCborCodec> for Ipld {
  fn references<E: Extend<Cid>>(
    c: DagCborCodec,
    r: &mut ByteCursor,
    set: &mut E,
  ) -> Result<(), String> {
    let major = read_u8(r)?;
    match major {
      // Major type 0: an unsigned integer
      0x00..=0x17 => {}
      0x18 => {
        r.seek(SeekFrom::Current(1))?;
      }
      0x19 => {
        r.seek(SeekFrom::Current(2))?;
      }
      0x1a => {
        r.seek(SeekFrom::Current(4))?;
      }
      0x1b => {
        r.seek(SeekFrom::Current(8))?;
      }

      // Major type 1: a negative integer
      0x20..=0x37 => {}
      0x38 => {
        r.seek(SeekFrom::Current(1))?;
      }
      0x39 => {
        r.seek(SeekFrom::Current(2))?;
      }
      0x3a => {
        r.seek(SeekFrom::Current(4))?;
      }
      0x3b => {
        r.seek(SeekFrom::Current(8))?;
      }

      // Major type 2: a byte string
      0x40..=0x5b => {
        let len = read_len(r, major - 0x40)?;
        r.seek(SeekFrom::Current(len as _))?;
      }

      // Major type 3: a text string
      0x60..=0x7b => {
        let len = read_len(r, major - 0x60)?;
        r.seek(SeekFrom::Current(len as _))?;
      }

      // Major type 4: an array of data items
      0x80..=0x9b => {
        let len = read_len(r, major - 0x80)?;
        for _ in 0..len {
          <Self as References<DagCborCodec>>::references(c, r, set)?;
        }
      }

      // Major type 4: an array of data items (indefinite length)
      0x9f => loop {
        let major = read_u8(r)?;
        if major == 0xff {
          break;
        }
        r.seek(SeekFrom::Current(-1))?;
        <Self as References<DagCborCodec>>::references(c, r, set)?;
      },

      // Major type 5: a map of pairs of data items
      0xa0..=0xbb => {
        let len = read_len(r, major - 0xa0)?;
        for _ in 0..len {
          <Self as References<DagCborCodec>>::references(c, r, set)?;
          <Self as References<DagCborCodec>>::references(c, r, set)?;
        }
      }

      // Major type 5: a map of pairs of data items (indefinite length)
      0xbf => loop {
        let major = read_u8(r)?;
        if major == 0xff {
          break;
        }
        r.seek(SeekFrom::Current(-1))?;
        <Self as References<DagCborCodec>>::references(c, r, set)?;
        <Self as References<DagCborCodec>>::references(c, r, set)?;
      },

      // Major type 6: optional semantic tagging of other major types
      0xd8 => {
        let tag = read_u8(r)?;
        if tag == 42 {
          set.extend(core::iter::once(read_link(r)?));
        }
        else {
          <Self as References<DagCborCodec>>::references(c, r, set)?;
        }
      }

      // Major type 7: floating-point numbers and other simple data types that
      // need no content
      0xf4..=0xf7 => {}
      0xf8 => {
        r.seek(SeekFrom::Current(1))?;
      }
      0xf9 => {
        r.seek(SeekFrom::Current(2))?;
      }
      0xfa => {
        r.seek(SeekFrom::Current(4))?;
      }
      0xfb => {
        r.seek(SeekFrom::Current(8))?;
      }
      major => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding Ipld.",
          major
        ));
      }
    };
    Ok(())
  }
}

impl<T: Decode<DagCborCodec>> Decode<DagCborCodec> for Arc<T> {
  fn decode(c: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    Ok(Arc::new(T::decode(c, r)?))
  }
}

impl Decode<DagCborCodec> for () {
  fn decode(_c: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    match major {
      0x80 => {}
      _ => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding ().",
          major
        ));
      }
    };
    Ok(())
  }
}

impl<A: Decode<DagCborCodec>> Decode<DagCborCodec> for (A,) {
  fn decode(c: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    let result = match major {
      0x81 => (A::decode(c, r)?,),
      _ => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding {}.",
          major,
          type_name::<Self>()
        ));
      }
    };
    Ok(result)
  }
}

impl<A: Decode<DagCborCodec>, B: Decode<DagCborCodec>> Decode<DagCborCodec>
  for (A, B)
{
  fn decode(c: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    let result = match major {
      0x82 => (A::decode(c, r)?, B::decode(c, r)?),
      _ => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding {}.",
          major,
          type_name::<Self>()
        ));
      }
    };
    Ok(result)
  }
}

impl<A: Decode<DagCborCodec>, B: Decode<DagCborCodec>, C: Decode<DagCborCodec>>
  Decode<DagCborCodec> for (A, B, C)
{
  fn decode(c: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    let result = match major {
      0x83 => (A::decode(c, r)?, B::decode(c, r)?, C::decode(c, r)?),
      _ => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding {}.",
          major,
          type_name::<Self>()
        ));
      }
    };
    Ok(result)
  }
}

impl<
  A: Decode<DagCborCodec>,
  B: Decode<DagCborCodec>,
  C: Decode<DagCborCodec>,
  D: Decode<DagCborCodec>,
> Decode<DagCborCodec> for (A, B, C, D)
{
  fn decode(c: DagCborCodec, r: &mut ByteCursor) -> Result<Self, String> {
    let major = read_u8(r)?;
    let result = match major {
      0x84 => {
        (A::decode(c, r)?, B::decode(c, r)?, C::decode(c, r)?, D::decode(c, r)?)
      }
      _ => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding {}.",
          major,
          type_name::<Self>()
        ));
      }
    };
    Ok(result)
  }
}

impl SkipOne for DagCborCodec {
  fn skip(&self, r: &mut ByteCursor) -> Result<(), String> {
    let major = read_u8(r)?;
    match major {
      // Major type 0: an unsigned integer
      0x00..=0x17 => {}
      0x18 => {
        r.seek(SeekFrom::Current(1))?;
      }
      0x19 => {
        r.seek(SeekFrom::Current(2))?;
      }
      0x1a => {
        r.seek(SeekFrom::Current(4))?;
      }
      0x1b => {
        r.seek(SeekFrom::Current(8))?;
      }

      // Major type 1: a negative integer
      0x20..=0x37 => {}
      0x38 => {
        r.seek(SeekFrom::Current(1))?;
      }
      0x39 => {
        r.seek(SeekFrom::Current(2))?;
      }
      0x3a => {
        r.seek(SeekFrom::Current(4))?;
      }
      0x3b => {
        r.seek(SeekFrom::Current(8))?;
      }

      // Major type 2: a byte string
      0x40..=0x5b => {
        let len = read_len(r, major - 0x40)?;
        r.seek(SeekFrom::Current(len as _))?;
      }

      // Major type 3: a text string
      0x60..=0x7b => {
        let len = read_len(r, major - 0x60)?;
        r.seek(SeekFrom::Current(len as _))?;
      }

      // Major type 4: an array of data items
      0x80..=0x9b => {
        let len = read_len(r, major - 0x80)?;
        for _ in 0..len {
          self.skip(r)?;
        }
      }

      // Major type 4: an array of data items (indefinite length)
      0x9f => loop {
        let major = read_u8(r)?;
        if major == 0xff {
          break;
        }
        r.seek(SeekFrom::Current(-1))?;
        self.skip(r)?;
      },

      // Major type 5: a map of pairs of data items
      0xa0..=0xbb => {
        let len = read_len(r, major - 0xa0)?;
        for _ in 0..len {
          self.skip(r)?;
          self.skip(r)?;
        }
      }

      // Major type 5: a map of pairs of data items (indefinite length)
      0xbf => loop {
        let major = read_u8(r)?;
        if major == 0xff {
          break;
        }
        r.seek(SeekFrom::Current(-1))?;
        self.skip(r)?;
        self.skip(r)?;
      },

      // Major type 6: optional semantic tagging of other major types
      0xd8 => {
        let _tag = read_u8(r)?;
        self.skip(r)?;
      }

      // Major type 7: floating-point numbers and other simple data types that
      // need no content
      0xf4..=0xf7 => {}
      0xf8 => {
        r.seek(SeekFrom::Current(1))?;
      }
      0xf9 => {
        r.seek(SeekFrom::Current(2))?;
      }
      0xfa => {
        r.seek(SeekFrom::Current(4))?;
      }
      0xfb => {
        r.seek(SeekFrom::Current(8))?;
      }
      major => {
        return Err(format!(
          "Unexpected cbor code `0x{}` when decoding Ipld.",
          major
        ));
      }
    };
    Ok(())
  }
}

pub fn write_null(w: &mut ByteCursor) -> Result<(), String> {
  w.write_all(&[0xf6])?;
  Ok(())
}

pub fn write_u8(
  w: &mut ByteCursor,
  major: u8,
  value: u8,
) -> Result<(), String> {
  if value <= 0x17 {
    let buf = [major << 5 | value];
    w.write_all(&buf)?;
  }
  else {
    let buf = [major << 5 | 24, value];
    w.write_all(&buf)?;
  }
  Ok(())
}

pub fn write_u16(
  w: &mut ByteCursor,
  major: u8,
  value: u16,
) -> Result<(), String> {
  if value <= u16::from(u8::max_value()) {
    write_u8(w, major, value as u8)?;
  }
  else {
    let mut buf = [major << 5 | 25, 0, 0];
    BigEndian::write_u16(&mut buf[1..], value);
    w.write_all(&buf)?;
  }
  Ok(())
}

pub fn write_u32(
  w: &mut ByteCursor,
  major: u8,
  value: u32,
) -> Result<(), String> {
  if value <= u32::from(u16::max_value()) {
    write_u16(w, major, value as u16)?;
  }
  else {
    let mut buf = [major << 5 | 26, 0, 0, 0, 0];
    BigEndian::write_u32(&mut buf[1..], value);
    w.write_all(&buf)?;
  }
  Ok(())
}

pub fn write_u64(
  w: &mut ByteCursor,
  major: u8,
  value: u64,
) -> Result<(), String> {
  if value <= u64::from(u32::max_value()) {
    write_u32(w, major, value as u32)?;
  }
  else {
    let mut buf = [major << 5 | 27, 0, 0, 0, 0, 0, 0, 0, 0];
    BigEndian::write_u64(&mut buf[1..], value);
    w.write_all(&buf)?;
  }
  Ok(())
}

pub fn write_tag(w: &mut ByteCursor, tag: u64) -> Result<(), String> {
  write_u64(w, 6, tag)
}

impl Encode<DagCborCodec> for bool {
  fn encode(&self, _: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    let buf = if *self { [0xf5] } else { [0xf4] };
    w.write_all(&buf)?;
    Ok(())
  }
}

impl Encode<DagCborCodec> for u8 {
  fn encode(&self, _: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    write_u8(w, 0, *self)
  }
}

impl Encode<DagCborCodec> for u16 {
  fn encode(&self, _: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    write_u16(w, 0, *self)
  }
}

impl Encode<DagCborCodec> for u32 {
  fn encode(&self, _: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    write_u32(w, 0, *self)
  }
}

impl Encode<DagCborCodec> for u64 {
  fn encode(&self, _: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    write_u64(w, 0, *self)
  }
}

impl Encode<DagCborCodec> for i8 {
  fn encode(&self, _: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    write_u8(w, 1, -(*self + 1) as u8)
  }
}

impl Encode<DagCborCodec> for i16 {
  fn encode(&self, _: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    write_u16(w, 1, -(*self + 1) as u16)
  }
}

impl Encode<DagCborCodec> for i32 {
  fn encode(&self, _: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    write_u32(w, 1, -(*self + 1) as u32)
  }
}

impl Encode<DagCborCodec> for i64 {
  fn encode(&self, _: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    write_u64(w, 1, -(*self + 1) as u64)
  }
}

impl Encode<DagCborCodec> for f32 {
  #[allow(clippy::float_cmp)]
  fn encode(&self, _: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    if self.is_infinite() {
      if self.is_sign_positive() {
        w.write_all(&[0xf9, 0x7c, 0x00])?;
      }
      else {
        w.write_all(&[0xf9, 0xfc, 0x00])?;
      }
    }
    else if self.is_nan() {
      w.write_all(&[0xf9, 0x7e, 0x00])?;
    }
    else {
      let mut buf = [0xfa, 0, 0, 0, 0];
      BigEndian::write_f32(&mut buf[1..], *self);
      w.write_all(&buf)?;
    }
    Ok(())
  }
}

impl Encode<DagCborCodec> for f64 {
  #[allow(clippy::float_cmp)]
  fn encode(&self, c: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    if !self.is_finite() || f64::from(*self as f32) == *self {
      let value = *self as f32;
      value.encode(c, w)?;
    }
    else {
      let mut buf = [0xfb, 0, 0, 0, 0, 0, 0, 0, 0];
      BigEndian::write_f64(&mut buf[1..], *self);
      w.write_all(&buf)?;
    }
    Ok(())
  }
}

impl Encode<DagCborCodec> for [u8] {
  fn encode(&self, _: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    write_u64(w, 2, self.len() as u64)?;
    w.write_all(self)?;
    Ok(())
  }
}

impl Encode<DagCborCodec> for Box<[u8]> {
  fn encode(&self, c: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    self[..].encode(c, w)
  }
}

impl Encode<DagCborCodec> for str {
  fn encode(&self, _: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    write_u64(w, 3, self.len() as u64)?;
    w.write_all(self.as_bytes())?;
    Ok(())
  }
}

impl Encode<DagCborCodec> for String {
  fn encode(&self, c: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    self.as_str().encode(c, w)
  }
}

impl Encode<DagCborCodec> for i128 {
  fn encode(&self, _: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    if *self < 0 {
      if -(*self + 1) > u64::max_value() as i128 {
        return Err("Number larger than u64.".to_owned());
      }
      write_u64(w, 1, -(*self + 1) as u64)?;
    }
    else {
      if *self > u64::max_value() as i128 {
        return Err("Number larger than u64.".to_owned());
      }
      write_u64(w, 0, *self as u64)?;
    }
    Ok(())
  }
}

impl Encode<DagCborCodec> for Cid {
  fn encode(&self, _: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    write_tag(w, 42)?;
    // insert zero byte per https://github.com/ipld/specs/blob/master/block-layer/codecs/dag-cbor.md#links
    // TODO: don't allocate
    let buf = self.to_bytes();
    let len = buf.len();
    write_u64(w, 2, len as u64 + 1)?;
    w.write_all(&[0])?;
    w.write_all(&buf[..len])?;
    Ok(())
  }
}

impl<T: Encode<DagCborCodec>> Encode<DagCborCodec> for Option<T> {
  fn encode(&self, c: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    if let Some(value) = self {
      value.encode(c, w)?;
    }
    else {
      write_null(w)?;
    }
    Ok(())
  }
}

impl<T: Encode<DagCborCodec>> Encode<DagCborCodec> for Vec<T> {
  fn encode(&self, c: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    write_u64(w, 4, self.len() as u64)?;
    for value in self {
      value.encode(c, w)?;
    }
    Ok(())
  }
}

impl<K: Encode<DagCborCodec>, T: Encode<DagCborCodec> + 'static>
  Encode<DagCborCodec> for BTreeMap<K, T>
{
  fn encode(&self, c: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    write_u64(w, 5, self.len() as u64)?;
    let mut vec: Vec<_> = self.into_iter().collect();
    vec.sort_unstable_by(|&(k1, _), &(k2, _)| {
      let mut bc1 = ByteCursor::new(Vec::new());
      let _ = k1.encode(c, &mut bc1);
      let mut bc2 = ByteCursor::new(Vec::new());
      let _ = k2.encode(c, &mut bc2);
      bc1.into_inner().cmp(&bc2.into_inner())
    });
    for (k, v) in vec {
      k.encode(c, w)?;
      v.encode(c, w)?;
    }
    Ok(())
  }
}

impl Encode<DagCborCodec> for Ipld {
  fn encode(&self, c: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    match self {
      Self::Null => write_null(w),
      Self::Bool(b) => b.encode(c, w),
      Self::Integer(i) => i.encode(c, w),
      Self::Float(f) => f.encode(c, w),
      Self::Bytes(b) => b.as_slice().encode(c, w),
      Self::String(s) => s.encode(c, w),
      Self::List(l) => l.encode(c, w),
      Self::StringMap(m) => m.encode(c, w),
      Self::Link(cid) => cid.encode(c, w),
    }
  }
}

impl<T: Encode<DagCborCodec>> Encode<DagCborCodec> for Arc<T> {
  fn encode(&self, c: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    self.deref().encode(c, w)
  }
}

impl Encode<DagCborCodec> for () {
  fn encode(&self, _c: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    write_u8(w, 4, 0)?;
    Ok(())
  }
}

impl<A: Encode<DagCborCodec>> Encode<DagCborCodec> for (A,) {
  fn encode(&self, c: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    write_u8(w, 4, 1)?;
    self.0.encode(c, w)?;
    Ok(())
  }
}

impl<A: Encode<DagCborCodec>, B: Encode<DagCborCodec>> Encode<DagCborCodec>
  for (A, B)
{
  fn encode(&self, c: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    write_u8(w, 4, 2)?;
    self.0.encode(c, w)?;
    self.1.encode(c, w)?;
    Ok(())
  }
}

impl<A: Encode<DagCborCodec>, B: Encode<DagCborCodec>, C: Encode<DagCborCodec>>
  Encode<DagCborCodec> for (A, B, C)
{
  fn encode(&self, c: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    write_u8(w, 4, 3)?;
    self.0.encode(c, w)?;
    self.1.encode(c, w)?;
    self.2.encode(c, w)?;
    Ok(())
  }
}

impl<
  A: Encode<DagCborCodec>,
  B: Encode<DagCborCodec>,
  C: Encode<DagCborCodec>,
  D: Encode<DagCborCodec>,
> Encode<DagCborCodec> for (A, B, C, D)
{
  fn encode(&self, c: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    write_u8(w, 4, 4)?;
    self.0.encode(c, w)?;
    self.1.encode(c, w)?;
    self.2.encode(c, w)?;
    self.3.encode(c, w)?;
    Ok(())
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  #[macro_use]
  use quickcheck::{
    quickcheck,
    Arbitrary,
    TestResult,
    Gen,
  };

  impl Arbitrary for Ipld {
    fn arbitrary(g: &mut Gen) -> Self {
      let choice = g.choose(&[0, 1, 2, 3, 4, 5, 6, 7]);
      match choice {
        Some(0) => Ipld::Bool(Arbitrary::arbitrary(g)),
        Some(1) => Ipld::Integer(Arbitrary::arbitrary(g)),
        Some(2) => Ipld::Float(Arbitrary::arbitrary(g)),
        Some(3) => Ipld::Bytes(Arbitrary::arbitrary(g)),
        Some(4) => Ipld::String(Arbitrary::arbitrary(g)),
        Some(5) => Ipld::List(Arbitrary::arbitrary(&mut Gen::new(5))),
        Some(6) => Ipld::StringMap(Arbitrary::arbitrary(&mut Gen::new(5))),
        Some(7) => Ipld::Link(arbitrary_link(g)),
        _ => Ipld::Null,
      }
    }
  }

  #[derive(Debug, Clone)]
  pub struct ACid(pub Cid);

  impl Arbitrary for ACid {
    fn arbitrary(g: &mut Gen) -> Self { ACid(arbitrary_link(g)) }
  }

  fn to_libipld(x: Ipld) -> libipld::Ipld {
    match x {
      Ipld::Null => libipld::Ipld::Null,
      Ipld::Bool(b) => libipld::Ipld::Bool(b),
      Ipld::Integer(i) => libipld::Ipld::Integer(i),
      Ipld::Float(f) => libipld::Ipld::Float(f),
      Ipld::Bytes(bs) => libipld::Ipld::Bytes(bs),
      Ipld::String(s) => libipld::Ipld::String(s),
      Ipld::List(xs) => {
        libipld::Ipld::List(xs.iter().map(|x| to_libipld(x.clone())).collect())
      }
      Ipld::StringMap(xs) => libipld::Ipld::StringMap(
        xs.iter().map(|(x, y)| (x.clone(), to_libipld(y.clone()))).collect(),
      ),
      Ipld::Link(l) => libipld::Ipld::Link(l),
    }
  }

  fn arbitrary_link_maybe(g: &mut Gen) -> Option<Cid> {
    let x: Vec<u8> = Arbitrary::arbitrary(g);
    Cid::try_from(x).ok()
  }

  fn arbitrary_link(g: &mut Gen) -> Cid {
    arbitrary_link_maybe(g).unwrap_or_else(|| arbitrary_link(g))
  }

  fn encode_equivalent(value1: Ipld) -> bool {
    let res1 = DagCborCodec.encode(&value1.clone());
    use libipld::codec::Codec;
    let res2 = libipld::cbor::DagCborCodec.encode(&to_libipld(value1));
    match (res1, res2) {
      (Ok(vec1), Ok(vec2)) => vec1.into_inner() == vec2,
      (Err(_), Err(_)) => true,
      _ => false,
    }
  }

  fn encode_decode_id<T: DagCbor + PartialEq<T> + Clone>(value: T) -> bool {
    let mut bc = ByteCursor::new(Vec::new());
    match Encode::encode(&value.clone(), DagCborCodec, &mut bc) {
      Ok(()) => {
        bc.set_position(0);
        match Decode::decode(DagCborCodec, &mut bc) {
          Ok(new_value) => return value == new_value,
          Err(e) => eprintln!("Error occurred during decoding: {}", e),
        }
      }
      Err(e) => eprintln!("Error occurred during encoding: {}", e),
    }
    false
  }

  #[quickcheck]
  pub fn ee_null() -> bool { encode_equivalent(Ipld::Null) }

  #[quickcheck]
  pub fn ee_bool(x: bool) -> bool { encode_equivalent(Ipld::Bool(x)) }

  #[quickcheck]
  pub fn ee_integer(x: i128) -> bool { encode_equivalent(Ipld::Integer(x)) }

  #[quickcheck]
  pub fn ee_float(x: f64) -> bool { encode_equivalent(Ipld::Float(x)) }

  #[quickcheck]
  pub fn ee_bytes(x: Vec<u8>) -> bool { encode_equivalent(Ipld::Bytes(x)) }

  #[quickcheck]
  pub fn ee_string(x: String) -> bool { encode_equivalent(Ipld::String(x)) }

  // overflows stack
  // #[quickcheck]
  // pub fn ee_list(x: Vec<Ipld>) -> bool { encode_equivalent(Ipld::List(x)) }

  // overflows stack
  // #[quickcheck]
  // pub fn ee_string_map(x: BTreeMap<String, Ipld>) -> bool {
  // encode_equivalent(Ipld::StringMap(x))
  // }

  #[quickcheck]
  pub fn ee_link(x: ACid) -> bool { encode_equivalent(Ipld::Link(x.0)) }

  #[quickcheck]
  pub fn edid_null() -> bool { encode_decode_id(Ipld::Null) }

  #[quickcheck]
  pub fn edid_bool(x: bool) -> bool { encode_decode_id(Ipld::Bool(x)) }

  // overflows stack
  // #[quickcheck]
  // pub fn edid_integer(x: i128) -> TestResult {
  // if (x > u64::MAX as i128) || (-x > u64::MAX as i128) {
  // TestResult::discard()
  // }
  // else {
  // if encode_decode_id(Ipld::Integer(x)) {
  // TestResult::passed()
  // }
  // else {
  // TestResult::failed()
  // }
  // }
  // }

  // overflows stack
  // #[quickcheck]
  // pub fn edid_float(x: f64) -> bool { encode_decode_id(Ipld::Float(x)) }

  #[quickcheck]
  pub fn edid_bytes(x: Vec<u8>) -> bool { encode_decode_id(Ipld::Bytes(x)) }

  #[quickcheck]
  pub fn edid_string(x: String) -> bool { encode_decode_id(Ipld::String(x)) }

  // overflows stack
  // #[quickcheck]
  // pub fn edid_list(x: Vec<Ipld>) -> bool { encode_decode_id(Ipld::List(x)) }

  // overflows stack
  // #[quickcheck]
  // pub fn edid_string_map(x: BTreeMap<String, Ipld>) -> bool {
  //   encode_decode_id(Ipld::StringMap(x))
  // }

  // errors with unknown cbor tag
  // #[quickcheck]
  // pub fn edid_link(x: ACid) -> bool { encode_decode_id(Ipld::Link(x.0)) }
}
