use crate::{
  codec::Encode,
  dag_cbor::DagCborCodec,
  ipld::Ipld,
};

use alloc::{
  borrow::ToOwned,
  boxed::Box,
  string::String,
  sync::Arc,
};
use byteorder::{
  BigEndian,
  ByteOrder,
};
use sp_std::{
  collections::btree_map::BTreeMap,
  convert::TryFrom,
  mem,
  ops::Deref,
  vec::Vec,
};
use bytecursor::ByteCursor;
use sp_cid::Cid;

/// # Errors
///
/// Will return `Err` if the cursor position exceeds maximum possible vector
/// length or we failed to write whole buffer
pub fn write_null(w: &mut ByteCursor) -> Result<(), String> {
  w.write_all(&[0xf6])?;
  Ok(())
}

/// # Errors
///
/// Will return `Err` if the cursor position exceeds maximum possible vector
/// length or we failed to write whole buffer
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

/// # Errors
///
/// Will return `Err` if the cursor position exceeds maximum possible vector
/// length or we failed to write whole buffer
pub fn write_u16(
  w: &mut ByteCursor,
  major: u8,
  value: u16,
) -> Result<(), String> {
  if let Ok(small) = u8::try_from(value) {
    write_u8(w, major, small)?;
  }
  else {
    let mut buf = [major << 5 | 25, 0, 0];
    BigEndian::write_u16(&mut buf[1..], value);
    w.write_all(&buf)?;
  }
  Ok(())
}

/// # Errors
///
/// Will return `Err` if the cursor position exceeds maximum possible vector
/// length or we failed to write whole buffer
pub fn write_u32(
  w: &mut ByteCursor,
  major: u8,
  value: u32,
) -> Result<(), String> {
  if let Ok(small) = u16::try_from(value) {
    write_u16(w, major, small)?;
  }
  else {
    let mut buf = [major << 5 | 26, 0, 0, 0, 0];
    BigEndian::write_u32(&mut buf[1..], value);
    w.write_all(&buf)?;
  }
  Ok(())
}

/// # Errors
///
/// Will return `Err` if the cursor position exceeds maximum possible vector
/// length or we failed to write whole buffer
pub fn write_u64(
  w: &mut ByteCursor,
  major: u8,
  value: u64,
) -> Result<(), String> {
  if let Ok(small) = u32::try_from(value) {
    write_u32(w, major, small)?;
  }
  else {
    let mut buf = [major << 5 | 27, 0, 0, 0, 0, 0, 0, 0, 0];
    BigEndian::write_u64(&mut buf[1..], value);
    w.write_all(&buf)?;
  }
  Ok(())
}

/// # Errors
///
/// Will return `Err` if the cursor position exceeds maximum possible vector
/// length or we failed to write whole buffer
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
    write_u8(w, 1, -(*self + 1) as u8) // may lose sign
  }
}
impl Encode<DagCborCodec> for i16 {
  fn encode(&self, _: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    write_u16(w, 1, -(*self + 1) as u16) // may lose sign
  }
}
impl Encode<DagCborCodec> for i32 {
  fn encode(&self, _: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    write_u32(w, 1, -(*self + 1) as u32) // may lose sign
  }
}
impl Encode<DagCborCodec> for i64 {
  fn encode(&self, _: DagCborCodec, w: &mut ByteCursor) -> Result<(), String> {
    write_u64(w, 1, -(*self + 1) as u64) // may lose sign
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
    if !self.is_finite() || Self::from(*self as f32) == *self {
      // conversion to `f32` is lossless
      let value = *self as f32;
      value.encode(c, w)?;
    }
    else {
      // conversion to `f32` is lossy
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
        return Err("Number out of range.".to_owned());
      }
      write_u64(w, 1, -(*self + 1) as u64)?;
    }
    else {
      if *self > u64::max_value() as i128 {
        return Err("Number out of range.".to_owned());
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
    let mut vec: Vec<_> = self.iter().collect();
    vec.sort_unstable_by(|&(k1, _), &(k2, _)| {
      let mut bc1 = ByteCursor::new(Vec::new());
      mem::drop(k1.encode(c, &mut bc1));
      let mut bc2 = ByteCursor::new(Vec::new());
      mem::drop(k2.encode(c, &mut bc2));
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
