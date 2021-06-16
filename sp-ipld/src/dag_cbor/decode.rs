use crate::{
  codec::{
    Decode,
    References,
    SkipOne,
  },
  dag_cbor::DagCborCodec,
  ipld::Ipld,
};

use alloc::{
  borrow::ToOwned,
  string::String,
  sync::Arc,
  vec,
};
use byteorder::{
  BigEndian,
  ByteOrder,
};

use sp_std::{
  any::type_name,
  collections::btree_map::BTreeMap,
  vec::Vec,
  convert::TryFrom,
};

use bytecursor::{
  ByteCursor,
  SeekFrom,
};
use sp_cid::Cid;

/// # Errors
///
/// Will return `Err` if the `ByteCursor` has less than 1 available bytes to
/// read
pub fn read_u8(r: &mut ByteCursor) -> Result<u8, String> {
  let mut buf = [0; 1];
  r.read_exact(&mut buf)?;
  Ok(buf[0])
}

/// # Errors
///
/// Will return `Err` if the `ByteCursor` has less than 2 available bytes to
/// read
pub fn read_u16(r: &mut ByteCursor) -> Result<u16, String> {
  let mut buf = [0; 2];
  r.read_exact(&mut buf)?;
  Ok(BigEndian::read_u16(&buf))
}

/// # Errors
///
/// Will return `Err` if the `ByteCursor` has less than 4 available bytes to
/// read
pub fn read_u32(r: &mut ByteCursor) -> Result<u32, String> {
  let mut buf = [0; 4];
  r.read_exact(&mut buf)?;
  Ok(BigEndian::read_u32(&buf))
}

/// # Errors
///
/// Will return `Err` if the `ByteCursor` has less than 8 available bytes to
/// read
pub fn read_u64(r: &mut ByteCursor) -> Result<u64, String> {
  let mut buf = [0; 8];
  r.read_exact(&mut buf)?;
  Ok(BigEndian::read_u64(&buf))
}

/// # Errors
///
/// Will return `Err` if the `ByteCursor` has less than 4 available bytes to
/// read
pub fn read_f32(r: &mut ByteCursor) -> Result<f32, String> {
  let mut buf = [0; 4];
  r.read_exact(&mut buf)?;
  Ok(BigEndian::read_f32(&buf))
}

/// # Errors
///
/// Will return `Err` if the `ByteCursor` has less than 8 available bytes to
/// read
pub fn read_f64(r: &mut ByteCursor) -> Result<f64, String> {
  let mut buf = [0; 8];
  r.read_exact(&mut buf)?;
  Ok(BigEndian::read_f64(&buf))
}

/// # Errors
///
/// Will return `Err` if the `ByteCursor` has less than `len` available bytes
/// to read
pub fn read_bytes(r: &mut ByteCursor, len: usize) -> Result<Vec<u8>, String> {
  let mut buf = vec![0; len];
  r.read_exact(&mut buf)?;
  Ok(buf)
}

/// # Errors
///
/// Will return `Err` if the `ByteCursor` has less than `len` available bytes
/// to read or the bytes read are not valid UTF-8
pub fn read_str(r: &mut ByteCursor, len: usize) -> Result<String, String> {
  let bytes = read_bytes(r, len)?;
  String::from_utf8(bytes).map_err(|_| "Error converting to UTF-8".to_owned())
}

/// Will return `Err` if there were any errors decoding `len` objects
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

/// # Errors
///
/// Will return `Err` if there were errors reading the major value, seeking
/// back, or decoding the component objects
pub fn read_list_il<T: Decode<DagCborCodec>>(
  r: &mut ByteCursor,
) -> Result<Vec<T>, String> {
  let mut list: Vec<T> = Vec::new();
  loop {
    let major = read_u8(r)?;
    if major == 0xff {
      break;
    }
    r.seek(&SeekFrom::Current(-1))?;
    let value = T::decode(DagCborCodec, r)?;
    list.push(value);
  }
  Ok(list)
}

/// # Errors
///
/// Will return `Err` if there were any errors decoding `len` key-value pairs
/// of objects
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

/// # Errors
///
/// Will return `Err` if there was an error reading the major value, seeking
/// backward, or decoding the component key-value pairs of objects
pub fn read_map_il<K: Decode<DagCborCodec> + Ord, T: Decode<DagCborCodec>>(
  r: &mut ByteCursor,
) -> Result<BTreeMap<K, T>, String> {
  let mut map: BTreeMap<K, T> = BTreeMap::new();
  loop {
    let major = read_u8(r)?;
    if major == 0xff {
      break;
    }
    r.seek(&SeekFrom::Current(-1))?;
    let key = K::decode(DagCborCodec, r)?;
    let value = T::decode(DagCborCodec, r)?;
    map.insert(key, value);
  }
  Ok(map)
}

/// # Errors
///
/// Will return `Err` if the `ByteCursor` is not long enough, the cbor tag is
/// not `0x58`, the len is `0`, `bytes[0]` is not `0`, or if the bytes are not
/// a valid Cid
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
  Cid::try_from(&bytes[1..]).map_err(|x| x.to_string())
}

/// # Errors
///
/// Will return `Err` if the major value is unknown or decoding a usize which
/// is greater than `u64::MAX`
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
      len as usize // may truncate
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
      0x00..=0x17 => Self::from(major),
      0x18 => Self::from(read_u8(r)?),
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
      0x00..=0x17 => Self::from(major),
      0x18 => Self::from(read_u8(r)?),
      0x19 => Self::from(read_u16(r)?),
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
      0x00..=0x17 => Self::from(major),
      0x18 => Self::from(read_u8(r)?),
      0x19 => Self::from(read_u16(r)?),
      0x1a => Self::from(read_u32(r)?),
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
      0x20..=0x37 => -1 - (major - 0x20) as Self, // may wrap
      0x38 => -1 - read_u8(r)? as Self,           // may wrap
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
      0x20..=0x37 => -1 - Self::from(major - 0x20),
      0x38 => -1 - Self::from(read_u8(r)?),
      0x39 => -1 - read_u16(r)? as Self, // may wrap
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
      0x20..=0x37 => -1 - Self::from(major - 0x20),
      0x38 => -1 - Self::from(read_u8(r)?),
      0x39 => -1 - Self::from(read_u16(r)?),
      0x3a => -1 - read_u32(r)? as Self, // may wrap
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
      0x20..=0x37 => -1 - Self::from(major - 0x20),
      0x38 => -1 - Self::from(read_u8(r)?),
      0x39 => -1 - Self::from(read_u16(r)?),
      0x3a => -1 - Self::from(read_u32(r)?),
      0x3b => -1 - read_u64(r)? as Self, // may wrap
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
      0xfa => Self::from(read_f32(r)?),
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
      0xf6 | 0xf7 => None,
      _ => {
        r.seek(&SeekFrom::Current(-1))?;
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
      0x00..=0x17 => Self::Integer(i128::from(major)),
      0x18 => Self::Integer(i128::from(read_u8(r)?)),
      0x19 => Self::Integer(i128::from(read_u16(r)?)),
      0x1a => Self::Integer(i128::from(read_u32(r)?)),
      0x1b => Self::Integer(i128::from(read_u64(r)?)),

      // Major type 1: a negative integer
      0x20..=0x37 => Self::Integer(-1 - i128::from(major - 0x20)),
      0x38 => Self::Integer(-1 - i128::from(read_u8(r)?)),
      0x39 => Self::Integer(-1 - i128::from(read_u16(r)?)),
      0x3a => Self::Integer(-1 - i128::from(read_u32(r)?)),
      0x3b => Self::Integer(-1 - i128::from(read_u64(r)?)),

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
        let pos = r.seek(&SeekFrom::Current(0))?;
        r.seek(&SeekFrom::Start(pos))?;
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
      0xf6 | 0xf7 => Self::Null,
      0xfa => Self::Float(f64::from(read_f32(r)?)),
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
      0x00..=0x17 | 0x20..=0x37 | 0xf4..=0xf7 => {}

      0x18 | 0x38 | 0xf8 => {
        r.seek(&SeekFrom::Current(1))?;
      }
      0x19 | 0x39 | 0xf9 => {
        r.seek(&SeekFrom::Current(2))?;
      }
      0x1a | 0x3a | 0xfa => {
        r.seek(&SeekFrom::Current(4))?;
      }
      0x1b | 0x3b | 0xfb => {
        r.seek(&SeekFrom::Current(8))?;
      }

      // Major type 2: a byte string
      0x40..=0x5b => {
        let len = read_len(r, major - 0x40)?;
        r.seek(&SeekFrom::Current(len as _))?;
      }

      // Major type 3: a text string
      0x60..=0x7b => {
        let len = read_len(r, major - 0x60)?;
        r.seek(&SeekFrom::Current(len as _))?;
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
        r.seek(&SeekFrom::Current(-1))?;
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
        r.seek(&SeekFrom::Current(-1))?;
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
    Ok(Self::new(T::decode(c, r)?))
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
      0x00..=0x17 | 0x20..=0x37 | 0xf4..=0xf7 => {}
      0x18 | 0x38 | 0xf8 => {
        r.seek(&SeekFrom::Current(1))?;
      }
      0x19 | 0x39 | 0xf9 => {
        r.seek(&SeekFrom::Current(2))?;
      }
      0x1a | 0x3a | 0xfa => {
        r.seek(&SeekFrom::Current(4))?;
      }
      0x1b | 0x3b | 0xfb => {
        r.seek(&SeekFrom::Current(8))?;
      }

      // Major type 2: a byte string
      0x40..=0x5b => {
        let len = read_len(r, major - 0x40)?;
        r.seek(&SeekFrom::Current(len as _))?;
      }

      // Major type 3: a text string
      0x60..=0x7b => {
        let len = read_len(r, major - 0x60)?;
        r.seek(&SeekFrom::Current(len as _))?;
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
        r.seek(&SeekFrom::Current(-1))?;
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
        r.seek(&SeekFrom::Current(-1))?;
        self.skip(r)?;
        self.skip(r)?;
      },

      // Major type 6: optional semantic tagging of other major types
      0xd8 => {
        let _tag = read_u8(r)?;
        self.skip(r)?;
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
