use bytecursor::ByteCursor;
use sp_cid::Cid;

use sp_std::{
  convert::TryFrom,
  ops::Deref,
  vec::Vec,
};

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
  /// # Errors
  ///
  /// Will return `Err` if there was a problem encoding the object into a
  /// `ByteCursor`
  fn encode<T: Encode<Self> + ?Sized>(
    &self,
    obj: &T,
  ) -> Result<ByteCursor, String> {
    let mut buf = ByteCursor::new(Vec::with_capacity(u16::MAX as usize));
    obj.encode(*self, &mut buf)?;
    Ok(buf)
  }

  /// # Errors
  ///
  /// Will return `Err` if there was a problem decoding the `ByteCursor` into an
  /// object
  fn decode<T: Decode<Self>>(
    &self,
    mut bytes: ByteCursor,
  ) -> Result<T, String> {
    T::decode(*self, &mut bytes)
  }

  /// # Errors
  ///
  /// TODO
  fn references<T: References<Self>, E: Extend<Cid>>(
    &self,
    mut bytes: ByteCursor,
    set: &mut E,
  ) -> Result<(), String> {
    T::references(*self, &mut bytes, set)
  }
}

pub trait Encode<C: Codec> {
  /// # Errors
  ///
  /// Will return `Err` if there was a problem during encoding
  fn encode(&self, c: C, w: &mut ByteCursor) -> Result<(), String>;
}

impl<C: Codec, T: Encode<C>> Encode<C> for &T {
  fn encode(&self, c: C, w: &mut ByteCursor) -> Result<(), String> {
    self.deref().encode(c, w)
  }
}

pub trait Decode<C: Codec>: Sized {
  /// # Errors
  ///
  /// Will return `Err` if there was a problem during decoding
  fn decode(c: C, r: &mut ByteCursor) -> Result<Self, String>;
}

pub trait References<C: Codec>: Sized {
  /// # Errors
  ///
  /// TODO
  fn references<E: Extend<Cid>>(
    c: C,
    r: &mut ByteCursor,
    set: &mut E,
  ) -> Result<(), String>;
}

pub trait SkipOne: Codec {
  /// # Errors
  ///
  /// Will return `Err` if there was a problem during skipping
  fn skip(&self, r: &mut ByteCursor) -> Result<(), String>;
}
