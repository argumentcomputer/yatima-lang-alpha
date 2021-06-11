pub mod cid;
pub mod error;
pub mod version;

pub use cid::Cid as CidGeneric;
pub use error::{Error, Result};
pub use version::Version;

/// A Cid that contains a multihash with an allocated size of 512 bits.
///
/// This is the same digest size the default multihash code table has.
///
/// If you need a CID that is generic over its digest size, use [`CidGeneric`] instead.
pub type Cid = CidGeneric<multihash::U64>;
