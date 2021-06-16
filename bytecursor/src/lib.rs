#![no_std]

extern crate alloc;

pub mod bytecursor;
pub use bytecursor::ByteCursor;
pub use bytecursor::SeekFrom;
