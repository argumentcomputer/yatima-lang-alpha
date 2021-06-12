#![no_std]

#[cfg(test)]
extern crate alloc;
#[cfg(test)]
#[macro_use]
extern crate std;

#[macro_use]
pub mod conslist;

pub mod shared;

pub use conslist::ConsList;

#[cfg(test)]
mod test;
