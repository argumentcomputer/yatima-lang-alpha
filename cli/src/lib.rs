#![feature(proc_macro_hygiene, decl_macro)]

//#[cfg(test)]
// extern crate quickcheck;
//#[cfg(test)]
//#[macro_use(quickcheck)]
// extern crate quickcheck_macros;
//#[cfg(test)]
// extern crate rand;

//#[macro_use]
extern crate yatima_core;

pub mod ipfs;
pub mod repl;
pub mod file;

pub use yatima_core::name::Name;
