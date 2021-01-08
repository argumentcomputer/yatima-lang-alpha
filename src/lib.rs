#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;
#[cfg(test)]
extern crate rand;

#[macro_use]
extern crate hashexpr;

pub mod dag;
pub mod decode_error;
pub mod defs;
pub mod imports;
pub mod package;
pub mod repl;
pub mod term;
