#![feature(proc_macro_hygiene, decl_macro)]

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;
#[cfg(test)]
extern crate rand;

#[macro_use]
extern crate rocket;

#[macro_use]
extern crate log;

#[macro_use]
extern crate hashexpr;

pub mod anon_term;
pub mod core;
pub mod decode_error;
pub mod definition;
pub mod hashspace;
pub mod imports;
pub mod meta_term;
pub mod package;
pub mod parse;
pub mod repl;
pub mod term;
pub mod unembed_error;
