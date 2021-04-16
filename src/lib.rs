#![feature(proc_macro_hygiene, decl_macro, const_raw_ptr_deref, const_mut_refs)]

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;
#[cfg(test)]
extern crate rand;

#[cfg(not(target_arch = "wasm32"))]
#[macro_use]
extern crate rocket;

#[cfg(not(target_arch = "wasm32"))]
#[macro_use]
extern crate log;

#[macro_use]
extern crate hashexpr;

pub mod anon_term;
pub mod core;
pub mod decode_error;
pub mod definition;
pub mod hashspace;
pub mod meta_term;
pub mod package;
pub mod parse;
pub mod repl;
pub mod term;
pub mod unembed_error;
pub mod utils;
#[cfg(target_arch = "wasm32")]
#[cfg(not(target = "wasm32-wasi"))]
pub mod wasm_binds;
