#![feature(proc_macro_hygiene, decl_macro)]

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
extern crate yatima_core;

// pub mod hashspace;
// pub mod parse;
// pub mod repl;
// pub mod utils;
//#[cfg(target_arch = "wasm32")]
//#[cfg(not(target = "wasm32-wasi"))]
// pub mod wasm_binds;
