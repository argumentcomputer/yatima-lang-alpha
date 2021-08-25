pub mod repl;
pub mod store;
pub mod terminal_sequences;
#[macro_use]
pub mod utils;
pub mod runtime;
pub mod ipfs;

#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
