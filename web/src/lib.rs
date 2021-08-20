pub mod repl;
pub mod store;
#[macro_use]
pub mod utils;
pub mod runtime;

#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;
