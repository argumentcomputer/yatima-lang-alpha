#[cfg(all(target_arch = "wasm32", not(os = "wasi"), feature = "js-bindings"))]
use web_sys::{console};
#[cfg(all(target_arch = "wasm32", not(os = "wasi"), not(os = "wasi"), feature = "js-bindings"))]
use wasm_bindgen::prelude::{JsValue};
use std::io::{self,Write};

#[cfg(all(target_arch = "wasm32", not(os = "wasi"), feature = "js-bindings"))]
pub fn log(message: &str) {
  console::log_1(&JsValue::from_str(message));
}

#[cfg(feature = "wasp")]
pub fn log(_message: &str) {
  // not in use
}

#[cfg(not(target_arch = "wasm32"))]
pub fn log(message: &str) {
  io::stdout().write_all(message.as_bytes()).unwrap();
}
