#[cfg(all(target_arch = "wasm32", not(os = "wasi")))]
use web_sys::{console};
#[cfg(all(target_arch = "wasm32", not(os = "wasi")))]
use wasm_bindgen::prelude::{JsValue};
use std::io::{self,Write};

#[cfg(all(target_arch = "wasm32", not(os = "wasi")))]
pub fn log(message: &str) {
  console::log_1(&JsValue::from_str(message));
}

#[cfg(not(target_arch = "wasm32"))]
pub fn log(message: &str) {
  io::stdout().write_all(message.as_bytes()).unwrap();
}
