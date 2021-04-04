#[cfg(target_arch = "wasm32")]
use web_sys::{console};
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::{JsValue};
#[cfg(not(target_arch = "wasm32"))]
use std::io::{self,Write};

#[cfg(target_arch = "wasm32")]
pub fn log(message: &str) {
  console::log_1(&JsValue::from_str(message));
}

#[cfg(not(target_arch = "wasm32"))]
pub fn log(message: &str) -> io::Result<()> {
  io::stdout().write_all(message.as_bytes())
}
