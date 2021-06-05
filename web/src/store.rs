use libipld::{
  cid::Cid,
  cbor::DagCborCodec,
  codec::Codec,
  ipld::Ipld,
};
use crate::{
  utils::log,
};
use yatima_core::cid::cid;
use yatima_utils::{
  store::{Store},
};
use web_sys::{
  self,
  Window,
  Storage,
};
use base64;
use wasm_bindgen::prelude::*;

#[derive(Debug, Clone)]
pub struct WebStore {
  window: Window,
  storage: Storage,
  ipfs: Ipfs,
}

#[wasm_bindgen(module = "ipfs-core")]
extern "C" {
  #[wasm_bindgen(js_name = "Ipfs")]
  #[derive(Debug, Clone)]
  pub type Ipfs;

  #[wasm_bindgen]
  pub fn create() -> Ipfs;

  #[wasm_bindgen(method, js_name = "add")]
  pub fn add(this: &Ipfs, data: Vec<u8>);

  #[wasm_bindgen(method, js_name = "get")]
  pub fn get(this: &Ipfs, link: &str);
}

impl WebStore {
  pub fn new() -> Self {
    let window = web_sys::window().expect("should have a window in this context");
    let storage = window.local_storage().expect("should have local storage").unwrap();
    let ipfs = create().into();

    WebStore {
      window,
      storage,
      ipfs,
    }
  }
}

impl Store for WebStore {
  fn get(&self, link: Cid) -> Option<Ipld> {

    match self.storage.get(&link.to_string()) {
      Ok(Some(s)) => {
        let bin = base64::decode(s).expect("invalid base64");
        Some(DagCborCodec.decode(&bin).expect("invalid cbor bytes"))
      },
      _ => {
        self.ipfs.get(&link.to_string());
        log(&format!("Failed to get {}", link));
        None
      }
    }
  }

  fn put(&self, expr: Ipld) -> Cid {
    let link = cid(&expr);
    let data = DagCborCodec.encode(&expr).unwrap();
    match self.storage.set(&link.to_string(), &base64::encode(data.clone())) {
      Ok(()) => (),
      Err(_) => log("Failed to put to local_storage"),
    }
    self.ipfs.add(data);

    link
  }
}

