use base64;
use bytecursor::ByteCursor;
use multiaddr::Multiaddr;
use sp_cid::Cid;
use sp_ipld::{
  dag_cbor::{
    cid,
    DagCborCodec,
  },
  Codec,
  Ipld,
};
use wasm_bindgen::prelude::*;
use web_sys::{
  self,
  Storage,
  Window,
};
use yatima_utils::{
  log,
  store::Store,
};

#[derive(Debug, Clone)]
pub struct WebStore {
  window: Window,
  storage: Storage,
  // ipfs: Ipfs,
}

#[wasm_bindgen(module = "ipfs-core")]
extern "C" {
  #[wasm_bindgen(js_name = "Ipfs")]
  #[derive(Debug, Clone)]
  pub type Ipfs;

  #[wasm_bindgen]
  pub fn create() -> Ipfs;

  #[wasm_bindgen(method, js_name = "add")]
  pub fn add(this: &Ipfs, data: Vec<u8>) -> JsValue;

  #[wasm_bindgen(method, js_name = "get")]
  pub fn get(this: &Ipfs, link: &str) -> JsValue;
}

impl WebStore {
  pub fn new() -> Self {
    let window =
      web_sys::window().expect("should have a window in this context");
    let storage =
      window.local_storage().expect("should have local storage").unwrap();
    // let ipfs = create().into();

    WebStore { window, storage }
  }
}

impl Store for WebStore {
  fn get_by_multiaddr(&self, addr: Multiaddr) -> Result<Ipld, String> {
    // let s = ipfs::dag_get(&addr.to_string())
    //     .ok_or(format!("Failed to load multiaddr {}", addr))?;
    // log!("{:?}", s);

    Err("nn".to_owned())
  }

  fn load_by_name(&self, _path: Vec<&str>) -> Result<Ipld, String> {
    Err("Not implemented".to_owned())
  }

  fn get(&self, link: Cid) -> Option<Ipld> {
    match self.storage.get(&link.to_string()) {
      Ok(Some(s)) => {
        let bin = ByteCursor::new(base64::decode(s).expect("invalid base64"));
        Some(DagCborCodec.decode(bin).expect("invalid cbor bytes"))
      }
      _ => {
        // let res = self.ipfs.get(&link.to_string());
        // log!("Failed to get {} {:?}", link, res);
        None
      }
    }
  }

  fn put(&self, expr: Ipld) -> Cid {
    let link = cid(&expr);
    let data = DagCborCodec.encode(&expr).unwrap();
    match self
      .storage
      .set(&link.to_string(), &base64::encode(data.clone().into_inner()))
    {
      Ok(()) => (),
      Err(_) => log!("Failed to put to local_storage"),
    }
    // let res = self.ipfs.add(data.into_inner());
    // log!("{:?}", res);

    link
  }
}
