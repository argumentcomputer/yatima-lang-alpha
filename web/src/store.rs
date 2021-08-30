use crate::ipfs::{
  self,
  IpfsApiConfig,
};
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
use web_sys::{
  self,
  Storage,
  Window,
};
use yatima_core::parse::parse_cid;
use yatima_utils::{
  log,
  debug,
  store::Store,
};

#[derive(Debug, Clone)]
pub struct WebStore {
  window: Window,
  storage: Storage,
  // ipfs: Ipfs,
  api_config: IpfsApiConfig,
}

// #[wasm_bindgen(module = "ipfs-core")]
// extern "C" {
//   #[wasm_bindgen(js_name = "Ipfs")]
//   #[derive(Debug, Clone)]
//   pub type Ipfs;

//   #[wasm_bindgen]
//   pub fn create() -> Ipfs;

//   #[wasm_bindgen(method, js_name = "add")]
//   pub fn add(this: &Ipfs, data: Vec<u8>) -> JsValue;

//   #[wasm_bindgen(method, js_name = "get")]
//   pub fn get(this: &Ipfs, link: &str) -> JsValue;
// }

impl WebStore {
  pub fn new() -> Self {
    let window = web_sys::window().expect("should have a window in this context");
    let storage = window.local_storage().expect("should have local storage").unwrap();
    // let ipfs = create().into();
    let api_config = IpfsApiConfig::local_daemon();

    WebStore { window, storage, api_config }
  }
}

impl Store for WebStore {
  fn get_by_multiaddr(&self, _addr: Multiaddr) -> Result<Ipld, String> {
    // let s = ipfs::dag_get(&self.api_config, addr.to_string())
    //   .map_err(|e| format!("Failed to load multiaddr {}: {}", addr, e))?;
    // log!("{:?}", s);

    Err("Not implemented".to_owned())
  }

  fn load_by_name(&self, _path: Vec<&str>) -> Result<Ipld, String> {
    Err("Not implemented".to_owned())
  }

  fn load_by_name_with_callback(&self, path: Vec<&str>, callback: Box<dyn FnOnce(Ipld)>) {
    debug!("load_by_name: {:?}", path);
    if let Some(Ok(link)) = path.last().map(|s| parse_cid(s)) {
      ipfs::dag_get(
        &self.api_config,
        link.to_string(),
        Box::new(|result| match result {
          Ok(ipld) => callback(ipld),
          Err(e) => log!("dag_get: {:?}", e),
        }),
      );
    }
    else {
      log!("Only CIDs supported");
    }
  }

  fn get(&self, link: Cid) -> Option<Ipld> {
    match self.storage.get(&link.to_string()) {
      Ok(Some(s)) => {
        let bin = ByteCursor::new(base64::decode(s).expect("invalid base64"));
        Some(DagCborCodec.decode(bin).expect("invalid cbor bytes"))
      }
      _ => {
        // ipfs::dag_get(
        //   &self.api_config,
        //   link.to_string(),
        //   Box::new(|result| result.map_err(|e| log!("dag_get: {:?}", e)).ok()?),
        // );
        // let res = self.ipfs.get(&link.to_string());
        // log!("Failed to get {} {:?}", &link, res);
        None
      }
    }
  }

  fn needs_callback(&self) -> bool { true }

  /// Necessary to circumvent the async limitations of wasm
  fn get_with_callback(&self, link: Cid, callback: Box<dyn FnOnce(Ipld)>) {
    match self.storage.get(&link.to_string()) {
      Ok(Some(s)) => {
        let bin = ByteCursor::new(base64::decode(s).expect("invalid base64"));
        callback(DagCborCodec.decode(bin).expect("invalid cbor bytes"))
      }
      _ => {
        ipfs::dag_get(
          &self.api_config,
          link.to_string(),
          Box::new(|result| match result {
            Ok(ipld) => callback(ipld),
            Err(e) => log!("dag_get: {:?}", e),
          }),
        );
      }
    }
  }

  fn put(&self, expr: Ipld) -> Cid {
    let link = cid(&expr);
    let data = DagCborCodec.encode(&expr).unwrap();
    match self.storage.set(&link.to_string(), &base64::encode(data.clone().into_inner())) {
      Ok(()) => (),
      Err(_) => log!("Failed to put to local_storage"),
    }
    // let res = self.ipfs.add(data.into_inner());
    // log!("{:?}", res);

    link
  }
}
