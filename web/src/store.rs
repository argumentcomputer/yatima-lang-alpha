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
use yatima_core::{
  defs::Defs,
  parse::parse_cid,
};
use yatima_utils::{
  debug,
  ipfs::IpfsApi,
  log,
  store::{
    Callback,
    Store,
  },
};

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct WebStore {
  window: Window,
  storage: Storage,
  // ipfs: Ipfs,
  api: IpfsApi,
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
    let api = IpfsApi::local_daemon();

    WebStore { window, storage, api }
  }
}

impl Store for WebStore {
  fn get_by_multiaddr(&self, _addr: Multiaddr) -> Result<Ipld, String> {
    // let s = ipfs::dag_get(&self.api, addr.to_string())
    //   .map_err(|e| format!("Failed to load multiaddr {}: {}", addr, e))?;
    // log!("{:?}", s);

    Err("Not implemented".to_owned())
  }

  fn load_by_name(&self, _path: Vec<&str>) -> Result<Ipld, String> {
    Err("Not implemented".to_owned())
  }

  fn load_by_name_with_callback(&self, path: Vec<&str>, callback: Callback<Ipld, Defs>) {
    debug!("load_by_name: {:?}", path);
    if let Some(Ok(link)) = path.last().map(|s| parse_cid(s)) {
      let monitor = callback.monitor;
      let id = callback.id;
      let monitor_c = monitor.clone();
      let f = callback.f;
      let status = callback.status;
      monitor.lock().unwrap().register_callback(id.clone(), status);
      self.api.dag_get_with_callback(
        link.to_string(),
        Box::new(move |result| {
          match result {
            Ok(ipld) => f(ipld),
            Err(e) => log!("Error dag_get: {:?}", e),
          }

          monitor_c.lock().unwrap().notify(id);
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
        None
      }
    }
  }

  fn needs_callback(&self) -> bool { true }

  /// Necessary to circumvent the async limitations of wasm
  fn get_with_callback(&self, link: Cid, callback: Callback<Ipld, Defs>) {
    match self.storage.get(&link.to_string()) {
      Ok(Some(s)) => {
        let bin = ByteCursor::new(base64::decode(s).expect("invalid base64"));
        let f = callback.f;
        f(DagCborCodec.decode(bin).expect("invalid cbor bytes"))
      }
      _ => {
        let monitor = callback.monitor;
        let id = callback.id;
        let f = callback.f;
        let status = callback.status;
        let monitor_c = monitor.clone();
        monitor.lock().unwrap().register_callback(id.clone(), status);
        self.api.dag_get_with_callback(
          link.to_string(),
          Box::new(move |result| {
            match result {
              Ok(ipld) => f(ipld),
              Err(e) => log!("Error dag_get: {:?}", e),
            }

            monitor_c.lock().unwrap().notify(id);
          }),
        );
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
