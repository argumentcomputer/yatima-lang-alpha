use bytecursor::ByteCursor;
use reqwest_wasm::{
  multipart,
  Client,
};
use serde_json;
use sp_ipld::{
  dag_cbor::{
    cid,
    DagCborCodec,
  },
  Codec,
  Ipld,
};
use std::sync::{
  Arc,
  Mutex,
};
use yatima_utils::log;

/// Settings for how to connect to an IPFS Http API
#[derive(Debug, Clone)]
pub struct IpfsApiConfig {
  host: String,
}

impl IpfsApiConfig {
  pub fn ipfs_yatima_io() -> Self { IpfsApiConfig { host: "http://ipfs.yatima.io:5001".to_owned() } }
  pub fn local_daemon() -> Self { IpfsApiConfig { host: "http://localhost:5001".to_owned() } }
}

fn log_err<T, E: std::fmt::Debug>(e: E) -> Result<T, ()> {
  log!("{:?}", e);
  Err(())
}

/// Pin an Ipld using the IPFS API
pub fn dag_put(config: &IpfsApiConfig, dag: Ipld) -> Result<String, String> {
  let url = format!(
    "{}{}?{}",
    config.host, "/api/v0/dag/put", "format=cbor&pin=true&input-enc=cbor&hash=blake2b-256"
  );
  let cbor =
    DagCborCodec.encode(&dag).map_err(|e| format!("encoding error: {:?}", e))?.into_inner();
  let client = Client::new();
  let form = multipart::Form::new().part("file", multipart::Part::bytes(cbor));
  let ptr: Arc<Mutex<serde_json::Value>> = Arc::new(Mutex::new(serde_json::Value::Null));
  let ptr2 = ptr.clone();
  wasm_bindgen_futures::spawn_local(async move {
    let r: serde_json::Value = client
      .post(&url)
      .multipart(form)
      .send()
      .await
      .or_else(log_err)
      .unwrap()
      .json()
      .await
      .unwrap();
    *ptr2.lock().unwrap() = r;
  });

  let response = ptr.lock().unwrap();
  let ipfs_cid: String = response["Cid"]["/"].as_str().unwrap().to_string();
  let local_cid: String = cid(&dag).to_string();

  if ipfs_cid == local_cid {
    Ok(ipfs_cid)
  }
  else {
    Err(format!("CIDs are different {} != {}", ipfs_cid, local_cid))
  }
}

/// Load Ipld from the IPFS API
pub fn dag_get(
  config: &IpfsApiConfig,
  cid: String,
  callback: Box<dyn FnOnce(Result<Ipld, String>)>,
) {
  let url = format!("{}{}?arg={}", config.host, "/api/v0/block/get", cid);
  // let ptr: Arc<Mutex<Vec<u8>>> = Arc::new(Mutex::new(vec![]));
  // let ptr2 = ptr.clone();
  wasm_bindgen_futures::spawn_local(async move {
    let client = Client::new();
    log!("Trying to call IPFS api at {}", url);
    let response = client.post(&url).send().await.or_else(log_err);
    if response.is_err() {
      return;
    }
    let response = response.unwrap().bytes().await.or_else(log_err).unwrap().to_vec();
    log!("response: {:?}", &response);
    // *ptr2.lock().unwrap() = r.into();

    let ipld_res = DagCborCodec
      .decode(ByteCursor::new(response.to_vec()))
      .map_err(|e| format!("Invalid ipld cbor: {}", e));

    callback(ipld_res);
  });
  // let response = ptr.lock().unwrap();
  // log!("response: {:?}", response);
}
