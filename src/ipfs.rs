use reqwest::{self, multipart};
use libipld::{
  cbor::DagCborCodec,
  codec::Codec,
  ipld::Ipld,
};
use crate::file::store::{cid};
use serde_json;


pub async fn dag_put(dag: Ipld) -> Result<String, reqwest::Error> {
    let host = "http://127.0.0.1:5001";
    let url = format!("{}{}?{}", host, "/api/v0/dag/put", "format=cbor&pin=true&input-enc=cbor&hash=blake2b-256");
    let cbor = DagCborCodec.encode(&dag).unwrap();
    let client = reqwest::Client::new();
    let form = multipart::Form::new()
      .part("file", multipart::Part::bytes(cbor));
    let response: serde_json::Value = client
        .post(url)
        .multipart(form)
        .send()
        .await?
        .json()
        .await?;

    let ipfs_cid: String = response["Cid"]["/"].as_str().unwrap().to_string();
    let local_cid: String = cid(&dag).to_string();

    if ipfs_cid == local_cid {
      Ok(ipfs_cid)
    } else {
      panic!("CIDs are different {} != {}", ipfs_cid, local_cid);
    }
}

pub async fn dag_get(cid: String) -> Result<Ipld, reqwest::Error> {
    let host = "http://127.0.0.1:5001";
    let url = format!("{}{}?arg={}", host, "/api/v0/block/get", cid);
    let client = reqwest::Client::new();
    let response = client
        .get(url)
        .send()
        .await?
        .bytes()
        .await?;
    let ipld = DagCborCodec.decode(&response).expect("invalid ipld cbor.");

    Ok(ipld)
}

mod tests {
  use super::dag_get;

  #[tokio::test]
  async fn test_get_boolya() {
    let cid_val = "bafyreibw2xnwsgg2t27zym4guigko5neeostr6a5a2i6i3v3cl3mv6d5n4";
    let _ipld = dag_get(cid_val.to_string()).await.unwrap();
  }
}
