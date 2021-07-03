use bytecursor::ByteCursor;
use reqwest::{
  self,
  multipart,
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

pub async fn dag_put(dag: Ipld) -> Result<String, reqwest::Error> {
  let host = "http://127.0.0.1:5001";
  let url = format!(
    "{}{}?{}",
    host, "/api/v0/dag/put", "format=cbor&pin=true&input-enc=cbor&hash=blake2b-256"
  );
  let cbor = DagCborCodec.encode(&dag).unwrap().into_inner();
  let client = reqwest::Client::new();
  let form = multipart::Form::new().part("file", multipart::Part::bytes(cbor));
  let response: serde_json::Value = client.post(url).multipart(form).send().await?.json().await?;

  let ipfs_cid: String = response["Cid"]["/"].as_str().unwrap().to_string();
  let local_cid: String = cid(&dag).to_string();

  if ipfs_cid == local_cid {
    Ok(ipfs_cid)
  }
  else {
    panic!("CIDs are different {} != {}", ipfs_cid, local_cid);
  }
}

pub async fn dag_get(cid: String) -> Result<Ipld, reqwest::Error> {
  let host = "http://127.0.0.1:5001";
  let url = format!("{}{}?arg={}", host, "/api/v0/block/get", cid);
  let client = reqwest::Client::new();
  let response = client.post(url).send().await?.bytes().await?;
  let response = response.to_vec();
  println!("response: {:?}", response);
  let ipld = DagCborCodec.decode(ByteCursor::new(response)).expect("invalid ipld cbor.");

  Ok(ipld)
}

#[cfg(test)]
mod tests {
  use std::{
    rc::Rc,
    path::PathBuf,
  };
  use crate::{
    file::store::{
      FileStore,
      FileStoreOpts
    },
  };
  use yatima_utils::{
      file::parse::{
          parse_text,
          PackageEnv,
      }
  };
  #[ignore]
  #[tokio::test(flavor = "multi_thread", worker_threads = 1)]
  async fn test_get_boolya() {
    let src = "
    package bool where

    def Bool: Type = #Bool

    def Bool.True: Bool = #Bool.true
    def Bool.False: Bool = #Bool.false

    def Bool.eql: ∀ (x y: Bool) -> Bool = #Bool.eql
    def Bool.lte: ∀ (x y: Bool) -> Bool = #Bool.lte
    def Bool.lth: ∀ (x y: Bool) -> Bool = #Bool.lth
    def Bool.gte: ∀ (x y: Bool) -> Bool = #Bool.gte
    def Bool.gth: ∀ (x y: Bool) -> Bool = #Bool.gth


    def Bool.and: ∀ (x y: Bool) -> Bool = #Bool.and
    def Bool.or:  ∀ (x y: Bool) -> Bool = #Bool.or
    def Bool.xor: ∀ (x y: Bool) -> Bool = #Bool.xor

    def Bool.not: ∀ (x: Bool) -> Bool = #Bool.not

    def Bool.neq (x y: Bool): Bool = Bool.not (Bool.eql x y)

    def Bool.if (A: Type) (bool : Bool) (t f: A): A = (case bool) (λ _ => A) t f
    ";
    let root = std::env::current_dir().unwrap();
    let path = PathBuf::from("bool.ya");
    let store = Rc::new(FileStore::new(FileStoreOpts { use_ipfs_daemon: true, use_file_store: true, root: root.clone() }));
    let env = PackageEnv::new(root, path, store);
    let (cid, p, _defs) = parse_text(src, env).unwrap();

    let ipld = super::dag_get(cid.to_string()).await.unwrap();
    assert_eq!(ipld, p.to_ipld());
  }
}
