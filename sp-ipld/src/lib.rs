//#![no_std]
extern crate alloc;
extern crate sp_std;

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;
#[cfg(test)]
extern crate rand;

pub mod codec;
pub mod dag_cbor;
pub mod ipld;

pub use codec::*;
pub use ipld::*;

#[cfg(test)]
pub mod tests {
  use super::{
    codec::*,
    dag_cbor::*,
    ipld::*,
  };
  use bytecursor::ByteCursor;
  use quickcheck::{
    quickcheck,
    Arbitrary,
    Gen,
  };
  use reqwest::multipart;
  use tokio::runtime::Runtime;

  pub async fn dag_put(dag: Ipld) -> Result<String, reqwest::Error> {
    let host = "http://127.0.0.1:5001";
    let url = format!(
      "{}{}?{}",
      host,
      "/api/v0/dag/put",
      "format=cbor&pin=true&input-enc=cbor&hash=blake2b-256"
    );
    let cbor = DagCborCodec.encode(&dag).unwrap().into_inner();
    let client = reqwest::Client::new();
    let form =
      multipart::Form::new().part("file", multipart::Part::bytes(cbor));
    let response: serde_json::Value =
      client.post(url).multipart(form).send().await?.json().await?;
    println!("response: {:?}", response);

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
    let url =
      format!("{}{}?arg={}", host, "/api/v0/block/get", cid.to_string());
    let client = reqwest::Client::new();
    let response = client.post(url).send().await?.bytes().await?;
    let response = response.to_vec();
    println!("response: {:?}", response);
    let ipld = DagCborCodec
      .decode(ByteCursor::new(response))
      .expect("invalid ipld cbor.");
    println!("ipld: {:?}", ipld);

    Ok(ipld)
  }
  async fn async_ipld_ipfs(ipld: Ipld) -> bool {
    match dag_put(ipld.clone()).await {
      Ok(cid) => match dag_get(cid.clone()).await {
        Ok(new_ipld) => {
          if ipld.clone() == new_ipld.clone() {
            true
          }
          else {
            eprintln!("Cid: {}", cid);
            eprintln!("Encoded ipld: {:?}", ipld);
            eprintln!("Decoded ipld: {:?}", new_ipld);
            false
          }
        }
        Err(e) => {
          eprintln!("Error during `dag_get`: {}", e);
          false
        }
      },
      Err(e) => {
        eprintln!("Error during `dag_put`: {}", e);
        false
      }
    }
  }
  fn ipld_ipfs(ipld: Ipld) -> bool {
    match Runtime::new() {
      Ok(runtime) => runtime.block_on(async_ipld_ipfs(ipld)),
      Err(e) => {
        eprintln!("Error creating runtime: {}", e);
        false
      }
    }
  }

  #[ignore]
  #[quickcheck]
  fn null_ipfs() -> bool { ipld_ipfs(Ipld::Null) }

  #[ignore]
  #[quickcheck]
  fn bool_ipfs(b: bool) -> bool { ipld_ipfs(Ipld::Bool(b)) }

  #[ignore]
  #[quickcheck]
  fn string_ipfs(x: String) -> bool { ipld_ipfs(Ipld::String(x)) }

  use crate::ipld::tests::arbitrary_i128;

  #[derive(Debug, Clone)]
  struct AInt(pub i128);
  impl Arbitrary for AInt {
    fn arbitrary(g: &mut Gen) -> Self { AInt(arbitrary_i128()(g)) }
  }

  #[ignore]
  #[test]
  fn integers_ipfs() {
    assert!(ipld_ipfs(Ipld::Integer(0i128)));
    assert!(ipld_ipfs(Ipld::Integer(1i128)));
    assert!(ipld_ipfs(Ipld::Integer(u64::MAX as i128)));
    assert!(ipld_ipfs(Ipld::Integer(i64::MAX as i128 + 1)));
  }

  #[ignore]
  #[quickcheck]
  fn integer_ipfs(x: AInt) -> bool { ipld_ipfs(Ipld::Integer(x.0)) }

  #[ignore]
  #[quickcheck]
  fn ipfs(x: Ipld) -> bool { ipld_ipfs(x) }
}
