use reqwest;
use libipld::{
  cbor::DagCborCodec,
  codec::Codec,
  ipld::Ipld,
};
use crate::file::store::{cid};


pub async fn dag_put(dag: Ipld) -> Result<(), reqwest::Error> {
    let host = "http://localhost:8080";
    let url = format!("{}{}", host, "/api/v0/dag/put");
    let link = cid(&dag);
    let cbor = DagCborCodec.encode(&dag).unwrap();
    let client = reqwest::Client::new();
    let response = client
        .post(url)
        .header("Accept", "application/cbor")
        .body(cbor)
        .send()
        .await?;
    println!("{:#?}", response);

    Ok(())
}

// pub async fn dag_get(dag: Ipld) -> Result<Ipld, reqwest::Error> {
//     let host = "http://localhost:8080";
//     let url = format!("{}{}", host, "/api/v0/dag/put");
//     let client = reqwest::Client::new();
//     let response = client
//         .get()
//         .await?;

//     response
// }
