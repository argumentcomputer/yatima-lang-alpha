use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::JsFuture;
use web_sys::{Request, RequestInit, RequestMode, Response};
use crate::{
  hashspace,
  parse,
};

// Import the `window.alert` function from the Web.
#[wasm_bindgen]
extern "C" {
  fn alert(s: &str);
}

#[wasm_bindgen(start)]
pub fn main() -> Result<(), JsValue> {
    // Use `web_sys`'s global `window` function to get a handle on the global
    // window object.
    // let window = web_sys::window().expect("no global `window` exists");
    // let document = window.document().expect("should have a document on window");
    // let body = document.body().expect("document should have a body");

    // Manufacture the element we're gonna append
    // let val = document.create_element("p")?;
    // val.set_inner_html("Hello from Rust!");

    // body.append_child(&val)?;

    Ok(())
}

#[wasm_bindgen]
pub async fn hashspace_get(hash: String) -> Result<JsValue, JsValue> {
    let mut opts = RequestInit::new();
    opts.method("GET");
    opts.mode(RequestMode::Cors);

    let url = format!("http://localhost:8000/store/{}", hash);

    let request = Request::new_with_str_and_init(&url, &opts)?;

    // request
    //     .headers()
    //     .set("Accept", "application/vnd.github.v3+json")?;

    let window = web_sys::window().unwrap();
    let resp_value = JsFuture::from(window.fetch_with_request(&request)).await?;

    // // `resp_value` is a `Response` object.
    // assert!(resp_value.is_instance_of::<Response>());
    let resp: Response = resp_value.dyn_into().unwrap();

    // Convert this other `Promise` into a rust `Future`.
    let text = JsFuture::from(resp.text()?).await?;

    // // Use serde to parse the JSON into a struct.
    // let branch_info: Branch = json.into_serde().unwrap();

    // // Send the `Branch` struct back to JS as an `Object`.
    // Ok(JsValue::from_serde(&branch_info).unwrap())
    Ok(text)
}

#[wasm_bindgen]
pub async fn hashspace_put(data: Vec<u8>) -> Result<JsValue, JsValue> {
    let mut opts = RequestInit::new();
    opts.method("PUT");
    opts.mode(RequestMode::Cors);
    opts.body(Some(&JsValue::from_serde(&data).unwrap()));

    let url = format!("http://localhost:8000/store");

    let request = Request::new_with_str_and_init(&url, &opts)?;

    let window = web_sys::window().unwrap();
    let resp_value = JsFuture::from(window.fetch_with_request(&request)).await?;

    let resp: Response = resp_value.dyn_into().unwrap();
    let text = JsFuture::from(resp.text()?).await?;
    Ok(text)
}

#[wasm_bindgen]
pub fn parse_source(source: &str) {
  alert(&format!("Parsing:\n{}", source));
  let hashspace = hashspace::Hashspace::with_hosts(vec!["localhost:8000".to_string()]);
  let (_, p, ..) = parse::package::parse_text(&source, None, &hashspace);
  alert(&format!("Package parsed:\n{}", hashspace::HashspaceImplWrapper::wrap(&hashspace, &p)));

  // Ok(JsValue::from_serde(&p.to_string()).unwrap())
}
