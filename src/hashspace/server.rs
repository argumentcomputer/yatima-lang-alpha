use crate::{
  hashspace,
  utils::log,
};
use hashexpr::Expr;

use rocket::{
  fairing::{
    Fairing,
    Info,
    Kind,
  },
  http::Header,
  Data,
  Request,
  Response,
};
use rocket_contrib::{
  serve::StaticFiles,
  templates::Template,
};
use std::collections::HashMap;

#[get("/")]
fn index() -> Template {
  let context: HashMap<&str, &str> = [("name", "")].iter().cloned().collect();
  Template::render("index", &context)
}

#[get("/store/<hash>")]
fn get(hash: String) -> Option<Vec<u8>> {
  let (_, link) = hashexpr::link::Link::parse(&hash).ok()?;
  info!("Your link {}", link);
  let hashspace = hashspace::Hashspace::local();
  let expr = hashspace.get(link)?;
  Some(expr.serialize())
}

#[put("/store", data = "<data>")]
fn put(data: Data) -> Result<String, std::io::Error> {
  let stream: &[u8] = data.peek();

  let expr = match Expr::deserialize(stream) {
    Ok((_, x)) => x,
    _ => Expr::from_bits(stream.clone().as_ref()),
  };
  let hash = expr.link().to_string();
  let hashspace = hashspace::Hashspace::local();

  let url = format!(
    "{host}/store/{hash}\n",
    host = "http://localhost:8000",
    hash = hash
  );

  // Write the paste out to the file and return the URL
  hashspace.put(expr);
  let reply = format!("Your hash {} at {}", hash, url);
  log(reply.as_str());
  Ok(reply)
}

#[options("/store")]
fn options() {}

/// Add CORS headers to requests
pub struct CORS();

impl Fairing for CORS {
  fn info(&self) -> Info {
    Info { name: "Add CORS headers to requests", kind: Kind::Response }
  }

  fn on_response(&self, _request: &Request, response: &mut Response) {
    response.set_header(Header::new("Access-Control-Allow-Origin", "*"));
    response.set_header(Header::new(
      "Access-Control-Allow-Methods",
      "GET, PUT, OPTIONS",
    ));
    response.set_header(Header::new("Access-Control-Allow-Headers", "*"));
    response
      .set_header(Header::new("Access-Control-Allow-Credentials", "true"));
  }
}

pub fn start_server(_opt_host: Option<String>) {
  rocket::ignite()
    .attach(Template::fairing())
    .attach(CORS())
    .mount("/pkg", StaticFiles::from("pkg"))
    .mount("/", routes![index, get, put, options])
    .launch();
}
