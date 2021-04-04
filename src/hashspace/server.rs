use crate::{
  hashspace,
  utils::log,
};
use hashexpr::Expr;

use rocket::{
  Data,
  Request,
  Response,
  fairing::{Fairing, Info, Kind},
  http::{Header},
};

#[get("/")]
fn index() -> &'static str {
  "
    USAGE

      PUT /store/

          accepts raw data in the body of the request and responds with a URL \
   of
          a page containing the body's content

      GET /store/<hash>

          retrieves the content for the paste with id `<hash>`
    "
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
fn options() {
  
}

pub struct CORS();

impl Fairing for CORS {
    fn info(&self) -> Info {
        Info {
            name: "Add CORS headers to requests",
            kind: Kind::Response
        }
    }

    fn on_response(&self, request: &Request, response: &mut Response) {
        response.set_header(Header::new("Access-Control-Allow-Origin", "*"));
        response.set_header(Header::new("Access-Control-Allow-Methods", "GET, PUT, OPTIONS"));
        response.set_header(Header::new("Access-Control-Allow-Headers", "*"));
        response.set_header(Header::new("Access-Control-Allow-Credentials", "true"));
    }
}

pub fn start_server(opt_host: Option<String>) {
  rocket::ignite()
    .attach(CORS())
    .mount("/", routes![index, get, put, options])
    .launch();
}
