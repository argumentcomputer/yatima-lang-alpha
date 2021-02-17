use crate::hashspace;
use hashexpr::Expr;

use rocket::Data;

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
  let expr = hashspace::get(link)?;
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

  let url = format!(
    "{host}/store/{hash}\n",
    host = "http://localhost:8000",
    hash = hash
  );

  // Write the paste out to the file and return the URL
  hashspace::put(expr);
  Ok(format!("Your hash {} at {}", hash, url))
}

#[allow(dead_code)]
fn main() { rocket::ignite().mount("/", routes![index, get, put]).launch(); }
