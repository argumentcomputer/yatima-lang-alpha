#![feature(proc_macro_hygiene, decl_macro)]

#[macro_use]
extern crate rocket;

#[macro_use]
extern crate log;

extern crate directories_next;

use directories_next::ProjectDirs;
use hashexpr::{
  base,
  Expr,
};
use rocket::Data;
use std::{
  fs,
  fs::File,
  io::prelude::*,
  path::Path,
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
fn get(hash: String) -> Option<File> {
  info!("Your hash {}", hash);
  let proj_dirs =
    ProjectDirs::from("io", "yatima", "hashspace").expect("needs directory");
  let path = proj_dirs.cache_dir().join(Path::new(&hash));
  File::open(&path).ok()
}

#[put("/store", data = "<data>")]
fn put(data: Data) -> Result<String, std::io::Error> {
  let proj_dirs =
    ProjectDirs::from("io", "yatima", "hashspace").expect("needs directory");
  fs::create_dir_all(proj_dirs.cache_dir()).ok();

  let mut stream = data.peek();

  // let hash = blake3::hash();
  let mut hasher = blake3::Hasher::new();
  hasher.update(stream);
  let hash = base::encode(base::Base::_58, hasher.finalize().as_bytes());

  let xp = Expr::from_bits(stream.clone().as_ref());

  let path = proj_dirs.cache_dir().join(Path::new(&hash));
  let url = format!(
    "{host}/store/{hash}\n",
    host = "http://localhost:8000",
    hash = hash
  );

  // Write the paste out to the file and return the URL
  let mut file = File::create(path)?;
  file.write_all(&mut stream)?;
  Ok(format!("Your hash {} at {}", hash, url))
}

fn main() { rocket::ignite().mount("/", routes![index, get, put]).launch(); }
