use libipld::Ipld;
use std::path::PathBuf;
use yatima_core::{
  check::error::CheckError,
  defs::Defs,
  position::Pos,
};

pub mod error;
pub mod parse;
pub mod store;

pub fn check_all(path: PathBuf) -> std::io::Result<Defs> {
  let root = std::env::current_dir()?;
  let env = parse::PackageEnv::new(root, path);
  let (_, p, ds) = parse::parse_file(env);
  let cid = store::put(p.to_ipld());
  // let _ipld_cid =
  //  ipfs::dag_put(p.to_ipld()).await.expect("Failed to put to ipfs.");
  println!("Checking package {} at {}", p.name, cid);
  for i in &p.imports {
    println!("Checking import  {} at {}", i.name, i.cid);
    for n in &i.with {
      match yatima_core::check::check_def(
        &ds,
        &yatima_core::package::import_alias(n.to_owned(), &i),
      ) {
        Ok(ty) => println!("✓ {}: {}", n, ty.pretty(Some(&n.to_string()))),
        Err(e @ CheckError::UndefinedReference(Pos::None, _)) => {
          println!("✕ {}: {}", n, e);
        }
        Err(err) => {
          let def = ds.get(n.clone()).unwrap();
          println!("✕ {}: {}", n, def.typ_.pretty(Some(&n.to_string())));
          if let Pos::Some(pos) = err.pos() {
            if let Some(Ipld::String(input)) = store::get(pos.input) {
              println!("{}", pos.range(input))
            }
          }
          print!("Error: {}", err);
        }
      }
    }
  }
  println!("Checking definitions:");
  for (n, _) in &p.index.0 {
    match yatima_core::check::check_def(&ds, n) {
      Ok(ty) => println!("✓ {}: {}", n, ty.pretty(Some(&n.to_string()))),
      Err(e @ CheckError::UndefinedReference(Pos::None, _)) => {
        println!("✕ {}: {}", n, e);
      }
      Err(err) => {
        let def = ds.get(n.clone()).unwrap();
        println!("✕ {}: {}", n, def.typ_.pretty(Some(&n.to_string())));
        if let Pos::Some(pos) = err.pos() {
          if let Some(Ipld::String(input)) = store::get(pos.input) {
            println!("{}", pos.range(input))
          }
        }
        print!("Error: {}", err);
      }
    }
  }
  Ok(ds)
}
