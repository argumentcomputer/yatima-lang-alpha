use sp_cid::Cid;
use sp_ipld::Ipld;
use std::{
  path::PathBuf,
  rc::Rc,
};
use structopt::StructOpt;
use yatima_cli::file::store::{
  FileStore,
  FileStoreOpts,
};
#[cfg(not(target_arch = "wasm32"))]
use yatima_cli::repl;
use yatima_core::{
  name::Name,
  parse::parse_cid,
};
use yatima_runtime::transform::StdIORuntime;
use yatima_utils::{
  file,
  ipfs::IpfsApi,
  store::{
    show,
    Store,
  },
};

#[derive(Debug, StructOpt)]
#[structopt(about = "A programming language for the decentralized web")]
struct Cli {
  /// Pin data to the local IPFS daemon
  #[structopt(short = "i", long = "ipfs", help = "Turn on adding data to the IPFS daemon.")]
  use_ipfs: bool,

  #[structopt(
    long,
    help = "Turn off writing to the file system. Data will only be kept in \
            memory."
  )]
  no_file_store: bool,

  #[structopt(
    long,
    help = "The root directory we are reading files relative to."
  )]
  root: Option<PathBuf>,

  /// Command to execute
  #[structopt(subcommand)]
  command: Command,
}

#[derive(Debug, StructOpt)]
#[structopt(name = "command")]
enum Command {
  Parse {
    #[structopt(parse(from_os_str))]
    path: PathBuf,
  },
  Check {
    #[structopt(parse(from_os_str))]
    path: PathBuf,
  },
  Show {
    #[structopt(subcommand)]
    typ: ShowType,
  },
  Run {
    #[structopt(parse(from_os_str))]
    path: PathBuf,
  },
  Repl,
  Pin {
    #[structopt(parse(from_os_str))]
    path: PathBuf,
  },
  Clone {
    cid: String,
  },
}

#[derive(Debug, StructOpt)]
enum ShowType {
  File {
    #[structopt(parse(from_os_str))]
    path: PathBuf,
  },
  Graph {
    #[structopt(parse(try_from_str = parse_cid))]
    input: Cid,
  },
  Package {
    #[structopt(parse(try_from_str = parse_cid))]
    input: Cid,
  },
  Entry {
    #[structopt(parse(try_from_str = parse_cid))]
    input: Cid,
    #[structopt(name = "var", long, short)]
    var: bool,
  },
  Anon {
    #[structopt(parse(try_from_str = parse_cid))]
    input: Cid,
  },
  Raw {
    #[structopt(parse(try_from_str = parse_cid))]
    input: Cid,
  },
}

#[cfg(not(target_arch = "wasm32"))]
#[tokio::main]
async fn main() -> std::io::Result<()> { run_cli() }

#[cfg(target_arch = "wasm32")]
fn main() -> std::io::Result<()> { run_cli() }

#[cfg(not(target_arch = "wasm32"))]
fn repl(store: Rc<FileStore>) -> std::io::Result<()> {
  repl::main(store);
  Ok(())
}

#[cfg(target_arch = "wasm32")]
fn repl(_store: Rc<dyn Store>) -> std::io::Result<()> {
  eprintln!("REPL not supported on WASI yet.");
  Ok(())
}

fn run_cli() -> std::io::Result<()> {
  let cli = Cli::from_args();
  let root = cli.root.unwrap_or_else(|| std::env::current_dir().unwrap());
  let ipfs = if cli.use_ipfs { Some(IpfsApi::new("localhost:5001".to_string())) } else { None };
  let store = Rc::new(FileStore::new(
    FileStoreOpts { use_file_store: !cli.no_file_store, root: root.clone() },
    ipfs,
  ));
  match cli.command {
    Command::Repl => repl(store),
    Command::Show { typ: ShowType::File { path } } => {
      let env = file::parse::PackageEnv::new(root, path, store);
      match file::parse::parse_file(env) {
        Ok((_, pack, _)) => {
          println!("{}", pack);
          Ok(())
        }
        Err(_) => Err(std::io::Error::from(std::io::ErrorKind::NotFound)),
      }
    }
    Command::Show { typ: ShowType::Graph { input } } => {
      match show(store, input, "graph".to_string(), false) {
        Ok(s) => {
          println!("{}", s);
          Ok(())
        }
        Err(s) => {
          eprintln!("{}", s);
          Err(std::io::Error::from(std::io::ErrorKind::NotFound))
        }
      }
    }
    Command::Show { typ: ShowType::Package { input } } => {
      match show(store, input, "package".to_string(), false) {
        Ok(s) => {
          println!("{}", s);
          Ok(())
        }
        Err(s) => {
          eprintln!("{}", s);
          Err(std::io::Error::from(std::io::ErrorKind::NotFound))
        }
      }
    }
    Command::Show { typ: ShowType::Entry { input, var } } => {
      match show(store, input, "entry".to_string(), var) {
        Ok(s) => {
          println!("{}", s);
          Ok(())
        }
        Err(s) => {
          eprintln!("{}", s);
          Err(std::io::Error::from(std::io::ErrorKind::NotFound))
        }
      }
    }
    Command::Show { typ: ShowType::Anon { input } } => {
      match show(store, input, "anon".to_string(), false) {
        Ok(s) => {
          println!("{}", s);
          Ok(())
        }
        Err(s) => {
          eprintln!("{}", s);
          Err(std::io::Error::from(std::io::ErrorKind::NotFound))
        }
      }
    }
    Command::Show { typ: ShowType::Raw { input } } => {
      match show(store, input, String::new(), false) {
        Ok(s) => {
          println!("{}", s);
          Ok(())
        }
        Err(s) => {
          eprintln!("{}", s);
          Err(std::io::Error::from(std::io::ErrorKind::NotFound))
        }
      }
    }
    Command::Parse { path } => {
      let env = file::parse::PackageEnv::new(root, path, store.clone());
      let (cid, p, d) = file::parse::parse_file(env).map_err(|e| {
        eprintln!("{}", e);
        std::io::Error::from(std::io::ErrorKind::Other)
      })?;
      store.put(p.to_ipld());

      println!("Package parsed:\n{}", cid);
      println!("{}", d);
      Ok(())
    }
    Command::Check { path } => {
      file::check_all_in_file(root, path, store)?;
      Ok(())
    }
    Command::Run { path } => {
      let env = file::parse::PackageEnv::new(root, path.clone(), store.clone());
      let (_, p, defs) = file::parse::parse_file(env).map_err(handle_error_string)?;
      let p = Rc::new(p);
      let defs = Rc::new(defs);

      let _cid = store.put(p.to_ipld());

      let checked = file::check_all(p.clone(), defs, store).map_err(handle_error_string)?;
      let def = checked.get(&Name::from("main")).unwrap_or_else(|| {
        panic!("No `main` expression in package {} from file {:?}", p.name, path)
      });
      let runtime_io = Rc::new(StdIORuntime::new());

      yatima_runtime::run(&mut def.to_owned().term, checked, runtime_io);
      Ok(())
    }
    Command::Pin { path } => {
      pin(path, root, store);
      Ok(())
    }
    Command::Clone { cid } => {
      let cid = parse_cid(&cid).unwrap();
      clone(cid, root, store);
      println!("Cloned directory from IPFS");
      Ok(())
    }
  }
}

fn pin(path: PathBuf, root: PathBuf, store: Rc<FileStore>) {
  let env =
    file::parse::PackageEnv::new(root.clone(), path.clone(), store.clone());
  let info = file::parse::parse_file(env).unwrap();
  let pkg = info.1;
  let imports = &pkg.imports;
  for import in imports {
    let mut import_path = root.clone();
    for n in import.name.split('.') {
      import_path.push(n);
    }
    import_path.set_extension("ya");
    pin(import_path, root.clone(), store.clone());
  }
  let source = std::fs::read_to_string(&path)
    .map_err(|e| format!("file {:?} not found {:?}", &path, e))
    .unwrap();
  let defs = info.2;
  for (name, _) in defs.names.iter() {
    let def = defs.get(name).unwrap();
    let entry = def.embed().0;
    store.put(Ipld::Link(entry.type_anon));
    store.put(Ipld::Link(entry.term_anon));
    store.put(entry.type_meta.to_ipld());
    store.put(entry.term_meta.to_ipld());
    store.put(entry.to_ipld());
  }
  store.put(Ipld::String(source.clone()));
  // let cid = store.put(Ipld::String(source.clone()));
  // println!("Pinned {} with CID {}", source, cid.to_string());
  let cid = store.put(pkg.to_ipld());
  println!("Pinned {} with CID {}", pkg, cid.to_string());
}

use yatima_core::{
  package::Package,
  position::Pos,
};

// Convert a package and each import into files and write to flat directory
fn clone(cid: Cid, root: PathBuf, store: Rc<FileStore>) {
  // For each import, convert to a package and recurse
  let ipld = store.get(cid).unwrap();
  let pkg = Package::from_ipld(&ipld).unwrap();
  let imports = &pkg.imports;
  for import in imports {
    clone(import.cid, root.clone(), store.clone());
  }
  // Convert package name to file path
  let mut path = root.clone();
  for n in pkg.name.split('.') {
    path.push(n);
  }
  path.set_extension("ya");
  // First, get source cid from Package
  let src_cid = match pkg.pos {
    Pos::Some(p) => Some(p.input),
    Pos::None => None,
  };
  let src_name = path.to_str().unwrap();
  // Convert cid into Ipld object
  let src_ipld = store.get(src_cid.unwrap()).unwrap();
  let src_txt = match src_ipld {
    Ipld::String(text) => text,
    _ => panic!("not a string"),
  };
  println!("{}", src_txt);
  // Convert Ipld into text
  std::fs::write(src_name, src_txt).unwrap();
}

pub fn handle_error_string(e: String) -> std::io::Error {
  eprintln!("{}", e);
  std::io::Error::from(std::io::ErrorKind::Other)
}

// for valgrind testing
// Command::Test => {
//  use im::HashMap;
//  use yatima::{
//    core::dag::DAG,
//    parse::span::Span,
//  };
//  pub fn parse(
//    i: &str,
//  ) -> nom::IResult<Span, DAG, crate::parse::error::ParseError<Span>>
//  {
//    let (i, tree) = crate::parse::term::parse(i)?;
//    let (i, _) = nom::character::complete::multispace0(i)?;
//    let (i, _) = nom::combinator::eof(i)?;
//    let dag = DAG::from_term(&tree);
//    Ok((i, dag))
//  }
//  fn norm_assert(input: &str, result: &str) {
//    match parse(&input) {
//      Ok((_, mut dag)) => {
//        dag.norm(&HashMap::new());
//        assert_eq!(format!("{}", dag), result)
//      }
//      Err(_) => panic!("Did not parse."),
//    }
//  }
//  norm_assert(
//    "∀ (f: ∀ (A: Type) (x: A) -> Type) -> Type",
//    "∀ (f: ∀ (A: Type) (x: A) -> Type) -> Type",
//  );
//  // norm_assert("let f (A: Type) (x: A): Type = A; f", "λ A x => A");
//}
