use cid::Cid;
use nom::{
  error::ParseError,
  Finish,
};
use nom_locate::LocatedSpan;
use std::{
  path::PathBuf,
  rc::Rc,
};
use structopt::StructOpt;
use yatima_cli::{
  file::store::FileStore,
  ipfs,
  repl,
};
use yatima_core::name::Name;
use yatima_utils::{
  file,
  store::Store,
};

#[derive(Debug, StructOpt)]
#[structopt(about = "A programming language for the decentralized web")]
enum Cli {
  Parse {
    /// Pin data to the local IPFS daemon
    #[structopt(short, long)]
    no_ipfs: bool,
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
}

#[derive(Debug, StructOpt)]
enum ShowType {
  Package {
    #[structopt(parse(try_from_str = parse_cid))]
    input: Cid,
  },
  Entry {
    #[structopt(parse(try_from_str = parse_cid))]
    input: Cid,
    #[structopt(name = "var_index", long, short)]
    var_index: bool,
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

fn parse_cid(
  s: &str,
) -> Result<Cid, yatima_core::parse::error::ParseError<LocatedSpan<&str>>> {
  let result = yatima_core::parse::package::parse_link(
    yatima_core::parse::span::Span::new(&s),
  )
  .finish()
  .map(|(_, x)| x);
  result
}

//   Test,
#[tokio::main]
async fn main() -> std::io::Result<()> {
  let command = Cli::from_args();
  match command {
    Cli::Repl => {
      repl::main();
      Ok(())
    }
    Cli::Show { typ: ShowType::Package { input } } => {
      let store = Rc::new(FileStore::new());
      match store.get(input) {
        Some(ipld) => match yatima_core::package::Package::from_ipld(&ipld) {
          Ok(pack) => println!("{}", pack),
          Err(_) => eprintln!("Expected package ipld"),
        },
        None => eprintln!("Found no result for cid"),
      };
      Ok(())
    }
    Cli::Show { typ: ShowType::Entry { input, var_index } } => {
      let store = Rc::new(FileStore::new());
      match store.get(input) {
        Some(ipld) => match yatima_core::package::Entry::from_ipld(&ipld) {
          Ok(entry) => {
            println!("{}", entry);
            match file::parse::entry_to_def(entry, store) {
              Ok(def) => println!("{}", def.pretty("#^".to_string(), var_index)),
              Err(_) => eprintln!("expected valid def"),
            }
          }
          Err(_) => eprintln!("Expected entry ipld"),
        },
        None => eprintln!("Found no result for cid"),
      };
      Ok(())
    }
    Cli::Show { typ: ShowType::Anon { input } } => {
      let store = Rc::new(FileStore::new());
      match store.get(input) {
        Some(ipld) => match yatima_core::anon::Anon::from_ipld(&ipld) {
          Ok(pack) => println!("{:?}", pack),
          Err(_) => eprintln!("Expected valid anon"),
        },
        None => eprintln!("Found no result for cid"),
      };
      Ok(())
    }
    Cli::Show { typ: ShowType::Raw { input } } => {
      let store = Rc::new(FileStore::new());
      match store.get(input) {
        Some(ipld) => println!("{:?}", ipld),
        None => eprintln!("Found no result for cid"),
      };
      Ok(())
    }
    Cli::Parse { no_ipfs, path } => {
      let root = std::env::current_dir()?;
      let store = Rc::new(FileStore::new());
      let env = file::parse::PackageEnv::new(root, path, store.clone());
      let (cid, p, d) = file::parse::parse_file(env);
      store.put(p.to_ipld());

      let ipld_cid = if !no_ipfs {
        ipfs::dag_put(p.to_ipld()).await.expect("Failed to put to ipfs.")
      }
      else {
        "Not using ipfs".to_string()
      };
      println!("Package parsed:\n{} ipld_cid={}", cid, ipld_cid);
      println!("{}", d);
      Ok(())
    }
    Cli::Check { path } => {
      let store = Rc::new(FileStore {});
      file::check_all(path, store)?;
      Ok(())
    }
    Cli::Run { path } => {
      let root = std::env::current_dir()?;
      let store = Rc::new(FileStore {});
      let env = file::parse::PackageEnv::new(root, path.clone(), store.clone());
      let (_, p, defs) = file::parse::parse_file(env);
      let _cid = store.put(p.to_ipld());
      let _ipld_cid =
        ipfs::dag_put(p.to_ipld()).await.expect("Failed to put to ipfs.");
      let def = defs.get(&Name::from("main")).expect(&format!(
        "No `main` expression in package {} from file {:?}",
        p.name, path
      ));
      let mut dag = yatima_core::dag::DAG::from_term(&def.to_owned().term);
      dag.norm(&defs);
      println!("{}", dag);
      Ok(())
    }
  }
}

// for valgrind testing
// Cli::Test => {
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
