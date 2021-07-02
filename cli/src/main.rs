use nom::{
  error::ParseError,
  Finish,
};
use nom_locate::LocatedSpan;
use sp_cid::Cid;
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
  store::{
    show,
    Store,
  },
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

fn parse_cid(
  s: &str,
) -> Result<
  Cid,
  yatima_core::parse::error::ParseError<nom_locate::LocatedSpan<&str>>,
> {
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
    Cli::Show { typ: ShowType::File { path } } => {
      let root = std::env::current_dir()?;
      let store = Rc::new(FileStore::new());
      let env = file::parse::PackageEnv::new(root, path, store.clone());
      match file::parse::parse_file(env) {
        Ok((_, pack, _)) => {
          println!("{}", pack);
          Ok(())
        }
        Err(_) => Err(std::io::Error::from(std::io::ErrorKind::NotFound)),
      }
    }
    Cli::Show { typ: ShowType::Graph { input } } => {
      let store = Rc::new(FileStore::new());
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
    Cli::Show { typ: ShowType::Package { input } } => {
      let store = Rc::new(FileStore::new());
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
    Cli::Show { typ: ShowType::Entry { input, var } } => {
      let store = Rc::new(FileStore::new());
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
    Cli::Show { typ: ShowType::Anon { input } } => {
      let store = Rc::new(FileStore::new());
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
    Cli::Show { typ: ShowType::Raw { input } } => {
      let store = Rc::new(FileStore::new());
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
    Cli::Parse { no_ipfs, path } => {
      let root = std::env::current_dir()?;
      let store = Rc::new(FileStore::new());
      let env = file::parse::PackageEnv::new(root, path, store.clone());
      let (cid, p, d) = file::parse::parse_file(env).map_err(|e| {
        eprintln!("{}", e);
        std::io::Error::from(std::io::ErrorKind::Other)
      })?;
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
      let store = Rc::new(FileStore::new());
      file::check_all_in_file(path, store)?;
      Ok(())
    }
    Cli::Run { path } => {
      let root = std::env::current_dir()?;
      let store = Rc::new(FileStore::new());
      let env = file::parse::PackageEnv::new(root, path.clone(), store.clone());
      let (_, p, defs) = file::parse::parse_file(env).map_err(|e| {
        eprintln!("{}", e);
        std::io::Error::from(std::io::ErrorKind::Other)
      })?;

      let _cid = store.put(p.to_ipld());
      let _ipld_cid =
        ipfs::dag_put(p.to_ipld()).await.expect("Failed to put to ipfs.");
      let def = defs.get(&Name::from("main")).expect(&format!(
        "No `main` expression in package {} from file {:?}",
        p.name, path
      ));
      let mut dag = yatima_core::dag::DAG::from_term(&def.to_owned().term);
      dag.norm(&defs, false);
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
