use nom::Finish;
use sp_cid::Cid;
use std::{
  path::PathBuf,
  rc::Rc,
};
use structopt::StructOpt;
use yatima_cli::{
  file::store::{
    FileStore,
    FileStoreOpts,
  },
  ipfs::dag_put,
  repl,
};
use yatima_core::name::Name;
use yatima_utils::{
  file,
  file::parse::parse_file,
  store::{
    show,
    Store,
  },
};

#[derive(Debug, StructOpt)]
#[structopt(about = "A programming language for the decentralized web")]
struct Cli {
  /// Pin data to the local IPFS daemon
  #[structopt(short, long, help = "Turn on adding data to the IPFS daemon.")]
  use_ipfs_daemon: bool,

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
    #[structopt(parse(from_os_str))]
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
  let cli = Cli::from_args();
  let root = cli.root.unwrap_or_else(|| std::env::current_dir().unwrap());
  let store = Rc::new(FileStore::new(FileStoreOpts {
    use_ipfs_daemon: cli.use_ipfs_daemon,
    use_file_store: !cli.no_file_store,
    root: root.clone(),
  }));
  match cli.command {
    Command::Repl => {
      repl::main(store);
      Ok(())
    }
    Command::Show { typ: ShowType::File { path } } => {
      let env = file::parse::PackageEnv::new(root, path, store.clone());
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
      let (_, p, defs) = file::parse::parse_file(env).map_err(|e| {
        eprintln!("{}", e);
        std::io::Error::from(std::io::ErrorKind::Other)
      })?;

      let _cid = store.put(p.to_ipld());
      let def = defs.get(&Name::from("main")).expect(&format!(
        "No `main` expression in package {} from file {:?}",
        p.name, path
      ));
      let mut dag = yatima_core::dag::DAG::from_term(&def.to_owned().term);
      dag.norm(&defs, false);
      println!("{}", dag);
      Ok(())
    }

    // Parse file into Package and Defs
    // Turn package into Ipld
    // Dag_put the Ipld object
    Command::Pin { path } => {
      for file in std::fs::read_dir(path)? {
        let file = file?;
        let path = file.path();
        let env = file::parse::PackageEnv::new(
          root.clone(),
          path.clone(),
          store.clone(),
        );
        let info = parse_file(env).unwrap();
        let pkg = info.1;
        let defs = info.2;
        for (name, _) in defs.names.iter() {
          let def = defs.get(name).unwrap();
          let (term_anon, term_meta) = def.term.embed();
          let (typ_anon, typ_meta) = def.typ_.embed();
          dag_put(term_anon.to_ipld()).await.unwrap();
          dag_put(term_meta.to_ipld()).await.unwrap();
          dag_put(typ_anon.to_ipld()).await.unwrap();
          dag_put(typ_meta.to_ipld()).await.unwrap();
          let entry = def.embed().0;
          dag_put(entry.to_ipld()).await.unwrap();
        }
        for import in &pkg.imports {
          dag_put(import.to_ipld()).await.unwrap();
        }
        dag_put(pkg.to_ipld()).await.unwrap();
      }
      println!("Pinned directory to IPFS");
      Ok(())
    }
    Command::Clone { cid } => {
      let dir = dag_get(cid).await.unwrap();
      println!("Cloned directory from IPFS");
      Ok(())
    }
  }
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
