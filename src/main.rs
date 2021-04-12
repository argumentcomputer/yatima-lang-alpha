use std::{
  fs,
  path::PathBuf,
};

// #[cfg(not(target_arch = "wasm32"))]
use std::io::{self, Read};
use structopt::StructOpt;
use yatima::{
  core,
  hashspace,
  hashspace::{HashspaceImplWrapper},
  parse,
  repl,
};

#[derive(Debug, StructOpt)]
#[structopt(about = "A programming language for the decentralized web")]
enum Cli {
  Parse {
    #[structopt(parse(from_os_str), name = "path")]
    opt_path: Option<PathBuf>,
  },
  Run {
    #[structopt(parse(from_os_str))]
    input: PathBuf,
  },
  Repl,
  #[structopt(name = "hashspace")]
  Hashspace {
    #[structopt(subcommand)]
    hashspace_opt: HashspaceOpt,
  }
}
//
#[derive(Debug, StructOpt)]
#[structopt(name = "subcommand", about = "Access hashspace")]
enum HashspaceOpt {
  Server {
    opt_host: Option<String>,
  },
  Save {
    #[structopt(parse(from_os_str))]
    input: PathBuf,
  },
  Show {
    input: String,
  },
}

fn handle_cli() -> io::Result<()> {
  let command = Cli::from_args();
  match command {
    Cli::Repl =>
      repl::main().unwrap(),
    Cli::Parse { opt_path } => {
      let hashspace = hashspace::Hashspace::local();
      match opt_path {
        Some(path) => {
          let env = parse::package::PackageEnv::new(path);
          let (_, p, ..) = parse::package::parse_file(env, &hashspace);
          println!("Package parsed:\n{}", HashspaceImplWrapper::wrap(&hashspace, &p));
        }
        None => {
          let mut source = String::new();
          let stdin = io::stdin();
          let mut handle = stdin.lock();
          handle.read_to_string(&mut source)?;
          let (_, p, ..) = parse::package::parse_text(source.as_str(), None, &hashspace);
          println!("Package parsed:\n{}", HashspaceImplWrapper::wrap(&hashspace, &p));
        }
      }
    }
    Cli::Run { input } => {
      let env = parse::package::PackageEnv::new(input.clone());
      let hashspace = hashspace::Hashspace::local();
      let (_, p, defs, refs) = parse::package::parse_file(env, &hashspace);
      let (def_link, _) = refs.get("main").expect(&format!(
        "No `main` expression in package {} from file {:?}",
        p.name, input
      ));
      let def = defs.get(def_link).expect("Unknown link for `main` expression");
      let dag = core::dag::DAG::from_term(def.to_owned().term);
      let red = core::eval::norm(&defs, dag);
      println!("{}", red);
    }
    Cli::Hashspace { hashspace_opt } => {
      handle_hashspace_cmd(hashspace_opt)
    }
  }
  Ok(())
}

fn handle_hashspace_cmd(hashspace_opt: HashspaceOpt) {
  match hashspace_opt {
    HashspaceOpt::Server { opt_host } => {
      #[cfg(not(target_arch = "wasm32"))]
      hashspace::server::start_server(opt_host);
    }
    HashspaceOpt::Save { input } => {
      let string = fs::read_to_string(input).unwrap();
      let expr = hashexpr::parse(&string).unwrap().1;
      let hashspace = hashspace::Hashspace::local();
      let link = hashspace.put(expr);
      println!("Saved as {}", link)
    }
    HashspaceOpt::Show { input } => {
      let link = hashexpr::link::Link::parse(&input).expect("valid link").1;
      println!("link {:?} {}", link, link);
      let hashspace = hashspace::Hashspace::local();
      let expr = hashspace.get(link).expect("unknown link");
      println!("{}", expr)
    }
  }
}

#[cfg(target_arch = "wasm32")]
fn main() {
  handle_cli();
}

#[cfg(not(target_arch = "wasm32"))]
fn main() -> io::Result<()> {
  handle_cli()
}
