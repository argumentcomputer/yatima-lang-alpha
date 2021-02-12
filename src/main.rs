use std::{
  fs,
  path::PathBuf,
};

use structopt::StructOpt;
use yatima::{
  core,
  hashspace,
  parse,
  repl,
};

#[derive(Debug, StructOpt)]
#[structopt(about = "A programming language for the decentralized web")]
enum Cli {
  Save {
    #[structopt(parse(from_os_str))]
    input: PathBuf,
  },
  Show {
    input: String,
  },
  Parse {
    #[structopt(parse(from_os_str))]
    input: PathBuf,
  },
  // Run {
  //  #[structopt(parse(from_os_str))]
  //  input: PathBuf,
  //},
  Repl,
}
//
fn main() {
  let command = Cli::from_args();
  match command {
    Cli::Repl => repl::main().unwrap(),
    Cli::Parse { input } => {
      let env = parse::package::PackageEnv::new(input);
      let (l, p) = parse::package::parse_file(env);
      println!("Package parsed:\n{}", p);
    }
    // Cli::Run { input } => {
    //  let env = parse::package::PackageEnv::new(input);
    //  let (l, p) = parse::package::parse_file(env);
    //  let d = p.defs.get("main");
    //  match d {
    //    None => panic!(
    //      "No `main` expression in package {} from file {:?}",
    //      p.name, input
    //    ),
    //    Some((d,_)) => {
    //      let d = hashspace::get(
    //      println!("{}", core::eval::norm(core::dag::DAG::from_term(d.term)));
    //    }
    //  }
    //}
    Cli::Save { input } => {
      let string = fs::read_to_string(input).unwrap();
      let expr = hashexpr::parse(&string).unwrap().1;
      let link = hashspace::put(expr);
      println!("Saved as {}", link)
    }
    Cli::Show { input } => {
      let link = hashexpr::link::Link::parse(&input).expect("valid link").1;
      println!("link {:?} {}", link, link);
      let expr = hashspace::get(link).expect("unknown link");
      println!("{}", expr)
    }
  }
}
