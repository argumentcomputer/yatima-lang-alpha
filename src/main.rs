use std::{
  fs,
  path::PathBuf,
};

use yatima::{
  core,
  hashspace,
  parse,
  repl,
};

use structopt::StructOpt;

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
  Run {
    #[structopt(parse(from_os_str))]
    input: PathBuf,
  },
  Repl,
}
//

fn main() {
  let command = Cli::from_args();
  match command {
    Cli::Repl => repl::main().unwrap(),
    Cli::Parse { input } => {
      let p = parse::package::parse_file(input);
      println!("Package parsed:\n{}", p);
    }
    Cli::Run { input } => {
      let p = parse::package::parse_file(input.clone());
      let d = p.defs.get("main");
      match d {
        None => panic!(
          "No `main` expression in package {} from file {:?}",
          p.name, input
        ),
        Some(d) => {
          println!("{}", core::eval::norm(core::dag::DAG::from_term(d.term)));
        }
      }
    }
    Cli::Save { input } => {
      let string = fs::read_to_string(input).unwrap();
      let expr = hashexpr::parse(&string).unwrap().1;
      let link = hashspace::put(expr);
      println!("Saved as {}", link)
    }
    Cli::Show { input } => {
      let link = hashexpr::link::Link::parse(&input).unwrap().1;
      let expr = hashspace::get(link).unwrap();
      println!("{}", expr)
    }
  }
}
