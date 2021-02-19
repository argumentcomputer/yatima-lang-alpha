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
  term::unembed_defs,
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
      let env = parse::package::PackageEnv::new(input);
      let (_, _, p) = parse::package::parse_file(env);
      println!("Package parsed:\n{}", p);
    }
    Cli::Run { input } => {
      let env = parse::package::PackageEnv::new(input.clone());
      let (_, defs, p) = parse::package::parse_file(env);
      let defs2 = defs.clone();
      let (def_link, _) = defs2.get("main").expect(&format!(
        "No `main` expression in package {} from file {:?}",
        p.name, input
      ));
      let def_map =
        unembed_defs(defs).expect("Embedding error while reading package defs");
      let def =
        def_map.get(def_link).expect("Unknown link for `main` expression");
      let dag = core::dag::DAG::from_term(def.to_owned().term);
      let red = core::eval::norm(&def_map, dag);
      println!("{}", red);
    }
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
