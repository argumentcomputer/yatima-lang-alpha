use std::{
  fs,
  path::{
    Path,
    PathBuf,
  },
};

use yatima::{
  package,
  repl,
};

use structopt::{
  clap::Shell,
  StructOpt,
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
  Repl,
}
//

fn main() {
  let command = Cli::from_args();
  match command {
    Cli::Repl => repl::main().unwrap(),
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
