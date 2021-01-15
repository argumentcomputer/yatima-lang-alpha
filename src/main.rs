use std::{
  fs,
  path::PathBuf,
};

use yatima::{
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
  Repl,
}
//

fn main() {
  let command = Cli::from_args();
  match command {
    Cli::Repl => repl::main().unwrap(),
    Cli::Parse { input } => {
      let p = parse::package::parse_file(input);
      println!("Package parsed: {}", p);
      println!("Package parsed: {:?}", p)
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
