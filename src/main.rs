use std::{
  fs,
  path::PathBuf,
};

use structopt::StructOpt;
use yatima::{
  core,
  hashspace,
  package,
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
  Check {
    #[structopt(parse(from_os_str))]
    input: PathBuf,
  },
  Run {
    #[structopt(parse(from_os_str))]
    input: PathBuf,
  },
  Repl,
  // Test,
}

fn main() {
  let command = Cli::from_args();
  match command {
    Cli::Repl => repl::main().unwrap(),
    Cli::Parse { input } => {
      let env = parse::package::PackageEnv::new(input);
      let (_, p, ..) = parse::package::parse_file(env);
      println!("Package parsed:\n{}", p);
    }
    Cli::Check { input } => {
      let env = parse::package::PackageEnv::new(input.clone());
      let (_, p, defs, refs) = parse::package::parse_file(env);
      for dec in p.decls {
        if let package::Declaration::Defn { name, .. } = &dec {
          match core::check::check_def(&defs, &refs, name) {
            Ok(_) => println!("{}: checks", name),
            Err(err) => {
              println!("{}: {}", name, err);
            }
          }
        }
      }
    }
    Cli::Run { input } => {
      let env = parse::package::PackageEnv::new(input.clone());
      let (_, p, defs, refs) = parse::package::parse_file(env);
      let (def_link, _) = refs.get("main").expect(&format!(
        "No `main` expression in package {} from file {:?}",
        p.name, input
      ));
      let def = defs.get(def_link).expect("Unknown link for `main` expression");
      let mut dag = core::dag::DAG::from_term(&def.to_owned().term);
      dag.norm(&defs);
      println!("{}", dag);
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

    // for valgrind testing
    //Cli::Test => {
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
  }
}
