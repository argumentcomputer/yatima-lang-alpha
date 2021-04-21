use std::{
  fs,
  path::{
    Path,
    PathBuf,
  },
};

use structopt::StructOpt;

use yatima::file;
use yatima_core;

#[derive(Debug, StructOpt)]
#[structopt(about = "A programming language for the decentralized web")]
enum Cli {
  Parse {
    #[structopt(parse(from_os_str))]
    path: PathBuf,
  },
}
//  Check {
//    #[structopt(parse(from_os_str))]
//    input: PathBuf,
//  },
//  Run {
//    #[structopt(parse(from_os_str))]
//    input: PathBuf,
//  },
//  Repl,
//   Test,

fn main() -> std::io::Result<()> {
  let command = Cli::from_args();
  match command {
    // Cli::Repl => repl::main().unwrap(),
    Cli::Parse { path } => {
      let root = std::env::current_dir()?;
      let env = file::parse::PackageEnv::new(root, path);
      let (cid, p, d) = file::parse::parse_file(env);
      file::store::put(p.to_ipld());
      println!("Package parsed:\n{}", cid);
      println!("{}", d);
      Ok(())
    }
  }
}
//    Cli::Check { input } => {
//      let hashspace = hashspace::Hashspace::local();
//      let env = parse::package::PackageEnv::new(input.clone());
//      let (_, p, defs, refs) = parse::package::parse_file(env, &hashspace);
//      for dec in p.decls {
//        if let package::Declaration::Defn { name, .. } = &dec {
//          match core::check::check_def(&defs, &refs, name) {
//            Ok(_) => println!("{}: checks", name),
//            Err(err) => {
//              println!("{}: {}", name, err);
//            }
//          }
//        }
//      }
//    }
//    Cli::Run { input } => {
//      let env = parse::package::PackageEnv::new(input.clone());
//      let hashspace = hashspace::Hashspace::local();
//      let (_, p, defs, refs) = parse::package::parse_file(env, &hashspace);
//      let (def_link, _) = refs.get("main").expect(&format!(
//        "No `main` expression in package {} from file {:?}",
//        p.name, input
//      ));
//      let def = defs.get(def_link).expect(
//        "Unknown link for `main`
// expression",
//      );
//      let mut dag = core::dag::DAG::from_term(&def.to_owned().term);
//      dag.norm(&defs);
//      println!("{}", dag);
//    }
// }

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
