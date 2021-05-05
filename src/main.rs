use std::path::PathBuf;

use structopt::StructOpt;

use yatima::{file, ipfs};

#[derive(Debug, StructOpt)]
#[structopt(about = "A programming language for the decentralized web")]
enum Cli {
  Parse {
    #[structopt(parse(from_os_str))]
    path: PathBuf,
  },
  Check {
    #[structopt(parse(from_os_str))]
    path: PathBuf,
  },
}
//  Run {
//    #[structopt(parse(from_os_str))]
//    input: PathBuf,
//  },
//  Repl,
//   Test,
#[tokio::main]
async fn main() -> std::io::Result<()> {
  let command = Cli::from_args();
  match command {
    // Cli::Repl => repl::main().unwrap(),
    Cli::Parse { path } => {
      let root = std::env::current_dir()?;
      let env = file::parse::PackageEnv::new(root, path);
      let (cid, p, d) = file::parse::parse_file(env);
      file::store::put(p.to_ipld());
      let ipld_cid = ipfs::dag_put(p.to_ipld()).await.expect("Failed to put to ipfs.");
      println!("Package parsed:\n{} {}", cid, ipld_cid);
      println!("{}", d);
      Ok(())
    }
    Cli::Check { path } => {
      let root = std::env::current_dir()?;
      let env = file::parse::PackageEnv::new(root, path);
      let (_, p, ds) = file::parse::parse_file(env);
      let cid = file::store::put(p.to_ipld());
      println!("Checking package {} at {}", p.name, cid);
      for i in &p.imports {
        println!("Checking import  {} at {}", i.name, i.cid);
        for n in &i.with {
          match yatima_core::check::check_def(
            &ds,
            &yatima_core::package::import_alias(n.to_owned(), &i.alias),
          ) {
            Ok(ty) => println!("✓ {}: {}", n, ty.pretty(Some(&n))),
            Err(err) => {
              println!("✕ {}: {}", n, err);
            }
          }
        }
      }
      println!("Checking definitions:");
      for (n, _) in &p.index.0 {
        match yatima_core::check::check_def(&ds, n) {
          Ok(ty) => println!("✓ {}: {}", n, ty.pretty(Some(&n))),
          Err(err) => {
            println!("✕ {}: {}", n, err);
          }
        }
      }
      Ok(())
    }
  }
}
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
