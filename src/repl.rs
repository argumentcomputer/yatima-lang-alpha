// use directories_next::ProjectDirs;
#[cfg(not(target_arch = "wasm32"))]
use rustyline::{
  error::ReadlineError,
  Cmd,
  Config,
  EditMode,
  Editor,
  KeyEvent,
};

use im::HashMap;

use nom::Err;

use crate::{
  core::{
    dag::DAG,
    eval::norm,
  },
  package::Declaration,
  parse::term::parse,
};

#[cfg(target_arch = "wasm32")]
pub fn main() -> Result<(),()> {
  println!("REPL not supported yet");
  Ok(())
}

#[cfg(not(target_arch = "wasm32"))]
pub fn main() -> rustyline::Result<()> {
  let config = Config::builder().edit_mode(EditMode::Vi).build();
  let mut rl = Editor::<()>::with_config(config);
  let defs = HashMap::new();
  let mut _decls: Vec<Declaration> = Vec::new();
  rl.bind_sequence(KeyEvent::alt('l'), Cmd::Insert(1, String::from("λ ")));
  rl.bind_sequence(KeyEvent::alt('a'), Cmd::Insert(1, String::from("∀ ")));
  if rl.load_history("history.txt").is_err() {
    println!("No previous history.");
  }
  loop {
    let readline = rl.readline("⅄ ");
    match readline {
      Ok(line) => {
        rl.add_history_entry(line.as_str());
        let res = parse(&line);
        match res {
          Ok((_, term)) => {
            println!("{}", norm(&defs, DAG::from_term(term)));
          }
          Err(e) => match e {
            Err::Incomplete(_) => println!("Incomplete"),
            Err::Failure(e) => {
              println!("Parse Failure:\n");
              println!("{}", e);
            }
            Err::Error(e) => {
              println!("Parse Error:\n");
              println!("{}", e);
            }
          },
        }
      }
      Err(ReadlineError::Interrupted) => {
        println!("CTRL-C");
        break;
      }
      Err(ReadlineError::Eof) => {
        println!("CTRL-D");
        break;
      }
      Err(err) => {
        println!("Error: {}", err);
        break;
      }
    }
  }
  rl.save_history("history.txt")
}
