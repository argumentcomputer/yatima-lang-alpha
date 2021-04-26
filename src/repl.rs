// use directories_next::ProjectDirs;
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

use yatima_core::{
  dag::DAG,
  defs::Defs,
  parse,
};

pub fn main() -> rustyline::Result<()> {
  let config = Config::builder().edit_mode(EditMode::Vi).build();
  let mut rl = Editor::<()>::with_config(config);
  let defs = Defs::new();
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
        let res = parse::term::parse(&line, defs.clone());
        match res {
          Ok((_, term)) => {
            let mut dag = DAG::from_term(&term);
            dag.norm(&defs);
            println!("{}", dag);
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
