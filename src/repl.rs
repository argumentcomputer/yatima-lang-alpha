use directories_next::ProjectDirs;
use rustyline::{
  error::ReadlineError,
  Cmd,
  Config,
  EditMode,
  Editor,
  KeyEvent,
};

use valus::{
  dag::norm,
  parser::parse,
};

pub fn main() -> rustyline::Result<()> {
  let config = Config::builder().edit_mode(EditMode::Vi).build();
  let mut rl = Editor::<()>::with_config(config);
  rl.bind_sequence(KeyEvent::alt('l'), Cmd::Insert(1, String::from("λ ")));
  if rl.load_history("history.txt").is_err() {
    println!("No previous history.");
  }
  loop {
    let readline = rl.readline("⅄ ");
    match readline {
      Ok(line) => {
        let res = parse(&line);
        match res {
          Ok((_, term)) => {
            rl.add_history_entry(line.as_str());
            println!("{}", norm(term));
          }
          Err(e) => println!("Error: {}", e),
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
        println!("Error: {:?}", err);
        break;
      }
    }
  }
  rl.save_history("history.txt")
}
