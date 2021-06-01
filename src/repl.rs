// use directories_next::ProjectDirs;
use rustyline::{
  error::ReadlineError,
  Cmd,
  Config,
  EditMode,
  Editor,
  KeyEvent,
};

use nom::Err;

pub mod command;

use command::Command;

use yatima_core::{
  check::{
    check_def,
    infer_term,
  },
  dag::DAG,
  defs::Defs,
  name::Name,
  parse::{
    span::Span,
    term::input_cid,
  },
};

use crate::file;

pub fn main() -> rustyline::Result<()> {
  let config = Config::builder().edit_mode(EditMode::Vi).build();
  let mut rl = Editor::<()>::with_config(config);
  let mut defs = Defs::new();
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
        let res = command::parse_command(
          input_cid(line.as_str()),
          defs.clone(),
        )(Span::new(&line));
        match res {
          Ok((_, command)) => match command {
            Command::Load(name) => {
              let root = std::env::current_dir()?;
              let mut path = root.clone();
              for n in name.split('.') {
                path.push(n);
              }
              path.set_extension("ya");
              let ds = file::check_all(path)?;
              defs = ds
            }
            Command::Eval(term) => {
              let mut dag = DAG::from_term(&term);
              dag.norm(&defs);
              println!("{}", dag);
            }
            Command::Type(term) => {
              let res = infer_term(&defs, *term);
              match res {
                Ok(term) => println!("{}", term),
                Err(e) => println!("Error: {}", e),
              }
            }
            Command::Define(boxed) => {
              let (n, def, _) = *boxed;
              let mut tmp_defs = defs.clone();
              tmp_defs.insert(n.clone(), def);
              let res = check_def(&tmp_defs, &n);
              match res {
                Ok(res) => {
                  defs = tmp_defs;
                  println!("{} : {}", n, res.pretty(Some(&n.to_string())))
                }
                Err(e) => println!("Error: {}", e),
              }
            }
            Command::Browse => {
              for (n, d) in defs.named_defs() {
                println!("{}", d.pretty(n.to_string()))
              }
            }
            Command::Quit => {
              println!("Goodbye.");
              break;
            }
          },
          Err(e) => match e {
            Err::Incomplete(_) => println!("Incomplete Input"),
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
