pub mod command;
pub mod error;

use std::{
  rc::Rc,
  sync::{Arc, Mutex},
};
use nom::Err;
use yatima_core::{
  check::{
    check_def,
    infer_term,
  },
  dag::DAG,
  defs::Defs,
  parse::{
    span::Span,
    term::input_cid,
  },
};
use crate::{
  file,
  store::Store,
};

use command::{
    Command
};
use error::ReplError;

pub trait Repl {
    fn readline(&mut self, prompt: &str) -> Result<String, ReplError>;
    fn println(&self, s: String);
    fn load_history(&mut self);
    fn add_history_entry(&mut self, s: &str);
    fn save_history(&mut self);
    fn get_defs(&self) -> Arc<Mutex<Defs>>;
    fn get_store(&self) -> Rc<dyn Store>;
    fn handle_line(&mut self, readline: Result<String, ReplError>) -> Result<(),()> {
      let mutex_defs = self.get_defs();
      let mut defs = mutex_defs.lock().unwrap();
      let store = self.get_store();
      match readline {
        Ok(line) => {
          self.add_history_entry(line.as_str());
          let res = command::parse_command(
            input_cid(line.as_str()),
            defs.clone(),
          )(Span::new(&line));
          match res {
            Ok((_, command)) => {
              match command {
                Command::Load(name) => {
                  let root = std::env::current_dir().unwrap();
                  let mut path = root.clone();
                  for n in name.split('.') {
                    path.push(n);
                  }
                  path.set_extension("ya");
                  if let Ok(ds) = file::check_all(path, store) {
                    *defs = ds;
                  }
                }
                Command::Eval(term) => {
                  let mut dag = DAG::from_term(&term);
                  dag.norm(&defs);
                  self.println(format!("{}", dag));
                }
                Command::Type(term) => {
                  let res = infer_term(&defs, *term);
                  match res {
                    Ok(term) => self.println(format!("{}", term)),
                    Err(e) => self.println(format!("Error: {}", e)),
                  }
                }
                Command::Define(boxed) => {
                  let (n, def, _) = *boxed;
                  let mut tmp_defs = defs.clone();
                  tmp_defs.insert(n.clone(), def);
                  let res = check_def(&tmp_defs, &n);
                  match res {
                    Ok(res) => {
                      *defs = tmp_defs;
                      self.println(format!("{} : {}", n, res.pretty(Some(&n.to_string()))))
                    }
                    Err(e) => self.println(format!("Error: {}", e)),
                  }
                }
                Command::Browse => {
                  for (n, d) in defs.named_defs() {
                    self.println(format!("{}", d.pretty(n.to_string())))
                  }
                }
                Command::Quit => {
                  self.println(format!("Goodbye."));
                  return Err(());
                }
              };
              Ok(())
            },
            Err(e) => {
              match e {
                Err::Incomplete(_) => self.println(format!("Incomplete Input")),
                Err::Failure(e) => {
                  self.println(format!("Parse Failure:\n"));
                  self.println(format!("{}", e));
                }
                Err::Error(e) => {
                  self.println(format!("Parse Error:\n"));
                  self.println(format!("{}", e));
                }
              };
              Err(())
            },
          }
        }
        Err(ReplError::Interrupted) => {
          self.println(format!("CTRL-C"));
          Err(())
        }
        Err(ReplError::Eof) => {
          self.println(format!("CTRL-D"));
          Err(())
        }
        Err(ReplError::Other(err)) => {
          self.println(format!("Error: {}", err));
          Err(())
        }
      }
    }
}

pub fn run_repl(rl: &mut dyn Repl) {
  rl.load_history();
  loop {
    let readline = rl.readline("⅄ ");
    match rl.handle_line(readline) {
      Ok(()) => continue,
      Err(()) => break,
    }
  }
  rl.save_history();
}

