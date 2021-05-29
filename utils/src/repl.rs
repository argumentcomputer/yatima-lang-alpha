pub mod command;
pub mod error;

use std::sync::{Arc, Mutex};
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

use command::{
    Command
};
use error::ReplError;

pub trait Repl {
    fn readline(&mut self, prompt: &str) -> Result<String, ReplError>;
    fn println(&self, s: String);
    fn load_history(&mut self);
    fn add_history_entry(&self, s: &str);
    fn save_history(&mut self);
    fn get_defs(&self) -> Arc<Mutex<Defs>>;
    fn handle_line(&self, readline: Result<String, ReplError>) -> Result<(),()> {
      let mutex_defs = self.get_defs();
      let mut defs = mutex_defs.lock().unwrap();
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
                      self.println(format!("{} : {}", n, res.pretty(Some(&n))))
                    }
                    Err(e) => self.println(format!("Error: {}", e)),
                  }
                }
                Command::Browse => {
                  for (n, d) in defs.named_defs() {
                    self.println(format!("{}", d.pretty(n)))
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

  let defs = Defs::new();
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

