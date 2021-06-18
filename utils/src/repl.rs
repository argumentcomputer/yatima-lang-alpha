pub mod command;
pub mod error;

use crate::{
  file,
  store::Store,
};
use nom::Err;
use sp_std::{
  cell::RefCell,
  rc::Rc,
  sync::Arc,
};

use std::sync::Mutex;
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

use command::Command;
use error::ReplError;

pub struct ReplEnv {
  type_system: bool,
  defs: Defs,
}

impl Default for ReplEnv {
  fn default() -> Self { ReplEnv { type_system: true, defs: Defs::new() } }
}

pub trait Repl {
  fn readline(&mut self, prompt: &str) -> Result<String, ReplError>;
  fn println(&self, s: String);
  fn load_history(&mut self);
  fn add_history_entry(&mut self, s: &str);
  fn save_history(&mut self);
  fn get_env(&self) -> Arc<Mutex<ReplEnv>>;
  fn get_store(&self) -> Rc<dyn Store>;
  fn handle_line(
    &mut self,
    readline: Result<String, ReplError>,
  ) -> Result<(), ()> {
    let mutex_env = self.get_env();
    let mut env = mutex_env.lock().unwrap();
    let store = self.get_store();
    match readline {
      Ok(line) => {
        self.add_history_entry(line.as_str());
        let res = command::parse_command(
          input_cid(line.as_str()),
          Rc::new(RefCell::new(env.defs.clone())),
        )(Span::new(&line));
        match res {
          Ok((_, command)) => {
            match command {
              Command::Set(field, setting) => match field.as_str() {
                "type-system" => {
                  env.type_system = setting;
                  println!(
                    "type-system: {}",
                    if setting { "on" } else { "off" }
                  )
                }
                _ => {
                  println!("Error: Unknown setting {}", field)
                }
              },
              Command::Load(name) => {
                let root = std::env::current_dir().unwrap();
                let mut path = root.clone();
                for n in name.split('.') {
                  path.push(n);
                }
                path.set_extension("ya");
                if let Ok(ds) = file::check_all(path, store) {
                  env.defs = ds;
                }
              }
              Command::Eval(term) => {
                let mut dag = DAG::from_term(&term);
                if env.type_system {
                  let res = infer_term(&env.defs, *term);
                  match res {
                    Ok(typ) => {
                      dag.norm(&env.defs);
                      self.println(format!("{}", dag));
                      self.println(format!(": {}", typ));
                    }
                    Err(e) => self.println(format!("Type Error: {}", e)),
                  }
                }
                else {
                  dag.norm(&env.defs);
                  self.println(format!("{}", dag));
                }
              }
              Command::Type(term) => {
                let res = infer_term(&env.defs, *term);
                match res {
                  Ok(term) => self.println(format!("{}", term)),
                  Err(e) => self.println(format!("Error: {}", e)),
                }
              }
              Command::Define(boxed) => {
                let (n, def, _) = *boxed;
                let mut tmp_defs = env.defs.clone();
                tmp_defs.insert(n.clone(), def);
                let res = check_def(&tmp_defs, &n);
                match res {
                  Ok(res) => {
                    env.defs = tmp_defs;
                    self.println(format!(
                      "{} : {}",
                      n,
                      res.pretty(Some(&n.to_string()), false)
                    ))
                  }
                  Err(e) => self.println(format!("Error: {}", e)),
                }
              }
              Command::Browse => {
                for (n, d) in env.defs.named_defs() {
                  self.println(format!("{}", d.pretty(n.to_string(), false)))
                }
              }
              Command::Quit => {
                self.println(format!("Goodbye."));
                return Err(());
              }
            };
            Ok(())
          }
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
            Ok(())
          }
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
