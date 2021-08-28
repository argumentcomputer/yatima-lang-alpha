pub mod command;
pub mod error;

use crate::{
  debug,
  file,
  store::{
    self,
    Store,
  },
};
use nom::Err;
use sp_std::{
  cell::RefCell,
  rc::Rc,
  sync::Arc,
};
use yatima_runtime::{
  run,
  transform::{
    RunIO,
    StdIORuntime,
  },
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

use command::{
  Command,
  Reference,
};
use error::ReplError;

/// Contains the state and configuration of the REPL
pub struct ReplEnv {
  type_system: bool,
  var_index: bool,
  defs: Defs,
  runtime_io: RunIO,
}

pub enum LineResult {
  Async,
  Success,
  Quit,
}

impl Default for ReplEnv {
  fn default() -> Self {
    ReplEnv {
      type_system: true,
      var_index: false,
      defs: Defs::new(),
      runtime_io: Rc::new(StdIORuntime {}),
    }
  }
}

/// Read evaluate print loop - REPL
/// A common interface for both the CLI REPL and the web REPL.
/// The design is currently based on rustyline.
pub trait Repl {
  /// Prompt and wait for the next line input.
  /// Not used in the web interface.
  fn readline(&mut self, prompt: &str) -> Result<String, ReplError>;

  /// Print results to the interface suited for this implementation.
  fn println(&self, s: String) -> Result<(), String>;

  /// Load the command history
  fn load_history(&mut self);

  /// Add a new entry to the command history
  fn add_history_entry(&mut self, s: &str);

  /// Save the command history
  fn save_history(&mut self);

  /// Get a thread safe mutable pointer to the current ReplEnv
  fn get_env(&self) -> Arc<Mutex<ReplEnv>>;

  /// Get store for this Repl
  fn get_store(&self) -> Rc<dyn Store>;

  /// Run a single line of input from the user
  /// This will mutably update the shell_state
  fn handle_line(&mut self, readline: Result<String, ReplError>) -> Result<LineResult, String> {
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
          Ok((_, command)) => match command {
            Command::Load(reference) => {
              if store.needs_callback() {
                let name = match reference {
                  Reference::FileName(n) => n.to_string(),
                  Reference::Multiaddr(n) => n.to_string(),
                  Reference::Cid(n) => n.to_string(),
                };
                let store_c = store.clone();
                let mutex_env_c = mutex_env.clone();
                store.load_by_name_with_callback(name.split('.').collect(), Box::new(move |ipld| {
                  let mut env = mutex_env_c.lock().unwrap();
                  if let Ok((_package, ds)) = file::check_all_in_ipld(ipld, store_c) {
                    env.defs.flat_merge_mut(ds);
                  }
                  else {
                    // self.println("Type checking failed.".to_owned());
                  }
                }));
                Ok(LineResult::Async)
              }
              else {
                let ipld = match reference {
                  Reference::FileName(name) => store.load_by_name(name.split('.').collect()),
                  Reference::Multiaddr(addr) => store.get_by_multiaddr(addr),
                  Reference::Cid(cid) => {
                    store.get(cid).ok_or(format!("Failed to get cid {}", cid))
                  }
                }?;

                if let Ok((_package, ds)) = file::check_all_in_ipld(ipld, store) {
                  env.defs.flat_merge_mut(ds);
                  Ok(LineResult::Success)
                }
                else {
                  Err("Type checking failed.".to_owned())
                }
              }
            }
            Command::Show { typ_, link } => {
              let var_index = env.var_index;
              match store::show(store, link, typ_, var_index) {
                Ok(s) => {
                  self.println(format!("{}", s))?;
                  Ok(LineResult::Success)
                }
                Err(s) => {
                  self.println(format!("{}", s))?;
                  Err("Show failed.".to_owned())
                }
              }
            }
            Command::Set(field, setting) => match field.as_str() {
              "type-system" => {
                env.type_system = setting;
                self.println(format!("type-system: {}", if setting { "on" } else { "off" }))?;
                Ok(LineResult::Success)
              }
              "var-index" => {
                env.var_index = setting;
                self.println(format!("var-index: {}", if setting { "on" } else { "off" }))?;
                Ok(LineResult::Success)
              }
              _ => {
                self.println(format!("Error: Unknown setting {}", field))?;
                Err("".to_owned())
              }
            },
            Command::Eval(term) => {
              let mut dag = DAG::from_term(&term);
              if env.type_system {
                let res = infer_term(&env.defs, &term, false);
                match res {
                  Ok(typ) => {
                    let mut mterm = term;
                    run(&mut mterm, Rc::new(env.defs.clone()), env.runtime_io.clone());
                    // dag.norm(&env.defs, false);
                    self.println(format!("{}", dag))?;
                    self.println(format!(": {}", typ))?;
                    Ok(LineResult::Success)
                  }
                  Err(e) => {
                    self.println(format!("Type Error: {}", e))?;
                    Err("Type Error.".to_owned())
                  }
                }
              }
              else {
                dag.norm(&env.defs, false);
                self.println(format!("{}", dag))?;
                Ok(LineResult::Success)
              }
            }
            Command::Type(term) => {
              let res = infer_term(&env.defs, &term, false);
              match res {
                Ok(term) => self.println(format!("{}", term))?,
                Err(e) => self.println(format!("Error: {}", e))?,
              };
              Ok(LineResult::Success)
            }
            Command::Define(boxed) => {
              let (n, def, _) = *boxed;
              let mut tmp_defs = env.defs.clone();
              tmp_defs.insert(n.clone(), def);
              let re = Rc::new(tmp_defs);
              let res = check_def(re.clone(), &n, false);
              match res {
                Ok(res) => {
                  env.defs.flat_merge_mut(re);
                  self.println(format!("{} : {}", n, res.pretty(Some(&n.to_string()), false)))?;
                }
                Err(e) => {
                  self.println(format!("Error: {}", e))?;
                }
              }
              Ok(LineResult::Success)
            }
            Command::Browse => {
              for (n, d) in env.defs.named_defs() {
                self.println(format!("{}", d.pretty(n.to_string(), false)))?;
              }
              Ok(LineResult::Success)
            }
            Command::Quit => {
              self.println(format!("Goodbye."))?;
              Ok(LineResult::Quit)
            }
          },
          Err(e) => {
            match e {
              Err::Incomplete(_) => {
                self.println(format!("Incomplete Input"))?;
              }
              Err::Failure(e) => {
                self.println(format!("Parse Failure:\n"))?;
                self.println(format!("{}", e))?;
              }
              Err::Error(e) => {
                self.println(format!("Parse Error:\n"))?;
                self.println(format!("{}", e))?;
              }
            };
            Err("Command Error.".to_owned())
          }
        }
      }
      Err(ReplError::Interrupted) => {
        self.println(format!("CTRL-C"))?;
        Ok(LineResult::Quit)
      }
      Err(ReplError::Eof) => {
        self.println(format!("CTRL-D"))?;
        Ok(LineResult::Quit)
      }
      Err(ReplError::Other(err)) => {
        self.println(format!("Error: {}", err))?;
        Err("Other Error".to_owned())
      }
    }
  }
}

pub fn run_repl(rl: &mut dyn Repl) {
  rl.load_history();
  loop {
    let readline = rl.readline("⅄ ");
    match rl.handle_line(readline) {
      Ok(LineResult::Success) => continue,
      Ok(LineResult::Async) => {
        rl.println("Async call initiated.".to_owned()).unwrap();
      },
      Ok(LineResult::Quit) => break,
      Err(e) => {
        debug!("handle_line: {}", e);
      }
    }
  }
  rl.save_history();
}
