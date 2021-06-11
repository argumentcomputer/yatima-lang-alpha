pub mod command;
pub mod error;

use crate::{
  file,
  store::{
    self,
    Store,
  },
};
use nom::Err;
use std::{
  rc::Rc,
  sync::{
    Arc,
    Mutex,
  },
};
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

/// Read evaluate print loop - REPL
/// A common interface for both the CLI REPL and the web REPL.
/// The design is currently based on rustyline.
pub trait Repl {
  /// Prompt and wait for the next line input.
  /// Not used in the web interface.
  fn readline(&mut self, prompt: &str) -> Result<String, ReplError>;

  /// Print results to the interface suited for this implementation.
  fn println(&self, s: String);

  /// Load the command history
  fn load_history(&mut self);

  /// Add a new entry to the command history
  fn add_history_entry(&mut self, s: &str);

  /// Save the command history
  fn save_history(&mut self);

  /// Get a thread safe mutable pointer to the current definitions
  fn get_defs(&self) -> Arc<Mutex<Defs>>;

  /// Get store for this Repl
  fn get_store(&self) -> Rc<dyn Store>;

  /// Run a single line of input from the user
  /// This will mutably update the shell_state
  fn handle_line(
    &mut self,
    readline: Result<String, ReplError>,
  ) -> Result<(), ()> {
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
              Command::Load(reference) => {
                match reference {
                  Reference::FileName(name) => {
                    // TODO make web compatible
                    store.load_by_name(name.split('.').collect());
                    let root = std::env::current_dir().unwrap();
                    let mut path = root.clone();
                    for n in name.split('.') {
                      path.push(n);
                    }
                    path.set_extension("ya");

                    if let Ok(ds) = file::check_all_in_file(path, store) {
                      *defs = ds;
                      Ok(())
                    }
                    else {
                      Err(())
                    }
                  }
                  Reference::Multiaddr(addr) => {
                    match store.get_by_multiaddr(addr) {
                      Ok(ipld) => {
                        if let Ok(ds) = file::check_all_in_ipld(ipld, store) {
                          *defs = ds;
                          Ok(())
                        } else {
                          Err(())
                        }
                      }
                      Err(_) => Err(()),
                    }
                  }
                }
              }
              Command::Show { typ_, link } => {
                match store::show(store, link, typ_) {
                  Ok(s) => {
                    self.println(format!("{}", s));
                    Ok(())
                  }
                  Err(s) => {
                    self.println(format!("{}", s));
                    Err(())
                  }
                }
              }
              Command::Eval(term) => {
                let mut dag = DAG::from_term(&term);
                dag.norm(&defs);
                self.println(format!("{}", dag));
                Ok(())
              }
              Command::Type(term) => {
                let res = infer_term(&defs, *term);
                match res {
                  Ok(term) => self.println(format!("{}", term)),
                  Err(e) => self.println(format!("Error: {}", e)),
                }
                Ok(())
              }
              Command::Define(boxed) => {
                let (n, def, _) = *boxed;
                let mut tmp_defs = defs.clone();
                tmp_defs.insert(n.clone(), def);
                let res = check_def(&tmp_defs, &n);
                match res {
                  Ok(res) => {
                    *defs = tmp_defs;
                    self.println(format!(
                      "{} : {}",
                      n,
                      res.pretty(Some(&n.to_string()))
                    ))
                  }
                  Err(e) => self.println(format!("Error: {}", e)),
                }
                Ok(())
              }
              Command::Browse => {
                for (n, d) in defs.named_defs() {
                  self.println(format!("{}", d.pretty(n.to_string())))
                }
                Ok(())
              }
              Command::Quit => {
                self.println(format!("Goodbye."));
                Ok(())
              }
            }
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
            Err(())
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
