use std::{
  io::{self, Write},
  sync::{Arc, Mutex},
  rc::Rc,
};
use rustyline::{
  error::ReadlineError,
  Cmd,
  Config,
  EditMode,
  Editor,
  KeyEvent,
};

use yatima_core::{
  defs::Defs,
};
use yatima_utils::{
  store::Store,
  repl::{
    run_repl,
    Repl,
    error::ReplError
  }
};
use crate::file::store::FileStore;


struct RustyLineRepl {
  rl: Editor<()>,
  defs: Arc<Mutex<Defs>>,
  store: Rc<FileStore>,
}

impl RustyLineRepl {
  pub fn new() -> Self {
    let config = Config::builder().edit_mode(EditMode::Vi).build();
    let mut rl = Editor::<()>::with_config(config);
    rl.bind_sequence(KeyEvent::alt('l'), Cmd::Insert(1, String::from("λ ")));
    rl.bind_sequence(KeyEvent::alt('a'), Cmd::Insert(1, String::from("∀ ")));
    let store = Rc::new(FileStore {});
    RustyLineRepl {
      rl: rl,
      defs: Arc::new(Mutex::new(Defs::new())),
      store: store,
    }
  }
}

impl Repl for RustyLineRepl {
  fn readline(&mut self, prompt: &str) -> Result<String, ReplError> {
    self.rl.readline(prompt).map_err(|e| match e {
      ReadlineError::Interrupted => ReplError::Interrupted,
      ReadlineError::Eof => ReplError::Eof,
      _ => ReplError::Other(e.to_string())
    })
  }

  fn println(&self, s: String) {
    let mut out = io::stdout();
    out.write(s.as_bytes()).unwrap();
    out.write("\n".as_bytes()).unwrap();
  }

  fn load_history(&mut self) {
    if self.rl.load_history("history.txt").is_err() {
      println!("No previous history.");
    }
  }

  fn add_history_entry(&mut self, s: &str) {
    self.rl.add_history_entry(s);
  }

  fn save_history(&mut self) {
    self.rl.save_history("history.txt").unwrap();
  }

  fn get_defs(&self) -> Arc<Mutex<Defs>> {
    self.defs.clone()
  }

  fn get_store(&self) -> Rc<dyn Store> {
    self.store.clone()
  }
}

pub fn main() {
  run_repl(&mut RustyLineRepl::new());
}
