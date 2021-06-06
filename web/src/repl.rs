use std::{
  path::PathBuf,
  rc::Rc,
  sync::{
    Arc,
    Mutex,
  },
};
use yatima_core::defs::Defs;
use yatima_utils::{
  file::parse::{
    self,
    PackageEnv,
  },
  repl::{
    error::ReplError,
    Repl,
  },
  store::Store,
};
// use wasm_bindgen_futures::JsFuture;

use crate::{
  store::WebStore,
  utils::{
    self,
    log,
  },
};
use wasm_bindgen::{
  prelude::*,
  JsCast,
};
use xterm_js_rs::{
  addons::{
    fit::FitAddon,
    search::SearchAddon,
    web_links::WebLinksAddon,
  },
  OnKeyEvent,
  Terminal,
  TerminalOptions,
  Theme,
};

const PROMPT: &str = "⅄ ";

fn prompt(term: &Terminal) {
  term.writeln("");
  term.write(PROMPT);
}

// Keyboard keys
// https://notes.burke.libbey.me/ansi-escape-codes/
const KEY_ENTER: u32 = 13;
const KEY_BACKSPACE: u32 = 8;
const KEY_LEFT_ARROW: u32 = 37;
const KEY_RIGHT_ARROW: u32 = 39;
const KEY_UP_ARROW: u32 = 38;
const KEY_DOWN_ARROW: u32 = 40;
const KEY_C: u32 = 67;
const KEY_V: u32 = 86;
const KEY_L: u32 = 76;

const CURSOR_LEFT: &str = "\x1b[D";
const CURSOR_RIGHT: &str = "\x1b[C";
const CURSOR_UP: &str = "\x1b[A";
const CURSOR_DOWN: &str = "\x1b[B";

struct WebRepl {
  terminal: Terminal,
  defs: Arc<Mutex<Defs>>,
  shell_state: Arc<Mutex<ShellState>>,
  store: Rc<WebStore>,
  history: Vec<String>,
}

#[derive(Debug, Clone)]
struct ShellState {
  line: String,
  cursor_col: usize,
}

impl WebRepl {
  pub fn new() -> Self {
    let defs = Arc::new(Mutex::new(Defs::new()));
    let terminal: Terminal = Terminal::new(
      TerminalOptions::new()
        .with_rows(50)
        .with_cursor_blink(true)
        .with_cursor_width(10)
        .with_font_size(20)
        .with_draw_bold_text_in_bright_colors(true)
        .with_right_click_selects_word(true)
        .with_theme(
          Theme::new().with_foreground("#98FB98").with_background("#000000"),
        ),
    );

    let elem = web_sys::window()
      .unwrap()
      .document()
      .unwrap()
      .get_element_by_id("terminal")
      .unwrap();

    terminal.writeln("Yatima REPL");
    terminal.writeln("Supported keys in this example:");
    terminal.writeln(
      " <Printable-Characters> <Enter> <Backspace> <Left-Arrow> <Right-Arrow> \
       <Ctrl-C> <Ctrl-L>",
    );
    terminal.open(elem.dyn_into().unwrap());
    prompt(&terminal);

    let line = String::new();
    let cursor_col = 0;
    let shell_state = Arc::new(Mutex::new(ShellState { line, cursor_col }));

    let fit_addon = FitAddon::new();
    terminal
      .load_addon(fit_addon.clone().dyn_into::<FitAddon>().unwrap().into());
    let search_addon = SearchAddon::new();
    terminal.load_addon(
      search_addon.clone().dyn_into::<SearchAddon>().unwrap().into(),
    );
    let web_links_addon = WebLinksAddon::new(None, None, None);
    terminal.load_addon(
      web_links_addon.clone().dyn_into::<WebLinksAddon>().unwrap().into(),
    );

    fit_addon.fit();
    terminal.focus();
    let store = Rc::new(WebStore::new());
    WebRepl { terminal, defs, shell_state, store, history: Vec::new() }
  }

  pub fn handle_event(&mut self, e: OnKeyEvent) {
    let shell_state = self.shell_state.lock().unwrap();
    let mut cursor_col = shell_state.cursor_col.clone();
    let mut line = shell_state.line.clone();

    // We are done reading from the shell_state
    drop(shell_state);
    let term: Terminal = self.terminal.clone().dyn_into().unwrap();
    let event = e.dom_event();
    match event.key_code() {
      KEY_ENTER => {
        if !line.is_empty() {
          term.writeln("");
          match self.handle_line(Ok(line.clone())) {
            Ok(()) => term.writeln("Ok"),
            Err(()) => term.writeln("Error"),
          }
          line.clear();
          cursor_col = 0;
        }
        prompt(&term);
      }
      KEY_BACKSPACE => {
        if cursor_col > 0 {
          term.write("\u{0008} \u{0008}");
          line.pop();
          cursor_col -= 1;
        }
      }
      KEY_LEFT_ARROW => {
        if cursor_col > 0 {
          term.write(CURSOR_LEFT);
          cursor_col -= 1;
        }
      }
      KEY_RIGHT_ARROW => {
        if cursor_col < line.len() {
          term.write(CURSOR_RIGHT);
          cursor_col += 1;
        }
      }
      KEY_UP_ARROW => {
        if cursor_col < line.len() {
          term.write(CURSOR_UP);
        }
      }
      KEY_DOWN_ARROW => {
        if cursor_col < line.len() {
          term.write(CURSOR_DOWN);
        }
      }
      KEY_L if event.ctrl_key() => term.clear(),
      KEY_V if event.ctrl_key() => {
        // prompt(&term);
        // line.clear();
        log("click v");
        term.paste("hello");
      }
      KEY_C if event.ctrl_key() && event.shift_key() => {
        prompt(&term);
        line.clear();
      }
      KEY_C if event.ctrl_key() => {
        prompt(&term);
        line.clear();
        cursor_col = 0;
      }
      _ => {
        if !event.alt_key()
          && !event.alt_key()
          && !event.ctrl_key()
          && !event.meta_key()
        {
          let s = &event.key();
          term.write(s);
          line.push_str(s);
          cursor_col += s.len();
        }
      }
    }
    self.save_history();
    let mut shell_state = self.shell_state.lock().unwrap();
    *shell_state = ShellState { cursor_col, line }
  }
}

impl Repl for WebRepl {
  fn readline(&mut self, _prompt: &str) -> Result<String, ReplError> {
    Ok(self.shell_state.lock().unwrap().clone().line)
  }

  fn get_defs(&self) -> Arc<Mutex<Defs>> { self.defs.clone() }

  fn get_store(&self) -> Rc<dyn Store> { self.store.clone() }

  fn println(&self, s: String) { self.terminal.writeln(s.as_str()); }

  fn load_history(&mut self) {
    let window =
      web_sys::window().expect("should have a window in this context");
    let storage =
      window.local_storage().expect("should have local storage").unwrap();
    if let Ok(Some(text)) = storage.get("history.txt") {
      let split: Vec<&str> = text.split("\n").collect();
      for s in split {
        self.history.push(s.to_owned());
      }
      log("History loaded");
    }
    else {
      log("Could not load history");
    }
  }

  fn add_history_entry(&mut self, s: &str) { self.history.push(s.to_owned()); }

  fn save_history(&mut self) {
    let window =
      web_sys::window().expect("should have a window in this context");
    let storage =
      window.local_storage().expect("should have local storage").unwrap();
    let history = self.history.join("\n");
    if let Ok(()) = storage.set("history.txt", &history) {
      log("History saved");
    }
    else {
      log("Could not save history");
    }
  }
}

#[wasm_bindgen(start)]
pub fn main() -> Result<(), JsValue> {
  utils::set_panic_hook();

  let mut repl = WebRepl::new();
  repl.load_history();

  let terminal: Terminal = repl.terminal.clone().dyn_into().unwrap();

  let callback =
    Closure::wrap(
      Box::new(move |e: OnKeyEvent| repl.handle_event(e)) as Box<dyn FnMut(_)>
    );

  terminal.on_key(callback.as_ref().unchecked_ref());

  callback.forget();

  Ok(())
}

#[wasm_bindgen]
pub fn parse_source(source: &str) {
  let store = Rc::new(WebStore::new());
  let env = PackageEnv::new(PathBuf::new(), PathBuf::new(), store.clone());
  let (cid, p, _d) = parse::parse_text(source.to_owned(), env);
  log(&format!("parsed {} {:#?}", cid, p));
}
