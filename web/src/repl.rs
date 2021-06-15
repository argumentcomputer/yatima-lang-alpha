use std::{
  path::PathBuf,
  rc::Rc,
  sync::{
    Arc,
    Mutex,
  },
  collections::VecDeque,
};
use yatima_core::defs::Defs;
use yatima_utils::{
  log,
  logging::log,
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
// const KEY_V: u32 = 86;
const KEY_L: u32 = 76;

const CLEAR_LINE: &str = "\x1b[2K";
const CURSOR_LEFT: &str = "\x1b[D";
const CURSOR_RIGHT: &str = "\x1b[C";
const CURSOR_UP: &str = "\x1b[A";
const CURSOR_DOWN: &str = "\x1b[B";
const BACKSPACE: &str = "\x1b[H";
const RETURN: &str = "\x1b[M";
const LINEFEED: &str = "\x1b[J";
const DELETE: &str = "\x1b[3";


fn set_column(term: &Terminal, col: u32) {
  term.write(&format!("\x1b[{}G", col));
}

fn clear_line(term: &Terminal) {
  term.write(CLEAR_LINE);
  set_column(&term, 0);
}

struct WebRepl {
  terminal: Terminal,
  defs: Arc<Mutex<Defs>>,
  shell_state: Arc<Mutex<ShellState>>,
  store: Rc<WebStore>,
  history: VecDeque<String>,
}

#[derive(Debug, Clone)]
struct ShellState {
  line: String,
  cursor_col: usize,
  history_index: usize,
}

impl Default for ShellState {
  fn default() -> Self {
    ShellState {
      line: String::new(),
      cursor_col: 0,
      history_index: 0,
    }
  }
}

impl Repl for WebRepl {
  fn readline(&mut self, _prompt: &str) -> Result<String, ReplError> {
    Ok(self.shell_state.lock().unwrap().clone().line)
  }

  fn get_defs(&self) -> Arc<Mutex<Defs>> { self.defs.clone() }

  fn get_store(&self) -> Rc<dyn Store> { self.store.clone() }

  fn println(&self, s: String) { 
    // The term needs \r to move the cursor back to the start of the line
    let m = s.replace("\n", "\r");
    self.terminal.writeln(m.as_str());
  }

  fn load_history(&mut self) {
    let window =
      web_sys::window().expect("should have a window in this context");
    let storage =
      window.local_storage().expect("should have local storage").unwrap();
    if let Ok(Some(text)) = storage.get("history.txt") {
      let split: Vec<&str> = text.split("\n").collect();
      for s in split {
        self.history.push_front(s.to_owned());
      }
      // Blank line is the current line
      self.history.push_front("".to_owned());
      log("History loaded");
    }
    else {
      log("Could not load history");
    }
  }

  fn add_history_entry(&mut self, s: &str) { self.history.push_front(s.to_owned()); }

  fn save_history(&mut self) {
    let window =
      web_sys::window().expect("should have a window in this context");
    let storage =
      window.local_storage().expect("should have local storage").unwrap();

    let v: Vec<String> = self.history.clone().into();
    let history = v.join("\n");
    if let Ok(()) = storage.set("history.txt", &history) {
      log("History saved");
    }
    else {
      log("Could not save history");
    }
  }
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

    let shell_state = Arc::new(Mutex::new(ShellState::default()));

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
    WebRepl { terminal, defs, shell_state, store, history: VecDeque::new() }
  }

  pub fn get_terminal(&self) -> Terminal {
    self.terminal.clone().dyn_into().unwrap()
  }

  pub fn handle_event(&mut self, e: OnKeyEvent) {
    let shell_state = self.shell_state.lock().unwrap();
    let mut cursor_col = shell_state.cursor_col.clone();
    let mut history_index = shell_state.history_index.clone();
    let mut line = shell_state.line.clone();

    // We are done reading from the shell_state
    drop(shell_state);
    let term: Terminal = self.get_terminal();
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
          history_index = 0;
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
        if history_index < self.history.len() - 1 {
          history_index += 1;
          if let Some(l) = self.history.get(history_index) {
            line = l.clone();
            clear_line(&term);
            term.write(PROMPT);
            term.write(&line);
            cursor_col = line.len();
          }
        }
      }
      KEY_DOWN_ARROW => {
        if history_index > 0 {
          history_index -= 1;
          if let Some(l) = self.history.get(history_index) {
            line = l.clone();
            clear_line(&term);
            term.write(PROMPT);
            term.write(&line);
            cursor_col = line.len();
          }
        }
      }
      KEY_L if event.ctrl_key() => term.clear(),
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
          // let s = &event.key();
          // self.write(s);
        }
      }
    }
    self.save_history();
    self.update_shell_state(ShellState { cursor_col, line, history_index });
  }

  pub fn read_shell_state(&self) -> ShellState {

    let shell_state = self.shell_state.lock().unwrap();
    shell_state.clone()
  }

  fn update_shell_state(&mut self, new_state: ShellState) {
    let mut shell_state = self.shell_state.lock().unwrap();
    *shell_state = new_state;
  }

  pub fn handle_data(&mut self, data: String) {
    let mut ss = self.read_shell_state();
    let term: Terminal = self.get_terminal();
    log!("line: {:X?}", &ss.line);
    log!("data: {:X?}", &data);

    match data.as_str() {
      CURSOR_RIGHT => {
        if ss.cursor_col < ss.line.len() {
          term.write(CURSOR_RIGHT);
          ss.cursor_col += 1;
        }
      }
      CURSOR_LEFT => {
        if ss.cursor_col > 0 {
          term.write(CURSOR_LEFT);
          ss.cursor_col -= 1;
        }
      } 
      CURSOR_UP => {
        if ss.history_index < self.history.len() - 1 {
          ss.history_index += 1;
          if let Some(l) = self.history.get(ss.history_index) {
            ss.line = l.clone();
            clear_line(&term);
            term.write(PROMPT);
            term.write(&ss.line);
            ss.cursor_col = ss.line.len();
          }
        }
      } 
      CURSOR_DOWN => {
        if ss.history_index > 0 {
          ss.history_index -= 1;
          if let Some(l) = self.history.get(ss.history_index) {
            ss.line = l.clone();
            clear_line(&term);
            term.write(PROMPT);
            term.write(&ss.line);
            ss.cursor_col = ss.line.len();
          }
        }
      }
      BACKSPACE | "\u{7f}" => {
        log("backspace");
        if ss.cursor_col > 0 {
          term.write("\u{0008} \u{0008}");
          ss.line.pop();
          ss.cursor_col -= 1;
        }
      } 
      RETURN | LINEFEED | "\n" | "\r" => {
        if !ss.line.is_empty() {
          self.println("".to_owned());
          match self.handle_line(Ok(ss.line.clone())) {
            Ok(()) => term.writeln("Ok"),
            Err(()) => term.writeln("Error"),
          }
          ss.line.clear();
          ss.cursor_col = 0;
          ss.history_index = 0;
        }
        prompt(&term);
      }
      DELETE => {
        if ss.cursor_col > 0 {
          term.write("\u{7f}");
          ss.line.pop();
          ss.cursor_col -= 1;
        }
      }
      _ if data.starts_with("\x1b") => {
        log("ESC");
      } 
      _ => {
        term.write(&data);
        ss.line.push_str(&data);
        ss.cursor_col += data.len();
        self.history.pop_front();
        self.history.push_front(ss.line.clone());
      }
    }
    self.update_shell_state(ss);
  }
}

#[wasm_bindgen(start)]
pub fn main() -> Result<(), JsValue> {
  utils::set_panic_hook();

  let repl_p = Arc::new(Mutex::new(WebRepl::new()));
  let mut repl = repl_p.lock().unwrap();
  repl.load_history();

  let terminal: Terminal = repl.terminal.clone().dyn_into().unwrap();

  let repl_p1 = repl_p.clone();
  let callback =
    Closure::wrap(
      Box::new(move |e: OnKeyEvent| {
        let mut repl = repl_p1.lock().unwrap();
        repl.handle_event(e)
      }) as Box<dyn FnMut(_)>
    );

  // terminal.on_key(callback.as_ref().unchecked_ref());

  callback.forget();

  let repl_p2 = repl_p.clone();
  let data_callback =
    Closure::wrap(
      Box::new(move |data: String| {
        let mut repl = repl_p2.lock().unwrap();
        repl.handle_data(data);
      }) as Box<dyn FnMut(_)>
    );

  terminal.on_data(data_callback.as_ref().unchecked_ref());

  data_callback.forget();

  Ok(())
}

#[wasm_bindgen]
pub fn parse_source(source: &str) {
  let store = Rc::new(WebStore::new());
  let env = PackageEnv::new(PathBuf::new(), PathBuf::new(), store.clone());
  let (cid, p, _d) = parse::parse_text(source.to_owned(), env);
  log(&format!("parsed {} {:#?}", cid, p));
}
