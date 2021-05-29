use std::sync::{Arc, Mutex};
use yatima_utils::{
  file::parse,
  repl::{
    Repl,
    run_repl,
    error::ReplError,
  }
};
use yatima_core::{
  defs::Defs,
};
// use wasm_bindgen_futures::JsFuture;

use crate::utils;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use xterm_js_rs::addons::fit::FitAddon;
use xterm_js_rs::{OnKeyEvent, Terminal, TerminalOptions, Theme};

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

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
const KEY_C: u32 = 67;
const KEY_L: u32 = 76;

const CURSOR_LEFT: &str = "\x1b[D";
const CURSOR_RIGHT: &str = "\x1b[C";

struct WebRepl {
  terminal: Terminal,
  defs: Arc<Mutex<Defs>>,
  shell_state: Arc<Mutex<ShellState>>,
}

#[derive(Debug,Clone)]
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
                Theme::new()
                    .with_foreground("#98FB98")
                    .with_background("#000000"),
            ),
    );

    let elem = web_sys::window()
        .unwrap()
        .document()
        .unwrap()
        .get_element_by_id("terminal")
        .unwrap();

    terminal.writeln("Supported keys in this example: <Printable-Characters> <Enter> <Backspace> <Left-Arrow> <Right-Arrow> <Ctrl-C> <Ctrl-L>");
    terminal.open(elem.dyn_into().unwrap());
    prompt(&terminal);

    let line = String::new();
    let cursor_col = 0;
    let shell_state = Arc::new(Mutex::new(ShellState { line, cursor_col }));


    let addon = FitAddon::new();
    terminal.load_addon(addon.clone().dyn_into::<FitAddon>().unwrap().into());
    addon.fit();
    terminal.focus();
    WebRepl { terminal, defs, shell_state }
  }

  pub fn handle_event(&mut self, e: OnKeyEvent) {
    let mut shell_state = self.shell_state.lock().unwrap();
    let mut cursor_col = shell_state.cursor_col;
    let mut line = shell_state.line.clone();
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
      KEY_L if event.ctrl_key() => term.clear(),
      KEY_C if event.ctrl_key() => {
        prompt(&term);
        line.clear();
        cursor_col = 0;
      }
      _ => {
        if !event.alt_key() && !event.alt_key() && !event.ctrl_key() && !event.meta_key() {
          term.write(&event.key());
          line.push_str(&e.key());
          cursor_col += 1;
        }
      }
    }
    *shell_state = ShellState { cursor_col, line }
  }

}

impl Repl for WebRepl {
  fn readline(&mut self, _prompt: &str) -> Result<String, ReplError> {

    Ok(self.shell_state.lock().unwrap().clone().line)
  }

  fn get_defs(&self) -> Arc<Mutex<Defs>> {
    self.defs.clone()
  }
  
  fn println(&self, s: String) {
    self.terminal.writeln(s.as_str());
  }

  fn load_history(&mut self) {
    // TODO
  }

  fn add_history_entry(&self, s: &str) {
    // TODO
  }

  fn save_history(&mut self) {
    // TODO
  }
}

#[wasm_bindgen(start)]
pub fn main() -> Result<(), JsValue> {
    utils::set_panic_hook();

    let mut repl = WebRepl::new();

    // let term: Terminal = terminal.clone().dyn_into().unwrap();
    let terminal: Terminal = repl.terminal.clone().dyn_into().unwrap();

    let callback = Closure::wrap(Box::new(move |e: OnKeyEvent| repl.handle_event(e)) as Box<dyn FnMut(_)>);

    terminal.on_key(callback.as_ref().unchecked_ref());

    callback.forget();

    Ok(())
}

#[wasm_bindgen]
pub fn run_repl_line(line: &str) {
}

#[wasm_bindgen]
pub fn parse_source(source: &str) {
  // alert(&format!("Parsing:\n{}", source));
  // let hashspace =
  //   hashspace::Hashspace::with_hosts(vec!["localhost:8000".to_string()]);
  // let store = FileStore {};
  // let env = file::parse::PackageEnv::new(root, path, store);

  // let (_, p, ..) = parse::parse_text(&source, env);
  // log(&format!(
  //   "Package parsed:\n{}",
  //   p
  //   // hashspace::HashspaceImplWrapper::wrap(&hashspace, &p)
  // ));

  // Ok(JsValue::from_serde(&p.to_string()).unwrap())
}
