use std::{
  collections::VecDeque,
  convert::TryInto,
  path::PathBuf,
  rc::Rc,
  sync::{Arc, Mutex},
};
use unicode_segmentation::{GraphemeCursor, UnicodeSegmentation};
use wasm_bindgen::{prelude::*, JsCast};
use xterm_js_rs::{
  addons::{fit::FitAddon, search::SearchAddon, web_links::WebLinksAddon},
  Terminal, TerminalOptions, Theme,
  FontWeight,
};
use yatima_utils::{
  file::parse::{self, PackageEnv},
  log,
  logging::log,
  repl::{error::ReplError, LineResult, Repl, ReplEnv},
  store::Store,
};
// use wasm_bindgen_futures::JsFuture;
use crate::{
  store::WebStore,
  terminal_sequences::terminal_sequences,
  utils::{self},
};

// Assumes this has no tab graphemes.
const PROMPT: &str = "⅄ ";
const TAB_WIDTH: usize = 2;

fn prompt(term: &Terminal) {
  term.writeln("");
  term.write(PROMPT);
}

fn clear_line(term: &Terminal) {
  term.write(&terminal_sequences::erase_in_line(2));
  term.write(&terminal_sequences::cursor_horizontal_absolute(0));
  term.write(PROMPT);
}

fn insert_mode(term: &Terminal) {
  term.write(&terminal_sequences::set_mode(vec![4]));
}

fn replace_mode(term: &Terminal) {
  term.write(&terminal_sequences::reset_mode(vec![4]));
}

#[derive(Debug, Clone)]
struct ShellState {
  cursor: GraphemeCursor,
  line: String,
  history_index: usize,
}

impl Default for ShellState {
  fn default() -> Self {
    ShellState {
      cursor: GraphemeCursor::new(0, 0, true),
      line: String::new(),
      history_index: 0,
    }
  }
}

struct WebRepl {
  terminal: Terminal,
  env: Arc<Mutex<ReplEnv>>,
  shell_state: Arc<Mutex<ShellState>>,
  store: Rc<WebStore>,
  history: VecDeque<String>,
}

impl Repl for WebRepl {
  fn readline(&mut self, _prompt: &str) -> Result<String, ReplError> {
    Ok(self.shell_state.lock().unwrap().clone().line)
  }

  fn get_env(&self) -> Arc<Mutex<ReplEnv>> {
    self.env.clone()
  }

  fn get_store(&self) -> Rc<dyn Store> {
    self.store.clone()
  }

  fn println(&self, s: String) {
    // The term needs \r to move the cursor back to the start of the line
    let m = s.replace(terminal_sequences::LF, &(terminal_sequences::LF.to_owned() + terminal_sequences::CR));
    self.terminal.writeln(m.as_str());
  }

  fn load_history(&mut self) {
    let window =
      web_sys::window().expect("should have a window in this context");
    let storage =
      window.local_storage().expect("should have local storage").unwrap();
    if let Ok(Some(text)) = storage.get("history.txt") {
      let split: Vec<&str> = text.split(terminal_sequences::LF).collect();
      for s in split {
        self.history.push_front(s.to_owned());
      }
      // Blank line is the current line
      self.history.push_front("".to_owned());
      log("History loaded");
    } else {
      log("Could not load history");
    }
  }

  fn add_history_entry(&mut self, s: &str) {
    self.history.push_front(s.to_owned());
  }

  fn save_history(&mut self) {
    let window =
      web_sys::window().expect("should have a window in this context");
    let storage =
      window.local_storage().expect("should have local storage").unwrap();

    let v: Vec<String> = self.history.clone().into();
    let history = v.join(terminal_sequences::LF);
    if let Ok(()) = storage.set("history.txt", &history) {
      log("History saved");
    } else {
      log("Could not save history");
    }
  }
}

// Solarized colors
// const BASE03: &str = "#002b36";
// const BASE02: &str = "#073642";
// const BASE01: &str = "#586e75";
const BASE00: &str = "#657b83";
// const BASE0:  &str = "#839496";
const BASE1:  &str = "#93a1a1";
const BASE2:  &str = "#eee8d5";
const BASE3:  &str = "#fdf6e3";
const YELLOW: &str = "#b58900";
// const ORANGE: &str = "#cb4b16";
const RED:    &str = "#dc322f";
const MAGENTA:&str = "#d33682";
// const VIOLET: &str = "#6c71c4";
const BLUE:   &str = "#268bd2";
const CYAN:   &str = "#2aa198";
const GREEN:  &str = "#859900";

fn solarized_light_theme() -> Theme {
  let theme = Theme::new();
  theme
    .with_foreground(BASE00)
    .with_background(BASE3)
    .with_bright_yellow(BASE00)
    .with_bright_cyan(BASE1)
    .with_white(BASE2)
    .with_bright_white(BASE3)
    .with_yellow(YELLOW)
    .with_red(RED)
    .with_magenta(MAGENTA)
    .with_blue(BLUE)
    .with_cyan(CYAN)
    .with_green(GREEN);
  theme
}

fn terminal_options() -> TerminalOptions {
  let terminal_options = TerminalOptions::new();
  terminal_options
    .with_rows(50)
    .with_cursor_blink(true)
    .with_cursor_width(10)
    .with_font_size(18)
    .with_draw_bold_text_in_bright_colors(true)
    .with_right_click_selects_word(true)
    .with_theme(&solarized_light_theme())
    .with_font_family("InconsolataLGCRegular")
    .with_font_weight(FontWeight::Normal)
    .with_tab_stop_width(TAB_WIDTH.try_into().unwrap());
  terminal_options
}

impl WebRepl {
  pub fn new() -> Self {
    let terminal: Terminal = Terminal::new(
      &terminal_options()
    );

    let elem = web_sys::window()
      .unwrap()
      .document()
      .unwrap()
      .get_element_by_id("terminal")
      .unwrap();

    terminal.writeln("Yatima REPL");
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
    let env = Arc::new(Mutex::new(ReplEnv::default()));
    insert_mode(&terminal);
    WebRepl { terminal, env, shell_state, store, history: VecDeque::new() }
  }

  pub fn get_terminal(&self) -> Terminal {
    self.terminal.clone().dyn_into().unwrap()
  }

  fn update_shell_state(&mut self, new_state: ShellState) {
    let mut shell_state = self.shell_state.lock().unwrap();
    *shell_state = new_state;
  }

  pub fn read_shell_state(&self) -> ShellState {
    let shell_state = self.shell_state.lock().unwrap();
    shell_state.clone()
  }

  // TODO: (properly) handle newline graphemes in handling of left, right, ^left, ^right, home, end, delete and backspace keys.
  pub fn handle_data(&mut self, data: String) {
    log("");
    let mut ss = self.read_shell_state();
    let term: Terminal = self.get_terminal();
    match data.as_str() {
      // Move one grapheme forwards.
      terminal_sequences::RIGHT => {
        if let (true, Ok(Some(next_boundary))) = (
          ss.cursor.cur_cursor() < ss.line.len(),
          ss.cursor.clone().next_boundary(&ss.line, 0),
        ) {
          match &ss.line[ss.cursor.cur_cursor()..next_boundary] {
            terminal_sequences::HT => {
              term.write(&terminal_sequences::cursor_forwards(TAB_WIDTH));
            }
            _ => {
              term.write(&terminal_sequences::cursor_forwards(1));
            }
          }
          ss.cursor.set_cursor(next_boundary);
        }
      }
      // Move one grapheme backwards.
      terminal_sequences::LEFT => {
        if let (true, Ok(Some(prev_boundary))) = (
          ss.cursor.cur_cursor() > 0,
          ss.cursor.clone().prev_boundary(&ss.line, 0),
        ) {
          match &ss.line[prev_boundary..ss.cursor.cur_cursor()] {
            terminal_sequences::HT => {
              term.write(&terminal_sequences::cursor_backwards(TAB_WIDTH));
            }
            _ => {
              term.write(&terminal_sequences::cursor_backwards(1));
            }
          }
          ss.cursor.set_cursor(prev_boundary);
        }
      }
      // Move one word boundary backwards.
      terminal_sequences::CTRL_LEFT => {
        if ss.cursor.cur_cursor() > 0 {
          let mut word_bound_index =
            ss.line.split_word_bound_indices().rev().peekable();
          while let Some(&(prev_boundary, _)) = word_bound_index.peek() {
            if prev_boundary < ss.cursor.cur_cursor() {
              if &ss.line[prev_boundary..ss.cursor.cur_cursor()]
                == terminal_sequences::HT
              {
                term.write(&terminal_sequences::cursor_backwards(TAB_WIDTH));
              } else {
                term.write(&terminal_sequences::cursor_backwards(
                  ss.cursor.cur_cursor() - prev_boundary,
                ));
              }
              ss.cursor.set_cursor(prev_boundary);
              break;
            } else {
              word_bound_index.next();
            }
          }
        }
      }
      // Move one word boundary forwards.
      terminal_sequences::CTRL_RIGHT => {
        if ss.cursor.cur_cursor() < ss.line.len() {
          let mut word_bound_index =
            ss.line.split_word_bound_indices().peekable();
          while let Some(&(next_boundary, _)) = word_bound_index.peek() {
            if next_boundary > ss.cursor.cur_cursor() {
              if &ss.line[ss.cursor.cur_cursor()..next_boundary]
                == terminal_sequences::HT
              {
                term.write(&terminal_sequences::cursor_forwards(TAB_WIDTH));
              } else {
                term.write(&terminal_sequences::cursor_forwards(
                  next_boundary - ss.cursor.cur_cursor(),
                ));
              }
              ss.cursor.set_cursor(next_boundary);
              break;
            } else {
              word_bound_index.next();
            }
          }
          if word_bound_index.peek().is_none() {
            term.write(&terminal_sequences::cursor_forwards(
              UnicodeSegmentation::graphemes(
                &ss.line[ss.cursor.cur_cursor()..ss.line.len()],
                true,
              )
              .count(),
            ));
            ss.cursor.set_cursor(ss.line.len());
          }
        }
      }
      // Move to start of line.
      terminal_sequences::HOME => {
        if ss.cursor.cur_cursor() > 0 {
          term.write(&terminal_sequences::cursor_horizontal_absolute(0));
          term.write(&terminal_sequences::cursor_forwards(
            UnicodeSegmentation::graphemes(PROMPT, true).count(),
          ));
          ss.cursor.set_cursor(0);
        }
      }
      // Move to end of line.
      terminal_sequences::END => {
        if ss.cursor.cur_cursor() < ss.line.len() {
          let (tabs, non_tabs) = UnicodeSegmentation::graphemes(
            &ss.line[ss.cursor.cur_cursor()..ss.line.len()],
            true,
          )
          .fold((0, 0), |acc, grapheme| {
            if grapheme == terminal_sequences::HT {
              (acc.0 + 1, acc.1)
            } else {
              (acc.0, acc.1 + 1)
            }
          });
          term.write(&terminal_sequences::cursor_forwards(
            (tabs * TAB_WIDTH) + non_tabs,
          ));
          ss.cursor.set_cursor(ss.line.len());
        }
      }
      // Load previous saved line.
      terminal_sequences::UP => {
        if ss.history_index < self.history.len() - 1 {
          ss.history_index += 1;
          if let Some(l) = self.history.get(ss.history_index) {
            ss.line = l.clone();
            clear_line(&term);
            term.write(&ss.line);
            ss.cursor = GraphemeCursor::new(ss.line.len(), ss.line.len(), true);
          }
        }
        self.save_history();
      }
      // Load next saved line.
      terminal_sequences::DOWN => {
        if ss.history_index > 0 {
          ss.history_index -= 1;
          if let Some(l) = self.history.get(ss.history_index) {
            ss.line = l.clone();
            clear_line(&term);
            term.write(&ss.line);
            ss.cursor = GraphemeCursor::new(ss.line.len(), ss.line.len(), true);
          }
        }
        self.save_history();
      }
      // Delete previous grapheme.
      terminal_sequences::BS | "\u{7f}" => {
        // Backspace should only work if the cursor is on a grapheme boundary > the first grapheme boundary (which should be the beginning of the line).
        if let (true, Ok(true), Ok(Some(prev_boundary))) = (
          ss.cursor.cur_cursor() > 0,
          ss.cursor.is_boundary(&ss.line, 0),
          ss.cursor.clone().prev_boundary(&ss.line, 0),
        ) {
          match &ss.line[prev_boundary..ss.cursor.cur_cursor()] {
            terminal_sequences::HT => {
              term.write(&terminal_sequences::cursor_backwards(TAB_WIDTH));
              term.write(&terminal_sequences::delete_characters(TAB_WIDTH));
            }
            _ => {
              term.write(&terminal_sequences::cursor_backwards(1));
              term.write(&terminal_sequences::delete_characters(1));
            }
          }
          ss.line.replace_range(prev_boundary..ss.cursor.cur_cursor(), "");
          ss.cursor = GraphemeCursor::new(prev_boundary, ss.line.len(), true);
          self.save_history();
        }
      }
      // Evaluate and save line.
      terminal_sequences::CR => {
        if !ss.line.is_empty() {
          self.println("".to_owned());
          match self.handle_line(Ok(ss.line.clone())) {
            Ok(LineResult::Success) => term.writeln("Ok"),
            Ok(LineResult::Quit) => term.writeln("Quit"),
            Err(()) => term.writeln("Error"),
          }
          ss.line.clear();
          ss.cursor = GraphemeCursor::new(0, 0, true);
          ss.history_index = 0;
        }
        prompt(&term);
        self.save_history();
      }
      // Delete current grapheme.
      terminal_sequences::DELETE => {
        // Delete should only work if the cursor is on a grapheme boundary < the last grapheme boundary (which should be the end of the line).
        if let (true, Ok(true), Ok(Some(next_boundary))) = (
          ss.cursor.cur_cursor() < ss.line.len(),
          ss.cursor.is_boundary(&ss.line, 0),
          ss.cursor.clone().next_boundary(&ss.line, 0),
        ) {
          match &ss.line[ss.cursor.cur_cursor()..next_boundary] {
            terminal_sequences::HT => {
              term.write(&terminal_sequences::delete_characters(TAB_WIDTH));
            }
            _ => {
              term.write(&terminal_sequences::delete_characters(1));
            }
          }
          ss.line.replace_range(ss.cursor.cur_cursor()..next_boundary, "");
          ss.cursor =
            GraphemeCursor::new(ss.cursor.cur_cursor(), ss.line.len(), true);
          self.save_history();
        }
      }
      // Cancel current evaluation.
      terminal_sequences::CTRL_C => {
        term.write(&data);
        ss.line.clear();
        clear_line(&term);
        ss.cursor = GraphemeCursor::new(0, 0, true);
        self.save_history();
      }
      // Clear line.
      terminal_sequences::CTRL_L => {
        ss.line.clear();
        clear_line(&term);
        ss.cursor = GraphemeCursor::new(0, 0, true);
        self.save_history();
      }
      _ => {
        if &data == terminal_sequences::HT {
          term.write(&" ".repeat(TAB_WIDTH));
        } else {
          term.write(&data);
        }
        ss.line.insert_str(ss.cursor.cur_cursor(), &data);
        ss.cursor = GraphemeCursor::new(
          ss.cursor.cur_cursor() + data.len(),
          ss.line.len(),
          true,
        );
        self.history.pop_front();
        self.history.push_front(ss.line.clone());
        self.save_history();
      }
    }
    log!("cursor: {:X?}", ss.cursor.cur_cursor());
    log!("line: {:X?}", ss.line);
    log!("line.len: {:X?}", ss.line.len());
    log!("data: {:X?}", data);
    log!("data.len: {:X?}", data.len());
    log!("is_boundary: {:X?}", ss.cursor.is_boundary(&ss.line, 0));
    log!("prev_boundary: {:X?}", ss.cursor.clone().prev_boundary(&ss.line, 0));
    log!("next_boundary: {:X?}", ss.cursor.clone().next_boundary(&ss.line, 0));
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

  let repl_p2 = repl_p.clone();
  let data_callback = Closure::wrap(Box::new(move |data: String| {
    let mut repl = repl_p2.lock().unwrap();
    repl.handle_data(data);
  }) as Box<dyn FnMut(_)>);

  terminal.on_data(data_callback.as_ref().unchecked_ref());

  data_callback.forget();

  Ok(())
}

#[wasm_bindgen]
pub fn parse_source(source: &str) -> Result<JsValue, JsValue> {
  let store = Rc::new(WebStore::new());
  let env = PackageEnv::new(PathBuf::new(), PathBuf::new(), store.clone());
  let (cid, p, _) = parse::parse_text(&source, env)?;
  log(&format!("parsed {} {:#?}", cid, p));
  Ok("ok".into())
}
