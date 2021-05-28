pub mod command;
pub mod error;

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
    fn add_history_entry(&mut self, s: &str);
    fn save_history(&mut self);
}

pub fn run_repl(rl: &mut dyn Repl) {
  let mut defs = Defs::new();
  rl.load_history();
  loop {
    let readline = rl.readline("⅄ ");
    match readline {
      Ok(line) => {
        rl.add_history_entry(line.as_str());
        let res = command::parse_command(
          input_cid(line.as_str()),
          defs.clone(),
        )(Span::new(&line));
        match res {
          Ok((_, command)) => match command {
            Command::Eval(term) => {
              let mut dag = DAG::from_term(&term);
              dag.norm(&defs);
              rl.println(format!("{}", dag));
            }
            Command::Type(term) => {
              let res = infer_term(&defs, *term);
              match res {
                Ok(term) => rl.println(format!("{}", term)),
                Err(e) => rl.println(format!("Error: {}", e)),
              }
            }
            Command::Define(boxed) => {
              let (n, def, _) = *boxed;
              let mut tmp_defs = defs.clone();
              tmp_defs.insert(n.clone(), def);
              let res = check_def(&tmp_defs, &n);
              match res {
                Ok(res) => {
                  defs = tmp_defs;
                  rl.println(format!("{} : {}", n, res.pretty(Some(&n))))
                }
                Err(e) => rl.println(format!("Error: {}", e)),
              }
            }
            Command::Browse => {
              for (n, d) in defs.named_defs() {
                rl.println(format!("{}", d.pretty(n)))
              }
            }
            Command::Quit => {
              rl.println(format!("Goodbye."));
              break;
            }
          },
          Err(e) => match e {
            Err::Incomplete(_) => rl.println(format!("Incomplete Input")),
            Err::Failure(e) => {
              rl.println(format!("Parse Failure:\n"));
              rl.println(format!("{}", e));
            }
            Err::Error(e) => {
              rl.println(format!("Parse Error:\n"));
              rl.println(format!("{}", e));
            }
          },
        }
      }
      Err(ReplError::Interrupted) => {
        rl.println(format!("CTRL-C"));
        break;
      }
      Err(ReplError::Eof) => {
        rl.println(format!("CTRL-D"));
        break;
      }
      Err(ReplError::Other(err)) => {
        rl.println(format!("Error: {}", err));
        break;
      }
    }
  }
  rl.save_history();
}

