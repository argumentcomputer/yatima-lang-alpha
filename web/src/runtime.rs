use yatima_runtime::{
  transform::{
    RuntimeIO,
  },
};
use yatima_utils::repl::Repl;
use std::{
  rc::Rc,
};
use yatima_core::{
  prim::io::IoOp,
  literal::Literal,
  term::Term,
  defs,
  parse,
  yatima,
};

use crate::repl::WebRepl;

#[derive(Clone)]
pub struct WebReplIORuntime {
  pub web_repl: Rc<WebRepl>,
}

impl RuntimeIO for WebReplIORuntime {
  fn write_stdout_op(&self) -> IoOp {
    // let println = self.println;
    fn fun_symbol() -> String { "write_stdout".to_string() }
    fn fun_type_of() -> Term { yatima!("#Bytes -> #Bool") }
    fn fun_arity() -> u64 { 1 }
    fn fun_apply0() -> Option<Literal> { None }
    let clone = self.clone();
    let fun_apply1 = Rc::new(move |lit: Literal| {
      match lit {
        Literal::Bytes(bytes) => {
          Some(Literal::Bool(clone.web_repl.println(String::from_utf8(bytes).unwrap()).is_ok()))
        }
        _ => None,
      }
    });// as Box<fn(Literal) -> Option<Literal>>;
    fn fun_apply2(_: Literal, _: Literal) -> Option<Literal> { None }
    IoOp {
      fun_symbol,
      fun_type_of,
      fun_arity,
      fun_apply0,
      fun_apply1,
      fun_apply2,
    }
  }

  fn read_stdin_op(&self) -> IoOp {
    fn fun_symbol() -> String { "read_stdin".to_string() }
    fn fun_type_of() -> Term { yatima!("#Bytes") }
    fn fun_arity() -> u64 { 0 }
    fn fun_apply0() -> Option<Literal> {
      let stdin = std::io::stdin();
      let mut buf = String::new();
      let _ = stdin.read_line(&mut buf).unwrap();
      Some(Literal::Bytes(buf.as_bytes().to_vec()))
    }
    let fun_apply1 = Rc::new(|_: Literal| -> Option<Literal> { None });
    fn fun_apply2(_: Literal, _: Literal) -> Option<Literal> { None }
    IoOp {
      fun_symbol,
      fun_type_of,
      fun_arity,
      fun_apply0,
      fun_apply1,
      fun_apply2,
    }
  }
}
