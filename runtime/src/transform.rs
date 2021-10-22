use std::rc::Rc;

use yatima_core::{
  defs,
  defs::Defs,
  literal::Literal,
  parse,
  prim::io::IoOp,
  term::{
    Op,
    Pos,
    Term,
  },
  yatima,
};

/// The str CID of IO.Return
const IO_RETURN: &str =
  "bafy2bzaceakfamngkzwsi3zzkxyfz5rfabb5x3t6it7nhsxo7jjb2grpu7gmm";

/// The str CID of IO.Print
const IO_PRINT: &str =
  "bafy2bzaceajjrezmgr6mulbz4htf72yknifanxgsoqquzhno5vtx5wggwo7k6";

/// The str CID of IO.Read
const IO_READ: &str =
  "bafy2bzaceabdrrnmkljwxmj5xe63nhtygjjpgxtptdrdolsipdojrbsjjyrd6";

/// The str CID of IO.Bind
const IO_BIND: &str =
  "bafy2bzaceanc3j2lxoz2jttiinlxqge7u3tnmufrfoil4udxptljacv5fmmq6";

pub fn transform(defs: Rc<Defs>, term: &mut Term, runtime: RunIO) {
  match term {
    Term::Var(_pos, _name, _index) => {}
    Term::Lam(_pos, _name, boxed) => {
      transform_boxed(defs, boxed, runtime);
    }
    Term::App(_pos, boxed) => {
      transform_boxed2(defs, boxed, runtime);
    }
    Term::All(_pos, _uses, _name, boxed) => {
      transform_boxed2(defs, boxed, runtime);
    }
    Term::Slf(_pos, _name, boxed) => {
      transform_boxed(defs, boxed, runtime);
    }
    Term::Dat(_pos, boxed) => {
      transform_boxed(defs, boxed, runtime);
    }
    Term::Cse(_pos, boxed) => {
      transform_boxed(defs, boxed, runtime);
    }
    Term::Ref(_pos, _name, exp, _cid2) => match exp.to_string().as_ref() {
      IO_RETURN => {
        *term = yatima!("lambda x => x");
        transform(defs, term, runtime);
      }
      IO_PRINT => {
        *term = yatima!(
          "lambda _type x => #$0 x",
          Term::Opr(Pos::None, Op::Io(runtime.write_stdout_op()))
        );
        transform(defs, term, runtime);
      }
      IO_READ => {
        *term = yatima!(
          "lambda _type => #$0",
          Term::Opr(Pos::None, Op::Io(runtime.read_stdin_op()))
        );
        transform(defs, term, runtime);
      }
      IO_BIND => {
        *term = yatima!("lambda _type1 _type2 io fun => fun io");
        transform(defs, term, runtime);
      }
      _ => {
        if let Some(def) = defs.defs.get(exp) {
          *term = def.term.clone();
          transform(defs, term, runtime);
        }
      }
    },
    Term::Let(_pos, _bool, _uses, _name, boxed) => {
      transform_boxed3(defs, boxed, runtime);
    }
    Term::Typ(_pos) => {}
    Term::Ann(_pos, boxed) => {
      transform_boxed2(defs, boxed, runtime);
    }
    Term::Lit(_pos, _lit) => {}
    Term::LTy(_pos, _lit_type) => {}
    Term::Opr(_pos, _op) => {}
    Term::Rec(_pos) => {}
  }
}

fn transform_boxed(
  defs: Rc<Defs>,
  boxed: &mut Box<Term>,
  runtime: RunIO,
) {
  transform(defs, boxed.as_mut(), runtime)
}

fn transform_boxed2(
  defs: Rc<Defs>,
  boxed: &mut Box<(Term, Term)>,
  runtime: RunIO,
) {
  let (t1, t2) = boxed.as_mut();
  transform(defs.clone(), t1, runtime.clone());
  transform(defs, t2, runtime);
}

fn transform_boxed3(
  defs: Rc<Defs>,
  boxed: &mut Box<(Term, Term, Term)>,
  runtime: RunIO,
) {
  let (t1, t2, t3) = boxed.as_mut();
  transform(defs.clone(), t1, runtime.clone());
  transform(defs.clone(), t2, runtime.clone());
  transform(defs, t3, runtime);
}

/// A trait defining the behavior of primitive IO operations.
pub trait RuntimeIO {
  fn write_stdout_op(&self) -> IoOp {
    todo!("write_stdout not implemented for this runtime")
  }
  fn read_stdin_op(&self) -> IoOp {
    todo!("read_stdin not implemented for this runtime")
  }
}
/// An Rc wrapper for a RuntimeIO instance
pub type RunIO = Rc<dyn RuntimeIO>;

/// A RuntimeIO using std::io
#[derive(Clone)]
pub struct StdIORuntime {}

impl StdIORuntime {
  pub fn new() -> Self {
    StdIORuntime {}
  }
}

impl RuntimeIO for StdIORuntime {
  fn write_stdout_op(&self) -> IoOp {
    fn fun_symbol() -> String { "write_stdout".to_string() }
    fn fun_type_of() -> Term { yatima!("#Bytes -> #Bool") }
    fn fun_arity() -> u64 { 1 }
    fn fun_apply0() -> Option<Literal> { None }
    let fun_apply1 = Rc::new(|lit: Literal| -> Option<Literal> {
      match lit {
        Literal::Bytes(bytes) => {
          use std::io::Write;
          let mut stdout = std::io::stdout();
          Some(Literal::Bool(stdout.write_all(&bytes).is_ok()))
        }
        _ => None,
      }
    });
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
