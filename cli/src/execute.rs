use std::rc::Rc;

use yatima_core::{
  defs,
  defs::Defs,
  literal::Literal,
  parse,
  prim::io::IoOp,
  term::{
    Name,
    Op,
    Pos,
    Term,
  },
  yatima,
};

pub fn transform(defs: Rc<Defs>, term: &mut Term) {
  match term {
    Term::Var(_pos, _name, _index) => {}
    Term::Lam(_pos, _name, boxed) => {
      transform_boxed(defs, boxed);
    }
    Term::App(_pos, boxed) => {
      transform_boxed2(defs, boxed);
    }
    Term::All(_pos, _uses, _name, boxed) => {
      transform_boxed2(defs, boxed);
    }
    Term::Slf(_pos, _name, boxed) => {
      transform_boxed(defs, boxed);
    }
    Term::Dat(_pos, boxed) => {
      transform_boxed(defs, boxed);
    }
    Term::Cse(_pos, boxed) => {
      transform_boxed(defs, boxed);
    }
    Term::Ref(_pos, _name, exp, _cid2) => {
      if exp.to_string()
        == "bafy2bzaceakfamngkzwsi3zzkxyfz5rfabb5x3t6it7nhsxo7jjb2grpu7gmm"
      {
        *term = io_return();
        transform(defs, term);
      }
      else if exp.to_string()
        == "bafy2bzaceajjrezmgr6mulbz4htf72yknifanxgsoqquzhno5vtx5wggwo7k6"
      {
        *term = io_print();
        transform(defs, term);
      }
      else if exp.to_string()
        == "bafy2bzaceabdrrnmkljwxmj5xe63nhtygjjpgxtptdrdolsipdojrbsjjyrd6"
      {
        *term = io_read();
        transform(defs, term);
      }
      else if exp.to_string()
        == "bafy2bzaceanc3j2lxoz2jttiinlxqge7u3tnmufrfoil4udxptljacv5fmmq6"
      {
        *term = io_bind();
        transform(defs, term);
      }
      else if let Some(def) = defs.defs.get(exp) {
        *term = def.term.clone();
        transform(defs, term);
      }
    }
    Term::Let(_pos, _bool, _uses, _name, boxed) => {
      transform_boxed3(defs, boxed);
    }
    Term::Typ(_pos) => {}
    Term::Ann(_pos, boxed) => {
      transform_boxed2(defs, boxed);
    }
    Term::Lit(_pos, _lit) => {}
    Term::LTy(_pos, _lit_type) => {}
    Term::Opr(_pos, _op) => {}
    Term::Rec(_pos) => {}
  }
}

fn transform_boxed(defs: Rc<Defs>, boxed: &mut Box<Term>) {
  transform(defs, boxed.as_mut())
}

fn transform_boxed2(defs: Rc<Defs>, boxed: &mut Box<(Term, Term)>) {
  let (t1, t2) = boxed.as_mut();
  transform(defs.clone(), t1);
  transform(defs, t2);
}

fn transform_boxed3(defs: Rc<Defs>, boxed: &mut Box<(Term, Term, Term)>) {
  let (t1, t2, t3) = boxed.as_mut();
  transform(defs.clone(), t1);
  transform(defs.clone(), t2);
  transform(defs, t3);
}

fn write_stdout_op() -> IoOp {
  fn fun_symbol() -> String { "write_stdout".to_string() }
  fn fun_type_of() -> Term { yatima!("#Bytes -> #Bool") }
  fn fun_arity() -> u64 { 1 }
  fn fun_apply0() -> Option<Literal> { None }
  fn fun_apply1(lit: Literal) -> Option<Literal> {
    match lit {
      Literal::Bytes(bytes) => {
        use std::io::Write;
        let mut stdout = std::io::stdout();
        Some(Literal::Bool(stdout.write_all(&bytes).is_ok()))
      }
      _ => None,
    }
  }
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

fn read_stdin_op() -> IoOp {
  fn fun_symbol() -> String { "read_stdin".to_string() }
  fn fun_type_of() -> Term { yatima!("#Bytes") }
  fn fun_arity() -> u64 { 0 }
  fn fun_apply0() -> Option<Literal> {
    let stdin = std::io::stdin();
    let mut buf = String::new();
    let _ = stdin.read_line(&mut buf).unwrap();
    Some(Literal::Bytes(buf.as_bytes().to_vec()))
  }
  fn fun_apply1(_: Literal) -> Option<Literal> { None }
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

fn io_return() -> Term { yatima!("lambda x => x") }
fn io_print() -> Term {
  let name = Name::from("_type");
  Term::Lam(
    Pos::None,
    name,
    Box::new(Term::Opr(Pos::None, Op::Io(write_stdout_op()))),
  )
}
fn io_read() -> Term {
  let name = Name::from("_type");
  Term::Lam(
    Pos::None,
    name,
    Box::new(Term::Opr(Pos::None, Op::Io(read_stdin_op()))),
  )
}
fn io_bind() -> Term { yatima!("lambda _type1 _type2 io fun => fun io") }
