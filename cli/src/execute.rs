use std::rc::Rc;

use yatima_core::{
  defs,
  defs::Defs,
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

fn io_return() -> Term { yatima!("lambda x => x") }
fn io_print() -> Term {
  let name = Name::from("_type");
  Term::Lam(
    Pos::None,
    name,
    Box::new(Term::Opr(Pos::None, Op::Io(IoOp::WriteStdout))),
  )
}
fn io_read() -> Term {
  let name = Name::from("_type");
  Term::Lam(
    Pos::None,
    name,
    Box::new(Term::Opr(Pos::None, Op::Io(IoOp::ReadAllStdin))),
  )
}
fn io_bind() -> Term { yatima!("lambda _type1 _type2 io fun => fun io") }
