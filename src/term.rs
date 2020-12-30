pub mod decode_error;
pub mod literal;
pub mod primop;
pub mod uses;

use decode_error::{
  or_else_join,
  DecodeError,
  Expected,
};
use literal::{
  LitType,
  Literal,
};
use primop::PrimOp;
use uses::Uses;

use hashexpr::{
  atom::{
    Atom::*,
    Link,
  },
  position::Pos,
  Expr,
  Expr::Atom,
};
use im::Vector;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Term {
  Var(Option<Pos>, String, u64),
  Lam(Option<Pos>, String, Box<Term>),
  App(Option<Pos>, Box<Term>, Box<Term>),
  All(Option<Pos>, String, Uses, Box<Term>, Box<Term>),
  Slf(Option<Pos>, String, Box<Term>),
  Dat(Option<Pos>, Box<Term>),
  Cse(Option<Pos>, Box<Term>),
  Ref(Option<Pos>, String, Link, Link),
  Let(Option<Pos>, bool, String, Uses, Box<Term>, Box<Term>, Box<Term>),
  Typ(Option<Pos>),
  Ann(Option<Pos>, Box<Term>, Box<Term>),
  Lit(Option<Pos>, Literal),
  LTy(Option<Pos>, LitType),
  Opr(Option<Pos>, PrimOp),
}

impl PartialEq for Term {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::Var(_, na, ia), Self::Var(_, nb, ib)) => na == nb && ia == ib,
      (Self::Lam(_, na, ba), Self::Lam(_, nb, bb)) => na == nb && ba == bb,
      (Self::App(_, fa, aa), Self::App(_, fb, ab)) => fa == fb && aa == ab,
      (Self::All(_, na, ua, ta, ba), Self::All(_, nb, ub, tb, bb)) => {
        na == nb && ua == ub && ta == tb && ba == bb
      }
      (Self::Slf(_, na, ba), Self::Slf(_, nb, bb)) => na == nb && ba == bb,
      (Self::Dat(_, ba), Self::Dat(_, bb)) => ba == bb,
      (Self::Cse(_, ba), Self::Cse(_, bb)) => ba == bb,
      (Self::Ref(_, na, ma, aa), Self::Ref(_, nb, mb, ab)) => {
        na == nb && ma == mb && aa == ab
      }
      (
        Self::Let(_, ra, na, ua, ta, xa, ba),
        Self::Let(_, rb, nb, ub, tb, xb, bb),
      ) => ra == rb && na == nb && ua == ub && ta == tb && xa == xb && ba == bb,
      (Self::Typ(_), Self::Typ(_)) => true,
      (Self::Ann(_, xa, ta), Self::Ann(_, xb, tb)) => xa == xb && ta == tb,
      (Self::Lit(_, a), Self::Lit(_, b)) => a == b,
      (Self::LTy(_, a), Self::LTy(_, b)) => a == b,
      (Self::Opr(_, a), Self::Opr(_, b)) => a == b,
      _ => false,
    }
  }
}

impl Term {
  pub fn encode(self) -> Expr {
    match self {
      Self::Var(_, nam, _) => atom!(symb!(nam)),
      Self::Lam(_, nam, bod) => {
        cons!(None, atom!(symb!("lambda")), atom!(symb!(nam)), bod.encode())
      }
      Self::App(_, fun, arg) => cons!(None, fun.encode(), arg.encode()),
      Self::All(_, nam, uses, typ, bod) => {
        cons!(
          None,
          atom!(symb!("forall")),
          atom!(symb!(nam)),
          uses.encode(),
          typ.encode(),
          bod.encode()
        )
      }
      Self::Slf(_, nam, bod) => {
        cons!(None, atom!(symb!("self")), atom!(symb!(nam)), bod.encode())
      }
      Self::Dat(_, bod) => {
        cons!(None, atom!(symb!("data")), bod.encode())
      }
      Self::Cse(_, bod) => {
        cons!(None, atom!(symb!("case")), bod.encode())
      }
      Self::Ref(_, nam, ..) => atom!(symb!(nam)),
      Self::Let(_, rec, nam, uses, typ, exp, bod) => {
        let ctor = if rec { symb!("letrec") } else { symb!("let") };
        cons!(
          None,
          atom!(ctor),
          atom!(symb!(nam)),
          uses.encode(),
          typ.encode(),
          exp.encode(),
          bod.encode()
        )
      }
      Self::Typ(_) => atom!(symb!("Type")),
      Self::Ann(_, exp, typ) => {
        cons!(None, atom!(symb!("typeann")), exp.encode(), typ.encode())
      }
      Self::Lit(_, lit) => lit.encode(),
      Self::LTy(_, lty) => lty.encode(),
      Self::Opr(_, opr) => opr.encode(),
    }
  }

  pub fn decode(
    refs: HashMap<String, (Link, Link)>,
    ctx: Vector<String>,
    expr: Expr,
  ) -> Result<Self, DecodeError> {
    match expr {
      Expr::Atom(pos, val) => {
        // println!("ctx {:?}", ctx);
        let decode_var = |val| match val {
          Symbol(n) => {
            // println!("var n {:?}", n);
            let (i, _) = ctx
              .iter()
              .enumerate()
              .find(|(_, m)| **m == n)
              .ok_or(DecodeError::new(pos, vec![Expected::BoundVar]))?;
            // println!("var i {:?}", i);
            Ok(Self::Var(pos, n.to_owned(), i as u64))
          }
          _ => Err(DecodeError::new(pos, vec![Expected::BoundVar])),
        };
        let decode_ref = |val| match val {
          Symbol(n) => {
            let (meta_link, ast_link) = refs
              .get(&n)
              .ok_or(DecodeError::new(pos, vec![Expected::DefinedRef]))?;
            Ok(Self::Ref(pos, n.to_owned(), *meta_link, *ast_link))
          }
          _ => Err(DecodeError::new(pos, vec![Expected::DefinedRef])),
        };
        let decode_typ = |val| match val {
          Symbol(n) if *n == String::from("Type") => Ok(Self::Typ(pos)),
          _ => Err(DecodeError::new(pos, vec![Expected::TypeOfTypes])),
        };

        vec![
          decode_var(val.clone()),
          decode_ref(val.clone()),
          decode_typ(val.clone()),
          Literal::decode(Expr::Atom(pos.clone(), val.clone()))
            .map(|x| Self::Lit(pos, x)),
          LitType::decode(Expr::Atom(pos.clone(), val.clone()))
            .map(|x| Self::LTy(pos, x)),
          PrimOp::decode(Expr::Atom(pos.clone(), val.clone()))
            .map(|x| Self::Opr(pos, x)),
        ]
        .into_iter()
        .fold(Err(DecodeError::new(pos, vec![])), or_else_join)
      }
      Expr::Cons(pos, xs) => match xs.as_slice() {
        [Atom(_, Symbol(n)), tail @ ..] if *n == String::from("letrec") => {
          match tail {
            [Atom(_, Symbol(nam)), uses, typ, exp, bod] => {
              let uses = Uses::decode(uses.to_owned())?;
              let typ =
                Term::decode(refs.to_owned(), ctx.to_owned(), typ.to_owned())?;
              let mut new_ctx = ctx.clone();
              new_ctx.push_front(nam.clone());
              let exp =
                Term::decode(refs.to_owned(), new_ctx.clone(), exp.to_owned())?;
              let bod = Term::decode(refs.to_owned(), new_ctx, bod.to_owned())?;
              Ok(Self::Let(
                pos,
                true,
                nam.clone(),
                uses,
                Box::new(typ),
                Box::new(exp),
                Box::new(bod),
              ))
            }
            _ => Err(DecodeError::new(pos, vec![Expected::LetRec])),
          }
        }
        [Atom(_, Symbol(n)), tail @ ..] if *n == String::from("let") => {
          match tail {
            [Atom(_, Symbol(nam)), uses, typ, exp, bod] => {
              let uses = Uses::decode(uses.to_owned())?;
              let typ =
                Term::decode(refs.to_owned(), ctx.to_owned(), typ.to_owned())?;
              let exp =
                Term::decode(refs.to_owned(), ctx.to_owned(), exp.to_owned())?;
              let mut new_ctx = ctx.clone();
              new_ctx.push_front(nam.clone());
              let bod = Term::decode(refs.to_owned(), new_ctx, bod.to_owned())?;
              Ok(Self::Let(
                pos,
                false,
                nam.clone(),
                uses,
                Box::new(typ),
                Box::new(exp),
                Box::new(bod),
              ))
            }
            _ => Err(DecodeError::new(pos, vec![Expected::Let])),
          }
        }
        [Atom(_, Symbol(n)), tail @ ..] if *n == String::from("data") => {
          match tail {
            [bod] => {
              let bod =
                Term::decode(refs.to_owned(), ctx.to_owned(), bod.to_owned())?;
              Ok(Self::Dat(pos, Box::new(bod)))
            }
            _ => Err(DecodeError::new(pos, vec![Expected::Data])),
          }
        }
        [Atom(_, Symbol(n)), tail @ ..] if *n == String::from("case") => {
          match tail {
            [bod] => {
              let bod =
                Term::decode(refs.to_owned(), ctx.to_owned(), bod.to_owned())?;
              Ok(Self::Cse(pos, Box::new(bod)))
            }
            _ => Err(DecodeError::new(pos, vec![Expected::Case])),
          }
        }
        [Atom(_, Symbol(n)), tail @ ..] if *n == String::from("self") => {
          match tail {
            [Atom(_, Symbol(nam)), bod] => {
              let mut new_ctx = ctx.clone();
              new_ctx.push_front(nam.clone());
              let bod = Term::decode(refs.to_owned(), new_ctx, bod.to_owned())?;
              Ok(Self::Slf(pos, nam.clone(), Box::new(bod)))
            }
            _ => Err(DecodeError::new(pos, vec![Expected::SelfType])),
          }
        }
        [Atom(_, Symbol(n)), tail @ ..] if *n == String::from("lambda") => {
          match tail {
            [Atom(_, Symbol(nam)), bod] => {
              let mut new_ctx = ctx.clone();
              new_ctx.push_front(nam.clone());
              let bod = Term::decode(refs.to_owned(), new_ctx, bod.to_owned())?;
              Ok(Self::Lam(pos, nam.to_owned(), Box::new(bod)))
            }
            _ => Err(DecodeError::new(pos, vec![Expected::Lambda])),
          }
        }
        [Atom(_, Symbol(n)), tail @ ..] if *n == String::from("forall") => {
          match tail {
            [Atom(_, Symbol(nam)), uses, typ, bod] => {
              let uses = Uses::decode(uses.to_owned())?;
              let typ =
                Term::decode(refs.to_owned(), ctx.to_owned(), typ.to_owned())?;
              let mut new_ctx = ctx.clone();
              new_ctx.push_front(nam.clone());
              let bod = Term::decode(refs.to_owned(), new_ctx, bod.to_owned())?;
              Ok(Self::All(
                pos,
                nam.to_owned(),
                uses,
                Box::new(typ),
                Box::new(bod),
              ))
            }
            _ => Err(DecodeError::new(pos, vec![Expected::Forall])),
          }
        }
        [Atom(_, Symbol(n)), tail @ ..] if *n == String::from("typeann") => {
          match tail {
            [exp, typ] => {
              let exp =
                Term::decode(refs.to_owned(), ctx.to_owned(), exp.to_owned())?;
              let typ =
                Term::decode(refs.to_owned(), ctx.to_owned(), typ.to_owned())?;
              Ok(Self::Ann(pos, Box::new(exp), Box::new(typ)))
            }
            _ => Err(DecodeError::new(pos, vec![Expected::Annotation])),
          }
        }
        [fun, arg] => {
          let fun =
            Term::decode(refs.to_owned(), ctx.to_owned(), fun.to_owned())?;
          let arg =
            Term::decode(refs.to_owned(), ctx.to_owned(), arg.to_owned())?;
          Ok(Self::App(pos, Box::new(fun), Box::new(arg)))
        }

        _ => Err(DecodeError::new(pos, vec![Expected::Constructor])),
      },
    }
  }
}

#[cfg(test)]
mod tests {
  use super::{
    Term::*,
    *,
  };
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  use rand::{
    prelude::IteratorRandom,
    Rng,
  };

  fn arbitrary_name<G: Gen>(g: &mut G) -> String {
    let alpha = "abcdefghjiklmnopqrstuvwxyz";
    let x = g.gen_range(0, alpha.len());
    format!("{}", alpha.as_bytes()[x] as char)
  }

  fn arbitrary_lam<G: Gen>(g: &mut G, ctx: Vector<String>) -> Term {
    let n = arbitrary_name(g);
    let mut ctx2 = ctx.clone();
    ctx2.push_front(n.clone());
    Lam(None, n, Box::new(arbitrary_term(g, ctx2)))
  }

  fn arbitrary_slf<G: Gen>(g: &mut G, ctx: Vector<String>) -> Term {
    let n = arbitrary_name(g);
    let mut ctx2 = ctx.clone();
    ctx2.push_front(n.clone());
    Slf(None, n, Box::new(arbitrary_term(g, ctx2)))
  }
  fn arbitrary_let<G: Gen>(g: &mut G, ctx: Vector<String>) -> Term {
    let rec: bool = Arbitrary::arbitrary(g);
    let n = arbitrary_name(g);
    let u: Uses = Arbitrary::arbitrary(g);
    let typ = Box::new(arbitrary_term(g, ctx.clone()));
    if rec {
      let mut ctx2 = ctx.clone();
      ctx2.push_front(n.clone());
      let exp = Box::new(arbitrary_term(g, ctx2.clone()));
      let bod = Box::new(arbitrary_term(g, ctx2.clone()));
      Let(None, rec, n, u, typ, exp, bod)
    }
    else {
      let mut ctx2 = ctx.clone();
      ctx2.push_front(n.clone());
      let exp = Box::new(arbitrary_term(g, ctx.clone()));
      let bod = Box::new(arbitrary_term(g, ctx2.clone()));
      Let(None, rec, n, u, typ, exp, bod)
    }
  }

  fn arbitrary_all<G: Gen>(g: &mut G, ctx: Vector<String>) -> Term {
    let n = arbitrary_name(g);
    let u: Uses = Arbitrary::arbitrary(g);
    let mut ctx2 = ctx.clone();
    ctx2.push_front(n.clone());
    All(
      None,
      n,
      u,
      Box::new(arbitrary_term(g, ctx)),
      Box::new(arbitrary_term(g, ctx2)),
    )
  }

  fn arbitrary_var<G: Gen>(g: &mut G, ctx: Vector<String>) -> Term {
    let n = ctx.iter().choose(g).unwrap();
    let (i, _) = ctx.iter().enumerate().find(|(_, x)| *x == n).unwrap();
    Var(None, n.clone(), i as u64)
  }

  fn arbitrary_term<G: Gen>(g: &mut G, ctx: Vector<String>) -> Term {
    let len = ctx.len();
    if len == 0 {
      arbitrary_lam(g, ctx)
    }
    else {
      let x: u32 = g.gen_range(0, 25);
      match x {
        0 => arbitrary_all(g, ctx.clone()),
        1 => arbitrary_let(g, ctx.clone()),
        2 | 3 => arbitrary_lam(g, ctx.clone()),
        4 | 5 => arbitrary_slf(g, ctx.clone()),
        6 | 7 => Term::App(
          None,
          Box::new(arbitrary_term(g, ctx.clone())),
          Box::new(arbitrary_term(g, ctx.clone())),
        ),
        8 | 9 => Term::Ann(
          None,
          Box::new(arbitrary_term(g, ctx.clone())),
          Box::new(arbitrary_term(g, ctx.clone())),
        ),
        10 | 11 => Term::Dat(None, Box::new(arbitrary_term(g, ctx.clone()))),
        12 | 13 => Term::Cse(None, Box::new(arbitrary_term(g, ctx.clone()))),
        14 | 15 | 16 => Term::Typ(None),
        _ => arbitrary_var(g, ctx),
      }
    }
  }

  impl Arbitrary for Term {
    fn arbitrary<G: Gen>(g: &mut G) -> Self { arbitrary_term(g, Vector::new()) }
  }

  #[quickcheck]
  fn term_encode_decode(x: Term) -> bool {
    match Term::decode(HashMap::new(), Vector::new(), x.clone().encode()) {
      Ok(y) => x == y,
      _ => false,
    }
  }

  #[test]
  fn term_test_cases() {
    let f =
      Lam(None, String::from("x"), Box::new(Var(None, String::from("x"), 0)));
    assert_eq!("(lambda x x)", format!("{}", f.clone().encode()));
    let b = App(None, Box::new(f.clone()), Box::new(f.clone()));
    assert_eq!(
      "((lambda x x) (lambda x x))",
      format!("{}", b.clone().encode())
    );
    assert_eq!(
      Ok(Var(None, String::from("x"), 0)),
      Term::decode(
        HashMap::new(),
        vec![String::from("x")].into(),
        hashexpr::parse("x").unwrap().1,
      )
    );

    let f =
      Lam(None, String::from("x"), Box::new(Var(None, String::from("x"), 0)));
    assert_eq!(
      Ok(f.clone()),
      Term::decode(
        HashMap::new(),
        Vector::new(),
        hashexpr::parse("(lambda x x)").unwrap().1
      )
    );

    assert_eq!(
      Ok(b.clone()),
      Term::decode(
        HashMap::new(),
        Vector::new(),
        hashexpr::parse("((lambda x x) (lambda x x))").unwrap().1
      )
    )
  }
}
