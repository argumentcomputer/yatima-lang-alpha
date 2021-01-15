use crate::decode_error::{
  or_else_join,
  DecodeError,
  Expected,
};

pub use crate::valus::{
  literal::Literal,
  primop::PrimOp,
  uses::Uses,
};

use hashexpr::{
  atom::Atom::*,
  link::Link,
  position::Pos,
  Expr,
  Expr::Atom,
};
use im::{
  HashMap,
  Vector,
};
use std::fmt;

#[derive(Clone, Debug)]
pub enum Term {
  Var(Option<Pos>, String, u64),
  Lam(Option<Pos>, String, Box<Term>),
  App(Option<Pos>, Box<Term>, Box<Term>),
  All(Option<Pos>, Uses, String, Box<Term>, Box<Term>),
  Slf(Option<Pos>, String, Box<Term>),
  Dat(Option<Pos>, Box<Term>),
  Cse(Option<Pos>, Box<Term>),
  Ref(Option<Pos>, String, Link, Link),
  Let(Option<Pos>, bool, Uses, String, Box<Term>, Box<Term>, Box<Term>),
  Typ(Option<Pos>),
  Ann(Option<Pos>, Box<Term>, Box<Term>),
  Lit(Option<Pos>, Literal),
  Opr(Option<Pos>, PrimOp),
}

pub type Refs = HashMap<String, (Link, Link)>;

impl PartialEq for Term {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::Var(_, na, ia), Self::Var(_, nb, ib)) => na == nb && ia == ib,
      (Self::Lam(_, na, ba), Self::Lam(_, nb, bb)) => na == nb && ba == bb,
      (Self::App(_, fa, aa), Self::App(_, fb, ab)) => fa == fb && aa == ab,
      (Self::All(_, ua, na, ta, ba), Self::All(_, ub, nb, tb, bb)) => {
        ua == ub && na == nb && ta == tb && ba == bb
      }
      (Self::Slf(_, na, ba), Self::Slf(_, nb, bb)) => na == nb && ba == bb,
      (Self::Dat(_, ba), Self::Dat(_, bb)) => ba == bb,
      (Self::Cse(_, ba), Self::Cse(_, bb)) => ba == bb,
      (Self::Ref(_, na, ma, aa), Self::Ref(_, nb, mb, ab)) => {
        na == nb && ma == mb && aa == ab
      }
      (
        Self::Let(_, ra, ua, na, ta, xa, ba),
        Self::Let(_, rb, ub, nb, tb, xb, bb),
      ) => ra == rb && ua == ub && na == nb && ta == tb && xa == xb && ba == bb,
      (Self::Typ(_), Self::Typ(_)) => true,
      (Self::Ann(_, xa, ta), Self::Ann(_, xb, tb)) => xa == xb && ta == tb,
      (Self::Lit(_, a), Self::Lit(_, b)) => a == b,
      (Self::Opr(_, a), Self::Opr(_, b)) => a == b,
      _ => false,
    }
  }
}

impl fmt::Display for Term {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use Term::*;
    const WILDCARD: &str = "_";

    fn name(nam: &str) -> &str { if nam.is_empty() { WILDCARD } else { nam } }

    fn uses(uses: &Uses) -> &str {
      match uses {
        Uses::None => "0 ",
        Uses::Affi => "& ",
        Uses::Once => "1 ",
        Uses::Many => "",
      }
    }

    fn is_atom(term: &Term) -> bool {
      match term {
        Var(..) => true,
        Ref(..) => true,
        Lit(..) => true,
        Opr(..) => true,
        Typ(..) => true,
        _ => false,
      }
    }

    fn lams(nam: &str, bod: &Term) -> String {
      match bod {
        Lam(_, nam2, bod2) => format!("{} {}", name(nam), lams(nam2, bod2)),
        _ => format!("{} => {}", nam, bod),
      }
    }

    fn alls(use_: &Uses, nam: &str, typ: &Term, bod: &Term) -> String {
      match bod {
        All(_, bod_use, bod_nam, bod_typ, bod_bod) => {
          format!(
            " ({}{}: {}){}",
            uses(use_),
            name(nam),
            typ,
            alls(bod_use, bod_nam, bod_typ, bod_bod)
          )
        }
        _ => format!(" ({}{}: {}) -> {}", uses(use_), name(nam), typ, bod),
      }
    }

    fn parens(term: &Term) -> String {
      if is_atom(term) { format!("{}", term) } else { format!("({})", term) }
    }

    fn apps(fun: &Term, arg: &Term) -> String {
      match (fun, arg) {
        (App(_, ff, fa), App(_, af, aa)) => {
          format!("{} ({})", apps(ff, fa), apps(af, aa))
        }
        (App(_, ff, fa), arg) => {
          format!("({}) {}", apps(ff, fa), parens(arg))
        }
        (fun, App(_, af, aa)) => {
          format!("{} ({})", parens(fun), apps(af, aa))
        }
        (fun, arg) => {
          format!("{} {}", parens(fun), parens(arg))
        }
      }
    }

    match self {
      Var(_, nam, ..) => write!(f, "{}", nam),
      Ref(_, nam, ..) => write!(f, "{}", nam),
      Lam(_, nam, term) => write!(f, "λ {}", lams(nam, term)),
      App(_, fun, arg) => write!(f, "{}", apps(fun, arg)),
      Let(_, true, u, n, typ, exp, bod) => {
        write!(f, "letrec {}{}: {} := {}; {}", uses(u), name(n), typ, exp, bod)
      }
      Let(_, false, u, n, typ, exp, bod) => {
        write!(f, "let {}{}: {} := {}; {}", uses(u), name(n), typ, exp, bod)
      }
      Slf(_, nam, bod) => write!(f, "@{} {}", name(nam), bod),
      All(_, us_, nam, typ, bod) => write!(f, "∀{}", alls(us_, nam, typ, bod)),
      Ann(_, typ, val) => write!(f, "({} :: {})", val, typ),
      Dat(_, bod) => write!(f, "data {}", bod),
      Cse(_, bod) => write!(f, "case {}", bod),
      Typ(_) => write!(f, "Type"),
      Lit(_, lit) => write!(f, "{}", lit),
      Opr(_, opr) => write!(f, "{}", opr),
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
      Self::All(_, uses, nam, typ, bod) => {
        cons!(
          None,
          atom!(symb!("forall")),
          uses.encode(),
          atom!(symb!(nam)),
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
      Self::Let(_, rec, uses, nam, typ, exp, bod) => {
        let ctor = if rec { symb!("letrec") } else { symb!("let") };
        cons!(
          None,
          atom!(ctor),
          uses.encode(),
          atom!(symb!(nam)),
          typ.encode(),
          exp.encode(),
          bod.encode()
        )
      }
      Self::Typ(_) => atom!(symb!("Type")),
      Self::Ann(_, typ, trm) => {
        cons!(None, atom!(symb!("type")), typ.encode(), trm.encode())
      }
      Self::Lit(_, lit) => lit.encode(),
      Self::Opr(_, opr) => opr.encode(),
    }
  }

  pub fn decode(
    refs: Refs,
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
            let (def_link, ast_link) = refs
              .get(&n)
              .ok_or(DecodeError::new(pos, vec![Expected::DefinedRef]))?;
            Ok(Self::Ref(pos, n.to_owned(), *def_link, *ast_link))
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
          PrimOp::decode(Expr::Atom(pos.clone(), val.clone()))
            .map(|x| Self::Opr(pos, x)),
        ]
        .into_iter()
        .fold(Err(DecodeError::new(pos, vec![])), or_else_join)
      }
      Expr::Cons(pos, xs) => match xs.as_slice() {
        [Atom(_, Symbol(n)), tail @ ..] if *n == String::from("letrec") => {
          match tail {
            [uses, Atom(_, Symbol(nam)), typ, exp, bod] => {
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
                uses,
                nam.clone(),
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
            [uses, Atom(_, Symbol(nam)), typ, exp, bod] => {
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
                uses,
                nam.clone(),
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
            [uses, Atom(_, Symbol(nam)), typ, bod] => {
              let uses = Uses::decode(uses.to_owned())?;
              let typ =
                Term::decode(refs.to_owned(), ctx.to_owned(), typ.to_owned())?;
              let mut new_ctx = ctx.clone();
              new_ctx.push_front(nam.clone());
              let bod = Term::decode(refs.to_owned(), new_ctx, bod.to_owned())?;
              Ok(Self::All(
                pos,
                uses,
                nam.to_owned(),
                Box::new(typ),
                Box::new(bod),
              ))
            }
            _ => Err(DecodeError::new(pos, vec![Expected::Forall])),
          }
        }
        [Atom(_, Symbol(n)), tail @ ..] if *n == String::from("type") => {
          match tail {
            [typ, trm] => {
              let typ =
                Term::decode(refs.to_owned(), ctx.to_owned(), typ.to_owned())?;
              let trm =
                Term::decode(refs.to_owned(), ctx.to_owned(), trm.to_owned())?;
              Ok(Self::Ann(pos, Box::new(typ), Box::new(trm)))
            }
            _ => Err(DecodeError::new(pos, vec![Expected::Annotation])),
          }
        }
        [Atom(_, Symbol(n)), tail @ ..] if *n == String::from("exception") => {
          match tail {
            [Atom(_, Text(err, _))] => {
              Ok(Self::Lit(pos, Literal::Exception(err.clone())))
            }
            _ => Err(DecodeError::new(pos, vec![Expected::ExceptionLiteral])),
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
pub mod tests {
  use super::{
    Term::*,
    *,
  };
  use im::HashMap;
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  use rand::{
    prelude::IteratorRandom,
    Rng,
  };

  pub fn arbitrary_link<G: Gen>(g: &mut G) -> hashexpr::Link {
    let mut bytes: [u8; 32] = [0; 32];
    for x in bytes.iter_mut() {
      *x = Arbitrary::arbitrary(g);
    }
    hashexpr::Link::from(bytes)
  }

  pub fn arbitrary_name<G: Gen>(g: &mut G) -> String {
    let s: String = Arbitrary::arbitrary(g);
    let mut s: String = s
      .chars()
      .filter(|x| {
        hashexpr::is_valid_symbol_char(*x) && char::is_ascii_alphabetic(x)
      })
      .collect();
    s.truncate(1);
    format!("_{}", s)
  }

  fn arbitrary_lam<G: Gen>(g: &mut G, refs: Refs, ctx: Vector<String>) -> Term {
    let n = arbitrary_name(g);
    let mut ctx2 = ctx.clone();
    ctx2.push_front(n.clone());
    Lam(None, n, Box::new(arbitrary_term(g, refs, ctx2)))
  }

  fn arbitrary_slf<G: Gen>(g: &mut G, refs: Refs, ctx: Vector<String>) -> Term {
    let n = arbitrary_name(g);
    let mut ctx2 = ctx.clone();
    ctx2.push_front(n.clone());
    Slf(None, n, Box::new(arbitrary_term(g, refs, ctx2)))
  }
  fn arbitrary_let<G: Gen>(g: &mut G, refs: Refs, ctx: Vector<String>) -> Term {
    let rec: bool = Arbitrary::arbitrary(g);
    let n = arbitrary_name(g);
    let u: Uses = Arbitrary::arbitrary(g);
    let typ = Box::new(arbitrary_term(g, refs.clone(), ctx.clone()));
    if rec {
      let mut ctx2 = ctx.clone();
      ctx2.push_front(n.clone());
      let exp = Box::new(arbitrary_term(g, refs.clone(), ctx2.clone()));
      let bod = Box::new(arbitrary_term(g, refs, ctx2));
      Let(None, rec, u, n, typ, exp, bod)
    }
    else {
      let mut ctx2 = ctx.clone();
      ctx2.push_front(n.clone());
      let exp = Box::new(arbitrary_term(g, refs.clone(), ctx.clone()));
      let bod = Box::new(arbitrary_term(g, refs, ctx2));
      Let(None, rec, u, n, typ, exp, bod)
    }
  }

  fn arbitrary_all<G: Gen>(g: &mut G, refs: Refs, ctx: Vector<String>) -> Term {
    let n = arbitrary_name(g);
    let u: Uses = Arbitrary::arbitrary(g);
    let mut ctx2 = ctx.clone();
    ctx2.push_front(n.clone());
    All(
      None,
      u,
      n,
      Box::new(arbitrary_term(g, refs.clone(), ctx)),
      Box::new(arbitrary_term(g, refs, ctx2)),
    )
  }

  fn arbitrary_var<G: Gen>(g: &mut G, ctx: Vector<String>) -> Term {
    match ctx.iter().choose(g) {
      Some(n) => {
        let (i, _) = ctx.iter().enumerate().find(|(_, x)| *x == n).unwrap();
        Var(None, n.clone(), i as u64)
      }
      None => Term::Typ(None),
    }
  }

  pub fn test_refs() -> Refs {
    let inp = "((def id \"\" (forall ω A Type A) (lambda x x)) (def id2 \"\" \
               (forall ω A Type A) (lambda x x)))";
    let (_, refs) = crate::defs::Defs::decode(
      HashMap::new(),
      hashexpr::parse(inp).unwrap().1,
    )
    .unwrap();
    refs
  }

  fn arbitrary_ref<G: Gen>(g: &mut G, refs: Refs, ctx: Vector<String>) -> Term {
    match refs.iter().filter(|(n, _)| !ctx.contains(n)).choose(g) {
      Some((n, (d, a))) => Ref(None, n.clone(), *d, *a),
      None => Term::Typ(None),
    }
  }

  pub fn arbitrary_term<G: Gen>(
    g: &mut G,
    refs: Refs,
    ctx: Vector<String>,
  ) -> Term {
    let len = ctx.len();
    if len == 0 {
      arbitrary_lam(g, refs, ctx)
    }
    else {
      let x: u32 = g.gen_range(0, 27);
      match x {
        //0 => arbitrary_all(g, refs, ctx.clone()),
        // 1 => arbitrary_let(g, refs, ctx.clone()),
        2 | 3 => arbitrary_lam(g, refs, ctx.clone()),
        // 4 | 5 => arbitrary_slf(g, refs, ctx.clone()),
        6 | 7 => Term::App(
          None,
          Box::new(arbitrary_term(g, refs.clone(), ctx.clone())),
          Box::new(arbitrary_term(g, refs, ctx.clone())),
        ),
       // 8 | 9 => Term::Ann(
       //   None,
       //   Box::new(arbitrary_term(g, refs.clone(), ctx.clone())),
       //   Box::new(arbitrary_term(g, refs, ctx.clone())),
       // ),
        //10 | 11 => {
        //  Term::Dat(None, Box::new(arbitrary_term(g, refs, ctx.clone())))
        //}
        //12 | 13 => {
        //  Term::Cse(None, Box::new(arbitrary_term(g, refs, ctx.clone())))
        //}
        //14 | 15 => Term::Typ(None),
        16 | 17 => arbitrary_var(g, ctx),
        _ => arbitrary_var(g, ctx),
        //18 | 19 => Term::Lit(None, Arbitrary::arbitrary(g)),
        //22 | 23 => Term::Opr(None, Arbitrary::arbitrary(g)),
        //_ => arbitrary_ref(g, refs, ctx),
      }
    }
  }

  impl Arbitrary for Term {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      arbitrary_term(g, test_refs(), Vector::new())
    }
  }

  #[quickcheck]
  fn term_encode_decode(x: Term) -> bool {
    match Term::decode(test_refs(), Vector::new(), x.clone().encode()) {
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
    );
    // let (id_def, id_ast) = test_refs().get("id").unwrap();
    // let x = Term::Ref(None, String::from("id"), id_def, id_ast);
    // assert_eq!(f.clone(), x);
  }
}
