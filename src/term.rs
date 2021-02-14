use crate::{
  decode_error::{
    or_else_join,
    DecodeError,
    Expected,
  },
  definition::Definition,
  hashspace,
  unembed_error::UnembedError,
};

use crate::{
  anon_term::AnonTerm,
  meta_term::MetaTerm,
};

pub use crate::core::{
  literal::{
    LitType,
    Literal,
  },
  primop::PrimOp,
  uses::Uses,
};

pub use hashexpr::link::Link;
use hashexpr::{
  position::Pos,
  AVal,
  AVal::*,
  Expr,
  Expr::Atom,
};

use im::{
  OrdMap,
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
  LTy(Option<Pos>, LitType),
  Opr(Option<Pos>, PrimOp),
}

#[derive(Clone, Debug)]
pub struct Def {
  pub pos: Option<Pos>,
  pub name: String,
  pub docs: String,
  pub typ_: Term,
  pub term: Term,
}

impl PartialEq for Def {
  fn eq(&self, other: &Def) -> bool {
    self.name == other.name
      && self.typ_ == other.typ_
      && self.term == other.term
  }
}

/// A map of names to pairs of links. The first link points to the
/// Definition, the second to the AnonTerm
pub type Defs = OrdMap<String, (Link, Link)>;

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
      (Self::Ref(_, na, da, aa), Self::Ref(_, nb, db, ab)) => {
        na == nb && da == db && aa == ab
      }
      (
        Self::Let(_, ra, ua, na, ta, xa, ba),
        Self::Let(_, rb, ub, nb, tb, xb, bb),
      ) => ra == rb && ua == ub && na == nb && ta == tb && xa == xb && ba == bb,
      (Self::Typ(_), Self::Typ(_)) => true,
      (Self::Ann(_, xa, ta), Self::Ann(_, xb, tb)) => xa == xb && ta == tb,
      (Self::Lit(_, a), Self::Lit(_, b)) => a == b,
      (Self::LTy(_, a), Self::LTy(_, b)) => a == b,
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
        LTy(..) => true,
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
          format!("{} {}", apps(ff, fa), parens(arg))
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
      LTy(_, lty) => write!(f, "{}", lty),
      Opr(_, opr) => write!(f, "{}", opr),
    }
  }
}

impl Term {
  pub fn encode(self) -> Expr {
    match self {
      Self::Var(_, nam, _) => symb!(nam),
      Self::Lam(_, nam, bod) => {
        cons!(None, symb!("lambda"), symb!(nam), bod.encode())
      }
      Self::App(_, fun, arg) => cons!(None, fun.encode(), arg.encode()),
      Self::All(_, uses, nam, typ, bod) => {
        cons!(
          None,
          symb!("forall"),
          uses.encode(),
          symb!(nam),
          typ.encode(),
          bod.encode()
        )
      }
      Self::Slf(_, nam, bod) => {
        cons!(None, symb!("self"), symb!(nam), bod.encode())
      }
      Self::Dat(_, bod) => {
        cons!(None, symb!("data"), bod.encode())
      }
      Self::Cse(_, bod) => {
        cons!(None, symb!("case"), bod.encode())
      }
      Self::Ref(_, nam, ..) => symb!(nam),
      Self::Let(_, rec, uses, nam, typ, exp, bod) => {
        let ctor = if rec { symb!("letrec") } else { symb!("let") };
        cons!(
          None,
          ctor,
          uses.encode(),
          symb!(nam),
          typ.encode(),
          exp.encode(),
          bod.encode()
        )
      }
      Self::Typ(_) => symb!("Type"),
      Self::Ann(_, typ, trm) => {
        cons!(None, symb!("type"), typ.encode(), trm.encode())
      }
      Self::Lit(_, lit) => lit.encode(),
      Self::LTy(_, lty) => lty.encode(),
      Self::Opr(_, opr) => opr.encode(),
    }
  }

  pub fn decode(
    defs: Defs,
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
            let (def_link, ast_link) = defs
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
            [uses, Atom(_, Symbol(nam)), typ, exp, bod] => {
              let uses = Uses::decode(uses.to_owned())?;
              let typ =
                Term::decode(defs.to_owned(), ctx.to_owned(), typ.to_owned())?;
              let mut new_ctx = ctx.clone();
              new_ctx.push_front(nam.clone());
              let exp =
                Term::decode(defs.to_owned(), new_ctx.clone(), exp.to_owned())?;
              let bod = Term::decode(defs.to_owned(), new_ctx, bod.to_owned())?;
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
                Term::decode(defs.to_owned(), ctx.to_owned(), typ.to_owned())?;
              let exp =
                Term::decode(defs.to_owned(), ctx.to_owned(), exp.to_owned())?;
              let mut new_ctx = ctx.clone();
              new_ctx.push_front(nam.clone());
              let bod = Term::decode(defs.to_owned(), new_ctx, bod.to_owned())?;
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
                Term::decode(defs.to_owned(), ctx.to_owned(), bod.to_owned())?;
              Ok(Self::Dat(pos, Box::new(bod)))
            }
            _ => Err(DecodeError::new(pos, vec![Expected::Data])),
          }
        }
        [Atom(_, Symbol(n)), tail @ ..] if *n == String::from("case") => {
          match tail {
            [bod] => {
              let bod =
                Term::decode(defs.to_owned(), ctx.to_owned(), bod.to_owned())?;
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
              let bod = Term::decode(defs.to_owned(), new_ctx, bod.to_owned())?;
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
              let bod = Term::decode(defs.to_owned(), new_ctx, bod.to_owned())?;
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
                Term::decode(defs.to_owned(), ctx.to_owned(), typ.to_owned())?;
              let mut new_ctx = ctx.clone();
              new_ctx.push_front(nam.clone());
              let bod = Term::decode(defs.to_owned(), new_ctx, bod.to_owned())?;
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
                Term::decode(defs.to_owned(), ctx.to_owned(), typ.to_owned())?;
              let trm =
                Term::decode(defs.to_owned(), ctx.to_owned(), trm.to_owned())?;
              Ok(Self::Ann(pos, Box::new(typ), Box::new(trm)))
            }
            _ => Err(DecodeError::new(pos, vec![Expected::Annotation])),
          }
        }
        [Atom(_, Symbol(n)), tail @ ..] if *n == String::from("exception") => {
          match tail {
            [Atom(_, Text(err))] => {
              Ok(Self::Lit(pos, Literal::Exception(err.clone())))
            }
            _ => Err(DecodeError::new(pos, vec![Expected::ExceptionLiteral])),
          }
        }
        [fun, arg] => {
          let fun =
            Term::decode(defs.to_owned(), ctx.to_owned(), fun.to_owned())?;
          let arg =
            Term::decode(defs.to_owned(), ctx.to_owned(), arg.to_owned())?;
          Ok(Self::App(pos, Box::new(fun), Box::new(arg)))
        }

        _ => Err(DecodeError::new(pos, vec![Expected::Constructor])),
      },
    }
  }

  pub fn embed(self) -> (AnonTerm, MetaTerm) {
    match self {
      Self::Var(pos, _, idx) => (
        AnonTerm::Ctor(String::from("var"), vec![AnonTerm::Vari(idx)]),
        MetaTerm::Ctor(pos, vec![MetaTerm::Leaf]),
      ),
      Self::Ref(pos, name, def, ast) => (
        AnonTerm::Ctor(String::from("ref"), vec![AnonTerm::Link(ast)]),
        MetaTerm::Ctor(pos, vec![MetaTerm::Link(name, def)]),
      ),
      Self::Lit(pos, lit) => (
        AnonTerm::Ctor(String::from("lit"), vec![AnonTerm::Data(
          lit.encode().serialize(),
        )]),
        MetaTerm::Ctor(pos, vec![MetaTerm::Leaf]),
      ),
      Self::LTy(pos, lty) => (
        AnonTerm::Ctor(String::from("lty"), vec![AnonTerm::Data(
          lty.encode().serialize(),
        )]),
        MetaTerm::Ctor(pos, vec![MetaTerm::Leaf]),
      ),
      Self::Opr(pos, opr) => (
        AnonTerm::Ctor(String::from("opr"), vec![AnonTerm::Data(
          opr.encode().serialize(),
        )]),
        MetaTerm::Ctor(pos, vec![MetaTerm::Leaf]),
      ),
      Self::Typ(pos) => (
        AnonTerm::Ctor(String::from("typ"), vec![]),
        MetaTerm::Ctor(pos, vec![]),
      ),
      Self::Lam(pos, name, body) => {
        let (anon, meta) = body.embed();
        (
          AnonTerm::Ctor(String::from("lam"), vec![AnonTerm::Bind(Box::new(
            anon,
          ))]),
          MetaTerm::Ctor(pos, vec![MetaTerm::Bind(
            name.to_owned(),
            Box::new(meta),
          )]),
        )
      }
      Self::Slf(pos, name, body) => {
        let (anon, meta) = body.embed();
        (
          AnonTerm::Ctor(String::from("slf"), vec![AnonTerm::Bind(Box::new(
            anon,
          ))]),
          MetaTerm::Ctor(pos, vec![MetaTerm::Bind(
            name.to_owned(),
            Box::new(meta),
          )]),
        )
      }
      Self::App(pos, fun, arg) => {
        let (fun_anon, fun_meta) = fun.embed();
        let (arg_anon, arg_meta) = arg.embed();
        (
          AnonTerm::Ctor(String::from("app"), vec![fun_anon, arg_anon]),
          MetaTerm::Ctor(pos, vec![fun_meta, arg_meta]),
        )
      }
      Self::Ann(pos, val, typ) => {
        let (val_anon, val_meta) = val.embed();
        let (typ_anon, typ_meta) = typ.embed();
        (
          AnonTerm::Ctor(String::from("ann"), vec![val_anon, typ_anon]),
          MetaTerm::Ctor(pos, vec![val_meta, typ_meta]),
        )
      }
      Self::Dat(pos, body) => {
        let (anon, meta) = body.embed();
        (
          AnonTerm::Ctor(String::from("dat"), vec![anon]),
          MetaTerm::Ctor(pos, vec![meta]),
        )
      }
      Self::Cse(pos, body) => {
        let (anon, meta) = body.embed();
        (
          AnonTerm::Ctor(String::from("cse"), vec![anon]),
          MetaTerm::Ctor(pos, vec![meta]),
        )
      }
      Self::All(pos, uses, name, typ_, body) => {
        let (typ_anon, typ_meta) = typ_.embed();
        let (bod_anon, bod_meta) = body.embed();
        (
          AnonTerm::Ctor(String::from("all"), vec![
            AnonTerm::Data(uses.encode().serialize()),
            typ_anon,
            bod_anon,
          ]),
          MetaTerm::Ctor(pos, vec![
            MetaTerm::Leaf,
            typ_meta,
            MetaTerm::Bind(name, Box::new(bod_meta)),
          ]),
        )
      }
      Self::Let(pos, true, uses, name, typ_, expr, body) => {
        let (typ_anon, typ_meta) = typ_.embed();
        let (exp_anon, exp_meta) = expr.embed();
        let (bod_anon, bod_meta) = body.embed();
        (
          AnonTerm::Ctor(String::from("rec"), vec![
            AnonTerm::Data(uses.encode().serialize()),
            typ_anon,
            AnonTerm::Bind(Box::new(exp_anon)),
            AnonTerm::Bind(Box::new(bod_anon)),
          ]),
          MetaTerm::Ctor(pos, vec![
            MetaTerm::Leaf,
            typ_meta,
            MetaTerm::Bind(name.clone(), Box::new(exp_meta)),
            MetaTerm::Bind(name, Box::new(bod_meta)),
          ]),
        )
      }
      Self::Let(pos, false, uses, name, typ_, expr, body) => {
        let (typ_anon, typ_meta) = typ_.embed();
        let (exp_anon, exp_meta) = expr.embed();
        let (bod_anon, bod_meta) = body.embed();
        (
          AnonTerm::Ctor(String::from("let"), vec![
            AnonTerm::Data(uses.encode().serialize()),
            typ_anon,
            exp_anon,
            AnonTerm::Bind(Box::new(bod_anon)),
          ]),
          MetaTerm::Ctor(pos, vec![
            MetaTerm::Leaf,
            typ_meta,
            exp_meta,
            MetaTerm::Bind(name, Box::new(bod_meta)),
          ]),
        )
      }
    }
  }

  pub fn unembed(
    ctx: Vector<String>,
    anon_term: &AnonTerm,
    name_meta: &MetaTerm,
  ) -> Result<Term, UnembedError> {
    match (anon_term, name_meta) {
      (AnonTerm::Ctor(n, xs), MetaTerm::Ctor(pos, ys)) => {
        match (&n[..], xs.as_slice(), ys.as_slice()) {
          ("var", [AnonTerm::Vari(idx)], [MetaTerm::Leaf]) => {
            match ctx.iter().enumerate().find(|(i, _)| (*i as u64) == *idx) {
              Some((_, n)) => Ok(Term::Var(*pos, n.to_owned(), *idx)),
              None => Err(UnembedError::FreeVariable),
            }
          }
          ("ref", [AnonTerm::Link(ast)], [MetaTerm::Link(name, def)]) => {
            Ok(Term::Ref(*pos, name.clone(), *def, *ast))
          }
          ("lit", [AnonTerm::Data(data)], [MetaTerm::Leaf]) => {
            let (_, lit) = hashexpr::Expr::deserialize(&data)
              .map_err(|_| UnembedError::DeserialError)?;
            let lit =
              Literal::decode(lit).map_err(|e| UnembedError::DecodeError(e))?;
            Ok(Term::Lit(*pos, lit))
          }
          ("lty", [AnonTerm::Data(data)], [MetaTerm::Leaf]) => {
            let (_, lty) = hashexpr::Expr::deserialize(&data)
              .map_err(|_| UnembedError::DeserialError)?;
            let lty =
              LitType::decode(lty).map_err(|e| UnembedError::DecodeError(e))?;
            Ok(Term::LTy(*pos, lty))
          }
          ("opr", [AnonTerm::Data(data)], [MetaTerm::Leaf]) => {
            let (_, opr) = hashexpr::Expr::deserialize(&data)
              .map_err(|_| UnembedError::DeserialError)?;
            let opr =
              PrimOp::decode(opr).map_err(|e| UnembedError::DecodeError(e))?;
            Ok(Term::Opr(*pos, opr))
          }
          ("typ", [], []) => Ok(Term::Typ(*pos)),
          ("dat", [anon], [meta]) => {
            let body = Term::unembed(ctx, anon, meta)?;
            Ok(Term::Dat(*pos, Box::new(body)))
          }
          ("cse", [anon], [meta]) => {
            let body = Term::unembed(ctx, anon, meta)?;
            Ok(Term::Cse(*pos, Box::new(body)))
          }
          ("lam", [AnonTerm::Bind(anon)], [MetaTerm::Bind(n, meta)]) => {
            let mut new_ctx = ctx.clone();
            new_ctx.push_front(n.clone());
            let body = Term::unembed(new_ctx, &anon, meta)?;
            Ok(Term::Lam(*pos, n.clone(), Box::new(body)))
          }
          ("slf", [AnonTerm::Bind(anon)], [MetaTerm::Bind(n, meta)]) => {
            let mut new_ctx = ctx.clone();
            new_ctx.push_front(n.clone());
            let body = Term::unembed(new_ctx, &anon, meta)?;
            Ok(Term::Slf(*pos, n.clone(), Box::new(body)))
          }
          ("app", [fanon, aanon], [fmeta, ameta]) => {
            let fun = Term::unembed(ctx.clone(), fanon, fmeta)?;
            let arg = Term::unembed(ctx.clone(), aanon, ameta)?;
            Ok(Term::App(*pos, Box::new(fun), Box::new(arg)))
          }
          ("ann", [xanon, tanon], [xmeta, tmeta]) => {
            let xpr = Term::unembed(ctx.clone(), xanon, xmeta)?;
            let typ = Term::unembed(ctx.clone(), tanon, tmeta)?;
            Ok(Term::Ann(*pos, Box::new(xpr), Box::new(typ)))
          }
          (
            "all",
            [AnonTerm::Data(uses), tanon, banon],
            [MetaTerm::Leaf, tmeta, MetaTerm::Bind(n, bmeta)],
          ) => {
            let (_, uses) = hashexpr::Expr::deserialize(&uses)
              .map_err(|_| UnembedError::DeserialError)?;
            let uses =
              Uses::decode(uses).map_err(|e| UnembedError::DecodeError(e))?;
            let typ_ = Term::unembed(ctx.clone(), tanon, tmeta)?;
            let mut new_ctx = ctx.clone();
            new_ctx.push_front(n.clone());
            let body = Term::unembed(new_ctx, banon, bmeta)?;
            Ok(Term::All(*pos, uses, n.clone(), Box::new(typ_), Box::new(body)))
          }
          (
            "rec",
            [AnonTerm::Data(uses), tanon, AnonTerm::Bind(xanon), AnonTerm::Bind(banon)],
            [MetaTerm::Leaf, tmeta, MetaTerm::Bind(n1, xmeta), MetaTerm::Bind(n2, bmeta)],
          ) => {
            let name =
              if n1 == n2 { Ok(n1) } else { Err(UnembedError::BadLet) }?;
            let (_, uses) = hashexpr::Expr::deserialize(&uses)
              .map_err(|_| UnembedError::DeserialError)?;
            let uses =
              Uses::decode(uses).map_err(|e| UnembedError::DecodeError(e))?;
            let typ_ = Term::unembed(ctx.clone(), tanon, tmeta)?;
            let mut new_ctx = ctx.clone();
            new_ctx.push_front(name.clone());
            let exp = Term::unembed(new_ctx.clone(), &xanon, xmeta)?;
            let body = Term::unembed(new_ctx, &banon, bmeta)?;
            Ok(Term::Let(
              *pos,
              true,
              uses,
              name.clone(),
              Box::new(typ_),
              Box::new(exp),
              Box::new(body),
            ))
          }
          (
            "let",
            [AnonTerm::Data(uses), tanon, xanon, AnonTerm::Bind(banon)],
            [MetaTerm::Leaf, tmeta, xmeta, MetaTerm::Bind(name, bmeta)],
          ) => {
            let (_, uses) = hashexpr::Expr::deserialize(&uses)
              .map_err(|_| UnembedError::DeserialError)?;
            let uses =
              Uses::decode(uses).map_err(|e| UnembedError::DecodeError(e))?;
            let typ_ = Term::unembed(ctx.clone(), tanon, tmeta)?;
            let exp = Term::unembed(ctx.clone(), xanon, xmeta)?;
            let mut new_ctx = ctx;
            new_ctx.push_front(name.clone());
            let body = Term::unembed(new_ctx, &banon, bmeta)?;
            Ok(Term::Let(
              *pos,
              false,
              uses,
              name.clone(),
              Box::new(typ_),
              Box::new(exp),
              Box::new(body),
            ))
          }
          _ => Err(UnembedError::UnexpectedCtor(
            anon_term.clone(),
            name_meta.clone(),
          )),
        }
      }
      _ => {
        Err(UnembedError::UnexpectedCtor(anon_term.clone(), name_meta.clone()))
      }
    }
  }
}

impl Def {
  pub fn new(
    pos: Option<Pos>,
    name: String,
    docs: String,
    typ_: Term,
    term: Term,
  ) -> Self {
    Def { pos, name, docs, typ_, term }
  }

  pub fn encode(self) -> Expr {
    Expr::Cons(self.pos, vec![
      symb!("def"),
      symb!(self.name),
      text!(self.docs),
      Term::encode(self.typ_),
      Term::encode(self.term),
    ])
  }

  pub fn decode(defs: Defs, expr: Expr) -> Result<Self, DecodeError> {
    match expr {
      Expr::Cons(pos, xs) => match xs.as_slice() {
        [Atom(_, Symbol(n)), tail @ ..] if *n == String::from("def") => {
          match tail {
            [Atom(_, Symbol(name)), Atom(_, Text(docs)), typ_, term] => {
              let mut ctx = Vector::new();
              let typ_ =
                Term::decode(defs.to_owned(), ctx.to_owned(), typ_.to_owned())?;
              ctx.push_front(name.clone());
              let term = Term::decode(defs, ctx, term.to_owned())?;
              Ok(Def::new(
                pos.to_owned(),
                name.to_owned(),
                docs.to_owned(),
                typ_,
                term,
              ))
            }
            _ => Err(DecodeError::new(pos, vec![Expected::TermDefContents])),
          }
        }
        _ => Err(DecodeError::new(pos, vec![Expected::TermDef])),
      },
      _ => Err(DecodeError::new(expr.position(), vec![Expected::TermDef])),
    }
  }

  pub fn embed(self) -> (Definition, AnonTerm, AnonTerm) {
    let (type_anon, type_meta) = self.typ_.embed();
    let (term_anon, term_meta) = self.term.embed();
    let d = Definition {
      pos: self.pos,
      name: self.name,
      docs: self.docs,
      term_anon: term_anon.clone().encode().link(),
      type_anon: type_anon.clone().encode().link(),
      term_meta,
      type_meta,
    };
    (d, type_anon, term_anon)
  }

  pub fn unembed(
    def: Definition,
    type_anon: AnonTerm,
    term_anon: AnonTerm,
  ) -> Result<Self, UnembedError> {
    let typ_ = Term::unembed(Vector::new(), &type_anon, &def.type_meta)?;
    let term = Term::unembed(
      Vector::from(vec![def.name.clone()]),
      &term_anon,
      &def.term_meta,
    )?;
    Ok(Def::new(def.pos, def.name, def.docs, typ_, term))
  }

  pub fn unembed_link(defn: Link) -> Result<Self, UnembedError> {
    let def = hashspace::get(defn).ok_or(UnembedError::UnknownLink(defn))?;
    let def =
      Definition::decode(def).map_err(|e| UnembedError::DecodeError(e))?;
    let type_anon =
      hashspace::get(def.type_anon).ok_or(UnembedError::UnknownLink(defn))?;
    let type_anon =
      AnonTerm::decode(type_anon).map_err(|e| UnembedError::DecodeError(e))?;
    let term_anon =
      hashspace::get(def.term_anon).ok_or(UnembedError::UnknownLink(defn))?;
    let term_anon =
      AnonTerm::decode(term_anon).map_err(|e| UnembedError::DecodeError(e))?;
    Def::unembed(def, type_anon, term_anon)
  }
}

impl fmt::Display for Def {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.docs.is_empty() {
      write!(f, "def {} : {} = {}", self.name, self.typ_, self.term)
    }
    else {
      write!(
        f,
        "//{}\n def {} : {} = {}",
        self.docs, self.name, self.typ_, self.term
      )
    }
  }
}

#[cfg(test)]
pub mod tests {
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

  fn arbitrary_lam<G: Gen>(g: &mut G, defs: Defs, ctx: Vector<String>) -> Term {
    let n = arbitrary_name(g);
    let mut ctx2 = ctx.clone();
    ctx2.push_front(n.clone());
    Lam(None, n, Box::new(arbitrary_term(g, defs, ctx2)))
  }

  fn arbitrary_slf<G: Gen>(g: &mut G, defs: Defs, ctx: Vector<String>) -> Term {
    let n = arbitrary_name(g);
    let mut ctx2 = ctx.clone();
    ctx2.push_front(n.clone());
    Slf(None, n, Box::new(arbitrary_term(g, defs, ctx2)))
  }
  fn arbitrary_let<G: Gen>(g: &mut G, defs: Defs, ctx: Vector<String>) -> Term {
    let rec: bool = Arbitrary::arbitrary(g);
    let n = arbitrary_name(g);
    let u: Uses = Arbitrary::arbitrary(g);
    let typ = Box::new(arbitrary_term(g, defs.clone(), ctx.clone()));
    if rec {
      let mut ctx2 = ctx.clone();
      ctx2.push_front(n.clone());
      let exp = Box::new(arbitrary_term(g, defs.clone(), ctx2.clone()));
      let bod = Box::new(arbitrary_term(g, defs, ctx2));
      Let(None, rec, u, n, typ, exp, bod)
    }
    else {
      let mut ctx2 = ctx.clone();
      ctx2.push_front(n.clone());
      let exp = Box::new(arbitrary_term(g, defs.clone(), ctx.clone()));
      let bod = Box::new(arbitrary_term(g, defs, ctx2));
      Let(None, rec, u, n, typ, exp, bod)
    }
  }

  fn arbitrary_all<G: Gen>(g: &mut G, defs: Defs, ctx: Vector<String>) -> Term {
    let n = arbitrary_name(g);
    let u: Uses = Arbitrary::arbitrary(g);
    let mut ctx2 = ctx.clone();
    ctx2.push_front(n.clone());
    All(
      None,
      u,
      n,
      Box::new(arbitrary_term(g, defs.clone(), ctx)),
      Box::new(arbitrary_term(g, defs, ctx2)),
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

  pub fn test_defs() -> Defs {
    let inp = "(def id \"\" (forall ω A Type A) (lambda x x))";
    let d =
      Def::decode(OrdMap::new(), hashexpr::parse(inp).unwrap().1).unwrap();
    let (d, _, t) = d.embed();
    let mut defs = OrdMap::new();
    defs.insert(String::from("id"), (d.encode().link(), t.encode().link()));
    defs
  }

  fn arbitrary_ref<G: Gen>(g: &mut G, defs: Defs, ctx: Vector<String>) -> Term {
    match defs.iter().filter(|(n, _)| !ctx.contains(n)).choose(g) {
      Some((n, (d, a))) => Ref(None, n.clone(), *d, *a),
      None => Term::Typ(None),
    }
  }

  pub fn arbitrary_term<G: Gen>(
    g: &mut G,
    defs: Defs,
    ctx: Vector<String>,
  ) -> Term {
    let len = ctx.len();
    if len == 0 {
      arbitrary_lam(g, defs, ctx)
    }
    else {
      let x: u32 = g.gen_range(0, 27);
      match x {
        0 => arbitrary_all(g, defs, ctx.clone()),
        // 1 => arbitrary_let(g, defs, ctx.clone()),
        2 | 3 => arbitrary_lam(g, defs, ctx.clone()),
        4 | 5 => arbitrary_slf(g, defs, ctx.clone()),
        6 | 7 => Term::App(
          None,
          Box::new(arbitrary_term(g, defs.clone(), ctx.clone())),
          Box::new(arbitrary_term(g, defs, ctx.clone())),
        ),
        8 | 9 => Term::Ann(
          None,
          Box::new(arbitrary_term(g, defs.clone(), ctx.clone())),
          Box::new(arbitrary_term(g, defs, ctx.clone())),
        ),
        10 | 11 => {
          Term::Dat(None, Box::new(arbitrary_term(g, defs, ctx.clone())))
        }
        12 | 13 => {
          Term::Cse(None, Box::new(arbitrary_term(g, defs, ctx.clone())))
        }
        14 | 15 => Term::Typ(None),
        16 | 17 => arbitrary_var(g, ctx),
        18 | 19 => Term::Lit(None, Arbitrary::arbitrary(g)),
        20 | 21 => Term::LTy(None, Arbitrary::arbitrary(g)),
        22 | 23 => Term::Opr(None, Arbitrary::arbitrary(g)),
        _ => arbitrary_ref(g, defs, ctx),
      }
    }
  }

  impl Arbitrary for Term {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      arbitrary_term(g, test_defs(), Vector::new())
    }
  }

  pub fn arbitrary_def<G: Gen>(g: &mut G, defs: Defs, name: String) -> Def {
    let mut ctx = Vector::new();
    ctx.push_front(name.clone());
    Def {
      pos: None,
      name,
      docs: String::from(""),
      typ_: arbitrary_term(g, defs.clone(), Vector::new()),
      term: arbitrary_term(g, defs, ctx),
    }
  }

  impl Arbitrary for Def {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
      let name = arbitrary_name(g);
      arbitrary_def(g, OrdMap::new(), name)
    }
  }

  #[quickcheck]
  fn term_encode_decode(x: Term) -> bool {
    match Term::decode(test_defs(), Vector::new(), x.clone().encode()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
  #[quickcheck]
  fn term_embed_unembed(x: Term) -> bool {
    let (a, m) = x.clone().embed();
    match Term::unembed(Vector::new(), &a, &m) {
      Ok(y) => {
        if x == y {
          true
        }
        else {
          //          println!("x: {:?}", x);
          //          println!("y: {:?}", y);
          false
        }
      }
      e => {
        //        println!("x: {:?}", x);
        //        println!("a: {:?}", a);
        //        println!("m: {:?}", m);
        //        println!("e: {:?}", e);
        false
      }
    }
  }
  #[quickcheck]
  fn def_embed_unembed(x: Def) -> bool {
    let (d, ta, xa) = x.clone().embed();
    match Def::unembed(d, ta, xa) {
      Ok(y) => {
        if x == y {
          true
        }
        else {
          //          println!("x: {:?}", x);
          //          println!("y: {:?}", y);
          false
        }
      }
      e => {
        //        println!("x: {:?}", x);
        //        println!("a: {:?}", a);
        //        println!("m: {:?}", m);
        //        println!("e: {:?}", e);
        false
      }
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
        OrdMap::new(),
        vec![String::from("x")].into(),
        hashexpr::parse("x").unwrap().1,
      )
    );

    let f =
      Lam(None, String::from("x"), Box::new(Var(None, String::from("x"), 0)));
    assert_eq!(
      Ok(f.clone()),
      Term::decode(
        OrdMap::new(),
        Vector::new(),
        hashexpr::parse("(lambda x x)").unwrap().1
      )
    );

    assert_eq!(
      Ok(b.clone()),
      Term::decode(
        OrdMap::new(),
        Vector::new(),
        hashexpr::parse("((lambda x x) (lambda x x))").unwrap().1
      )
    );

    // let (id_def, id_ast) = test_defs().get("id").unwrap();
    // let x = Term::Ref(None, String::from("id"), id_def, id_ast);
    // assert_eq!(f.clone(), x);
  }
}
