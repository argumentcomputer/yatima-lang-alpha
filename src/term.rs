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
  atom,
  atom::Atom::*,
  position::Pos,
  Expr,
  Expr::*,
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
  App(Option<Pos>, Box<(Term, Term)>),
  All(Option<Pos>, Uses, String, Box<(Term, Term)>),
  Slf(Option<Pos>, String, Box<Term>),
  Dat(Option<Pos>, Box<Term>),
  Cse(Option<Pos>, Box<Term>),
  Ref(Option<Pos>, String, Link, Link),
  Let(Option<Pos>, bool, Uses, String, Box<(Term, Term, Term)>),
  Typ(Option<Pos>),
  Ann(Option<Pos>, Box<(Term, Term)>),
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
pub type Refs = HashMap<String, (Link, Link)>;
pub type Defs = HashMap<Link, Def>;

impl PartialEq for Term {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::Var(_, na, ia), Self::Var(_, nb, ib)) => na == nb && ia == ib,
      (Self::Lam(_, na, ba), Self::Lam(_, nb, bb)) => na == nb && ba == bb,
      (Self::App(_, ta), Self::App(_, tb)) => ta.0 == tb.0 && ta.1 == tb.1,
      (Self::All(_, ua, na, ta), Self::All(_, ub, nb, tb)) => {
        ua == ub && na == nb && ta.0 == tb.0 && ta.1 == tb.1
      }
      (Self::Slf(_, na, ba), Self::Slf(_, nb, bb)) => na == nb && ba == bb,
      (Self::Dat(_, ba), Self::Dat(_, bb)) => ba == bb,
      (Self::Cse(_, ba), Self::Cse(_, bb)) => ba == bb,
      (Self::Ref(_, na, da, aa), Self::Ref(_, nb, db, ab)) => {
        na == nb && da == db && aa == ab
      }
      (
        Self::Let(_, ra, ua, na, ta),
        Self::Let(_, rb, ub, nb, tb),
      ) => ra == rb && ua == ub && na == nb && ta.0 == tb.0 && ta.1 == tb.1 && ta.2 == tb.2,
      (Self::Typ(_), Self::Typ(_)) => true,
      (Self::Ann(_, ta), Self::Ann(_, tb)) => ta.0 == tb.0 && ta.1 == tb.1,
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
        All(_, bod_use, bod_nam, bod) => {
          format!(
            " ({}{}: {}){}",
            uses(use_),
            name(nam),
            typ,
            alls(bod_use, bod_nam, &bod.0, &bod.1)
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
        (App(_, f), App(_, a)) => {
          format!("{} ({})", apps(&f.0, &f.1), apps(&a.0, &a.1))
        }
        (App(_, f), arg) => {
          format!("{} {}", apps(&f.0, &f.1), parens(arg))
        }
        (fun, App(_, a)) => {
          format!("{} ({})", parens(fun), apps(&a.0, &a.1))
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
      App(_, terms) => write!(f, "{}", apps(&terms.0, &terms.1)),
      Let(_, true, u, n, terms) => {
        write!(f, "letrec {}{}: {} := {}; {}", uses(u), name(n), terms.0, terms.1, terms.2)
      }
      Let(_, false, u, n, terms) => {
        write!(f, "let {}{}: {} := {}; {}", uses(u), name(n), terms.0, terms.1, terms.2)
      }
      Slf(_, nam, bod) => write!(f, "@{} {}", name(nam), bod),
      All(_, us_, nam, terms) => write!(f, "∀{}", alls(us_, nam, &terms.0, &terms.1)),
      Ann(_, terms) => write!(f, "{} :: {}", parens(&terms.1), parens(&terms.0)),
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
        let (anon, meta) = (*body).clone().embed();
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
        let (anon, meta) = (*body).clone().embed();
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
      Self::App(pos, terms) => {
        let (fun_anon, fun_meta) = terms.0.clone().embed();
        let (arg_anon, arg_meta) = terms.1.clone().embed();
        (
          AnonTerm::Ctor(String::from("app"), vec![fun_anon, arg_anon]),
          MetaTerm::Ctor(pos, vec![fun_meta, arg_meta]),
        )
      }
      Self::Ann(pos, terms) => {
        let (val_anon, val_meta) = terms.0.clone().embed();
        let (typ_anon, typ_meta) = terms.1.clone().embed();
        (
          AnonTerm::Ctor(String::from("ann"), vec![val_anon, typ_anon]),
          MetaTerm::Ctor(pos, vec![val_meta, typ_meta]),
        )
      }
      Self::Dat(pos, body) => {
        let (anon, meta) = (*body).clone().embed();
        (
          AnonTerm::Ctor(String::from("dat"), vec![anon]),
          MetaTerm::Ctor(pos, vec![meta]),
        )
      }
      Self::Cse(pos, body) => {
        let (anon, meta) = (*body).clone().embed();
        (
          AnonTerm::Ctor(String::from("cse"), vec![anon]),
          MetaTerm::Ctor(pos, vec![meta]),
        )
      }
      Self::All(pos, uses, name, terms) => {
        let (typ_anon, typ_meta) = terms.0.clone().embed();
        let (bod_anon, bod_meta) = terms.1.clone().embed();
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
      Self::Let(pos, true, uses, name, terms) => {
        let (typ_anon, typ_meta) = terms.0.clone().embed();
        let (exp_anon, exp_meta) = terms.1.clone().embed();
        let (bod_anon, bod_meta) = terms.2.clone().embed();
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
      Self::Let(pos, false, uses, name, terms) => {
        let (typ_anon, typ_meta) = terms.0.clone().embed();
        let (exp_anon, exp_meta) = terms.1.clone().embed();
        let (bod_anon, bod_meta) = terms.2.clone().embed();
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
            Ok(Term::App(*pos, Box::new((fun, arg))))
          }
          ("ann", [xanon, tanon], [xmeta, tmeta]) => {
            let xpr = Term::unembed(ctx.clone(), xanon, xmeta)?;
            let typ = Term::unembed(ctx.clone(), tanon, tmeta)?;
            Ok(Term::Ann(*pos, Box::new((xpr, typ))))
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
            Ok(Term::All(*pos, uses, n.clone(), Box::new((typ_, body))))
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
              Box::new((typ_, exp, body))
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
              Box::new((typ_, exp, body))
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

  pub fn get_link(defn: Link) -> Result<Self, UnembedError> {
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

pub fn refs_to_defs(refs: Refs) -> Result<Defs, UnembedError> {
  let mut def_map = HashMap::new();
  for (_, (d, _)) in refs {
    let def = Def::get_link(d)?;
    def_map.insert(d, def);
  }
  Ok(def_map)
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

  pub fn arbitrary_link(g: &mut Gen) -> hashexpr::Link {
  //pub fn arbitrary_link() -> Box<dyn Fn(&mut Gen) -> hashexpr::Link> {
    //Box::new(move |g: &mut Gen| {
      let mut bytes: [u8; 32] = [0; 32];
      for x in bytes.iter_mut() {
        *x = Arbitrary::arbitrary(g);
      }
      hashexpr::Link::from(bytes)
    //})
  }

  pub fn arbitrary_name(g: &mut Gen) -> String {
    let s: String = Arbitrary::arbitrary(g);
    let mut s: String = s
      .chars()
      .filter(|x| {
        crate::parse::term::is_valid_symbol_char(*x)
          && char::is_ascii_alphabetic(x)
      })
      .collect();
    s.truncate(1);
    format!("_{}", s)
  }

  fn arbitrary_lam(
    refs: Refs,
    ctx: Vector<String>,
  ) -> Box<dyn Fn(&mut Gen) -> Term> {
    Box::new(move |g: &mut Gen| {
      let n = arbitrary_name(g);
      let mut ctx2 = ctx.clone();
      ctx2.push_front(n.clone());
      Lam(None, n, Box::new(arbitrary_term(g, refs.clone(), ctx2)))
    })
  }

  fn arbitrary_slf(
    refs: Refs,
    ctx: Vector<String>,
  ) -> Box<dyn Fn(&mut Gen) -> Term> {
    Box::new(move |g: &mut Gen| {
      let n = arbitrary_name(g);
      let mut ctx2 = ctx.clone();
      ctx2.push_front(n.clone());
      Slf(None, n, Box::new(arbitrary_term(g, refs.clone(), ctx2)))
    })
  }

  fn arbitrary_let(
    refs: Refs,
    ctx: Vector<String>,
  ) -> Box<dyn Fn(&mut Gen) -> Term> {
    Box::new(move |g: &mut Gen| {
      let rec: bool = Arbitrary::arbitrary(g);
      let n = arbitrary_name(g);
      let u: Uses = Arbitrary::arbitrary(g);
      let typ = arbitrary_term(g, refs.clone(), ctx.clone());
      if rec {
        let mut ctx2 = ctx.clone();
        ctx2.push_front(n.clone());
        let exp = arbitrary_term(g, refs.clone(), ctx2.clone());
        let bod = arbitrary_term(g, refs.clone(), ctx2);
        Let(None, rec, u, n, Box::new((typ, exp, bod)))
      }
      else {
        let mut ctx2 = ctx.clone();
        ctx2.push_front(n.clone());
        let exp = arbitrary_term(g, refs.clone(), ctx.clone());
        let bod = arbitrary_term(g, refs.clone(), ctx2);
        Let(None, rec, u, n, Box::new((typ, exp, bod)))
      }
    })
  }

  fn arbitrary_all(
    refs: Refs,
    ctx: Vector<String>,
  ) -> Box<dyn Fn(&mut Gen) -> Term> {
    Box::new(move |g: &mut Gen| {
      let n = arbitrary_name(g);
      let u: Uses = Arbitrary::arbitrary(g);
      let mut ctx2 = ctx.clone();
      ctx2.push_front(n.clone());
      All(
        None,
        u,
        n,
        Box::new((arbitrary_term(g, refs.clone(), ctx.clone()), arbitrary_term(g, refs.clone(), ctx2)))
      )
    })
  }

  pub fn test_refs() -> Refs {
    // let inp = "(\"def\" \"id\" \"\" (\"forall\" \"ω\" \"A\" \"Type\" \"A\") \
    //           (\"lambda\" \"x\" \"x\"))";
    // let d =
    //  Def::decode(HashMap::new(), hashexpr::parse(inp).unwrap().1).unwrap();
    // let (d, _, t) = d.embed();
    // let mut refs = HashMap::new();
    // refs.insert(String::from("id"), (d.encode().link(), t.encode().link()));
    // refs
    HashMap::new()
  }

  fn arbitrary_var(ctx: Vector<String>) -> Box<dyn Fn(&mut Gen) -> Term> {
    Box::new(move |_g: &mut Gen| {
      if ctx.len() == 0 {
        return Term::Typ(None);
      }
      let mut rng = rand::thread_rng();
      let gen = rng.gen_range(0..ctx.len());
      let n = &ctx[gen];
      let (i, _) = ctx.iter().enumerate().find(|(_, x)| *x == n).unwrap();
      Var(None, n.clone(), i as u64)
    })
  }

  fn arbitrary_ref(
    refs: Refs,
    ctx: Vector<String>,
  ) -> Box<dyn Fn(&mut Gen) -> Term> {
    Box::new(move |_g: &mut Gen| {
      let mut rng = rand::thread_rng();
      let mut ref_iter = refs.iter().filter(|(n, _)| !ctx.contains(n));
      let len = ref_iter.by_ref().count();
      if len == 0 {
        return Term::Typ(None);
      }
      let gen = rng.gen_range(0..len);
      match ref_iter.nth(gen) {
        Some((n, (d, a))) => Ref(None, n.clone(), *d, *a),
        None => Term::Typ(None),
      }
    })
  }

  fn arbitrary_app(
    refs: Refs,
    ctx: Vector<String>,
  ) -> Box<dyn Fn(&mut Gen) -> Term> {
    Box::new(move |g: &mut Gen| {
      Term::App(
        None,
        Box::new((arbitrary_term(g, refs.clone(), ctx.clone()), arbitrary_term(g, refs.clone(), ctx.clone())))
      )
    })
  }

  fn arbitrary_ann(
    refs: Refs,
    ctx: Vector<String>,
  ) -> Box<dyn Fn(&mut Gen) -> Term> {
    Box::new(move |g: &mut Gen| {
      Term::Ann(
        None,
        Box::new((arbitrary_term(g, refs.clone(), ctx.clone()), arbitrary_term(g, refs.clone(), ctx.clone())))
      )
    })
  }

  fn arbitrary_dat(
    refs: Refs,
    ctx: Vector<String>,
  ) -> Box<dyn Fn(&mut Gen) -> Term> {
    Box::new(move |g: &mut Gen| {
      Term::Dat(None, Box::new(arbitrary_term(g, refs.clone(), ctx.clone())))
    })
  }
  fn arbitrary_cse(
    refs: Refs,
    ctx: Vector<String>,
  ) -> Box<dyn Fn(&mut Gen) -> Term> {
    Box::new(move |g: &mut Gen| {
      Term::Cse(None, Box::new(arbitrary_term(g, refs.clone(), ctx.clone())))
    })
  }

  pub fn frequency<T, F: Fn(&mut Gen) -> T>(
    g: &mut Gen,
    gens: Vec<(i64, F)>,
  ) -> T {
    if gens.iter().any(|(v, _)| *v < 0) {
      panic!("Negative weight");
    }
    let sum: i64 = gens.iter().map(|x| x.0).sum();
    let mut rng = rand::thread_rng();
    let mut weight: i64 = rng.gen_range(1..=sum);
    //let mut weight: i64 = g.rng.gen_range(1, sum);
    for gen in gens {
      if weight - gen.0 <= 0 {
        return gen.1(g);
      }
      else {
        weight -= gen.0;
      }
    }
    panic!("Calculation error for weight = {}", weight);
  }

  pub fn arbitrary_term(
    g: &mut Gen,
    refs: Refs,
    ctx: Vector<String>,
  ) -> Term {
    let len = ctx.len();
    if len == 0 {
      arbitrary_lam(refs, ctx)(g)
    }
    else {
      frequency(g, vec![
        (100, arbitrary_var(ctx.clone())),
        (100, arbitrary_ref(refs.clone(), ctx.clone())),
        (100, Box::new(|_| Term::Typ(None))),
        (100, Box::new(|g| Term::Lit(None, Arbitrary::arbitrary(g)))),
        (100, Box::new(|g| Term::LTy(None, Arbitrary::arbitrary(g)))),
        (100, Box::new(|g| Term::Opr(None, Arbitrary::arbitrary(g)))),
        (100, arbitrary_lam(refs.clone(), ctx.clone())),
        (100, arbitrary_dat(refs.clone(), ctx.clone())),
        (100, arbitrary_cse(refs.clone(), ctx.clone())),
        (100, arbitrary_slf(refs.clone(), ctx.clone())),
        (90, arbitrary_all(refs.clone(), ctx.clone())),
        (90, arbitrary_app(refs.clone(), ctx.clone())),
        (90, arbitrary_ann(refs.clone(), ctx.clone())),
      ])
    }
  }

  impl Arbitrary for Term {
    fn arbitrary(g: &mut Gen) -> Self {
      arbitrary_term(g, test_refs(), Vector::new())
    }
  }

  pub fn arbitrary_def(g: &mut Gen, refs: Refs, name: String) -> Def {
    let mut ctx = Vector::new();
    ctx.push_front(name.clone());
    Def {
      pos: None,
      name,
      docs: String::from(""),
      typ_: arbitrary_term(g, refs.clone(), Vector::new()),
      term: arbitrary_term(g, refs, ctx),
    }
  }

  impl Arbitrary for Def {
    fn arbitrary(g: &mut Gen) -> Self {
      let name = arbitrary_name(g);
      arbitrary_def(g, HashMap::new(), name)
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

  //#[test]
  // fn term_test_cases() {
  //  let f =
  //    Lam(None, String::from("x"), Box::new(Var(None, String::from("x"), 0)));
  //  assert_eq!("(\"lambda\" \"x\" \"x\")", format!("{}", f.clone().encode()));
  //  let b = App(None, Box::new(f.clone()), Box::new(f.clone()));
  //  assert_eq!(
  //    "((\"lambda\" \"x\" \"x\") (\"lambda\" \"x\" \"x\"))",
  //    format!("{}", b.clone().encode())
  //  );
  //  assert_eq!(
  //    Ok(Var(None, String::from("x"), 0)),
  //    Term::decode(
  //      HashMap::new(),
  //      vec![String::from("x")].into(),
  //      hashexpr::parse("\"x\"").unwrap().1,
  //    )
  //  );

  //  let f =
  //    Lam(None, String::from("x"), Box::new(Var(None, String::from("x"), 0)));
  //  assert_eq!(
  //    Ok(f.clone()),
  //    Term::decode(
  //      HashMap::new(),
  //      Vector::new(),
  //      hashexpr::parse("(\"lambda\" \"x\" \"x\")").unwrap().1
  //    )
  //  );

  //  assert_eq!(
  //    Ok(b.clone()),
  //    Term::decode(
  //      HashMap::new(),
  //      Vector::new(),
  //      hashexpr::parse(r#"(("lambda" "x" "x") ("lambda" "x" "x"))"#)
  //        .unwrap()
  //        .1
  //    )
  //  );

  //  // let (id_def, id_ast) = test_defs().get("id").unwrap();
  //  // let x = Term::Ref(None, String::from("id"), id_def, id_ast);
  //  // assert_eq!(f.clone(), x);
  //}
}




