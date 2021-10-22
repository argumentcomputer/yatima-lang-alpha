pub use crate::{
  anon::Anon,
  defs,
  embed_error::EmbedError,
  literal::{
    LitType,
    Literal,
  },
  meta::Meta,
  name::Name,
  parse,
  position::Pos,
  prim::Op,
  uses::Uses,
};

use sp_cid::Cid;

use core::fmt;

use alloc::{
  string::{
    String,
    ToString,
  },
  borrow::ToOwned,
  boxed::Box,
  rc::Rc,
};

/// Yatima terms with source positions
#[derive(Clone)]
pub enum Term {
  /// Local variable
  Var(Pos, Name, u64),
  /// Lambda
  Lam(Pos, Name, Box<Term>),
  /// Application of a function to an argument
  App(Pos, Box<(Term, Term)>),
  /// Forall
  All(Pos, Uses, Name, Box<(Term, Term)>),
  /// Self type
  Slf(Pos, Name, Box<Term>),
  /// Self type constructor
  Dat(Pos, Box<Term>),
  /// Self type destructor
  Cse(Pos, Box<Term>),
  /// Immutable global reference to a term
  Ref(Pos, Name, Cid, Cid),
  /// Inline local definition
  Let(Pos, bool, Uses, Name, Box<(Term, Term, Term)>),
  /// Type of types
  Typ(Pos),
  /// Type annotation
  Ann(Pos, Box<(Term, Term)>),
  /// Primitive literal
  Lit(Pos, Literal),
  /// The type of a literal
  LTy(Pos, LitType),
  /// Primitive operation
  Opr(Pos, Op),
  /// Recursion marker
  Rec(Pos),
}

impl fmt::Debug for Term {
  fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Var(_, n, i) => fmt.debug_tuple("Var").field(&n).field(i).finish(),
      Self::Lam(_, n, b) => fmt.debug_tuple("Lam").field(&n).field(&b).finish(),
      Self::App(_, t) => fmt.debug_tuple("App").field(&t).finish(),
      Self::All(_, u, n, t) => {
        fmt.debug_tuple("All").field(&u).field(&n).field(&t).finish()
      }
      Self::Slf(_, n, b) => fmt.debug_tuple("Slf").field(&n).field(&b).finish(),
      Self::Dat(_, b) => fmt.debug_tuple("Dat").field(&b).finish(),
      Self::Cse(_, b) => fmt.debug_tuple("Cse").field(&b).finish(),
      Self::Ref(_, n, d, a) => {
        fmt.debug_tuple("Ref").field(&n).field(&d).field(&a).finish()
      }
      Self::Let(_, r, u, n, t) => {
        fmt.debug_tuple("Let").field(r).field(&u).field(&n).field(&t).finish()
      }
      Self::Typ(_) => write!(fmt, "Typ(..)"),
      Self::Ann(_, t) => fmt.debug_tuple("Ann").field(&t).finish(),
      Self::Lit(_, a) => fmt.debug_tuple("Lit").field(&a).finish(),
      Self::LTy(_, a) => fmt.debug_tuple("LTy").field(&a).finish(),
      Self::Opr(_, a) => fmt.debug_tuple("Opr").field(&a).finish(),
      Self::Rec(_) => write!(fmt, "Rec(..)"),
    }
  }
}

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
      (Self::Let(_, ra, ua, na, ta), Self::Let(_, rb, ub, nb, tb)) => {
        ra == rb
          && ua == ub
          && na == nb
          && ta.0 == tb.0
          && ta.1 == tb.1
          && ta.2 == tb.2
      }
      (Self::Typ(_), Self::Typ(_)) => true,
      (Self::Rec(_), Self::Rec(_)) => true,
      (Self::Ann(_, ta), Self::Ann(_, tb)) => ta.0 == tb.0 && ta.1 == tb.1,
      (Self::Lit(_, a), Self::Lit(_, b)) => a == b,
      (Self::LTy(_, a), Self::LTy(_, b)) => a == b,
      (Self::Opr(_, a), Self::Opr(_, b)) => a == b,
      _ => false,
    }
  }
}

impl Term {
  /// Returns the position of the term
  pub fn pos(&self) -> Pos {
    match self {
      Term::Var(pos, ..) => *pos,
      Term::Ref(pos, ..) => *pos,
      Term::Lam(pos, ..) => *pos,
      Term::App(pos, _) => *pos,
      Term::Ann(pos, _) => *pos,
      Term::All(pos, ..) => *pos,
      Term::Slf(pos, ..) => *pos,
      Term::Dat(pos, _) => *pos,
      Term::Cse(pos, _) => *pos,
      Term::Let(pos, ..) => *pos,
      Term::Typ(pos) => *pos,
      Term::LTy(pos, _) => *pos,
      Term::Lit(pos, _) => *pos,
      Term::Opr(pos, _) => *pos,
      Term::Rec(pos) => *pos,
    }
  }

  /// Shifts the term's de Bruijn index by a given incrementor
  pub fn shift(self, inc: i64, dep: Option<u64>) -> Self {
    match self {
      Self::Var(pos, nam, idx) => match dep {
        Some(dep) if idx < dep => Self::Var(pos, nam, idx),
        _ => Self::Var(pos, nam, ((idx as i64) + inc) as u64),
      },
      Self::Lam(pos, nam, bod) => {
        Self::Lam(pos, nam, Box::new((*bod).shift(inc, dep.map(|x| x + 1))))
      }
      Self::Slf(pos, nam, bod) => {
        Self::Slf(pos, nam, Box::new((*bod).shift(inc, dep.map(|x| x + 1))))
      }
      Self::Cse(pos, bod) => Self::Cse(pos, Box::new((*bod).shift(inc, dep))),
      Self::Dat(pos, bod) => Self::Dat(pos, Box::new((*bod).shift(inc, dep))),
      Self::App(pos, fun_arg) => {
        let (fun, arg) = *fun_arg;
        Self::App(pos, Box::new((fun.shift(inc, dep), arg.shift(inc, dep))))
      }
      Self::Ann(pos, typ_exp) => {
        let (typ, exp) = *typ_exp;
        Self::Ann(pos, Box::new((typ.shift(inc, dep), exp.shift(inc, dep))))
      }
      Self::All(pos, uses, nam, dom_img) => {
        let (dom, img) = *dom_img;
        Self::All(
          pos,
          uses,
          nam,
          Box::new((dom.shift(inc, dep), img.shift(inc, dep.map(|x| x + 1)))),
        )
      }
      Self::Let(pos, rec, uses, nam, typ_exp_bod) => {
        let (typ, exp, bod) = *typ_exp_bod;
        Self::Let(
          pos,
          rec,
          uses,
          nam,
          Box::new((
            typ.shift(inc, dep),
            exp.shift(inc, if rec { dep.map(|x| x + 1) } else { dep }),
            bod.shift(inc, dep.map(|x| x + 1)),
          )),
        )
      }
      x => x,
    }
  }

  /// Unwinds a recursive function
  pub fn un_rec(self, trm: Rc<Term>) -> Self {
    match self {
      Self::Rec(_) => trm.as_ref().clone(),
      Self::Lam(pos, nam, bod) => {
        Self::Lam(pos, nam, Box::new((*bod).un_rec(trm)))
      }
      Self::Slf(pos, nam, bod) => {
        Self::Slf(pos, nam, Box::new((*bod).un_rec(trm)))
      }
      Self::Cse(pos, bod) => Self::Cse(pos, Box::new((*bod).un_rec(trm))),
      Self::Dat(pos, bod) => Self::Dat(pos, Box::new((*bod).un_rec(trm))),
      Self::App(pos, fun_arg) => {
        let (fun, arg) = *fun_arg;
        Self::App(pos, Box::new((fun.un_rec(trm.clone()), arg.un_rec(trm))))
      }
      Self::Ann(pos, typ_exp) => {
        let (typ, exp) = *typ_exp;
        Self::Ann(pos, Box::new((typ.un_rec(trm.clone()), exp.un_rec(trm))))
      }
      Self::All(pos, uses, nam, dom_img) => {
        let (dom, img) = *dom_img;
        Self::All(
          pos,
          uses,
          nam,
          Box::new((dom.un_rec(trm.clone()), img.un_rec(trm))),
        )
      }
      Self::Let(pos, rec, uses, nam, typ_exp_bod) => {
        let (typ, exp, bod) = *typ_exp_bod;
        Self::Let(
          pos,
          rec,
          uses,
          nam,
          Box::new((
            typ.un_rec(trm.clone()),
            exp.un_rec(trm.clone()),
            bod.un_rec(trm),
          )),
        )
      }
      x => x,
    }
  }

  /// Embeds term into anonymous data and metadata for package definition and
  /// IPFS storage
  pub fn embed(&self) -> (Anon, Meta) {
    match self {
      Self::Var(pos, name, idx) => {
        (Anon::Var(*idx), Meta::Var(*pos, name.clone()))
      }
      Self::Ref(pos, name, def, ast) => {
        (Anon::Ref(*ast), Meta::Ref(*pos, name.clone(), *def))
      }
      Self::Lit(pos, lit) => (Anon::Lit(lit.clone()), Meta::Lit(*pos)),
      Self::LTy(pos, lty) => (Anon::LTy(*lty), Meta::LTy(*pos)),
      Self::Opr(pos, opr) => (Anon::Opr(opr.clone()), Meta::Opr(*pos)),
      Self::Rec(pos) => (Anon::Rec, Meta::Rec(*pos)),
      Self::Typ(pos) => (Anon::Typ, Meta::Typ(*pos)),
      Self::Lam(pos, name, body) => {
        let (anon, meta) = (*body).embed();
        (
          Anon::Lam(Box::new(anon)),
          Meta::Lam(*pos, name.clone(), Box::new(meta)),
        )
      }
      Self::Slf(pos, name, body) => {
        let (anon, meta) = (*body).embed();
        (
          Anon::Slf(Box::new(anon)),
          Meta::Slf(*pos, name.clone(), Box::new(meta)),
        )
      }
      Self::App(pos, terms) => {
        let (fun_anon, fun_meta) = terms.0.embed();
        let (arg_anon, arg_meta) = terms.1.embed();
        (
          Anon::App(Box::new((fun_anon, arg_anon))),
          Meta::App(*pos, Box::new((fun_meta, arg_meta))),
        )
      }
      Self::Ann(pos, terms) => {
        let (typ_anon, typ_meta) = terms.0.embed();
        let (exp_anon, exp_meta) = terms.1.embed();
        (
          Anon::Ann(Box::new((typ_anon, exp_anon))),
          Meta::Ann(*pos, Box::new((typ_meta, exp_meta))),
        )
      }
      Self::Dat(pos, body) => {
        let (anon, meta) = (*body).embed();
        (Anon::Dat(Box::new(anon)), Meta::Dat(*pos, Box::new(meta)))
      }
      Self::Cse(pos, body) => {
        let (anon, meta) = (*body).embed();
        (Anon::Cse(Box::new(anon)), Meta::Cse(*pos, Box::new(meta)))
      }
      Self::All(pos, uses, name, terms) => {
        let (typ_anon, typ_meta) = terms.0.embed();
        let (bod_anon, bod_meta) = terms.1.embed();
        (
          Anon::All(*uses, Box::new((typ_anon, bod_anon))),
          Meta::All(*pos, name.clone(), Box::new((typ_meta, bod_meta))),
        )
      }
      Self::Let(pos, rec, uses, name, terms) => {
        let (typ_anon, typ_meta) = terms.0.embed();
        let (exp_anon, exp_meta) = terms.1.embed();
        let (bod_anon, bod_meta) = terms.2.embed();
        (
          Anon::Let(*rec, *uses, Box::new((typ_anon, exp_anon, bod_anon))),
          Meta::Let(
            *pos,
            name.clone(),
            Box::new((typ_meta, exp_meta, bod_meta)),
          ),
        )
      }
    }
  }

  /// Unembeds anon and meta into term
  pub fn unembed(anon: &Anon, meta: &Meta) -> Result<Self, EmbedError> {
    match (anon, meta) {
      (Anon::Var(idx), Meta::Var(pos, nam)) => {
        Ok(Self::Var(*pos, nam.clone(), *idx))
      }
      (Anon::Ref(ast), Meta::Ref(pos, nam, def)) => {
        Ok(Self::Ref(*pos, nam.clone(), *def, *ast))
      }
      (Anon::Lit(lit), Meta::Lit(pos)) => Ok(Self::Lit(*pos, lit.clone())),
      (Anon::LTy(lty), Meta::LTy(pos)) => Ok(Self::LTy(*pos, *lty)),
      (Anon::Opr(opr), Meta::Opr(pos)) => Ok(Self::Opr(*pos, opr.clone())),
      (Anon::Typ, Meta::Typ(pos)) => Ok(Self::Typ(*pos)),
      (Anon::Rec, Meta::Rec(pos)) => Ok(Self::Rec(*pos)),
      (Anon::Lam(anon_bod), Meta::Lam(pos, nam, meta_bod)) => {
        let bod = Term::unembed(anon_bod, meta_bod)?;
        Ok(Self::Lam(*pos, nam.clone(), Box::new(bod)))
      }
      (Anon::Slf(anon_bod), Meta::Slf(pos, nam, meta_bod)) => {
        let bod = Term::unembed(anon_bod, meta_bod)?;
        Ok(Self::Slf(*pos, nam.clone(), Box::new(bod)))
      }
      (Anon::Dat(anon_bod), Meta::Dat(pos, meta_bod)) => {
        let bod = Term::unembed(anon_bod, meta_bod)?;
        Ok(Self::Dat(*pos, Box::new(bod)))
      }
      (Anon::Cse(anon_bod), Meta::Cse(pos, meta_bod)) => {
        let bod = Term::unembed(anon_bod, meta_bod)?;
        Ok(Self::Cse(*pos, Box::new(bod)))
      }
      (Anon::App(anon), Meta::App(pos, meta)) => {
        let (fun_anon, arg_anon) = anon.as_ref();
        let (fun_meta, arg_meta) = meta.as_ref();
        let fun = Term::unembed(fun_anon, fun_meta)?;
        let arg = Term::unembed(arg_anon, arg_meta)?;
        Ok(Self::App(*pos, Box::new((fun, arg))))
      }
      (Anon::Ann(anon), Meta::Ann(pos, meta)) => {
        let (typ_anon, exp_anon) = anon.as_ref();
        let (typ_meta, exp_meta) = meta.as_ref();
        let typ = Term::unembed(typ_anon, typ_meta)?;
        let exp = Term::unembed(exp_anon, exp_meta)?;
        Ok(Self::Ann(*pos, Box::new((typ, exp))))
      }
      (Anon::All(uses, anon), Meta::All(pos, name, meta)) => {
        let (dom_anon, img_anon) = anon.as_ref();
        let (dom_meta, img_meta) = meta.as_ref();
        let dom = Term::unembed(dom_anon, dom_meta)?;
        let img = Term::unembed(img_anon, img_meta)?;
        Ok(Self::All(*pos, *uses, name.clone(), Box::new((dom, img))))
      }
      (Anon::Let(rec, uses, anon), Meta::Let(pos, name, meta)) => {
        let (typ_anon, exp_anon, bod_anon) = anon.as_ref();
        let (typ_meta, exp_meta, bod_meta) = meta.as_ref();
        let typ = Term::unembed(typ_anon, typ_meta)?;
        let exp = Term::unembed(exp_anon, exp_meta)?;
        let bod = Term::unembed(bod_anon, bod_meta)?;
        Ok(Self::Let(
          *pos,
          *rec,
          *uses,
          name.clone(),
          Box::new((typ, exp, bod)),
        ))
      }
      (anon, meta) => Err(EmbedError::Term(anon.clone(), meta.clone())),
    }
  }

  /// Formats term for pretty-printing
  pub fn pretty(&self, rec: Option<&String>, ind: bool) -> String {
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
      matches!(term, Var(..) | Ref(..) | Lit(..) | LTy(..) | Opr(..) | Typ(..))
    }

    fn lams(rec: Option<&String>, ind: bool, nam: &str, bod: &Term) -> String {
      match bod {
        Lam(_, nam2, bod2) => {
          format!("{} {}", name(nam), lams(rec, ind, nam2, bod2))
        }
        _ => format!("{} => {}", nam, bod.pretty(rec, ind)),
      }
    }

    fn alls(
      rec: Option<&String>,
      ind: bool,
      use_: &Uses,
      nam: &str,
      typ: &Term,
      bod: &Term,
    ) -> String {
      match bod {
        All(_, bod_use, bod_nam, bod) => {
          format!(
            " ({}{}: {}){}",
            uses(use_),
            name(nam),
            typ.pretty(rec, ind),
            alls(rec, ind, bod_use, bod_nam, &bod.0, &bod.1)
          )
        }
        _ => format!(
          " ({}{}: {}) -> {}",
          uses(use_),
          name(nam),
          typ.pretty(rec, ind),
          bod.pretty(rec, ind)
        ),
      }
    }

    fn parens(rec: Option<&String>, ind: bool, term: &Term) -> String {
      if is_atom(term) {
        term.pretty(rec, ind)
      }
      else {
        format!("({})", term.pretty(rec, ind))
      }
    }

    fn apps(rec: Option<&String>, ind: bool, fun: &Term, arg: &Term) -> String {
      match (fun, arg) {
        (App(_, f), App(_, a)) => {
          format!(
            "{} ({})",
            apps(rec, ind, &f.0, &f.1),
            apps(rec, ind, &a.0, &a.1)
          )
        }
        (App(_, f), arg) => {
          format!("{} {}", apps(rec, ind, &f.0, &f.1), parens(rec, ind, arg))
        }
        (fun, App(_, a)) => {
          format!("{} ({})", parens(rec, ind, fun), apps(rec, ind, &a.0, &a.1))
        }
        (fun, arg) => {
          format!("{} {}", parens(rec, ind, fun), parens(rec, ind, arg))
        }
      }
    }

    match self {
      Var(_, nam, index) => {
        if ind {
          format!("{}^{}", nam, index)
        }
        else {
          nam.to_string()
        }
      }
      Ref(_, nam, ..) => nam.to_string(),
      Rec(_) => match rec {
        Some(rec) => rec.to_owned(),
        _ => "#^".to_string(),
      },

      Lam(_, nam, term) => format!("λ {}", lams(rec, ind, nam, term)),
      App(_, terms) => apps(rec, ind, &terms.0, &terms.1),
      Let(_, letrec, u, n, terms) => {
        format!(
          "let{} {}{}: {} = {}; {}",
          if *letrec { "rec" } else { "" },
          uses(u),
          name(n),
          terms.0.pretty(rec, ind),
          terms.1.pretty(rec, ind),
          terms.2.pretty(rec, ind),
        )
      }
      Slf(_, nam, bod) => format!("@{} {}", name(nam), bod.pretty(rec, ind)),
      All(_, us_, nam, terms) => {
        format!("∀{}", alls(rec, ind, us_, nam, &terms.0, &terms.1))
      }
      Ann(_, terms) => {
        format!(
          "{} :: {}",
          parens(rec, ind, &terms.1),
          parens(rec, ind, &terms.0)
        )
      }
      Dat(_, bod) => format!("data {}", bod.pretty(rec, ind)),
      Cse(_, bod) => format!("case {}", bod.pretty(rec, ind)),
      Typ(_) => "Type".to_string(),
      Lit(_, lit) => format!("{}", lit),
      LTy(_, lty) => format!("{}", lty),
      Opr(_, opr) => format!("{}", opr),
    }
  }
}

impl fmt::Display for Term {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.pretty(None, false))
  }
}

#[cfg(test)]
pub mod tests {

  use super::{
    *,
  };
  use crate::{
    defs::{
      Def,
      Defs,
    },
    yatima,
  };
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  use core::ops::Range;

  use sp_im::Vector;
  use alloc::{
    boxed::Box,
    vec::Vec,
  };

  use crate::{
    name::Name,
    parse::term::is_valid_symbol_char,
  };

  pub fn arbitrary_name(g: &mut Gen) -> Name {
    let s: String = Arbitrary::arbitrary(g);
    let mut s: String = s
      .chars()
      .filter(|x| is_valid_symbol_char(*x) && char::is_ascii_alphabetic(x))
      .collect();
    s.truncate(1);
    Name::from(format!("_{}", s))
  }

  pub fn test_defs() -> Defs {
    let mut defs = Defs::new();
    let (id, _) = Def::make(
      Pos::None,
      yatima!("∀ (A: Type) (x: A) -> A"),
      yatima!("λ A x => x"),
    );
    let (fst, _) = Def::make(
      Pos::None,
      yatima!("∀ (A: Type) (x y: A) -> A"),
      yatima!("λ A x y => x"),
    );
    let (snd, _) = Def::make(
      Pos::None,
      yatima!("∀ (A: Type) (x y: A) -> A"),
      yatima!("λ A x y => y"),
    );
    defs.insert(Name::from("id"), id);
    defs.insert(Name::from("fst"), fst);
    defs.insert(Name::from("snd"), snd);
    defs
  }

  fn arbitrary_ref(g: &mut Gen, defs: Defs, ctx: Vector<Name>) -> Tree {
    let refs: Vec<(Name, Cid)> = defs
      .names
      .clone()
      .into_iter()
      .filter(|(n, _)| !ctx.contains(n))
      .collect();
    let len = refs.len();
    if len == 0 {
      return Tree::Typ;
    };
    let gen = gen_range(g, 0..(len - 1));
    let (n, _) = refs[gen].clone();
    let def = defs.get(&n).unwrap();
    Tree::Ref(n.clone(), def.def_cid, def.ast_cid)
  }

  #[derive(Debug, Clone)]
  pub enum Tree {
    Var(Name, u64),
    Typ,
    Rec,
    Ref(Name, Cid, Cid),
    Opr(Op),
    Lit(Literal),
    LTy(LitType),
    Lam(Name, usize),
    Slf(Name, usize),
    App(usize, usize),
    Ann(usize, usize),
    Cse(usize),
    Dat(usize),
    All(Uses, Name, usize, usize),
    Let(bool, Uses, Name, usize, usize, usize),
  }

  impl Tree {
    pub fn into_term(&self, arena: &Vec<Tree>) -> Term {
      match self {
        Self::Var(n, i) => Term::Var(Pos::None, n.clone(), *i),
        Self::Rec => Term::Rec(Pos::None),
        Self::Typ => Term::Typ(Pos::None),
        Self::Ref(n, d, a) => Term::Ref(Pos::None, n.clone(), *d, *a),
        Self::Opr(x) => Term::Opr(Pos::None, x.clone()),
        Self::Lit(x) => Term::Lit(Pos::None, x.clone()),
        Self::LTy(x) => Term::LTy(Pos::None, *x),
        Self::Lam(n, bod) => {
          let bod = arena[*bod].into_term(&arena);
          Term::Lam(Pos::None, n.clone(), Box::new(bod))
        }
        Self::Slf(n, bod) => {
          let bod = arena[*bod].into_term(&arena);
          Term::Slf(Pos::None, n.clone(), Box::new(bod))
        }
        Self::Cse(bod) => {
          let bod = arena[*bod].into_term(&arena);
          Term::Cse(Pos::None, Box::new(bod))
        }
        Self::Dat(bod) => {
          let bod = arena[*bod].into_term(&arena);
          Term::Dat(Pos::None, Box::new(bod))
        }
        Self::App(fun, arg) => {
          let fun = arena[*fun].into_term(&arena);
          let arg = arena[*arg].into_term(&arena);
          Term::App(Pos::None, Box::new((fun, arg)))
        }
        Self::Ann(typ, trm) => {
          let typ = arena[*typ].into_term(&arena);
          let trm = arena[*trm].into_term(&arena);
          Term::Ann(Pos::None, Box::new((typ, trm)))
        }
        Self::All(uses, n, dom, img) => {
          let dom = arena[*dom].into_term(&arena);
          let img = arena[*img].into_term(&arena);
          Term::All(Pos::None, *uses, n.clone(), Box::new((dom, img)))
        }
        Self::Let(rec, uses, n, typ, trm, bod) => {
          let typ = arena[*typ].into_term(&arena);
          let trm = arena[*trm].into_term(&arena);
          let bod = arena[*bod].into_term(&arena);
          Term::Let(
            Pos::None,
            *rec,
            *uses,
            n.clone(),
            Box::new((typ, trm, bod)),
          )
        }
      }
    }
  }

  #[derive(Debug, Clone, Copy)]
  pub enum Case {
    VAR,
    TYP,
    REC,
    REF,
    LIT,
    LTY,
    OPR,
    LAM,
    APP,
    ANN,
    SLF,
    CSE,
    DAT,
    ALL,
    LET,
  }

  pub fn gen_range(g: &mut Gen, range: Range<usize>) -> usize {
    let res: usize = Arbitrary::arbitrary(g);
    (res % (range.end - range.start)) + range.start
  }

  pub fn next_case(g: &mut Gen, gens: &Vec<(usize, Case)>) -> Case {
    let sum: usize = gens.iter().map(|x| x.0).sum();
    let mut weight: usize = gen_range(g, 1..(sum + 1));
    for gen in gens {
      match weight.checked_sub(gen.0 + 1) {
        None | Some(0) => {
          return gen.1;
        }
        _ => {
          weight -= gen.0;
        }
      }
    }
    panic!("Calculation error for weight = {}", weight);
  }

  pub fn arbitrary_term(
    g: &mut Gen,
    rec: bool,
    defs: Defs,
    ctx0: Vector<Name>,
  ) -> Term {
    let name = Name::from("_r");
    let mut ctx1 = ctx0.clone();
    ctx1.push_front(name.clone());
    let mut ctxs: Vec<Vector<Name>> = vec![ctx0, ctx1];
    let mut arena: Vec<Option<Tree>> = vec![Some(Tree::Lam(name, 1)), None];
    let mut todo: Vec<usize> = vec![1];

    while let Some(idx) = todo.pop() {
      let ctx: Vector<Name> = ctxs[idx].clone();
      let depth = ctx.len();
      let gens: Vec<(usize, Case)> = vec![
        (100, Case::VAR),
        (100, Case::TYP),
        (100, Case::REF),
        (100, Case::LIT),
        (100, Case::LTY),
        (100, Case::OPR),
        (if rec { 100 } else { 0 }, Case::REC),
        (90usize.saturating_sub(depth), Case::LAM),
        (90usize.saturating_sub(depth), Case::SLF),
        (90usize.saturating_sub(depth), Case::CSE),
        (90usize.saturating_sub(depth), Case::DAT),
        (80usize.saturating_sub(2 * depth), Case::APP),
        (80usize.saturating_sub(2 * depth), Case::ANN),
        (80usize.saturating_sub(2 * depth), Case::ALL),
        (30usize.saturating_sub(3 * depth), Case::LET),
      ];

      match next_case(g, &gens) {
        Case::TYP => {
          arena[idx] = Some(Tree::Typ);
        }
        Case::REC => {
          arena[idx] = Some(Tree::Rec);
        }
        Case::LTY => {
          arena[idx] = Some(Tree::LTy(Arbitrary::arbitrary(g)));
        }
        Case::LIT => {
          arena[idx] = Some(Tree::Lit(Arbitrary::arbitrary(g)));
        }
        Case::OPR => {
          arena[idx] = Some(Tree::Opr(Arbitrary::arbitrary(g)));
        }
        Case::REF => {
          arena[idx] = Some(arbitrary_ref(g, defs.clone(), ctx.clone()));
        }
        Case::VAR => {
          let gen = gen_range(g, 0..ctx.len());
          let n = &ctx[gen];
          let (i, _) = ctx.iter().enumerate().find(|(_, x)| *x == n).unwrap();
          arena[idx] = Some(Tree::Var(n.clone(), i as u64));
        }
        Case::LAM => {
          let n = arbitrary_name(g);
          let mut ctx2 = ctx.clone();
          ctx2.push_front(n.clone());
          let bod = arena.len();
          todo.push(bod);
          ctxs.push(ctx2);
          arena.push(None);
          arena[idx] = Some(Tree::Lam(n, bod));
        }
        Case::SLF => {
          let n = arbitrary_name(g);
          let mut ctx2 = ctx.clone();
          ctx2.push_front(n.clone());
          let bod = arena.len();
          todo.push(bod);
          ctxs.push(ctx2);
          arena.push(None);
          arena[idx] = Some(Tree::Slf(n, bod));
        }
        Case::CSE => {
          let bod = arena.len();
          todo.push(bod);
          ctxs.push(ctx.clone());
          arena.push(None);
          arena[idx] = Some(Tree::Cse(bod));
        }
        Case::DAT => {
          let bod = arena.len();
          todo.push(bod);
          ctxs.push(ctx.clone());
          arena.push(None);
          arena[idx] = Some(Tree::Dat(bod));
        }
        Case::APP => {
          let fun = arena.len();
          todo.push(fun);
          ctxs.push(ctx.clone());
          arena.push(None);
          let arg = arena.len();
          todo.push(arg);
          ctxs.push(ctx.clone());
          arena.push(None);
          arena[idx] = Some(Tree::App(fun, arg));
        }
        Case::ANN => {
          let typ = arena.len();
          todo.push(typ);
          ctxs.push(ctx.clone());
          arena.push(None);
          let trm = arena.len();
          todo.push(trm);
          ctxs.push(ctx.clone());
          arena.push(None);
          arena[idx] = Some(Tree::Ann(typ, trm));
        }
        Case::ALL => {
          let uses: Uses = Arbitrary::arbitrary(g);
          let n = arbitrary_name(g);
          let mut ctx2 = ctx.clone();
          ctx2.push_front(n.clone());
          let dom = arena.len();
          todo.push(dom);
          ctxs.push(ctx.clone());
          arena.push(None);
          let img = arena.len();
          todo.push(img);
          ctxs.push(ctx2);
          arena.push(None);
          arena[idx] = Some(Tree::All(uses, n, dom, img));
        }
        Case::LET => {
          let letrec: bool = Arbitrary::arbitrary(g);
          let uses: Uses = Arbitrary::arbitrary(g);
          let n = arbitrary_name(g);
          let mut ctx2 = ctx.clone();
          ctx2.push_front(n.clone());
          let typ = arena.len();
          todo.push(typ);
          ctxs.push(ctx.clone());
          arena.push(None);
          let trm = arena.len();
          todo.push(trm);
          if letrec {
            ctxs.push(ctx2.clone());
          }
          else {
            ctxs.push(ctx.clone())
          };
          arena.push(None);
          let bod = arena.len();
          todo.push(bod);
          ctxs.push(ctx2.clone());
          arena.push(None);
          arena[idx] = Some(Tree::Let(letrec, uses, n, typ, trm, bod));
        }
      }
    }
    //    println!("arena: {:?}", arena);
    let arena: Vec<Tree> = arena.into_iter().map(|x| x.unwrap()).collect();
    //    println!("arena: {:?}", arena);
    arena[0].into_term(&arena)
  }

  impl Arbitrary for Term {
    fn arbitrary(g: &mut Gen) -> Self {
      arbitrary_term(g, false, test_defs(), Vector::new())
    }
  }

  #[quickcheck]
  fn term_embed_unembed(x: Term) -> bool {
    let (a, m) = x.clone().embed();
    println!("x: {}", x);
    match Term::unembed(&a, &m) {
      Ok(y) => {
        if x == y {
          true
        }
        else {
          // println!("x: {:?}", x);
          // println!("y: {:?}", y);
          false
        }
      }
      _e => {
        // println!("x: {:?}", x);
        // println!("a: {:?}", a);
        // println!("m: {:?}", m);
        // println!("e: {:?}", _e);
        false
      }
    }
  }
}
