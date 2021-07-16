pub use crate::{
  anon::Anon,
  embed_error::EmbedError,
  literal::{
    LitType,
    Literal,
  },
  meta::Meta,
  name::Name,
  position::Pos,
  prim::Op,
  uses::Uses,
};

use sp_cid::Cid;

use sp_std::{
  borrow::ToOwned,
  boxed::Box,
  fmt,
  mem,
  mem::MaybeUninit,
  ptr::NonNull,
  rc::Rc,
};

use alloc::string::{
  String,
  ToString,
};

#[derive(Clone)]
pub enum Term {
  Var(Pos, Name, u64),
  Lam(Pos, Uses, Name, Box<Term>, Box<Term>),
  App(Pos, Uses, Box<Term>, Box<Term>, Box<Term>),
  All(Pos, Uses, Name, Box<Term>, Box<Term>),
  Slf(Pos, Name, Box<Term>),
  Dat(Pos, Box<Term>, Box<Term>),
  Cse(Pos, Box<Term>),
  Ref(Pos, Name, Cid, Cid),
  Let(Pos, bool, Uses, Name, Box<Term>, Box<Term>, Box<Term>),
  Typ(Pos),
  Lit(Pos, Literal),
  LTy(Pos, LitType),
  Opr(Pos, Op),
  Rec(Pos),
}

impl fmt::Debug for Term {
  fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Var(_, n, i) => fmt.debug_tuple("Var").field(&n).field(i).finish(),
      Self::Lam(_, u, n, t, b) => {
        fmt.debug_tuple("Lam").field(&u).field(&n).field(&t).field(&b).finish()
      }
      Self::App(_, u, f, t, a) => {
        fmt.debug_tuple("App").field(&u).field(&f).field(&t).field(&a).finish()
      }
      Self::All(_, u, n, d, i) => {
        fmt.debug_tuple("All").field(&u).field(&n).field(&d).field(&i).finish()
      }
      Self::Slf(_, n, b) => fmt.debug_tuple("Slf").field(&n).field(&b).finish(),
      Self::Dat(_, t, b) => fmt.debug_tuple("Dat").field(&t).field(&b).finish(),
      Self::Cse(_, b) => fmt.debug_tuple("Cse").field(&b).finish(),
      Self::Ref(_, n, d, a) => {
        fmt.debug_tuple("Ref").field(&n).field(&d).field(&a).finish()
      }
      Self::Let(_, r, u, n, t, x, b) => fmt
        .debug_tuple("Let")
        .field(r)
        .field(&u)
        .field(&n)
        .field(&t)
        .field(&x)
        .field(&b)
        .finish(),
      Self::Typ(_) => write!(fmt, "Typ(..)"),
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
      (Self::Lam(_, ua, na, ta, ba), Self::Lam(_, ub, nb, tb, bb)) => {
        ua == ub && na == nb && ta == tb && ba == bb
      }
      (Self::App(_, ua, fa, ta, aa), Self::App(_, ub, fb, tb, ab)) => {
        ua == ub && fa == fb && ta == tb && aa == ab
      }
      (Self::All(_, ua, na, da, ia), Self::All(_, ub, nb, db, ib)) => {
        ua == ub && na == nb && da == db && ia == ib
      }
      (Self::Slf(_, na, ba), Self::Slf(_, nb, bb)) => na == nb && ba == bb,
      (Self::Dat(_, ta, ba), Self::Dat(_, tb, bb)) => ta == tb && ba == bb,
      (Self::Cse(_, ba), Self::Cse(_, bb)) => ba == bb,
      (Self::Ref(_, na, da, aa), Self::Ref(_, nb, db, ab)) => {
        na == nb && da == db && aa == ab
      }
      (
        Self::Let(_, ra, ua, na, ta, xa, ba),
        Self::Let(_, rb, ub, nb, tb, xb, bb),
      ) => ra == rb && ua == ub && na == nb && ta == tb && xa == xb && ba == bb,
      (Self::Typ(_), Self::Typ(_)) => true,
      (Self::Rec(_), Self::Rec(_)) => true,
      (Self::Lit(_, a), Self::Lit(_, b)) => a == b,
      (Self::LTy(_, a), Self::LTy(_, b)) => a == b,
      (Self::Opr(_, a), Self::Opr(_, b)) => a == b,
      _ => false,
    }
  }
}

impl Eq for Term {}

impl Term {
  pub fn pos(&self) -> Pos {
    match self {
      Self::Var(pos, ..) => *pos,
      Self::Ref(pos, ..) => *pos,
      Self::Lam(pos, ..) => *pos,
      Self::App(pos, ..) => *pos,
      Self::All(pos, ..) => *pos,
      Self::Slf(pos, ..) => *pos,
      Self::Dat(pos, ..) => *pos,
      Self::Cse(pos, ..) => *pos,
      Self::Let(pos, ..) => *pos,
      Self::Typ(pos) => *pos,
      Self::LTy(pos, ..) => *pos,
      Self::Lit(pos, ..) => *pos,
      Self::Opr(pos, ..) => *pos,
      Self::Rec(pos) => *pos,
    }
  }

  pub fn shift(self, inc: u64, dep: Option<u64>) -> Self {
    match self {
      Self::Var(pos, nam, idx) => match dep {
        Some(dep) if idx < dep => Self::Var(pos, nam, idx),
        _ => Self::Var(pos, nam, (idx as u64) + inc),
      },
      Self::Lam(pos, uses, nam, typ, bod) => Self::Lam(
        pos,
        uses,
        nam,
        Box::new((*typ).shift(inc, dep)),
        Box::new((*bod).shift(inc, dep.map(|x| x + 1))),
      ),
      Self::Slf(pos, nam, bod) => {
        Self::Slf(pos, nam, Box::new((*bod).shift(inc, dep.map(|x| x + 1))))
      }
      Self::Cse(pos, bod) => Self::Cse(pos, Box::new((*bod).shift(inc, dep))),
      Self::Dat(pos, typ, bod) => Self::Dat(
        pos,
        Box::new((*typ).shift(inc, dep)),
        Box::new((*bod).shift(inc, dep)),
      ),
      Self::App(pos, uses, fun, typ, arg) => Self::App(
        pos,
        uses,
        Box::new((*fun).shift(inc, dep)),
        Box::new((*typ).shift(inc, dep)),
        Box::new((*arg).shift(inc, dep)),
      ),
      Self::All(pos, uses, nam, dom, img) => Self::All(
        pos,
        uses,
        nam,
        Box::new((*dom).shift(inc, dep)),
        Box::new((*img).shift(inc, dep.map(|x| x + 1))),
      ),
      Self::Let(pos, rec, uses, nam, typ, exp, bod) => Self::Let(
        pos,
        rec,
        uses,
        nam,
        Box::new((*typ).shift(inc, dep)),
        Box::new((*exp).shift(inc, if rec { dep.map(|x| x + 1) } else { dep })),
        Box::new((*bod).shift(inc, dep.map(|x| x + 1))),
      ),
      x => x,
    }
  }

  pub fn un_rec(self, trm: Rc<Term>) -> Self {
    match self {
      Self::Rec(_) => trm.as_ref().clone(),
      Self::Lam(pos, uses, nam, typ, bod) => Self::Lam(
        pos,
        uses,
        nam,
        Box::new((*typ).un_rec(trm.clone())),
        Box::new((*bod).un_rec(trm)),
      ),
      Self::Slf(pos, nam, bod) => {
        Self::Slf(pos, nam, Box::new((*bod).un_rec(trm)))
      }
      Self::Cse(pos, bod) => Self::Cse(pos, Box::new((*bod).un_rec(trm))),
      Self::Dat(pos, typ, bod) => Self::Dat(
        pos,
        Box::new((*typ).un_rec(trm.clone())),
        Box::new((*bod).un_rec(trm)),
      ),
      Self::App(pos, uses, fun, typ, arg) => Self::App(
        pos,
        uses,
        Box::new((*fun).un_rec(trm.clone())),
        Box::new((*typ).un_rec(trm.clone())),
        Box::new((*arg).un_rec(trm)),
      ),
      Self::All(pos, uses, nam, dom, img) => Self::All(
        pos,
        uses,
        nam,
        Box::new((*dom).un_rec(trm.clone())),
        Box::new((*img).un_rec(trm)),
      ),
      Self::Let(pos, rec, uses, nam, typ, exp, bod) => Self::Let(
        pos,
        rec,
        uses,
        nam,
        Box::new((*typ).un_rec(trm.clone())),
        Box::new((*exp).un_rec(trm.clone())),
        Box::new((*bod).un_rec(trm)),
      ),
      x => x,
    }
  }

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
      Self::Opr(pos, opr) => (Anon::Opr(*opr), Meta::Opr(*pos)),
      Self::Rec(pos) => (Anon::Rec, Meta::Rec(*pos)),
      Self::Typ(pos) => (Anon::Typ, Meta::Typ(*pos)),
      Self::Lam(pos, uses, name, typ, bod) => {
        let (typ_anon, typ_meta) = (*typ).embed();
        let (bod_anon, bod_meta) = (*bod).embed();
        (
          Anon::Lam(*uses, Box::new((typ_anon, bod_anon))),
          Meta::Lam(*pos, name.clone(), Box::new((typ_meta, bod_meta))),
        )
      }
      Self::Slf(pos, name, body) => {
        let (anon, meta) = (*body).embed();
        (
          Anon::Slf(Box::new(anon)),
          Meta::Slf(*pos, name.clone(), Box::new(meta)),
        )
      }
      Self::App(pos, uses, fun, typ, arg) => {
        let (fun_anon, fun_meta) = (*fun).embed();
        let (typ_anon, typ_meta) = (*typ).embed();
        let (arg_anon, arg_meta) = (*arg).embed();
        (
          Anon::App(*uses, Box::new((fun_anon, typ_anon, arg_anon))),
          Meta::App(*pos, Box::new((fun_meta, typ_meta, arg_meta))),
        )
      }
      Self::Dat(pos, typ, bod) => {
        let (typ_anon, typ_meta) = (*typ).embed();
        let (bod_anon, bod_meta) = (*bod).embed();
        (
          Anon::Dat(Box::new((typ_anon, bod_anon))),
          Meta::Dat(*pos, Box::new((typ_meta, bod_meta))),
        )
      }
      Self::Cse(pos, body) => {
        let (anon, meta) = (*body).embed();
        (Anon::Cse(Box::new(anon)), Meta::Cse(*pos, Box::new(meta)))
      }
      Self::All(pos, uses, name, dom, img) => {
        let (typ_anon, typ_meta) = dom.embed();
        let (bod_anon, bod_meta) = img.embed();
        (
          Anon::All(*uses, Box::new((typ_anon, bod_anon))),
          Meta::All(*pos, name.clone(), Box::new((typ_meta, bod_meta))),
        )
      }
      Self::Let(pos, rec, uses, name, typ, exp, bod) => {
        let (typ_anon, typ_meta) = typ.embed();
        let (exp_anon, exp_meta) = exp.embed();
        let (bod_anon, bod_meta) = bod.embed();
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
      (Anon::Opr(opr), Meta::Opr(pos)) => Ok(Self::Opr(*pos, *opr)),
      (Anon::Typ, Meta::Typ(pos)) => Ok(Self::Typ(*pos)),
      (Anon::Rec, Meta::Rec(pos)) => Ok(Self::Rec(*pos)),
      (Anon::Lam(uses, anon), Meta::Lam(pos, nam, meta)) => {
        let (typ_anon, bod_anon) = anon.as_ref();
        let (typ_meta, bod_meta) = meta.as_ref();
        let typ = Term::unembed(typ_anon, typ_meta)?;
        let bod = Term::unembed(bod_anon, bod_meta)?;
        Ok(Self::Lam(*pos, *uses, nam.clone(), Box::new(typ), Box::new(bod)))
      }
      (Anon::Slf(anon_bod), Meta::Slf(pos, nam, meta_bod)) => {
        let bod = Term::unembed(anon_bod, meta_bod)?;
        Ok(Self::Slf(*pos, nam.clone(), Box::new(bod)))
      }
      (Anon::Dat(anon), Meta::Dat(pos, meta)) => {
        let (typ_anon, bod_anon) = anon.as_ref();
        let (typ_meta, bod_meta) = meta.as_ref();
        let typ = Term::unembed(typ_anon, typ_meta)?;
        let bod = Term::unembed(bod_anon, bod_meta)?;
        Ok(Self::Dat(*pos, Box::new(typ), Box::new(bod)))
      }
      (Anon::Cse(anon_bod), Meta::Cse(pos, meta_bod)) => {
        let bod = Term::unembed(anon_bod, meta_bod)?;
        Ok(Self::Cse(*pos, Box::new(bod)))
      }
      (Anon::App(uses, anon), Meta::App(pos, meta)) => {
        let (fun_anon, typ_anon, arg_anon) = anon.as_ref();
        let (fun_meta, typ_meta, arg_meta) = meta.as_ref();
        let fun = Term::unembed(fun_anon, fun_meta)?;
        let typ = Term::unembed(typ_anon, typ_meta)?;
        let arg = Term::unembed(arg_anon, arg_meta)?;
        Ok(Self::App(*pos, *uses, Box::new(fun), Box::new(typ), Box::new(arg)))
      }
      (Anon::All(uses, anon), Meta::All(pos, name, meta)) => {
        let (dom_anon, img_anon) = anon.as_ref();
        let (dom_meta, img_meta) = meta.as_ref();
        let dom = Term::unembed(dom_anon, dom_meta)?;
        let img = Term::unembed(img_anon, img_meta)?;
        Ok(Self::All(*pos, *uses, name.clone(), Box::new(dom), Box::new(img)))
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
          Name::from(name.clone()),
          Box::new(typ),
          Box::new(exp),
          Box::new(bod),
        ))
      }
      (anon, meta) => Err(EmbedError::Term(anon.clone(), meta.clone())),
    }
  }

  pub fn pretty(&self, rec: Option<&String>, ind: bool) -> String {
    use Term::*;
    const WILDCARD: &str = "_";

    fn pretty_name(nam: &str) -> &str {
      if nam.is_empty() { WILDCARD } else { nam }
    }

    fn is_atom(term: &Term) -> bool {
      matches!(term, Var(..) | Ref(..) | Lit(..) | LTy(..) | Opr(..) | Typ(..))
    }

    fn parens(rec: Option<&String>, ind: bool, term: &Term) -> String {
      if is_atom(term) {
        term.pretty(rec, ind)
      }
      else {
        format!("({})", term.pretty(rec, ind))
      }
    }

    fn lams(
      rec: Option<&String>,
      ind: bool,
      uses: &Uses,
      name: &str,
      typ: &Term,
      bod: &Term,
    ) -> String {
      match bod {
        Lam(_, bod_use, bod_nam, bod_typ, bod_bod) => {
          format!(
            "({} {}: {}) {}",
            uses,
            pretty_name(name),
            typ.pretty(rec, ind),
            lams(rec, ind, bod_use, bod_nam, bod_typ, bod_bod)
          )
        }
        _ => format!(
          "({} {}: {}) => {}",
          uses,
          pretty_name(name),
          typ.pretty(rec, ind),
          bod.pretty(rec, ind)
        ),
      }
    }

    fn alls(
      rec: Option<&String>,
      ind: bool,
      uses: &Uses,
      name: &str,
      typ: &Term,
      bod: &Term,
    ) -> String {
      match bod {
        All(_, bod_use, bod_nam, bod_dom, bod_img) => {
          format!(
            "({} {}: {}) {}",
            uses,
            pretty_name(name),
            typ.pretty(rec, ind),
            alls(rec, ind, bod_use, bod_nam, bod_dom, bod_img)
          )
        }
        _ => format!(
          "({} {}: {}) -> {}",
          uses,
          pretty_name(name),
          typ.pretty(rec, ind),
          bod.pretty(rec, ind)
        ),
      }
    }
    fn apps(rec: Option<&String>, ind: bool, fun: &Term) -> String {
      match fun {
        App(_, f_uses, f_fun, f_typ, f_arg) => {
          format!(
            "{} ({} :: {} {})",
            apps(rec, ind, &f_fun),
            &f_arg.pretty(rec, ind),
            f_uses,
            &f_typ.pretty(rec, ind),
          )
        }
        _ => format!("{}", parens(rec, ind, fun)),
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

      Lam(_, uses, name, typ, bod) => {
        format!("λ {}", lams(rec, ind, uses, name, typ, bod))
      }
      App(_, uses, fun, typ, arg) => {
        format!(
          "({} ({} :: {} {}))",
          apps(rec, ind, &fun),
          arg.pretty(rec, ind),
          uses,
          typ.pretty(rec, ind),
        )
      }
      Let(_, letrec, uses, name, typ, exp, bod) => {
        format!(
          "{} {} {}: {} = {} in {}",
          if *letrec { "letrec" } else { "let" },
          uses,
          pretty_name(name),
          typ.pretty(rec, ind),
          exp.pretty(rec, ind),
          bod.pretty(rec, ind),
        )
      }
      Slf(_, name, bod) => {
        format!("self {} {}", pretty_name(name), bod.pretty(rec, ind))
      }
      All(_, uses, name, dom, img) => {
        format!("∀ {}", alls(rec, ind, uses, name, dom, img))
      }
      Dat(_, typ, bod) => {
        format!("data {} {}", parens(rec, ind, typ), bod.pretty(rec, ind),)
      }
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

#[derive(Debug)]
pub enum GenTerm {
  Var(Pos, Name, u64),
  Lam(
    Pos,
    Uses,
    Name,
    NonNull<MaybeUninit<GenTerm>>,
    NonNull<MaybeUninit<GenTerm>>,
  ),
  App(
    Pos,
    Uses,
    NonNull<MaybeUninit<GenTerm>>,
    NonNull<MaybeUninit<GenTerm>>,
    NonNull<MaybeUninit<GenTerm>>,
  ),
  All(
    Pos,
    Uses,
    Name,
    NonNull<MaybeUninit<GenTerm>>,
    NonNull<MaybeUninit<GenTerm>>,
  ),
  Slf(Pos, Name, NonNull<MaybeUninit<GenTerm>>),
  Dat(Pos, NonNull<MaybeUninit<GenTerm>>, NonNull<MaybeUninit<GenTerm>>),
  Cse(Pos, NonNull<MaybeUninit<GenTerm>>),
  Ref(Pos, Name, Cid, Cid),
  Let(
    Pos,
    bool,
    Uses,
    Name,
    NonNull<MaybeUninit<GenTerm>>,
    NonNull<MaybeUninit<GenTerm>>,
    NonNull<MaybeUninit<GenTerm>>,
  ),
  Typ(Pos),
  Lit(Pos, Literal),
  LTy(Pos, LitType),
  Opr(Pos, Op),
  Rec(Pos),
}

#[cfg(test)]
pub mod tests {

  use super::{
    Term::*,
    *,
  };
  use crate::defs::{
    Def,
    Defs,
  };
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  use rand::Rng;
  use sp_std::ops::Range;

  use sp_im::Vector;
  use sp_std::{
    boxed::Box,
    vec::Vec,
  };

  use crate::yatima;

  use crate::name::{
    is_valid_symbol_char,
    Name,
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
  pub fn test_defs() -> Defs { Defs::new() }

  // pub fn test_defs() -> Defs {
  //  let mut defs = Defs::new();
  //  let (id, _) = Def::make(
  //    Pos::None,
  //    yatima!("∀ (ω A: Type) (ω x: A) -> A"),
  //    yatima!("λ (ω A: Type) (ω x: A) => x"),
  //  );
  //  let (fst, _) = Def::make(
  //    Pos::None,
  //    yatima!("∀ (ω A: Type) (ω x y: A) -> A"),
  //    yatima!("λ (ω A: Type) (ω x y: A) => x"),
  //  );
  //  let (snd, _) = Def::make(
  //    Pos::None,
  //    yatima!("∀ (ω A: Type) (ω x y: A) -> A"),
  //    yatima!("λ (ω A: Type) (ω x y: A) => y"),
  //  );
  //  defs.insert(Name::from("id"), id);
  //  defs.insert(Name::from("fst"), fst);
  //  defs.insert(Name::from("snd"), snd);
  //  defs
  //}

  fn arbitrary_ref(g: &mut Gen, defs: Defs, ctx: Vector<Name>) -> GenTerm {
    let refs: Vec<(Name, Cid)> = defs
      .names
      .clone()
      .into_iter()
      .filter(|(n, _)| !ctx.contains(n))
      .collect();
    let len = refs.len();
    if len == 0 {
      return GenTerm::Typ(Pos::None);
    };
    let gen = gen_range(g, 0..(len - 1));
    let (n, _) = refs[gen].clone();
    let def = defs.get(&n).unwrap();
    GenTerm::Ref(Pos::None, n.clone(), def.def_cid, def.ast_cid)
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
    SLF,
    DAT,
    ALL,
    LET,
    CSE,
  }

  pub fn gen_range(g: &mut Gen, range: Range<usize>) -> usize {
    if range.end <= range.start {
      range.start
    }
    else {
      let res: usize = Arbitrary::arbitrary(g);
      (res % (range.end - range.start)) + range.start
    }
  }
  pub fn next_case(g: &mut Gen, gens: &Vec<(usize, Case)>) -> Case {
    let sum: usize = gens.iter().map(|x| x.0).sum();
    let mut weight: usize = gen_range(g, 1..sum);
    for gen in gens {
      match weight.checked_sub(gen.0) {
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

  #[inline]
  pub fn alloc_val<T>(val: T) -> NonNull<T> {
    NonNull::new(Box::leak(Box::new(val))).unwrap()
  }

  pub fn arbitrary_term(
    g: &mut Gen,
    rec: bool,
    defs: Defs,
    ctx0: Vector<Name>,
  ) -> Term {
    let res = alloc_val(MaybeUninit::<GenTerm>::uninit());
    let mut stack = vec![(ctx0, res.clone())];
    let term = Box::new(res);
    while let Some((ctx, mut ptr)) = stack.pop() {
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
        //(80usize.saturating_sub(2 * depth), Case::ALL),
        //(30usize.saturating_sub(3 * depth), Case::LET),
      ];
      use GenTerm::*;
      match next_case(g, &gens) {
        Case::TYP => unsafe {
          *ptr.as_mut() = MaybeUninit::new(Typ(Pos::None));
        },
        Case::REC => unsafe {
          *ptr.as_mut() = MaybeUninit::new(Rec(Pos::None));
        },
        Case::LTY => unsafe {
          *ptr.as_mut() =
            MaybeUninit::new(LTy(Pos::None, Arbitrary::arbitrary(g)));
        },
        Case::LIT => unsafe {
          *ptr.as_mut() =
            MaybeUninit::new(Lit(Pos::None, Arbitrary::arbitrary(g)));
        },
        Case::OPR => unsafe {
          *ptr.as_mut() =
            MaybeUninit::new(Opr(Pos::None, Arbitrary::arbitrary(g)));
        },
        Case::REF => unsafe {
          *ptr.as_mut() =
            MaybeUninit::new(arbitrary_ref(g, defs.clone(), ctx.clone()));
        },
        Case::VAR => {
          if ctx.len() == 0 {
            unsafe {
              *ptr.as_mut() = MaybeUninit::new(Typ(Pos::None));
            }
          }
          else {
            let gen = gen_range(g, 0..ctx.len());
            let n = &ctx[gen];
            let (i, _) = ctx.iter().enumerate().find(|(_, x)| *x == n).unwrap();
            unsafe {
              *ptr.as_mut() =
                MaybeUninit::new(Var(Pos::None, n.clone(), i as u64));
            }
          }
        }
        Case::LAM => {
          let n = arbitrary_name(g);
          let u = Arbitrary::arbitrary(g);
          let mut ctx2 = ctx.clone();
          ctx2.push_front(n.clone());
          let typ = alloc_val(MaybeUninit::<GenTerm>::uninit());
          let bod = alloc_val(MaybeUninit::<GenTerm>::uninit());
          stack.push((ctx, typ));
          stack.push((ctx2, bod));
          unsafe {
            *ptr.as_mut() =
              MaybeUninit::new(Lam(Pos::None, u, n.clone(), typ, bod));
          }
        }
        Case::SLF => {
          let n = arbitrary_name(g);
          let mut ctx2 = ctx.clone();
          ctx2.push_front(n.clone());
          let bod = alloc_val(MaybeUninit::<GenTerm>::uninit());
          stack.push((ctx2, bod));
          unsafe {
            *ptr.as_mut() = MaybeUninit::new(Slf(Pos::None, n.clone(), bod));
          }
        }
        Case::CSE => {
          let mut bod = alloc_val(MaybeUninit::<GenTerm>::uninit());
          stack.push((ctx, bod));
          unsafe {
            *ptr.as_mut() = MaybeUninit::new(Cse(Pos::None, bod));
          }
        }
        Case::DAT => {
          let typ = alloc_val(MaybeUninit::<GenTerm>::uninit());
          let bod = alloc_val(MaybeUninit::<GenTerm>::uninit());
          stack.push((ctx.clone(), typ));
          stack.push((ctx, bod));
          unsafe {
            *ptr.as_mut() = MaybeUninit::new(Dat(Pos::None, typ, bod));
          }
        }
        Case::APP => {
          let u = Arbitrary::arbitrary(g);
          let fun = alloc_val(MaybeUninit::<GenTerm>::uninit());
          let typ = alloc_val(MaybeUninit::<GenTerm>::uninit());
          let arg = alloc_val(MaybeUninit::<GenTerm>::uninit());
          stack.push((ctx.clone(), fun));
          stack.push((ctx.clone(), typ));
          stack.push((ctx, arg));
          unsafe {
            *ptr.as_mut() = MaybeUninit::new(App(Pos::None, u, fun, typ, arg));
          }
        }
        Case::ALL => {
          let n = arbitrary_name(g);
          let u = Arbitrary::arbitrary(g);
          let mut ctx2 = ctx.clone();
          ctx2.push_front(n.clone());
          let typ = alloc_val(MaybeUninit::<GenTerm>::uninit());
          let bod = alloc_val(MaybeUninit::<GenTerm>::uninit());
          stack.push((ctx, typ));
          stack.push((ctx2, bod));
          unsafe {
            *ptr.as_mut() =
              MaybeUninit::new(All(Pos::None, u, n.clone(), typ, bod));
          }
        }
        Case::LET => {
          let letrec: bool = Arbitrary::arbitrary(g);
          let uses: Uses = Arbitrary::arbitrary(g);
          let n = arbitrary_name(g);
          let mut ctx2 = ctx.clone();
          ctx2.push_front(n.clone());
          let typ = alloc_val(MaybeUninit::<GenTerm>::uninit());
          let exp = alloc_val(MaybeUninit::<GenTerm>::uninit());
          let bod = alloc_val(MaybeUninit::<GenTerm>::uninit());
          stack.push((ctx.clone(), typ));
          if letrec {
            stack.push((ctx2.clone(), exp));
          }
          else {
            stack.push((ctx, exp));
          };
          stack.push((ctx2.clone(), bod));
          unsafe {
            *ptr.as_mut() =
              MaybeUninit::new(Let(Pos::None, letrec, uses, n, typ, exp, bod));
          }
        }
      }
    }
    unsafe {
      let term = Box::from_raw(res.as_ptr());
      let term = term.assume_init();
      mem::transmute::<GenTerm, Term>(Box::into_inner(term))
    }
  }

  impl Arbitrary for Term {
    fn arbitrary(g: &mut Gen) -> Self {
      arbitrary_term(g, false, test_defs(), Vector::new())
    }
  }

  //#[quickcheck]
  // fn term_gen(x: Term) -> bool {
  //  println!("x: {}", x);
  //  use rand;
  //  let mut rng = rand::thread_rng();
  //  let a = rng.gen_bool(0.9);
  //  a
  //}

  #[quickcheck]
  fn term_embed_unembed(x: Term) -> bool {
    let (a, m) = x.clone().embed();
    match Term::unembed(&a, &m) {
      Ok(y) => {
        if x == y {
          true
        }
        else {
          println!("x: {:?}", x);
          println!("x: {}", x);
          println!("y: {:?}", y);
          println!("y: {}", y);
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
