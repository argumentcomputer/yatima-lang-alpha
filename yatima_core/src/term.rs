pub use crate::{
  anon::Anon,
  embed_error::EmbedError,
  literal::{
    LitType,
    Literal,
  },
  meta::Meta,
  position::Pos,
  primop::PrimOp,
  uses::Uses,
};

use cid::Cid;

use std::fmt;

#[derive(Clone, Debug)]
pub enum Term {
  Var(Pos, String, u64),
  Lam(Pos, String, Box<Term>),
  App(Pos, Box<(Term, Term)>),
  All(Pos, Uses, String, Box<(Term, Term)>),
  Slf(Pos, String, Box<Term>),
  Dat(Pos, Box<Term>),
  Cse(Pos, Box<Term>),
  Ref(Pos, String, Cid, Cid),
  Let(Pos, bool, Uses, String, Box<(Term, Term, Term)>),
  Typ(Pos),
  Ann(Pos, Box<(Term, Term)>),
  Lit(Pos, Literal),
  LTy(Pos, LitType),
  Opr(Pos, PrimOp),
  Rec(Pos),
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
  pub fn shift(self, inc: u64, dep: u64) -> Self {
    match self {
      Self::Var(pos, nam, idx) if idx < dep => Self::Var(pos, nam, idx),
      Self::Var(pos, nam, idx) => Self::Var(pos, nam, idx + inc),
      Self::Lam(pos, nam, bod) => {
        Self::Lam(pos, nam, Box::new((*bod).shift(inc, dep + 1)))
      }
      Self::Slf(pos, nam, bod) => {
        Self::Slf(pos, nam, Box::new((*bod).shift(inc, dep + 1)))
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
          Box::new((dom.shift(inc, dep), img.shift(inc, dep + 1))),
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
            exp.shift(inc, if rec { dep + 1 } else { dep }),
            bod.shift(inc, dep + 1),
          )),
        )
      }
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

  pub fn unembed(anon: &Anon, meta: &Meta) -> Result<Self, EmbedError> {
    match (anon, meta) {
      (Anon::Var(idx), Meta::Var(pos, nam)) => {
        Ok(Self::Var(*pos, nam.clone(), *idx))
      }
      (Anon::Ref(ast), Meta::Ref(pos, nam, def)) => {
        Ok(Self::Ref(*pos, nam.clone(), *ast, *def))
      }
      (Anon::Lit(lit), Meta::Lit(pos)) => Ok(Self::Lit(*pos, lit.clone())),
      (Anon::LTy(lty), Meta::LTy(pos)) => Ok(Self::LTy(*pos, *lty)),
      (Anon::Opr(opr), Meta::Opr(pos)) => Ok(Self::Opr(*pos, *opr)),
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

  pub fn pretty(&self, rec: Option<&String>) -> String {
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

    fn lams(rec: Option<&String>, nam: &str, bod: &Term) -> String {
      match bod {
        Lam(_, nam2, bod2) => {
          format!("{} {}", name(nam), lams(rec, nam2, bod2))
        }
        _ => format!("{} => {}", nam, bod.pretty(rec)),
      }
    }

    fn alls(
      rec: Option<&String>,
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
            typ.pretty(rec),
            alls(rec, bod_use, bod_nam, &bod.0, &bod.1)
          )
        }
        _ => format!(
          " ({}{}: {}) -> {}",
          uses(use_),
          name(nam),
          typ.pretty(rec),
          bod.pretty(rec)
        ),
      }
    }

    fn parens(rec: Option<&String>, term: &Term) -> String {
      if is_atom(term) {
        term.pretty(rec)
      }
      else {
        format!("({})", term.pretty(rec))
      }
    }

    fn apps(rec: Option<&String>, fun: &Term, arg: &Term) -> String {
      match (fun, arg) {
        (App(_, f), App(_, a)) => {
          format!("{} ({})", apps(rec, &f.0, &f.1), apps(rec, &a.0, &a.1))
        }
        (App(_, f), arg) => {
          format!("{} {}", apps(rec, &f.0, &f.1), parens(rec, arg))
        }
        (fun, App(_, a)) => {
          format!("{} ({})", parens(rec, fun), apps(rec, &a.0, &a.1))
        }
        (fun, arg) => {
          format!("{} {}", parens(rec, fun), parens(rec, arg))
        }
      }
    }

    match self {
      Var(_, nam, ..) => nam.to_string(),
      Ref(_, nam, ..) => nam.to_string(),
      Rec(_) => match rec {
        Some(rec) => rec.to_owned(),
        _ => "#^".to_string(),
      },

      Lam(_, nam, term) => format!("λ {}", lams(rec, nam, term)),
      App(_, terms) => apps(rec, &terms.0, &terms.1),
      Let(_, letrec, u, n, terms) => {
        format!(
          "let{} {}{}: {} = {}; {}",
          if *letrec { "rec" } else { "" },
          uses(u),
          name(n),
          terms.0.pretty(rec),
          terms.1.pretty(rec),
          terms.2.pretty(rec),
        )
      }
      Slf(_, nam, bod) => format!("@{} {}", name(nam), bod.pretty(rec)),
      All(_, us_, nam, terms) => {
        format!("∀{}", alls(rec, us_, nam, &terms.0, &terms.1))
      }
      Ann(_, terms) => {
        format!("{} :: {}", parens(rec, &terms.1), parens(rec, &terms.0))
      }
      Dat(_, bod) => format!("data {}", bod.pretty(rec)),
      Cse(_, bod) => format!("case {}", bod.pretty(rec)),
      Typ(_) => "Type".to_string(),
      Lit(_, lit) => format!("{}", lit),
      LTy(_, lty) => format!("{}", lty),
      Opr(_, opr) => format!("{}", opr),
    }
  }
}

impl fmt::Display for Term {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.pretty(None))
  }
}

#[cfg(test)]
pub mod tests {
  use super::{
    Term::*,
    *,
  };
  use crate::defs::Defs;
  use quickcheck::{
    Arbitrary,
    Gen,
  };
  use rand::Rng;

  use im::Vector;

  use crate::{
    parse::term::is_valid_symbol_char,
    tests::frequency,
  };

  pub fn arbitrary_name(g: &mut Gen) -> String {
    let s: String = Arbitrary::arbitrary(g);
    let mut s: String = s
      .chars()
      .filter(|x| is_valid_symbol_char(*x) && char::is_ascii_alphabetic(x))
      .collect();
    s.truncate(1);
    format!("_{}", s)
  }

  fn arbitrary_var(ctx: Vector<String>) -> Box<dyn Fn(&mut Gen) -> Term> {
    Box::new(move |_g: &mut Gen| {
      if ctx.len() == 0 {
        return Term::Typ(Pos::None);
      }
      let mut rng = rand::thread_rng();
      let gen = rng.gen_range(0..ctx.len());
      let n = &ctx[gen];
      let (i, _) = ctx.iter().enumerate().find(|(_, x)| *x == n).unwrap();
      Var(Pos::None, n.clone(), i as u64)
    })
  }

  pub fn test_defs() -> Defs {
    // let inp = "(\"def\" \"id\" \"\" (\"forall\" \"ω\" \"A\" \"Type\" \"A\")
    //           (\"lambda\" \"x\" \"x\"))";
    // let d =
    //  Def::decode(HashMap::new(), hashexpr::parse(inp).unwrap().1).unwrap();
    // let (d, _, t) = d.embed();
    // let mut refs = HashMap::new();
    // refs.insert(String::from("id"), (d.encode().link(), t.encode().link()));
    // refs
    Defs::new()
  }

  fn arbitrary_ref(
    defs: Defs,
    ctx: Vector<String>,
  ) -> Box<dyn Fn(&mut Gen) -> Term> {
    Box::new(move |_g: &mut Gen| {
      let mut rng = rand::thread_rng();
      let mut ref_iter = defs.names.iter().filter(|(n, _)| !ctx.contains(n));
      let len = ref_iter.by_ref().count();
      if len == 0 {
        return Term::Typ(Pos::None);
      }
      let gen = rng.gen_range(0..len);
      match ref_iter.nth(gen) {
        Some((n, _)) => {
          let def = defs.get(n).unwrap();
          Ref(Pos::None, n.clone(), def.def_cid, def.ast_cid)
        }
        None => Term::Typ(Pos::None),
      }
    })
  }

  fn arbitrary_lam(
    rec: bool,
    defs: Defs,
    ctx: Vector<String>,
  ) -> Box<dyn Fn(&mut Gen) -> Term> {
    Box::new(move |g: &mut Gen| {
      let n = arbitrary_name(g);
      let mut ctx2 = ctx.clone();
      ctx2.push_front(n.clone());
      Lam(Pos::None, n, Box::new(arbitrary_term(g, rec, defs.clone(), ctx2)))
    })
  }

  fn arbitrary_app(
    rec: bool,
    defs: Defs,
    ctx: Vector<String>,
  ) -> Box<dyn Fn(&mut Gen) -> Term> {
    Box::new(move |g: &mut Gen| {
      Term::App(
        Pos::None,
        Box::new((
          arbitrary_term(g, rec, defs.clone(), ctx.clone()),
          arbitrary_term(g, rec, defs.clone(), ctx.clone()),
        )),
      )
    })
  }

  fn arbitrary_ann(
    rec: bool,
    defs: Defs,
    ctx: Vector<String>,
  ) -> Box<dyn Fn(&mut Gen) -> Term> {
    Box::new(move |g: &mut Gen| {
      Term::Ann(
        Pos::None,
        Box::new((
          arbitrary_term(g, rec, defs.clone(), ctx.clone()),
          arbitrary_term(g, rec, defs.clone(), ctx.clone()),
        )),
      )
    })
  }

  fn arbitrary_slf(
    rec: bool,
    defs: Defs,
    ctx: Vector<String>,
  ) -> Box<dyn Fn(&mut Gen) -> Term> {
    Box::new(move |g: &mut Gen| {
      let n = arbitrary_name(g);
      let mut ctx2 = ctx.clone();
      ctx2.push_front(n.clone());
      Slf(Pos::None, n, Box::new(arbitrary_term(g, rec, defs.clone(), ctx2)))
    })
  }

  fn arbitrary_dat(
    rec: bool,
    defs: Defs,
    ctx: Vector<String>,
  ) -> Box<dyn Fn(&mut Gen) -> Term> {
    Box::new(move |g: &mut Gen| {
      Term::Dat(
        Pos::None,
        Box::new(arbitrary_term(g, rec, defs.clone(), ctx.clone())),
      )
    })
  }

  fn arbitrary_cse(
    rec: bool,
    defs: Defs,
    ctx: Vector<String>,
  ) -> Box<dyn Fn(&mut Gen) -> Term> {
    Box::new(move |g: &mut Gen| {
      Term::Cse(
        Pos::None,
        Box::new(arbitrary_term(g, rec, defs.clone(), ctx.clone())),
      )
    })
  }

  fn arbitrary_all(
    rec: bool,
    defs: Defs,
    ctx: Vector<String>,
  ) -> Box<dyn Fn(&mut Gen) -> Term> {
    Box::new(move |g: &mut Gen| {
      let n = arbitrary_name(g);
      let u: Uses = Arbitrary::arbitrary(g);
      let mut ctx2 = ctx.clone();
      ctx2.push_front(n.clone());
      All(
        Pos::None,
        u,
        n,
        Box::new((
          arbitrary_term(g, rec, defs.clone(), ctx.clone()),
          arbitrary_term(g, rec, defs.clone(), ctx2),
        )),
      )
    })
  }

  fn arbitrary_let(
    rec: bool,
    defs: Defs,
    ctx: Vector<String>,
  ) -> Box<dyn Fn(&mut Gen) -> Term> {
    Box::new(move |g: &mut Gen| {
      // let rec: bool = Arbitrary::arbitrary(g);
      let letrec: bool = false;
      let n = arbitrary_name(g);
      let u: Uses = Arbitrary::arbitrary(g);
      let typ = arbitrary_term(g, rec, defs.clone(), ctx.clone());
      if letrec {
        let mut ctx2 = ctx.clone();
        ctx2.push_front(n.clone());
        let exp = arbitrary_term(g, rec, defs.clone(), ctx2.clone());
        let bod = arbitrary_term(g, rec, defs.clone(), ctx2);
        Let(Pos::None, letrec, u, n, Box::new((typ, exp, bod)))
      }
      else {
        let mut ctx2 = ctx.clone();
        ctx2.push_front(n.clone());
        let exp = arbitrary_term(g, rec, defs.clone(), ctx.clone());
        let bod = arbitrary_term(g, rec, defs.clone(), ctx2);
        Let(Pos::None, letrec, u, n, Box::new((typ, exp, bod)))
      }
    })
  }

  pub fn arbitrary_term(
    g: &mut Gen,
    rec: bool,
    defs: Defs,
    ctx: Vector<String>,
  ) -> Term {
    let len = ctx.len();
    if len == 0 {
      arbitrary_lam(rec, defs, ctx)(g)
    }
    else {
      frequency(g, vec![
        (100, arbitrary_var(ctx.clone())),
        (100, arbitrary_ref(defs.clone(), ctx.clone())),
        (100, Box::new(|_| Term::Typ(Pos::None))),
        (if rec { 100 } else { 0 }, Box::new(|_| Term::Rec(Pos::None))),
        (100, Box::new(|g| Term::Lit(Pos::None, Arbitrary::arbitrary(g)))),
        (100, Box::new(|g| Term::LTy(Pos::None, Arbitrary::arbitrary(g)))),
        (100, Box::new(|g| Term::Opr(Pos::None, Arbitrary::arbitrary(g)))),
        (90, arbitrary_lam(rec, defs.clone(), ctx.clone())),
        (90, arbitrary_dat(rec, defs.clone(), ctx.clone())),
        (90, arbitrary_cse(rec, defs.clone(), ctx.clone())),
        (90, arbitrary_slf(rec, defs.clone(), ctx.clone())),
        (80, arbitrary_all(rec, defs.clone(), ctx.clone())),
        (80, arbitrary_app(rec, defs.clone(), ctx.clone())),
        (80, arbitrary_ann(rec, defs.clone(), ctx.clone())),
        (30, arbitrary_let(rec, defs.clone(), ctx.clone())),
      ])
    }
  }

  impl Arbitrary for Term {
    fn arbitrary(g: &mut Gen) -> Self {
      arbitrary_term(g, false, test_defs(), Vector::new())
    }
  }
  #[quickcheck]
  fn term_embed_unembed(x: Term) -> bool {
    let (a, m) = x.clone().embed();
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
