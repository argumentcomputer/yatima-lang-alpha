use yatima_core::{
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
};

use sp_cid::Cid;

use sp_std::{
  borrow::ToOwned,
  boxed::Box,
  fmt,
  rc::Rc,
};

use crate::uses::Uses;
use alloc::string::{
  String,
  ToString,
};

pub enum Term {
  Var(Pos, Name, u64),
  Hol(Pos, Name),
  Lam(Pos, Uses, Name, Box<Term>, Box<Term>),
  App(Pos, Uses, Box<Term>, Box<Term>, Box<Term>),
  All(Pos, Uses, Name, Box<Term>, Box<Term>),
  Slf(Pos, Name, Box<Term>),
  Dat(Pos, Box<Term>, Box<Term>),
  Cse(Pos, Box<Term>),
  Ref(Pos, Name),
  Let(Pos, bool, Uses, Name, Box<Term>, Box<Term>, Box<Term>),
  Typ(Pos),
  Lit(Pos, Literal),
  LTy(Pos, LitType),
  Opr(Pos, Op),
}

impl fmt::Debug for Term {
  fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Var(_, n, i) => fmt.debug_tuple("Var").field(&n).field(i).finish(),
      Self::Hol(_, n) => fmt.debug_tuple("Hol").field(&n).finish(),
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
      Self::Ref(_, n) => fmt.debug_tuple("Ref").field(&n).finish(),
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
    }
  }
}

impl PartialEq for Term {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::Var(_, na, ia), Self::Var(_, nb, ib)) => na == nb && ia == ib,
      (Self::Hol(_, na), Self::Hol(_, nb)) => na == nb,
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
      (Self::Ref(_, na), Self::Ref(_, nb)) => na == nb,
      (
        Self::Let(_, ra, ua, na, ta, xa, ba),
        Self::Let(_, rb, ub, nb, tb, xb, bb),
      ) => ra == rb && ua == ub && na == nb && ta == tb && xa == xb && ba == bb,
      (Self::Typ(_), Self::Typ(_)) => true,
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
      Term::Var(pos, ..) => *pos,
      Term::Hol(pos, ..) => *pos,
      Term::Ref(pos, ..) => *pos,
      Term::Lam(pos, ..) => *pos,
      Term::App(pos, ..) => *pos,
      Term::All(pos, ..) => *pos,
      Term::Slf(pos, ..) => *pos,
      Term::Dat(pos, ..) => *pos,
      Term::Cse(pos, _) => *pos,
      Term::Let(pos, ..) => *pos,
      Term::Typ(pos) => *pos,
      Term::LTy(pos, _) => *pos,
      Term::Lit(pos, _) => *pos,
      Term::Opr(pos, _) => *pos,
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

  pub fn pretty(&self, ind: bool) -> String {
    use Term::*;
    const WILDCARD: &str = "_";

    fn pretty_name(nam: &str) -> &str {
      if nam.is_empty() { WILDCARD } else { nam }
    }

    fn is_atom(term: &Term) -> bool {
      matches!(term, Var(..) | Ref(..) | Lit(..) | LTy(..) | Opr(..) | Typ(..))
    }

    fn parens(ind: bool, term: &Term) -> String {
      if is_atom(term) {
        term.pretty(ind)
      }
      else {
        format!("({})", term.pretty(ind))
      }
    }

    fn lams(
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
            typ.pretty(ind),
            lams(ind, bod_use, bod_nam, bod_typ, bod_bod)
          )
        }
        _ => format!(
          "({} {}: {}) => {}",
          uses,
          pretty_name(name),
          typ.pretty(ind),
          bod.pretty(ind)
        ),
      }
    }

    fn alls(
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
            typ.pretty(ind),
            alls(ind, bod_use, bod_nam, bod_dom, bod_img)
          )
        }
        _ => format!(
          "({} {}: {}) -> {}",
          uses,
          pretty_name(name),
          typ.pretty(ind),
          bod.pretty(ind)
        ),
      }
    }
    fn apps(ind: bool, fun: &Term) -> String {
      match fun {
        App(_, f_uses, f_fun, f_typ, f_arg) => {
          format!(
            "{} ({} :: {} {})",
            apps(ind, &f_fun),
            &f_arg.pretty(ind),
            f_uses,
            &f_typ.pretty(ind),
          )
        }
        _ => format!("{}", parens(ind, fun)),
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
      Hol(_, nam) => format!("?{}", nam),
      Ref(_, nam, ..) => nam.to_string(),

      Lam(_, uses, name, typ, bod) => {
        format!("λ {}", lams(ind, uses, name, typ, bod))
      }
      App(_, uses, fun, typ, arg) => {
        format!(
          "({} ({} :: {} {}))",
          apps(ind, &fun),
          arg.pretty(ind),
          uses,
          typ.pretty(ind),
        )
      }
      Let(_, letrec, uses, name, typ, exp, bod) => {
        format!(
          "{} {} {}: {} = {} in {}",
          if *letrec { "letrec" } else { "let" },
          uses,
          pretty_name(name),
          typ.pretty(ind),
          exp.pretty(ind),
          bod.pretty(ind),
        )
      }
      Slf(_, name, bod) => {
        format!("self {} {}", pretty_name(name), bod.pretty(ind))
      }
      All(_, uses, name, dom, img) => {
        format!("∀ {}", alls(ind, uses, name, dom, img))
      }
      Dat(_, typ, bod) => {
        format!("data {} {}", parens(ind, typ), bod.pretty(ind),)
      }
      Cse(_, bod) => format!("case {}", bod.pretty(ind)),
      Typ(_) => "Type".to_string(),
      Lit(_, lit) => format!("{}", lit),
      LTy(_, lty) => format!("{}", lty),
      Opr(_, opr) => format!("{}", opr),
    }
  }
}

impl fmt::Display for Term {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.pretty(false))
  }
}

//// #[cfg(test)]
//// pub mod tests {
////
////  use super::{
////    Term::*,
////    *,
////  };
////  use crate::{
////    defs::{
////      Def,
////      Defs,
////    },
////    yatima,
////  };
////  use quickcheck::{
////    Arbitrary,
////    Gen,
////  };
////  use sp_std::ops::Range;
////
////  use sp_im::Vector;
////  use sp_std::{
////    boxed::Box,
////    vec::Vec,
////  };
////
////  use crate::{
////    name::Name,
////    parse::term::is_valid_symbol_char,
////  };
////
////  pub fn arbitrary_name(g: &mut Gen) -> Name {
////    let s: String = Arbitrary::arbitrary(g);
////    let mut s: String = s
////      .chars()
////      .filter(|x| is_valid_symbol_char(*x) && char::is_ascii_alphabetic(x))
////      .collect();
////    s.truncate(1);
////    Name::from(format!("_{}", s))
////  }
////
////  pub fn test_defs() -> Defs {
////    let mut defs = Defs::new();
////    let (id, _) = Def::make(
////      Pos::None,
////      yatima!("∀ (A: Type) (x: A) -> A"),
////      yatima!("λ A x => x"),
////    );
////    let (fst, _) = Def::make(
////      Pos::None,
////      yatima!("∀ (A: Type) (x y: A) -> A"),
////      yatima!("λ A x y => x"),
////    );
////    let (snd, _) = Def::make(
////      Pos::None,
////      yatima!("∀ (A: Type) (x y: A) -> A"),
////      yatima!("λ A x y => y"),
////    );
////    defs.insert(Name::from("id"), id);
////    defs.insert(Name::from("fst"), fst);
////    defs.insert(Name::from("snd"), snd);
////    defs
////  }
////
////  fn arbitrary_ref(g: &mut Gen, defs: Defs, ctx: Vector<Name>) -> Tree {
////    let refs: Vec<(Name, Cid)> = defs
////      .names
////      .clone()
////      .into_iter()
////      .filter(|(n, _)| !ctx.contains(n))
////      .collect();
////    let len = refs.len();
////    if len == 0 {
////      return Tree::Typ;
////    };
////    let gen = gen_range(g, 0..(len - 1));
////    let (n, _) = refs[gen].clone();
////    let def = defs.get(&n).unwrap();
////    Tree::Ref(n.clone(), def.def_cid, def.ast_cid)
////  }
////
////  #[derive(Debug, Clone)]
////  pub enum Tree {
////    Var(Name, u64),
////    Typ,
////    Rec,
////    Ref(Name, Cid, Cid),
////    Opr(Op),
////    Lit(Literal),
////    LTy(LitType),
////    Lam(Name, usize),
////    Slf(Name, usize),
////    App(usize, usize),
////    Ann(usize, usize),
////    Cse(usize),
////    Dat(usize),
////    All(Uses, Name, usize, usize),
////    Let(bool, Uses, Name, usize, usize, usize),
////  }
////
////  impl Tree {
////    pub fn into_term(&self, arena: &Vec<Tree>) -> Term {
////      match self {
////        Self::Var(n, i) => Term::Var(Pos::None, n.clone(), *i),
////        Self::Rec => Term::Rec(Pos::None),
////        Self::Typ => Term::Typ(Pos::None),
////        Self::Ref(n, d, a) => Term::Ref(Pos::None, n.clone(), *d, *a),
////        Self::Opr(x) => Term::Opr(Pos::None, *x),
////        Self::Lit(x) => Term::Lit(Pos::None, x.clone()),
////        Self::LTy(x) => Term::LTy(Pos::None, *x),
////        Self::Lam(n, bod) => {
////          let bod = arena[*bod].into_term(&arena);
////          Term::Lam(Pos::None, n.clone(), Box::new(bod))
////        }
////        Self::Slf(n, bod) => {
////          let bod = arena[*bod].into_term(&arena);
////          Term::Slf(Pos::None, n.clone(), Box::new(bod))
////        }
////        Self::Cse(bod) => {
////          let bod = arena[*bod].into_term(&arena);
////          Term::Cse(Pos::None, Box::new(bod))
////        }
////        Self::Dat(bod) => {
////          let bod = arena[*bod].into_term(&arena);
////          Term::Dat(Pos::None, Box::new(bod))
////        }
////        Self::App(fun, arg) => {
////          let fun = arena[*fun].into_term(&arena);
////          let arg = arena[*arg].into_term(&arena);
////          Term::App(Pos::None, Box::new((fun, arg)))
////        }
////        Self::Ann(typ, trm) => {
////          let typ = arena[*typ].into_term(&arena);
////          let trm = arena[*trm].into_term(&arena);
////          Term::Ann(Pos::None, Box::new((typ, trm)))
////        }
////        Self::All(uses, n, dom, img) => {
////          let dom = arena[*dom].into_term(&arena);
////          let img = arena[*img].into_term(&arena);
////          Term::All(Pos::None, *uses, n.clone(), Box::new((dom, img)))
////        }
////        Self::Let(rec, uses, n, typ, trm, bod) => {
////          let typ = arena[*typ].into_term(&arena);
////          let trm = arena[*trm].into_term(&arena);
////          let bod = arena[*bod].into_term(&arena);
////          Term::Let(
////            Pos::None,
////            *rec,
////            *uses,
////            n.clone(),
////            Box::new((typ, trm, bod)),
////          )
////        }
////      }
////    }
////  }
////
////  #[derive(Debug, Clone, Copy)]
////  pub enum Case {
////    VAR,
////    TYP,
////    REC,
////    REF,
////    LIT,
////    LTY,
////    OPR,
////    LAM,
////    APP,
////    ANN,
////    SLF,
////    CSE,
////    DAT,
////    ALL,
////    LET,
////  }
////
////  pub fn gen_range(g: &mut Gen, range: Range<usize>) -> usize {
////    let res: usize = Arbitrary::arbitrary(g);
////    (res % (range.end - range.start)) + range.start
////  }
////
////  pub fn next_case(g: &mut Gen, gens: &Vec<(usize, Case)>) -> Case {
////    let sum: usize = gens.iter().map(|x| x.0).sum();
////    let mut weight: usize = gen_range(g, 1..(sum + 1));
////    for gen in gens {
////      match weight.checked_sub(gen.0 + 1) {
////        None | Some(0) => {
////          return gen.1;
////        }
////        _ => {
////          weight -= gen.0;
////        }
////      }
////    }
////    panic!("Calculation error for weight = {}", weight);
////  }
////
////  pub fn arbitrary_term(
////    g: &mut Gen,
////    rec: bool,
////    defs: Defs,
////    ctx0: Vector<Name>,
////  ) -> Term {
////    let name = Name::from("_r");
////    let mut ctx1 = ctx0.clone();
////    ctx1.push_front(name.clone());
////    let mut ctxs: Vec<Vector<Name>> = vec![ctx0, ctx1];
////    let mut arena: Vec<Option<Tree>> = vec![Some(Tree::Lam(name, 1)), None];
////    let mut todo: Vec<usize> = vec![1];
////
////    while let Some(idx) = todo.pop() {
////      let ctx: Vector<Name> = ctxs[idx].clone();
////      let depth = ctx.len();
////      let gens: Vec<(usize, Case)> = vec![
////        (100, Case::VAR),
////        (100, Case::TYP),
////        (100, Case::REF),
////        (100, Case::LIT),
////        (100, Case::LTY),
////        (100, Case::OPR),
////        (if rec { 100 } else { 0 }, Case::REC),
////        (90usize.saturating_sub(depth), Case::LAM),
////        (90usize.saturating_sub(depth), Case::SLF),
////        (90usize.saturating_sub(depth), Case::CSE),
////        (90usize.saturating_sub(depth), Case::DAT),
////        (80usize.saturating_sub(2 * depth), Case::APP),
////        (80usize.saturating_sub(2 * depth), Case::ANN),
////        (80usize.saturating_sub(2 * depth), Case::ALL),
////        (30usize.saturating_sub(3 * depth), Case::LET),
////      ];
////
////      match next_case(g, &gens) {
////        Case::TYP => {
////          arena[idx] = Some(Tree::Typ);
////        }
////        Case::REC => {
////          arena[idx] = Some(Tree::Rec);
////        }
////        Case::LTY => {
////          arena[idx] = Some(Tree::LTy(Arbitrary::arbitrary(g)));
////        }
////        Case::LIT => {
////          arena[idx] = Some(Tree::Lit(Arbitrary::arbitrary(g)));
////        }
////        Case::OPR => {
////          arena[idx] = Some(Tree::Opr(Arbitrary::arbitrary(g)));
////        }
////        Case::REF => {
////          arena[idx] = Some(arbitrary_ref(g, defs.clone(), ctx.clone()));
////        }
////        Case::VAR => {
////          let gen = gen_range(g, 0..ctx.len());
////          let n = &ctx[gen];
////          let (i, _) = ctx.iter().enumerate().find(|(_, x)| *x == n).unwrap();
////          arena[idx] = Some(Tree::Var(n.clone(), i as u64));
////        }
////        Case::LAM => {
////          let n = arbitrary_name(g);
////          let mut ctx2 = ctx.clone();
////          ctx2.push_front(n.clone());
////          let bod = arena.len();
////          todo.push(bod);
////          ctxs.push(ctx2);
////          arena.push(None);
////          arena[idx] = Some(Tree::Lam(n, bod));
////        }
////        Case::SLF => {
////          let n = arbitrary_name(g);
////          let mut ctx2 = ctx.clone();
////          ctx2.push_front(n.clone());
////          let bod = arena.len();
////          todo.push(bod);
////          ctxs.push(ctx2);
////          arena.push(None);
////          arena[idx] = Some(Tree::Slf(n, bod));
////        }
////        Case::CSE => {
////          let bod = arena.len();
////          todo.push(bod);
////          ctxs.push(ctx.clone());
////          arena.push(None);
////          arena[idx] = Some(Tree::Cse(bod));
////        }
////        Case::DAT => {
////          let bod = arena.len();
////          todo.push(bod);
////          ctxs.push(ctx.clone());
////          arena.push(None);
////          arena[idx] = Some(Tree::Dat(bod));
////        }
////        Case::APP => {
////          let fun = arena.len();
////          todo.push(fun);
////          ctxs.push(ctx.clone());
////          arena.push(None);
////          let arg = arena.len();
////          todo.push(arg);
////          ctxs.push(ctx.clone());
////          arena.push(None);
////          arena[idx] = Some(Tree::App(fun, arg));
////        }
////        Case::ANN => {
////          let typ = arena.len();
////          todo.push(typ);
////          ctxs.push(ctx.clone());
////          arena.push(None);
////          let trm = arena.len();
////          todo.push(trm);
////          ctxs.push(ctx.clone());
////          arena.push(None);
////          arena[idx] = Some(Tree::Ann(typ, trm));
////        }
////        Case::ALL => {
////          let uses: Uses = Arbitrary::arbitrary(g);
////          let n = arbitrary_name(g);
////          let mut ctx2 = ctx.clone();
////          ctx2.push_front(n.clone());
////          let dom = arena.len();
////          todo.push(dom);
////          ctxs.push(ctx.clone());
////          arena.push(None);
////          let img = arena.len();
////          todo.push(img);
////          ctxs.push(ctx2);
////          arena.push(None);
////          arena[idx] = Some(Tree::All(uses, n, dom, img));
////        }
////        Case::LET => {
////          let letrec: bool = Arbitrary::arbitrary(g);
////          let uses: Uses = Arbitrary::arbitrary(g);
////          let n = arbitrary_name(g);
////          let mut ctx2 = ctx.clone();
////          ctx2.push_front(n.clone());
////          let typ = arena.len();
////          todo.push(typ);
////          ctxs.push(ctx.clone());
////          arena.push(None);
////          let trm = arena.len();
////          todo.push(trm);
////          if letrec {
////            ctxs.push(ctx2.clone());
////          }
////          else {
////            ctxs.push(ctx.clone())
////          };
////          arena.push(None);
////          let bod = arena.len();
////          todo.push(bod);
////          ctxs.push(ctx2.clone());
////          arena.push(None);
////          arena[idx] = Some(Tree::Let(letrec, uses, n, typ, trm, bod));
////        }
////      }
////    }
////    //    println!("arena: {:?}", arena);
////    let arena: Vec<Tree> = arena.into_iter().map(|x| x.unwrap()).collect();
////    //    println!("arena: {:?}", arena);
////    arena[0].into_term(&arena)
////  }
////
////  impl Arbitrary for Term {
////    fn arbitrary(g: &mut Gen) -> Self {
////      arbitrary_term(g, false, test_defs(), Vector::new())
////    }
////  }
////
////  #[quickcheck]
////  fn term_embed_unembed(x: Term) -> bool {
////    let (a, m) = x.clone().embed();
////    println!("x: {}", x);
////    match Term::unembed(&a, &m) {
////      Ok(y) => {
////        if x == y {
////          true
////        }
////        else {
////          // println!("x: {:?}", x);
////          // println!("y: {:?}", y);
////          false
////        }
////      }
////      _e => {
////        // println!("x: {:?}", x);
////        // println!("a: {:?}", a);
////        // println!("m: {:?}", m);
////        // println!("e: {:?}", _e);
////        false
////      }
////    }
////  }
//// }
