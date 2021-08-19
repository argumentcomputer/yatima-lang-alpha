use crate::{
  defs::Defs,
  literal::{
    LitType,
    Literal,
  },
  name::Name,
  prim::Op,
  uses::Uses,
  term::Term,
  yatima,
};

use sp_cid::Cid;

use sp_std::{
  vec::Vec,
  collections::{
    btree_set::BTreeSet,
    btree_map::BTreeMap,
  },
  rc::Rc,
};

use alloc::string::String;

#[derive(Debug, Clone)]
pub enum Naive {
  Var(Name, usize),
  Free(Name, usize),
  Lam(Name, Rc<Naive>),
  App(Rc<Naive>, Rc<Naive>),
  All(Uses, Name, Rc<Naive>, Rc<Naive>),
  Slf(Name, Rc<Naive>),
  Dat(Rc<Naive>),
  Cse(Rc<Naive>),
  Ref(usize),
  Let(Uses, Name, Rc<Naive>, Rc<Naive>, Rc<Naive>),
  Fix(Name, Rc<Naive>),
  Typ,
  Ann(Rc<Naive>, Rc<Naive>),
  Lit(Literal),
  LTy(LitType),
  Opr(Op),
  Unr(usize, Rc<Naive>),
}


// Auxiliary definition positions
pub const NAT_INDUCTION: usize = 0;
pub const INT_INDUCTION: usize = 1;
pub const BYTES_INDUCTION: usize = 2;
pub const BITS_INDUCTION: usize = 3;
pub const TEXT_INDUCTION: usize = 4;
pub const BOOL_INDUCTION: usize = 5;
pub const DEF_START: usize = 6;

pub fn aux_defs() -> Vec<DefCell> {
  let defs = &mut Defs::new();
  let done = &mut BTreeMap::new();
  let ir_defs = &mut vec![];
  let nat_induction = term_to_naive(defs, done, ir_defs, 0, &yatima!(
    "λ self => ∀ (0 P: ∀ #Nat -> Type)
             (& zero: P 0)
             (& succ: ∀ (pred: #Nat) -> P (#Nat.suc pred))
           -> P self
          "));
  let int_induction = term_to_naive(defs, done, ir_defs, 0, &yatima!(
    "λ self => ∀ (0 P: ∀ #Int -> Type)
             (& int: ∀ (sign: #Bool) (abs: #Nat) -> P (#Int.new sign abs))
           -> P self
          "));
  let bytes_induction = term_to_naive(defs, done, ir_defs, 0, &yatima!(
    "λ self => ∀ (0 P: ∀ #Bytes -> Type)
             (& nil: P \"\")
             (& cons: ∀ (x: #U8) (xs: #Bytes) -> P (#Bytes.cons x xs))
           -> P self
          "));
  let bits_induction = term_to_naive(defs, done, ir_defs, 0, &yatima!(
    "λ self => ∀ (0 P: ∀ #Bits -> Type)
             (& nil: P #b)
             (& cons: ∀ (x: #Bool) (xs: #Bits) -> P (#Bits.cons x xs))
           -> P self
          "));
  let text_induction = term_to_naive(defs, done, ir_defs, 0, &yatima!(
    "λ self => ∀ (0 P: ∀ #Text -> Type)
             (& nil: P \"\")
             (& cons: ∀ (x: #Char) (xs: #Text) -> P (#Text.cons x xs))
           -> P self
          "));
  let bool_induction = term_to_naive(defs, done, ir_defs, 0, &yatima!(
    "λ self => ∀ (0 P: ∀ #Bool -> Type)
             (& t: P #Bool.true)
             (& f: P #Bool.false)
           -> P self
          "));
  let typ = Rc::new(Naive::Typ);
  vec![
    DefCell { name: Name::from("#NAT_INDUCTION"),   term: nat_induction,   typ_: typ.clone() },
    DefCell { name: Name::from("#INT_INDUCTION"),   term: int_induction,   typ_: typ.clone() },
    DefCell { name: Name::from("#BYTES_INDUCTION"), term: bytes_induction, typ_: typ.clone() },
    DefCell { name: Name::from("#BITS_INDUCTION"),  term: bits_induction,  typ_: typ.clone() },
    DefCell { name: Name::from("#TEXT_INDUCTION"),  term: text_induction,  typ_: typ.clone() },
    DefCell { name: Name::from("#BOOL_INDUCTION"),  term: bool_induction,  typ_: typ },
  ]
}

pub fn term_to_naive(
  defs: &Defs,
  done: &mut BTreeMap<Cid, usize>,
  naive_defs: &mut Vec<DefCell>,
  rec_idx: usize,
  term: &Term,
) -> Rc<Naive> {
  match term {
    Term::Var(_, name, idx) => Rc::new(Naive::Var(name.clone(), *idx as usize)),
    Term::Lam(_, nam, link) => {
      let bod = &**link;
      let bod = term_to_naive(defs, done, naive_defs, rec_idx, bod);
      let naive = Naive::Lam(nam.clone(), bod);
      Rc::new(naive)
    },
    Term::App(_, link) => {
      let (fun, arg) = &**link;
      let fun = term_to_naive(defs, done, naive_defs, rec_idx, fun);
      let arg = term_to_naive(defs, done, naive_defs, rec_idx, arg);
      let naive = Naive::App(fun, arg);
      Rc::new(naive)
    },
    Term::Rec(_) => {
      Rc::new(Naive::Ref(rec_idx))
    }
    Term::Ref(_, nam, def_link, _) => {
      match done.get(def_link) {
        Some(idx) => Rc::new(Naive::Ref(*idx)),
        None => {
          let idx = naive_defs.len();
          done.insert(*def_link, idx);
          naive_defs.push(DefCell {
            name: Name::from(""),
            term: Rc::new(Naive::Typ),
            typ_: Rc::new(Naive::Typ)
          });
          let def = &defs.defs.get(def_link).unwrap();
          let term = &def.term;
          let typ = &def.typ_;
          let term_naive = term_to_naive(defs, done, naive_defs, idx, term);
          let typ_naive = term_to_naive(defs, done, naive_defs, idx, typ);
          naive_defs[idx] = DefCell {
            name: nam.clone(),
            term: term_naive,
            typ_: typ_naive
          };
          Rc::new(Naive::Ref(idx))
        },
      }
    },
    Term::All(_, uses, nam, link) => {
      let (dom, img) = &**link;
      let dom = term_to_naive(defs, done, naive_defs, rec_idx, dom);
      let img = term_to_naive(defs, done, naive_defs, rec_idx, img);
      let naive = Naive::All(*uses, nam.clone(), dom, img);
      Rc::new(naive)
    },
    Term::Slf(_, nam, link) => {
      let bod = &**link;
      let bod = term_to_naive(defs, done, naive_defs, rec_idx, bod);
      let naive = Naive::Slf(nam.clone(), bod);
      Rc::new(naive)
    },
    Term::Dat(_, link) => {
      let bod = &**link;
      let bod = term_to_naive(defs, done, naive_defs, rec_idx, bod);
      let naive = Naive::Dat(bod);
      Rc::new(naive)
    },
    Term::Cse(_, link) => {
      let bod = &**link;
      let bod = term_to_naive(defs, done, naive_defs, rec_idx, bod);
      let naive = Naive::Cse(bod);
      Rc::new(naive)
    },
    Term::Ann(_, link) => {
      let (typ, exp) = &**link;
      let typ = term_to_naive(defs, done, naive_defs, rec_idx, typ);
      let exp = term_to_naive(defs, done, naive_defs, rec_idx, exp);
      let naive = Naive::Ann(typ, exp);
      Rc::new(naive)
    },
    Term::Let(_, false, uses, nam, link) => {
      let (typ, exp, bod) = &**link;
      let typ = term_to_naive(defs, done, naive_defs, rec_idx, typ);
      let exp = term_to_naive(defs, done, naive_defs, rec_idx, exp);
      let bod = term_to_naive(defs, done, naive_defs, rec_idx, bod);
      let naive = Naive::Let(*uses, nam.clone(), typ, exp, bod);
      Rc::new(naive)
    },
    Term::Let(_, true, uses, nam, link) => {
      let (typ, exp, bod) = &**link;
      let typ = term_to_naive(defs, done, naive_defs, rec_idx, typ);
      let exp = term_to_naive(defs, done, naive_defs, rec_idx, exp);
      let bod = term_to_naive(defs, done, naive_defs, rec_idx, bod);
      let fix = Rc::new(Naive::Fix(nam.clone(), exp));
      let naive = Naive::Let(*uses, nam.clone(), typ, fix, bod);
      Rc::new(naive)
    },
    Term::Typ(_) => Rc::new(Naive::Typ),
    Term::LTy(_, lty) => Rc::new(Naive::LTy(*lty)),
    Term::Lit(_, lit) => Rc::new(Naive::Lit(lit.clone())),
    Term::Opr(_, opr) => Rc::new(Naive::Opr(*opr)),
  }
}

pub fn from_term_no_ref(term: &Term) -> Rc<Naive> {
  // Assumes there are no references
  term_to_naive(&Defs::new(), &mut BTreeMap::new(), &mut vec![], 0, term)
}

pub fn defs_to_naive(defs: &Defs) -> Vec<DefCell> {
  let mut done = BTreeMap::new();
  let mut naive_defs = aux_defs();
  for (nam, cid) in &defs.names {
    if !done.contains_key(cid) {
      let idx = naive_defs.len();
      done.insert(*cid, idx);
      naive_defs.push(DefCell {
        name: Name::from(""),
        term: Rc::new(Naive::Typ),
        typ_: Rc::new(Naive::Typ)
      });
      let def = &defs.defs.get(cid).unwrap();
      let term = &def.term;
      let typ = &def.typ_;
      let term_naive = term_to_naive(defs, &mut done, &mut naive_defs, idx, term);
      let typ_naive = term_to_naive(defs, &mut done, &mut naive_defs, idx, typ);
      naive_defs[idx] = DefCell {
        name: nam.clone(),
        term: term_naive,
        typ_: typ_naive
      };
    }
  }
  naive_defs
}

#[derive(Clone, Debug)]
pub struct DefCell {
  pub name: Name,
  pub term: Rc<Naive>,
  pub typ_: Rc<Naive>,
}

pub fn subst(bod: Rc<Naive>, idx: usize, val: Rc<Naive>) -> Rc<Naive> {
  match &*bod {
    Naive::Var(nam, jdx) => {
      if idx == *jdx {
        val
      }
      else if *jdx > idx {
        Rc::new(Naive::Var(nam.clone(), *jdx-1))
      }
      else {
        bod
      }
    }
    Naive::Lam(nam, bod) => {
      let bod = subst(bod.clone(), idx+1, val);
      Rc::new(Naive::Lam(nam.clone(), bod))
    }
    Naive::App(fun, arg) => {
      let fun = subst(fun.clone(), idx, val.clone());
      let arg = subst(arg.clone(), idx, val);
      Rc::new(Naive::App(fun, arg))
    }
    Naive::All(uses, nam, dom, img) => {
      let dom = subst(dom.clone(), idx, val.clone());
      let img = subst(img.clone(), idx+1, val);
      Rc::new(Naive::All(*uses, nam.clone(), dom, img))
    }
    Naive::Slf(nam, bod) => {
      let bod = subst(bod.clone(), idx+1, val);
      Rc::new(Naive::Slf(nam.clone(), bod))
    }
    Naive::Dat(bod) => {
      let bod = subst(bod.clone(), idx, val);
      Rc::new(Naive::Dat(bod))
    }
    Naive::Cse(bod) => {
      let bod = subst(bod.clone(), idx, val);
      Rc::new(Naive::Cse(bod))
    }
    Naive::Let(uses, nam, typ, exp, bod) => {
      let typ = subst(typ.clone(), idx, val.clone());
      let exp = subst(exp.clone(), idx, val.clone());
      let bod = subst(bod.clone(), idx+1, val);
      Rc::new(Naive::Let(*uses, nam.clone(), typ, exp, bod))
    }
    Naive::Fix(nam, bod) => {
      let bod = subst(bod.clone(), idx+1, val);
      Rc::new(Naive::Fix(nam.clone(), bod))
    }
    Naive::Ann(typ, exp) => {
      let typ = subst(typ.clone(), idx, val.clone());
      let exp = subst(exp.clone(), idx, val);
      Rc::new(Naive::Ann(typ, exp))
    }
    Naive::Unr(jdx, bod) => {
      let bod = subst(bod.clone(), idx, val);
      Rc::new(Naive::Unr(*jdx, bod))
    }
    _ => bod,
  }
}

pub fn reduce(
  globals: &Vec<DefCell>,
  top_node: Rc<Naive>
) -> Rc<Naive> {
  let mut node = top_node;
  let mut trail = vec![];
  loop {
    let next_node = {
      match &*node {
        Naive::App(fun, _) => {
          trail.push(node.clone());
          fun.clone()
        }
        Naive::Lam(_, bod) => {
          let len = trail.len();
          if len > 0 {
            let redex = trail[len-1].clone();
            let arg = match &*redex {
              Naive::App(_, arg) => {
                arg.clone()
              }
              // Must be a Cse node, in which case we are in whnf
              _ => break
            };
            trail.pop();
            subst(bod.clone(), 0, arg)
          }
          else {
            break
          }
        },
        Naive::Ref(idx) => {
          globals[*idx].term.clone()
        },
        Naive::Cse(bod) => {
          trail.push(node.clone());
          bod.clone()
        }
        Naive::Dat(bod) => {
          let len = trail.len();
          if len > 0 {
            let redex = trail[len-1].clone();
            match &*redex {
              Naive::Cse(_) => (),
              // Must be an App node, in which case we are in whnf
              _ => break,
            };
            trail.pop();
            bod.clone()
          }
          else {
            break
          }
        }
        Naive::Ann(_, exp) => {
          exp.clone()
        }
        Naive::Let(_, _, _, exp, bod) => {
          subst(bod.clone(), 0, exp.clone())
        }
        Naive::Fix(_, bod) => {
          subst(bod.clone(), 0, node.clone())
        }
        Naive::Lit(lit) => {
          let len = trail.len();
          if len > 0 {
            let redex = trail[len-1].clone();
            match &*redex {
              Naive::Cse(_) => (),
              _ => break,
            };
            trail.pop();
            match &lit.clone().expand() {
              None => break,
              Some(expand) => from_term_no_ref(expand)
            }
          }
          else {
            break
          }
        }
        Naive::Opr(opr) => {
          let len = trail.len();
          if len == 0 && opr.arity() == 0 {
            let res = opr.apply0();
            if let Some(res) = res {
              Rc::new(Naive::Lit(res))
            }
            else {
              break;
            }
          }
          else if len >= 1 && opr.arity() == 1 {
            let app = trail[len-1].clone();
            let arg = match &*app {
              Naive::App(_, arg) => arg.clone(),
              _ => break,
            };
            let arg = reduce(globals, arg);
            match &*arg {
              Naive::Lit(x) => {
                let res = opr.apply1(x);
                if let Some(res) = res {
                  trail.pop().unwrap();
                  Rc::new(Naive::Lit(res))
                }
                else {
                  break;
                }
              }
              _ => break,
            }
          }
          else if len >= 2 && opr.arity() == 2 {
            let app = trail[len-1].clone();
            let arg1 = match &*app {
              Naive::App(_, arg) => arg.clone(),
              _ => break,
            };
            let app = trail[len-2].clone();
            let arg2 = match &*app {
              Naive::App(_, arg) => arg.clone(),
              _ => break,
            };
            let arg1 = reduce(globals, arg1);
            let arg2 = reduce(globals, arg2);
            match (&*arg1, &*arg2) {
              (Naive::Lit(x), Naive::Lit(y)) => {
                let res = opr.apply2(x, y);
                if let Some(res) = res {
                  trail.pop();
                  trail.pop().unwrap();
                  Rc::new(Naive::Lit(res))
                }
                else {
                  break;
                }
              }
              _ => break,
            }
          }
          else if len >= 3 && opr.arity() == 3 {
            let app = trail[len-1].clone();
            let arg1 = match &*app {
              Naive::App(_, arg) => arg.clone(),
              _ => break,
            };
            let app = trail[len-2].clone();
            let arg2 = match &*app {
              Naive::App(_, arg) => arg.clone(),
              _ => break,
            };
            let app = trail[len-3].clone();
            let arg3 = match &*app {
              Naive::App(_, arg) => arg.clone(),
              _ => break,
            };
            let arg1 = reduce(globals, arg1);
            let arg2 = reduce(globals, arg2);
            let arg3 = reduce(globals, arg3);
            match (&*arg1, &*arg2, &*arg3) {
              (Naive::Lit(x), Naive::Lit(y), Naive::Lit(z)) => {
                let res = opr.apply3(x, y, z);
                if let Some(res) = res {
                  trail.pop();
                  trail.pop();
                  trail.pop().unwrap();
                  Rc::new(Naive::Lit(res))
                }
                else {
                  break;
                }
              }
              _ => break,
            }
          }
          else {
            break;
          }
        }
        Naive::Unr(_, exp) => {
          exp.clone()
        }
        _ => break,
      }
    };
    node = next_node;
  }
  while let Some(top) = trail.pop() {
    match &*top {
      Naive::App(_, arg) => {
        node = Rc::new(Naive::App(node, arg.clone()));
      }
      Naive::Cse(_) => {
        node = Rc::new(Naive::Cse(node));
      }
      _ => panic!("incorrect reduction"),
    }
  }
  node
}

pub fn hash_term(
  term: Rc<Naive>,
  dep: usize,
  ini: usize,
) -> String {
  let mut stack = vec![(dep, term)];
  let mut string = String::new();
  while let Some((dep, term)) = stack.pop() {
    match &*term {
      Naive::App(fun, arg) => {
        string.push('@');
        stack.push((dep, arg.clone()));
        stack.push((dep, fun.clone()));
      }
      Naive::Lam(_, bod) => {
        string.push('λ');
        stack.push((dep+1, bod.clone()));
      }
      Naive::Free(_, lvl) => string.push_str(&format!("#{}", lvl)),
      Naive::Var(_, idx) => {
        let lvl = dep-(*idx)-1;
        if lvl >= ini {
          string.push_str(&format!("^{}", idx));
        }
        else {
          string.push_str(&format!("#{}", lvl));
        }
      }
      Naive::Ref(idx) => {
        string.push_str(&format!("R{}", idx));
      },
      Naive::Typ => {
        string.push('*')
      },
      Naive::All(uses, _, dom, img) => {
        string.push_str(&format!("∀{}", uses));
        stack.push((dep+1, img.clone()));
        stack.push((dep, dom.clone()));
      },
      Naive::Slf(_, bod) => {
        string.push('$');
        stack.push((dep+1, bod.clone()));
      },
      Naive::Dat(bod) => {
        string.push('D');
        stack.push((dep, bod.clone()));
      },
      Naive::Cse(bod) => {
        string.push('C');
        stack.push((dep, bod.clone()));
      },
      Naive::Ann(_, exp) => {
        stack.push((dep, exp.clone()))
      },
      Naive::Let(_, _, _, exp, bod) => {
        string.push('L');
        stack.push((dep+1, bod.clone()));
        stack.push((dep, exp.clone()));
      },
      Naive::Fix(_, bod) => {
        string.push('μ');
        stack.push((dep+1, bod.clone()));
      },
      Naive::Lit(lit) => {
        string.push_str(&format!("I{}", lit))
      },
      Naive::LTy(lty) => {
        string.push_str(&format!("Y{}", lty))
      },
      Naive::Opr(opr) => {
        string.push_str(&format!("O{}", opr))
      },
      Naive::Unr(_, exp) => {
        stack.push((dep, exp.clone()))
      },
    }
  }
  string
}

pub fn equal(
  globals: &Vec<DefCell>,
  dep: usize,
  a: Rc<Naive>,
  b: Rc<Naive>,
) -> bool {
  let a = reduce(globals, a);
  let b = reduce(globals, b);
  let mut triples = vec![(a, b, dep)];
  let mut set = BTreeSet::new();
  while let Some((a, b, dep)) = triples.pop() {
    let hash_a = hash_term(a.clone(), dep, dep);
    let hash_b = hash_term(b.clone(), dep, dep);
    let pair = (hash_a, hash_b);
    let eq = pair.0 == pair.1 || set.contains(&pair) || set.contains(&pair);
    set.insert(pair);
    if !eq {
      match (&*a, &*b) {
        (Naive::Lam(_, a_bod), Naive::Lam(nam, b_bod)) => {
          let new_var = Rc::new(Naive::Free(nam.clone(), dep));
          let a_bod = subst(a_bod.clone(), 0, new_var.clone());
          let b_bod = subst(b_bod.clone(), 0, new_var.clone());
          let a_bod = reduce(globals, a_bod);
          let b_bod = reduce(globals, b_bod);
          triples.push((a_bod, b_bod, dep + 1));
        },
        (Naive::Slf(_, a_bod), Naive::Slf(nam, b_bod)) => {
          let new_var = Rc::new(Naive::Free(nam.clone(), dep));
          let a_bod = subst(a_bod.clone(), 0, new_var.clone());
          let b_bod = subst(b_bod.clone(), 0, new_var.clone());
          let a_bod = reduce(globals, a_bod);
          let b_bod = reduce(globals, b_bod);
          triples.push((a_bod, b_bod, dep + 1));
        },
        (Naive::Cse(a_bod), Naive::Cse(b_bod)) => {
          let a_bod = reduce(globals, a_bod.clone());
          let b_bod = reduce(globals, b_bod.clone());
          triples.push((a_bod, b_bod, dep));
        },
        (Naive::Dat(a_bod), Naive::Dat(b_bod)) => {
          let a_bod = reduce(globals, a_bod.clone());
          let b_bod = reduce(globals, b_bod.clone());
          triples.push((a_bod.clone(), b_bod.clone(), dep));
        },
        (Naive::All(a_uses, _, a_dom, a_img), Naive::All(b_uses, nam, b_dom, b_img)) => {
          if a_uses != b_uses {
            return false;
          }
          let new_var = Rc::new(Naive::Free(nam.clone(), dep));
          let a_dom = reduce(globals, a_dom.clone());
          let b_dom = reduce(globals, b_dom.clone());
          let a_img = subst(a_img.clone(), 0, new_var.clone());
          let b_img = subst(b_img.clone(), 0, new_var.clone());
          let a_img = reduce(globals, a_img);
          let b_img = reduce(globals, b_img);
          triples.push((a_dom, b_dom, dep));
          triples.push((a_img, b_img, dep + 1));
        },
        (Naive::App(a_fun, a_arg), Naive::App(b_fun, b_arg)) => {
          // No need to reduce the functions, since we assume the parent nodes are in whnf
          triples.push((a_fun.clone(), b_fun.clone(), dep));
          let a_arg = reduce(globals, a_arg.clone());
          let b_arg = reduce(globals, b_arg.clone());
          triples.push((a_arg, b_arg, dep));
        },
        _ => return false,
      }
    }
  }
  true
}

#[derive(Debug)]
pub enum CheckError {
  GenericError(String),
}

pub type Ctx = Vec<(Name, Uses, Rc<Naive>)>;

#[inline]
pub fn add_mul_ctx(uses: Uses, use_ctx: &mut Ctx, use_ctx2: Ctx) {
  #[allow(clippy::needless_range_loop)]
  for i in 0..use_ctx.len() {
    use_ctx[i].1 = (uses * use_ctx[i].1) + use_ctx2[i].1
  }
}

#[inline]
pub fn add_ctx(use_ctx: &mut Ctx, use_ctx2: Ctx) {
  #[allow(clippy::needless_range_loop)]
  for i in 0..use_ctx.len() {
    use_ctx[i].1 = use_ctx[i].1 + use_ctx2[i].1
  }
}

#[inline]
pub fn mul_ctx(uses: Uses, use_ctx: &mut Ctx) {
  #[allow(clippy::needless_range_loop)]
  for i in 0..use_ctx.len() {
    use_ctx[i].1 = use_ctx[i].1 * uses
  }
}

#[inline]
pub fn div_ctx(uses: Uses, use_ctx: &mut Ctx) -> Ctx {
  let mut rest = use_ctx.clone();
  #[allow(clippy::needless_range_loop)]
  for i in 0..use_ctx.len() {
    rest[i].1 = use_ctx[i].1 % uses;
    use_ctx[i].1 = use_ctx[i].1 / uses
  }
  rest
}

// The core typechecking function. It assumes typ is in whnf for performance,
// so make sure to call reduce on the type before checking
pub fn check(
  globals: &Vec<DefCell>,
  ctx: &mut Ctx,
  uses: Uses,
  term: Rc<Naive>,
  typ: Rc<Naive>,
) -> Result<(), CheckError> {
  let typ = reduce(globals, typ);
  match &*term {
    Naive::Lam(_, bod) => {
      match &*typ {
        Naive::All(lam_uses, all_nam, dom, img) => {
          let new_var = Rc::new(Naive::Free(all_nam.clone(), ctx.len()));
          let bod = subst(bod.clone(), 0, new_var.clone());
          let img = subst(img.clone(), 0, new_var);
          // Adjust the context multiplicity, add the argument to the context and check the body
          let rest_ctx = div_ctx(uses, ctx);
          ctx.push((all_nam.clone(), *lam_uses, dom.clone()));
          check(globals, ctx, Uses::Once, bod, img)?;
          // Check whether the rest 'contains' zero (i.e., zero is less than or
          // equal to the rest), otherwise the variable was not used enough
          let (_, rest, _) = ctx.last().unwrap();
          if !Uses::lte(Uses::None, *rest) {
            Err(CheckError::GenericError(format!("Variable not used enough")))
          }
          else {
            // Remove the argument from the context, readjust the context
            // multiplicity
            ctx.pop();
            add_mul_ctx(uses, ctx, rest_ctx);
            Ok(())
          }
        }
        _ => {
          Err(CheckError::GenericError(format!("Lambda not a function")))
        }
      }
    }
    Naive::Dat(bod) => {
      match &*typ {
        Naive::Slf(_, slf_bod) => {
          // The type of the body of the data must be the body of the self with term
          // substituted for its variable.
          let unrolled_slf = subst(slf_bod.clone(), 0, term.clone());
          check(globals, ctx, uses, bod.clone(), unrolled_slf)?;
          Ok(())
        }
        _ => {
          Err(CheckError::GenericError(format!("Data not a self type")))
        }
      }
    }
    Naive::Let(_, _, exp_typ, exp, bod) => {
      // Since we don't have a good function for substituting free variables
      // we are going to be expanding let expressions
      let ann_exp = Rc::new(Naive::Ann(exp_typ.clone(), exp.clone()));
      let bod = subst(bod.clone(), 0, ann_exp);
      check(globals, ctx, uses, bod, typ)
    }
    Naive::Fix(name, bod) => {
      let arg = Rc::new(Naive::Unr(ctx.len(), term.clone()));
      let bod = subst(bod.clone(), 0, arg);
      // This will remove all linear and affine variables, which cannot be used inside a fix
      let rest_ctx = div_ctx(Uses::Many, ctx);
      // Push the recursion to the context and check
      ctx.push((name.clone(), Uses::Many, typ.clone()));
      check(globals, ctx, uses, bod, typ)?;
      ctx.pop();
      // TODO: check error caused by the removal of linear/affine variables should have a special explanation
      add_ctx(ctx, rest_ctx);
      Ok(())
    }
    _ => {
      let depth = ctx.len();
      let detected_typ = infer(globals, ctx, uses, term.clone())?;
      if equal(globals, depth, typ.clone(), detected_typ.clone()) {
        Ok(())
      }
      else {
        Err(CheckError::GenericError(
          format!("Wrong types: expected {:?} found {:?}", detected_typ.clone(), typ.clone())))
      }
    }
  }
}

pub fn infer(
  globals: &Vec<DefCell>,
  ctx: &mut Ctx,
  uses: Uses,
  term: Rc<Naive>,
) -> Result<Rc<Naive>, CheckError> {
  match &*term {
    Naive::Var(_, _) => panic!("Free variable found"),
    Naive::Free(_, dep) => {
      let (_, ctx_uses, typ) = ctx.get_mut(*dep).ok_or_else(|| {
        CheckError::GenericError(format!("Unbound variable"))
      })?;
      let subtract_use = (*ctx_uses - uses).ok_or_else(|| {
        CheckError::GenericError(format!("Variable used too much"))
      })?;
      *ctx_uses = subtract_use;
      Ok(typ.clone())
    },
    Naive::Ref(idx) => {
      Ok(globals[*idx].typ_.clone())
    },
    Naive::App(fun, arg) => {
      let fun_typ = infer(globals, ctx, uses, fun.clone())?;
      match &*reduce(globals, fun_typ) {
        Naive::All(lam_uses, _, dom, img) => {
          check(globals, ctx, *lam_uses * uses, arg.clone(), dom.clone())?;
          let app_typ = subst(img.clone(), 0, arg.clone());
          Ok(app_typ)
        }
        _ => Err(CheckError::GenericError(
          format!("Tried to apply an expression which is not a function")
        )),
      }
    },
    Naive::Cse(exp) => {
      let exp_typ = infer(globals, ctx, uses, exp.clone())?;
      match &*reduce(globals, exp_typ.clone()) {
        Naive::Slf(_, bod) => {
          let cse_typ = subst(bod.clone(), 0, exp.clone());
          Ok(cse_typ)
        }
        Naive::LTy(lty) => {
          match lty_induction(globals, lty, exp.clone()) {
            None => Err(CheckError::GenericError(format!("{} is not an inductive literal", lty))),
            Some(ind) => Ok(ind),
          }
        }
        _ => Err(CheckError::GenericError(format!(
          "Tried to case match on an expression which is not an inductive datatype"
        ))),
      }
    }
    Naive::All(_, name, dom, img) => {
      let typ = Rc::new(Naive::Typ);
      check(globals, ctx, Uses::None, dom.clone(), typ.clone())?;
      let img = subst(img.clone(), 0, Rc::new(Naive::Free(name.clone(), ctx.len())));
      ctx.push((name.clone(), Uses::None, dom.clone()));
      check(globals, ctx, Uses::None, img, typ.clone())?;
      ctx.pop();
      Ok(typ)
    }
    Naive::Slf(name, bod) => {
      let typ = Rc::new(Naive::Typ);
      let bod = subst(bod.clone(), 0, Rc::new(Naive::Free(name.clone(), ctx.len())));
      ctx.push((name.clone(), Uses::None, term.clone()));
      check(globals, ctx, Uses::None, bod, typ.clone())?;
      ctx.pop();
      Ok(typ)
    }
    Naive::Let(_, _, exp_typ, exp, bod) => {
      let ann_exp = Rc::new(Naive::Ann(exp_typ.clone(), exp.clone()));
      let bod = subst(bod.clone(), 0, ann_exp);
      infer(globals, ctx, uses, bod)
    }
    Naive::Typ => {
      Ok(term.clone())
    }
    Naive::Ann(typ, exp) => {
      check(globals, ctx, uses, exp.clone(), typ.clone())?;
      Ok(typ.clone())
    }
    Naive::Lam(..) => Err(CheckError::GenericError(format!("Untyped lambda"))),
    Naive::Dat(..) => Err(CheckError::GenericError(format!("Untyped data"))),
    Naive::Fix(..) => Err(CheckError::GenericError(format!("Untyped fix"))),
    Naive::Lit(lit) => {
      let lty = infer_lit(lit);
      Ok(Rc::new(Naive::LTy(lty)))
    },
    Naive::LTy(_) => {
      let typ = Rc::new(Naive::Typ);
      Ok(typ)
    },
    Naive::Opr(opr) => {
      Ok(from_term_no_ref(&opr.type_of()))
    },
    Naive::Unr(idx, _) => {
      Ok(ctx[*idx].2.clone())
    }
  }
}

#[inline]
pub fn lty_induction(globals: &Vec<DefCell>, lty: &LitType, term: Rc<Naive>) -> Option<Rc<Naive>> {
  let idx = match lty {
    LitType::Nat => NAT_INDUCTION,
    LitType::Int => INT_INDUCTION,
    LitType::Bytes => BYTES_INDUCTION,
    LitType::Bits => BITS_INDUCTION,
    LitType::Text => TEXT_INDUCTION,
    LitType::Bool => BOOL_INDUCTION,
    _ => return None,
  };
  let induction = &globals[idx].term;
  match &**induction {
    Naive::Lam(_, bod) => Some(subst(bod.clone(), 0, term)),
    _ => panic!("Implementation of induction of literals incorrect"),
  }
}

#[inline]
pub fn infer_lit(lit: &Literal) -> LitType {
  match lit {
    Literal::Nat(_) => LitType::Nat,
    Literal::Int(_) => LitType::Int,
    Literal::Bytes(_) => LitType::Bytes,
    Literal::Bits(_) => LitType::Bits,
    Literal::Text(_) => LitType::Text,
    Literal::Char(_) => LitType::Char,
    Literal::Bool(_) => LitType::Bool,
    Literal::U8(_) => LitType::U8,
    Literal::U16(_) => LitType::U16,
    Literal::U32(_) => LitType::U32,
    Literal::U64(_) => LitType::U64,
    Literal::U128(_) => LitType::U128,
    Literal::I8(_) => LitType::I8,
    Literal::I16(_) => LitType::I16,
    Literal::I32(_) => LitType::I32,
    Literal::I64(_) => LitType::I64,
    Literal::I128(_) => LitType::I128,
  }
}

pub fn check_defs(
  globals: &Vec<DefCell>,
) -> Vec<(Name, Result<(), CheckError>)> {
  let mut result = vec![];
  for i in DEF_START..globals.len() {
    let DefCell { name, term, typ_ } = &globals[i];
    let mut ctx = vec![];
    result.push((
      name.clone(),
      check(globals, &mut ctx, Uses::Once, term.clone(), typ_.clone())
    ));
  }
  result
}
