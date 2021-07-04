use core::ptr::NonNull;

use crate::{
  dag::*,
  defs::Defs,
  dll::*,
  upcopy::*,
};

use crate::prim::{
  Op,
  nat::NatOp,
};

use std::collections::HashMap;

use sp_std::mem;

enum Single {
  Lam(Var),
  Slf(Var),
  Fix(Var),
  Dat,
  Cse,
}

enum Branch {
  All(NonNull<All>),
  App(NonNull<App>),
  Ann(NonNull<Ann>),
  Let(NonNull<Let>),
}

// Substitute a variable
#[inline]
pub fn subst(bod: DAGPtr, var: &Var, arg: DAGPtr, fix: bool) -> DAGPtr {
  let mut input = bod;
  let mut top_branch = None;
  let mut result = arg;
  let mut spine = vec![];
  loop {
    match input {
      DAGPtr::Lam(link) => {
        let Lam { var, bod, .. } = unsafe { link.as_ref() };
        input = *bod;
        spine.push(Single::Lam(var.clone()));
      }
      DAGPtr::Slf(link) => {
        let Slf { var, bod, .. } = unsafe { link.as_ref() };
        input = *bod;
        spine.push(Single::Slf(var.clone()));
      }
      DAGPtr::Fix(link) => {
        let Fix { var, bod, .. } = unsafe { link.as_ref() };
        input = *bod;
        spine.push(Single::Fix(var.clone()));
      }
      DAGPtr::Dat(link) => {
        let Dat { bod, .. } = unsafe { link.as_ref() };
        input = *bod;
        spine.push(Single::Dat);
      }
      DAGPtr::Cse(link) => {
        let Cse { bod, .. } = unsafe { link.as_ref() };
        input = *bod;
        spine.push(Single::Cse);
      }
      DAGPtr::App(link) => {
        let App { fun, arg: app_arg, .. } = unsafe { link.as_ref() };
        let new_app = alloc_app(*fun, *app_arg, None);
        unsafe {
          (*link.as_ptr()).copy = Some(new_app);
        }
        top_branch = Some(Branch::App(link));
        for parent in DLL::iter_option(var.parents) {
          upcopy(arg, *parent);
        }
        result = DAGPtr::App(new_app);
        break;
      }
      DAGPtr::All(link) => {
        let All { uses, dom, img, .. } = unsafe { link.as_ref() };
        let new_all = alloc_all(*uses, *dom, *img, None);
        unsafe {
          (*link.as_ptr()).copy = Some(new_all);
        }
        top_branch = Some(Branch::All(link));
        for parent in DLL::iter_option(var.parents) {
          upcopy(arg, *parent);
        }
        result = DAGPtr::All(new_all);
        break;
      }
      DAGPtr::Ann(link) => {
        let Ann { typ, exp, .. } = unsafe { link.as_ref() };
        let new_ann = alloc_ann(*typ, *exp, None);
        unsafe {
          (*link.as_ptr()).copy = Some(new_ann);
        }
        top_branch = Some(Branch::Ann(link));
        for parent in DLL::iter_option(var.parents) {
          upcopy(arg, *parent);
        }
        result = DAGPtr::Ann(new_ann);
        break;
      }
      DAGPtr::Let(link) => {
        let Let { uses, typ, exp, bod, .. } = unsafe { link.as_ref() };
        let new_let = alloc_let(*uses, *typ, *exp, *bod, None);
        unsafe {
          (*link.as_ptr()).copy = Some(new_let);
        }
        top_branch = Some(Branch::Let(link));
        for parent in DLL::iter_option(var.parents) {
          upcopy(arg, *parent);
        }
        result = DAGPtr::Let(new_let);
        break;
      }
      // Otherwise it must be `var`, since `var` necessarily appears inside
      // `body`
      _ => break,
    }
  }
  if fix && top_branch.is_none() && spine.is_empty() {
    panic!("Infinite loop found");
  }
  while let Some(single) = spine.pop() {
    match single {
      Single::Lam(var) => {
        let Var { nam, dep, parents: var_parents, .. } = var;
        let new_lam = alloc_lam(nam, dep, result, None);
        let ptr: *mut Parents = unsafe { &mut (*new_lam.as_ptr()).bod_ref };
        add_to_parents(result, NonNull::new(ptr).unwrap());
        let ptr: *mut Var = unsafe { &mut (*new_lam.as_ptr()).var };
        for parent in DLL::iter_option(var_parents) {
          upcopy(DAGPtr::Var(NonNull::new(ptr).unwrap()), *parent)
        }
        result = DAGPtr::Lam(new_lam);
      }
      Single::Slf(var) => {
        let Var { nam, dep, parents: var_parents, .. } = var;
        let new_slf = alloc_slf(nam, dep, result, None);
        let ptr: *mut Parents = unsafe { &mut (*new_slf.as_ptr()).bod_ref };
        add_to_parents(result, NonNull::new(ptr).unwrap());
        let ptr: *mut Var = unsafe { &mut (*new_slf.as_ptr()).var };
        for parent in DLL::iter_option(var_parents) {
          upcopy(DAGPtr::Var(NonNull::new(ptr).unwrap()), *parent)
        }
        result = DAGPtr::Slf(new_slf);
      }
      Single::Fix(var) => {
        let Var { nam, dep, parents: var_parents, .. } = var;
        let new_fix = alloc_fix(nam, dep, result, None);
        let ptr: *mut Parents = unsafe { &mut (*new_fix.as_ptr()).bod_ref };
        add_to_parents(result, NonNull::new(ptr).unwrap());
        let ptr: *mut Var = unsafe { &mut (*new_fix.as_ptr()).var };
        for parent in DLL::iter_option(var_parents) {
          upcopy(DAGPtr::Var(NonNull::new(ptr).unwrap()), *parent)
        }
        result = DAGPtr::Fix(new_fix);
      }
      Single::Dat => {
        let new_dat = alloc_dat(result, None);
        let ptr: *mut Parents = unsafe { &mut (*new_dat.as_ptr()).bod_ref };
        add_to_parents(result, NonNull::new(ptr).unwrap());
        result = DAGPtr::Dat(new_dat);
      }
      Single::Cse => {
        let new_cse = alloc_cse(result, None);
        let ptr: *mut Parents = unsafe { &mut (*new_cse.as_ptr()).bod_ref };
        add_to_parents(result, NonNull::new(ptr).unwrap());
        result = DAGPtr::Cse(new_cse);
      }
    }
  }
  // If the top branch is non-null, then clear the copies and fix the uplinks
  if let Some(top_branch) = top_branch {
    match top_branch {
      Branch::App(link) => unsafe {
        let top_app = &mut *link.as_ptr();
        let link = top_app.copy.unwrap();
        top_app.copy = None;
        let App { fun, fun_ref, arg, arg_ref, .. } = &mut *link.as_ptr();
        add_to_parents(*fun, NonNull::new(fun_ref).unwrap());
        add_to_parents(*arg, NonNull::new(arg_ref).unwrap());
      },
      Branch::All(link) => unsafe {
        let top_all = &mut *link.as_ptr();
        let link = top_all.copy.unwrap();
        top_all.copy = None;
        let All { dom, dom_ref, img, img_ref, .. } = &mut *link.as_ptr();
        add_to_parents(*dom, NonNull::new(dom_ref).unwrap());
        add_to_parents(DAGPtr::Lam(*img), NonNull::new(img_ref).unwrap());
      },
      Branch::Ann(link) => unsafe {
        let top_ann = &mut *link.as_ptr();
        let link = top_ann.copy.unwrap();
        top_ann.copy = None;
        let Ann { typ, typ_ref, exp, exp_ref, .. } = &mut *link.as_ptr();
        add_to_parents(*typ, NonNull::new(typ_ref).unwrap());
        add_to_parents(*exp, NonNull::new(exp_ref).unwrap());
      },
      Branch::Let(link) => unsafe {
        let top_let = &mut *link.as_ptr();
        let link = top_let.copy.unwrap();
        top_let.copy = None;
        let Let { typ, typ_ref, exp, exp_ref, bod, bod_ref, .. } =
          &mut *link.as_ptr();
        add_to_parents(*typ, NonNull::new(typ_ref).unwrap());
        add_to_parents(*exp, NonNull::new(exp_ref).unwrap());
        add_to_parents(DAGPtr::Lam(*bod), NonNull::new(bod_ref).unwrap());
      },
    }
    for parent in DLL::iter_option(var.parents) {
      clean_up(parent);
    }
    let mut spine = bod;
    loop {
      match spine {
        DAGPtr::Lam(link) => unsafe {
          let Lam { var, bod, .. } = &mut *link.as_ptr();
          for parent in DLL::iter_option(var.parents) {
            clean_up(parent);
          }
          spine = *bod;
        },
        DAGPtr::Slf(link) => unsafe {
          let Slf { var, bod, .. } = &mut *link.as_ptr();
          for parent in DLL::iter_option(var.parents) {
            clean_up(parent);
          }
          spine = *bod;
        },
        DAGPtr::Fix(link) => unsafe {
          let Fix { var, bod, .. } = &mut *link.as_ptr();
          for parent in DLL::iter_option(var.parents) {
            clean_up(parent);
          }
          spine = *bod;
        },
        DAGPtr::Dat(link) => unsafe {
          spine = link.as_ref().bod;
        },
        DAGPtr::Cse(link) => unsafe {
          spine = link.as_ref().bod;
        },
        _ => break,
      }
    }
  }
  result
}

// Contract a lambda redex, return the body.
#[inline]
pub fn reduce_lam(redex: NonNull<App>, lam: NonNull<Lam>) -> DAGPtr {
  let App { arg, .. } = unsafe { redex.as_ref() };
  let Lam { var, bod, parents, .. } = unsafe { &mut *lam.as_ptr() };
  let top_node = if DLL::is_singleton(*parents) {
    replace_child(DAGPtr::Var(NonNull::new(var).unwrap()), *arg);
    *bod
  }
  else if var.parents.is_none() {
    *bod
  }
  else {
    subst(*bod, var, *arg, false)
  };
  replace_child(DAGPtr::App(redex), top_node);
  free_dead_node(DAGPtr::App(redex));
  top_node
}

// Contract a let redex, return the body.
#[inline]
pub fn reduce_let(redex: NonNull<Let>) -> DAGPtr {
  let Let { bod: lam, exp: arg, .. } = unsafe { redex.as_ref() };
  let Lam { var, bod, parents, .. } = unsafe { &mut *lam.as_ptr() };
  let top_node = if DLL::is_singleton(*parents) {
    replace_child(DAGPtr::Var(NonNull::new(var).unwrap()), *arg);
    *bod
  }
  else if var.parents.is_none() {
    *bod
  }
  else {
    subst(*bod, var, *arg, false)
  };
  replace_child(DAGPtr::Let(redex), top_node);
  free_dead_node(DAGPtr::Let(redex));
  top_node
}

pub fn print_trail(trail: &Vec<NonNull<App>>) -> Vec<String> {
  let mut res: Vec<String> = vec![];
  for link in trail {
    let App { arg, .. } = unsafe { link.as_ref() };
    res.push(format!("{}", arg));
  }
  res
}

pub fn simplify_opr(opr1: Op, node: DAGPtr) -> Option<DAGPtr> {
  match node {
    DAGPtr::App(link) => {
      let App { fun, arg, .. } = unsafe { &*link.as_ptr() };
      let opr2 = match fun {
        DAGPtr::Opr(link) => unsafe {
          (*link.as_ptr()).opr
        },
        _ => return None,
      };
      match (opr1, opr2) {
        (Op::Nat(NatOp::Pre), Op::Nat(NatOp::Suc)) => Some(*arg),
        // TODO
        _ => None
      }
    }
    DAGPtr::Lit(link) => {
      // TODO
      None
    }
    _ => None,
  }
}

impl DAG {
  // Reduce term to its weak head normal form
  pub fn whnf(&mut self, defs: &Defs) {
    let mut node = self.head;
    let mut trail: Vec<NonNull<App>> = vec![];
    loop {
      match node {
        DAGPtr::App(link) => {
          let App { fun, .. } = unsafe { link.as_ref() };
          trail.push(link);
          node = *fun;
        }
        DAGPtr::Lam(link) => {
          if let Some(app_link) = trail.pop() {
            node = reduce_lam(app_link, link);
          }
          else {
            break;
          }
        }
        DAGPtr::Ann(link) => {
          let Ann { exp, .. } = unsafe { link.as_ref() };
          replace_child(node, *exp);
          free_dead_node(node);
          node = *exp;
        }
        DAGPtr::Cse(link) => {
          let mut body = unsafe { DAG::new((*link.as_ptr()).bod) };
          body.whnf(defs);
          match body.head {
            DAGPtr::Dat(body_link) => {
              let bod = unsafe { body_link.as_ref().bod };
              replace_child(node, bod);
              free_dead_node(node);
              node = bod;
            }
            DAGPtr::Lit(link) => {
              let Lit { lit, parents, .. } = unsafe { link.as_ref() };
              match &lit.clone().expand() {
                None => break,
                Some(expand) => {
                  let expand = DAG::from_term_inner(
                    expand,
                    0,
                    HashMap::new(),
                    *parents,
                    None,
                  );
                  replace_child(node, expand);
                  free_dead_node(node);
                  node = expand;
                }
              }
            }
            _ => break,
          }
        }
        DAGPtr::Let(link) => {
          node = reduce_let(link);
        }
        DAGPtr::Fix(link) => unsafe {
          let Fix { var, bod, .. } = &mut *link.as_ptr();
          replace_child(node, *bod);
          if !var.parents.is_none() {
            let new_fix =
              alloc_fix(var.nam.clone(), 0, mem::zeroed(), None).as_mut();
            let result = subst(
              *bod,
              var,
              DAGPtr::Var(NonNull::new_unchecked(&mut new_fix.var)),
              true,
            );
            new_fix.bod = result;
            add_to_parents(
              result,
              NonNull::new_unchecked(&mut new_fix.bod_ref),
            );
            replace_child(
              DAGPtr::Var(NonNull::new(var).unwrap()),
              DAGPtr::Fix(NonNull::new_unchecked(new_fix)),
            );
          }
          free_dead_node(node);
          node = *bod;
        },
        DAGPtr::Ref(link) => {
          let Ref { nam, exp, ast, parents: ref_parents, .. } =
            unsafe { &mut *link.as_ptr() };
          if let Some(def) = defs.defs.get(exp) {
            let parents = *ref_parents;
            *ref_parents = None;
            let ref_node = node;
            node = DAG::from_ref(&def, nam.clone(), *exp, *ast, parents);
            free_dead_node(ref_node);
            for parent in DLL::iter_option(parents) {
              install_child(parent, node);
            }
          }
          else {
            panic!("undefined runtime reference: {}, {}", nam, exp);
          }
        }
        DAGPtr::Opr(link) => {
          let opr = unsafe { (*link.as_ptr()).opr };
          let len = trail.len();
          if opr.arity() == 0 {
            let res = opr.apply0();
            if let Some(res) = res {
              let new_node =
                DAGPtr::Lit(alloc_val(Lit { lit: res, parents: None }));
              replace_child(node, new_node);
              free_dead_node(node);
              node = new_node;
              continue;
            }
            else {
              break;
            }
          }
          if len == 0 {
            break;
          }
          let mut arg1 = unsafe { DAG::new((*trail[len - 1].as_ptr()).arg) };
          arg1.whnf(defs);
          if opr.arity() == 1 {
            match arg1.head {
              DAGPtr::Lit(link) => {
                let x = unsafe { &(*link.as_ptr()).lit };
                let res = opr.apply1(x);
                if let Some(res) = res {
                  let top = DAGPtr::App(trail.pop().unwrap());
                  let new_node =
                    DAGPtr::Lit(alloc_val(Lit { lit: res, parents: None }));
                  replace_child(top, new_node);
                  free_dead_node(top);
                  node = new_node;
                  continue;
                }
                else {
                  break;
                }
              }
              _ => (),
            }
          }
          else if len >= 2 && opr.arity() == 2 {
            let mut arg2 = unsafe { DAG::new((*trail[len - 2].as_ptr()).arg) };
            arg2.whnf(defs);
            match (arg1.head, arg2.head) {
              (DAGPtr::Lit(x_link), DAGPtr::Lit(y_link)) => {
                let x = unsafe { &(*x_link.as_ptr()).lit };
                let y = unsafe { &(*y_link.as_ptr()).lit };
                let res = opr.apply2(x, y);
                if let Some(res) = res {
                  trail.pop();
                  let top = DAGPtr::App(trail.pop().unwrap());
                  let new_node =
                    DAGPtr::Lit(alloc_val(Lit { lit: res, parents: None }));
                  replace_child(top, new_node);
                  free_dead_node(top);
                  node = new_node;
                  continue;
                }
                else {
                  break;
                }
              }
              _ => (),
            }
          }
          else if len >= 3 && opr.arity() == 3 {
            let mut arg2 = unsafe { DAG::new((*trail[len - 2].as_ptr()).arg) };
            let mut arg3 = unsafe { DAG::new((*trail[len - 3].as_ptr()).arg) };
            arg2.whnf(defs);
            arg3.whnf(defs);
            match (arg1.head, arg2.head, arg3.head) {
              (
                DAGPtr::Lit(x_link),
                DAGPtr::Lit(y_link),
                DAGPtr::Lit(z_link),
              ) => {
                let x = unsafe { &(*x_link.as_ptr()).lit };
                let y = unsafe { &(*y_link.as_ptr()).lit };
                let z = unsafe { &(*z_link.as_ptr()).lit };
                let res = opr.apply3(x, y, z);
                if let Some(res) = res {
                  trail.pop();
                  trail.pop();
                  let top = DAGPtr::App(trail.pop().unwrap());
                  let new_node =
                    DAGPtr::Lit(alloc_val(Lit { lit: res, parents: None }));
                  replace_child(top, new_node);
                  free_dead_node(top);
                  node = new_node;
                  continue;
                }
                else {
                  break;
                }
              }
              _ => (),
            }
          }
          let maybe_new_node = simplify_opr(opr, arg1.head);
          match maybe_new_node {
            Some(new_node) => {
              let top = DAGPtr::App(trail.pop().unwrap());
              replace_child(top, new_node);
              free_dead_node(top);
              node = new_node;
            },
            None => break,
          }
        }
        _ => break,
      }
    }
    if trail.is_empty() {
      self.head = node;
    }
    else {
      self.head = DAGPtr::App(trail[0]);
    }
  }

  // Reduce term to its normal form
  pub fn norm(&mut self, defs: &Defs) {
    self.whnf(defs);
    let mut trail = vec![self.head];
    while let Some(node) = trail.pop() {
      match node {
        DAGPtr::App(link) => unsafe {
          let app = link.as_ptr();
          let mut fun = DAG::new((*app).fun);
          let mut arg = DAG::new((*app).arg);
          fun.whnf(defs);
          arg.whnf(defs);
          trail.push(fun.head);
          trail.push(arg.head);
        },
        DAGPtr::All(link) => unsafe {
          let all = link.as_ptr();
          let mut dom = DAG::new((*all).dom);
          let mut img = DAG::new(DAGPtr::Lam((*all).img));
          dom.whnf(defs);
          img.whnf(defs);
          trail.push(dom.head);
          trail.push(img.head);
        },
        DAGPtr::Lam(link) => unsafe {
          let lam = link.as_ptr();
          let mut body = DAG::new((*lam).bod);
          body.whnf(defs);
          trail.push(body.head);
        },
        DAGPtr::Slf(link) => unsafe {
          let slf = link.as_ptr();
          let mut body = DAG::new((*slf).bod);
          body.whnf(defs);
          trail.push(body.head);
        },
        DAGPtr::Cse(link) => unsafe {
          let cse = link.as_ptr();
          let mut body = DAG::new((*cse).bod);
          body.whnf(defs);
          trail.push(body.head);
        },
        DAGPtr::Dat(link) => unsafe {
          let dat = link.as_ptr();
          let mut body = DAG::new((*dat).bod);
          body.whnf(defs);
          trail.push(body.head);
        },
        _ => (),
      }
    }
  }
}

#[cfg(test)]
pub mod test {
  use super::DAG;
  use crate::{
    defs::Defs,
    parse::{
      package,
      span::Span,
      term::input_cid,
    },
  };

  pub fn parse(
    i: &str,
  ) -> nom::IResult<Span, DAG, crate::parse::error::ParseError<Span>> {
    let (i, tree) = crate::parse::term::parse(i, Defs::new())?;
    let (i, _) = nom::character::complete::multispace0(i)?;
    let (i, _) = nom::combinator::eof(i)?;
    let dag = DAG::from_term(&tree);
    Ok((i, dag))
  }
  pub fn parse_defs(
    i: &str,
  ) -> nom::IResult<Span, Defs, crate::parse::error::ParseError<Span>> {
    let (i, (defs, _)) =
      package::parse_defs(input_cid(i), Defs::new())(Span::new(i))?;
    Ok((i, defs))
  }

  #[test]
  pub fn parse_test() {
    fn parse_assert(input: &str) {
      match parse(&input) {
        Ok((_, dag)) => assert_eq!(format!("{}", dag), input),
        Err(_) => panic!("Did not parse."),
      }
    }
    parse_assert("λ x => x");
    parse_assert("λ x y => x y");
    parse_assert("λ y => (λ x => x) y");
    parse_assert("λ y => (λ z => z z) ((λ x => x) y)");
  }

  fn norm_assert(input: &str, result: &str) {
    match parse(&input) {
      Ok((_, mut dag)) => {
        dag.norm(&Defs::new());
        assert_eq!(format!("{}", dag), result)
      }
      Err(_) => panic!("Did not parse."),
    }
  }
  fn norm_assert_defs(input: &str, result: &str, defs: Defs) {
    match parse(&input) {
      Ok((_, mut dag)) => {
        dag.norm(&defs);
        assert_eq!(format!("{}", dag), result)
      }
      Err(_) => panic!("Did not parse."),
    }
  }

  #[test]
  pub fn reduce_test_ann() {
    norm_assert("(Type :: Type)", "Type");
    norm_assert("((λ x => x) #Nat :: Type)", "#Nat");
    norm_assert("(λ A => A :: ∀ (A: Type) -> Type)", "λ A => A");
    norm_assert("(λ A x => A :: ∀ (A: Type) (x: A) -> Type)", "λ A x => A");
    norm_assert(
      "(∀ (A: Type) (x: A) -> Type :: Type)",
      "∀ (A: Type) (x: A) -> Type",
    );
    norm_assert("Type :: ∀ (A: Type) (x: A) -> Type", "Type");
  }

  #[test]
  pub fn reduce_test_app() {
    norm_assert(
      "Type (∀ (A: Type) (x: A) -> Type)",
      "Type (∀ (A: Type) (x: A) -> Type)",
    );
    norm_assert(
      "(∀ (A: Type) (x: A) -> Type) Type",
      "(∀ (A: Type) (x: A) -> Type) Type",
    )
  }

  #[test]
  pub fn reduce_test_all() {
    norm_assert(
      "∀ (f: ∀ (A: Type) (x: A) -> Type) -> Type",
      "∀ (f: ∀ (A: Type) (x: A) -> Type) -> Type",
    );
    norm_assert(
      "∀ (f: Type) -> ∀ (A: Type) (x: A) -> Type",
      "∀ (f: Type) (A: Type) (x: A) -> Type",
    );
  }

  #[test]
  pub fn reduce_test_let() {
    norm_assert("let f: Type = Type; f", "Type");
    norm_assert("let f: ∀ (A: Type) (x: A) -> A = λ A x => x; f", "λ A x => x");
    norm_assert(
      "let f: Type = ∀ (A: Type) (x: A) -> A; f",
      "∀ (A: Type) (x: A) -> A",
    );
    norm_assert(
      "let f: Type = Type; ∀ (A: Type) (x: A) -> A",
      "∀ (A: Type) (x: A) -> A",
    );
  }

  #[test]
  pub fn reduce_test() {
    // Already normalized
    norm_assert("λ x => x", "λ x => x");
    norm_assert("λ x y => x y", "λ x y => x y");
    // Not normalized cases
    norm_assert("λ y => (λ x => x) y", "λ y => y");
    norm_assert("λ y => (λ z => z z) ((λ x => x) y)", "λ y => y y");
    // // Church arithmetic
    let zero = "λ s z => z";
    let one = "λ s z => (s z)";
    let two = "λ s z => s (s z)";
    let three = "λ s z => s (s (s z))";
    let four = "λ s z => s (s (s (s z)))";
    let seven = "λ s z => s (s (s (s (s (s (s z))))))";
    let add = "λ m n s z => m s (n s z)";
    let is_three = format!("(({}) ({}) {})", add, zero, three);
    let is_three2 = format!("(({}) ({}) {})", add, one, two);
    let is_seven = format!("(({}) ({}) {})", add, four, three);
    norm_assert(&is_three, three);
    norm_assert(&is_three2, three);
    norm_assert(&is_seven, seven);
    let id = "λ x => x";
    norm_assert(
      &format!("({three}) (({three}) ({id})) ({id})", id = id, three = three),
      id,
    );
    let trm_str =
      &format!("(({n}) (({m}) ({id})) {id})", n = three, m = three, id = id);
    println!("{}", trm_str);
    let (_, trm) = parse(trm_str).unwrap();
    println!("{:?}", DAG::to_term(&trm, true));
    // assert_eq!(true, false);
    norm_assert(trm_str, id);
  }
}
