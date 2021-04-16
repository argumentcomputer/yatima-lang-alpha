use core::ptr::NonNull;

use crate::{
  core::{
    dag::*,
    dll::*,
    primop::{
      apply_bin_op,
      apply_una_op,
    },
    upcopy::*,
  },
  term::{
    Def,
    Link,
  },
};

use im::{
  HashMap,
  Vector,
};

enum Single {
  Lam(Var),
  Slf(Var),
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
#[must_use]
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
        let All { var: old_var, uses, dom, img, .. } = unsafe { link.as_ref() };
        let Var { nam, dep, .. } = old_var;
        let new_all =
          alloc_all(nam.clone(), *dep, None, *uses, *dom, *img, None);
        unsafe {
          (*link.as_ptr()).copy = Some(new_all);
          let ptr: *mut Var = &mut (*new_all.as_ptr()).var;
          for parent in DLL::iter_option(old_var.parents) {
            upcopy(DAGPtr::Var(NonNull::new(ptr).unwrap()), *parent)
          }
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
        let Let { var: old_var, uses, typ, exp, bod, .. } =
          unsafe { link.as_ref() };
        let Var { nam, dep, .. } = old_var;
        let new_let =
          alloc_let(nam.clone(), *dep, None, *uses, *typ, *exp, *bod, None);
        unsafe {
          (*link.as_ptr()).copy = Some(new_let);
          let ptr: *mut Var = &mut (*new_let.as_ptr()).var;
          for parent in DLL::iter_option(old_var.parents) {
            upcopy(DAGPtr::Var(NonNull::new(ptr).unwrap()), *parent)
          }
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
        let new_lam = alloc_lam(nam, dep, None, result, None);
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
        let new_slf = alloc_slf(nam, dep, None, result, None);
        let ptr: *mut Parents = unsafe { &mut (*new_slf.as_ptr()).bod_ref };
        add_to_parents(result, NonNull::new(ptr).unwrap());
        let ptr: *mut Var = unsafe { &mut (*new_slf.as_ptr()).var };
        for parent in DLL::iter_option(var_parents) {
          upcopy(DAGPtr::Var(NonNull::new(ptr).unwrap()), *parent)
        }
        result = DAGPtr::Slf(new_slf);
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
        let All { var, dom, dom_ref, img, img_ref, .. } = &mut *link.as_ptr();
        add_to_parents(*dom, NonNull::new(dom_ref).unwrap());
        add_to_parents(*img, NonNull::new(img_ref).unwrap());
        for var_parent in DLL::iter_option(var.parents) {
          clean_up(var_parent);
        }
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
        let Let { var, typ, typ_ref, exp, exp_ref, bod, bod_ref, .. } =
          &mut *link.as_ptr();
        add_to_parents(*typ, NonNull::new(typ_ref).unwrap());
        add_to_parents(*exp, NonNull::new(exp_ref).unwrap());
        add_to_parents(*bod, NonNull::new(bod_ref).unwrap());
        for var_parent in DLL::iter_option(var.parents) {
          clean_up(var_parent);
        }
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
#[must_use]
pub fn reduce_lam(redex: NonNull<App>, lam: NonNull<Lam>) -> DAGPtr {
  let App { arg, .. } = unsafe { redex.as_ref() };
  let Lam { var, bod, parents, .. } = unsafe { &mut *lam.as_ptr() };
  let top_node = if DLL::is_singleton(*parents) {
    replace_child(DAGPtr::Var(NonNull::new(var).unwrap()), *arg);
    // We have to read `body` again because `lam`'s body could be mutated
    // through `replace_child`
    unsafe { (*lam.as_ptr()).bod }
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

impl DAG {
  // Reduce term to its weak head normal form
  pub fn whnf(&mut self, defs: &HashMap<Link, Def>) {
    let mut node = self.head;
    let mut trail = vec![];
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
              let Dat { bod: single_body, .. } = unsafe { body_link.as_ref() };
              replace_child(node, *single_body);
              free_dead_node(node);
              node = *single_body;
            }
            _ => break,
          }
        }
        DAGPtr::Let(link) => {
          let Let { var, exp, bod, .. } = unsafe { &mut *link.as_ptr() };
          replace_child(node, *bod);
          replace_child(DAGPtr::Var(NonNull::new(var).unwrap()), *exp);
          free_dead_node(node);
          node = *bod;
          break;
        }
        DAGPtr::Ref(link) => {
          let Ref { nam, exp, ast, parents: ref_parents, .. } =
            unsafe { &mut *link.as_ptr() };
          if let Some(def) = defs.get(exp) {
            let parents = *ref_parents;
            *ref_parents = None;
            node = DAG::from_subterm(
              &def.term,
              0,
              Vector::new(),
              parents,
              Some((nam.clone(), *exp, *ast)),
            );
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
          if len >= 1 && opr.arity() == 1 {
            let mut arg = unsafe { Self::new((*trail[len - 1].as_ptr()).arg) };
            arg.whnf(defs);
            match arg.head {
              DAGPtr::Lit(link) => {
                let x = unsafe { (*link.as_ptr()).lit.clone() };
                let res = apply_una_op(opr, x);
                if let Some(res) = res {
                  trail.pop();
                  node =
                    DAGPtr::Lit(alloc_val(Lit { lit: res, parents: None }));
                  replace_child(arg.head, node);
                  free_dead_node(arg.head);
                }
                else {
                  break;
                }
              }
              _ => break,
            }
          }
          else if len >= 2 && opr.arity() == 2 {
            let mut arg1 = unsafe { Self::new((*trail[len - 2].as_ptr()).arg) };
            let mut arg2 = unsafe { Self::new((*trail[len - 1].as_ptr()).arg) };
            arg1.whnf(defs);
            arg2.whnf(defs);
            match (arg1.head, arg2.head) {
              (DAGPtr::Lit(x_link), DAGPtr::Lit(y_link)) => {
                let x = unsafe { (*x_link.as_ptr()).lit.clone() };
                let y = unsafe { (*y_link.as_ptr()).lit.clone() };
                let res = apply_bin_op(opr, y, x);
                if let Some(res) = res {
                  trail.pop();
                  trail.pop();
                  node =
                    DAGPtr::Lit(alloc_val(Lit { lit: res, parents: None }));
                  replace_child(arg1.head, node);
                  free_dead_node(arg1.head);
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
  pub fn norm(&mut self, defs: &HashMap<Link, Def>) {
    self.whnf(defs);
    let top_node = self.head;
    let mut trail = vec![top_node];
    while let Some(node) = trail.pop() {
      match node {
        DAGPtr::App(link) => unsafe {
          let app = link.as_ptr();
          let mut fun = Self::new((*app).fun);
          let mut arg = Self::new((*app).arg);
          fun.whnf(defs);
          arg.whnf(defs);
          trail.push(fun.head);
          trail.push(arg.head);
        },
        DAGPtr::All(link) => unsafe {
          let all = link.as_ptr();
          let mut dom = Self::new((*all).dom);
          let mut img = Self::new((*all).img);
          dom.whnf(defs);
          img.whnf(defs);
          trail.push(dom.head);
          trail.push(img.head);
        },
        DAGPtr::Lam(link) => unsafe {
          let lam = link.as_ptr();
          let mut body = Self::new((*lam).bod);
          body.whnf(defs);
          trail.push(body.head);
        },
        DAGPtr::Slf(link) => unsafe {
          let slf = link.as_ptr();
          let mut body = Self::new((*slf).bod);
          body.whnf(defs);
          trail.push(body.head);
        },
        DAGPtr::Cse(link) => unsafe {
          let cse = link.as_ptr();
          let mut body = Self::new((*cse).bod);
          body.whnf(defs);
          trail.push(body.head);
        },
        DAGPtr::Dat(link) => unsafe {
          let dat = link.as_ptr();
          let mut body = Self::new((*dat).bod);
          body.whnf(defs);
          trail.push(body.head);
        },
        _ => (),
      }
    }
  }
}

#[cfg(test)]
mod test {
  use super::DAG;
  use hashexpr::span::Span;
  use im::HashMap;

  pub fn parse(
    i: &str,
  ) -> nom::IResult<Span, DAG, crate::parse::error::ParseError<Span>> {
    let (i, tree) = crate::parse::term::parse(i)?;
    let (i, _) = nom::character::complete::multispace0(i)?;
    let (i, _) = nom::combinator::eof(i)?;
    let dag = DAG::from_term(&tree);
    Ok((i, dag))
  }

  #[test]
  pub fn parse_test() {
    fn parse_assert(input: &str) {
      match parse(input) {
        Ok((_, dag)) => assert_eq!(format!("{}", dag), input),
        Err(_) => panic!("Did not parse."),
      }
    }
    parse_assert("\u{3bb} x => x");
    parse_assert("\u{3bb} x y => x y");
    parse_assert("\u{3bb} y => (\u{3bb} x => x) y");
    parse_assert("\u{3bb} y => (\u{3bb} z => z z) ((\u{3bb} x => x) y)");
  }

  fn norm_assert(input: &str, result: &str) {
    match parse(input) {
      Ok((_, mut dag)) => {
        dag.norm(&HashMap::new());
        assert_eq!(format!("{}", dag), result)
      }
      Err(_) => panic!("Did not parse."),
    }
  }

  #[test]
  pub fn reduce_test_ann() {
    norm_assert("(Type :: Type)", "Type");
    norm_assert("((\u{3bb} x => x) #Natural :: Type)", "#Natural");
    norm_assert("(\u{3bb} A => A :: \u{2200} (A: Type) -> Type)", "\u{3bb} A => A");
    norm_assert("(\u{3bb} A x => A :: \u{2200} (A: Type) (x: A) -> Type)", "\u{3bb} A x => A");
    norm_assert(
      "(\u{2200} (A: Type) (x: A) -> Type :: Type)",
      "\u{2200} (A: Type) (x: A) -> Type",
    );
    norm_assert("Type :: \u{2200} (A: Type) (x: A) -> Type", "Type");
  }

  #[test]
  pub fn reduce_test_app() {
    norm_assert(
      "Type (\u{2200} (A: Type) (x: A) -> Type)",
      "Type (\u{2200} (A: Type) (x: A) -> Type)",
    );
    norm_assert(
      "(\u{2200} (A: Type) (x: A) -> Type) Type",
      "(\u{2200} (A: Type) (x: A) -> Type) Type",
    )
  }

  #[test]
  pub fn reduce_test_all() {
    norm_assert(
      "\u{2200} (f: \u{2200} (A: Type) (x: A) -> Type) -> Type",
      "\u{2200} (f: \u{2200} (A: Type) (x: A) -> Type) -> Type",
    );
    norm_assert(
      "\u{2200} (f: Type) -> \u{2200} (A: Type) (x: A) -> Type",
      "\u{2200} (f: Type) (A: Type) (x: A) -> Type",
    );
  }

  #[test]
  pub fn reduce_test_let() {
    norm_assert("let f: Type = Type; f", "Type");
    norm_assert("let f: \u{2200} (A: Type) (x: A) -> A = \u{3bb} A x => x; f", "\u{3bb} A x => x");
    norm_assert(
      "let f: Type = \u{2200} (A: Type) (x: A) -> A; f",
      "\u{2200} (A: Type) (x: A) -> A",
    );
    norm_assert(
      "let f: Type = Type; \u{2200} (A: Type) (x: A) -> A",
      "\u{2200} (A: Type) (x: A) -> A",
    );
  }

  #[test]
  pub fn reduce_test() {
    // Already normalized
    norm_assert("\u{3bb} x => x", "\u{3bb} x => x");
    norm_assert("\u{3bb} x y => x y", "\u{3bb} x y => x y");
    // Not normalized cases
    norm_assert("\u{3bb} y => (\u{3bb} x => x) y", "\u{3bb} y => y");
    norm_assert("\u{3bb} y => (\u{3bb} z => z z) ((\u{3bb} x => x) y)", "\u{3bb} y => y y");
    // // Church arithmetic
    let zero = "\u{3bb} s z => z";
    let one = "\u{3bb} s z => (s z)";
    let two = "\u{3bb} s z => s (s z)";
    let three = "\u{3bb} s z => s (s (s z))";
    let four = "\u{3bb} s z => s (s (s (s z)))";
    let seven = "\u{3bb} s z => s (s (s (s (s (s (s z))))))";
    let add = "\u{3bb} m n s z => m s (n s z)";
    let is_three = format!("(({}) ({}) {})", add, zero, three);
    let is_three2 = format!("(({}) ({}) {})", add, one, two);
    let is_seven = format!("(({}) ({}) {})", add, four, three);
    norm_assert(&is_three, three);
    norm_assert(&is_three2, three);
    norm_assert(&is_seven, seven);
    let id = "\u{3bb} x => x";
    norm_assert(
      &format!("({three}) (({three}) ({id})) ({id})", id = id, three = three),
      id,
    );
    let trm_str =
      &format!("(({n}) (({m}) ({id})) {id})", n = three, m = three, id = id,);
    println!("{}", trm_str);
    let (_, trm) = parse(trm_str).unwrap();
    println!("{:?}", DAG::to_term(&trm));
    // assert_eq!(true, false);
    norm_assert(trm_str, id)
  }
}
