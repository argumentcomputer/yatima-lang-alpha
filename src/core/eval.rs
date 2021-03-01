use core::ptr::NonNull;

use crate::{
  core::{
    dag::*,
    dll::*,
    primop::{
      apply_bin_op,
      apply_una_op,
    },
  },
  term::{
    // Term,
    Def,
    Link,
  },
};

use im::{
  Vector,
  HashMap,
};

// Substitute a variable
pub fn subst(lam: NonNull<Single>, arg: DAG) -> DAG {
  unsafe {
    let Single { var, body, parents: lam_parents, .. } = *lam.as_ptr();
    let var = match var {
      Some(var) => var,
      None => panic!("Misuse of substitution"),
    };
    let Var { parents: var_parents, .. } = *var.as_ptr();
    let ans = if DLL::is_singleton(lam_parents) {
      replace_child(DAG::Var(var), arg);
      // We have to read `body` again because `lam`'s body could be mutated
      // through `replace_child`
      (*lam.as_ptr()).body
    }
    else if var_parents.is_none() {
      body
    }
    else {
      let mut input = body;
      let mut top_branch = None;
      let mut result = arg;
      let mut spine = vec![];
      loop {
        match input {
          DAG::Single(single) => {
            let Single { var, body, .. } = *single.as_ptr();
            let tag = &(*single.as_ptr()).tag;
            input = body;
            spine.push((var, tag));
          }
          DAG::Branch(branch) => {
            let Branch { left, right, .. } = *branch.as_ptr();
            let new_branch = new_branch(branch, left, right);
            top_branch = Some(branch);
            for parent in DLL::iter_option(var_parents) {
              upcopy(arg, *parent);
            }
            result = DAG::Branch(new_branch);
            break;
          }
          // Otherwise it must be `var`, since `var` necessarily appears inside
          // `body`
          _ => break,
        }
      }
      while let Some((var, tag)) = spine.pop() {
        result = DAG::Single(new_single(var, result, tag.clone()));
      }
      top_branch
        .map_or((), |mut app| clear_copies(lam.as_ref(), app.as_mut()));
      result
    };
    ans
  }
}

// Contract a lambda redex, return the body.
pub fn reduce_lam(redex: NonNull<Branch>, lam: NonNull<Single>) -> DAG {
  let Branch { right: arg, .. } = unsafe { redex.as_ref() };
  let top_node = subst(lam, *arg);
  replace_child(DAG::Branch(redex), top_node);
  free_dead_node(DAG::Branch(redex));
  top_node
}

// Reduce term to its weak head normal form
pub fn whnf(defs: &HashMap<Link, Def>, dag: &mut DAG) -> DAG {
  let mut node = *dag;
  let mut trail = vec![];
  loop {
    match node {
      DAG::Branch(link) => unsafe {
        let Branch { left, tag, .. } = *link.as_ptr();
        match tag {
          BranchTag::App => {
            trail.push(link);
            node = left;
          }
          _ => break,
        }
      },
      DAG::Single(link) => unsafe {
        let Single { tag, mut body, var, .. } = *link.as_ptr();
        match tag {
          SingleTag::Lam => {
            if let Some(app_link) = trail.pop() {
              node = reduce_lam(app_link, link);
            }
            else {
              break;
            }
          }
          SingleTag::Cse => {
            whnf(defs, &mut body);
            match body {
              DAG::Single(body_link) => {
                let Single { tag: single_tag, body: single_body, .. } = *body_link.as_ptr();
                if single_tag == SingleTag::Dat {
                  replace_child(node, single_body);
                  free_dead_node(node);
                  node = single_body;
                }
                else {
                  break
                }
              },
              _ => break,
            }
          },
          SingleTag::Fix => {
            match var {
              None => panic!("Malformed Fix"),
              Some(var) => {
                let Var { parents: var_parents, depth: var_depth, .. } = *var.as_ptr();
                let var_name = &(*var.as_ptr()).name;
                replace_child(node, body);
                if !var_parents.is_none() {
                  let new_var = new_var(var_name.clone(), var_depth);
                  let mut input = body;
                  let mut top_branch = None;
                  let mut result = DAG::Var(new_var);
                  let mut spine = vec![];
                  loop {
                    match input {
                      DAG::Single(single) => {
                        let Single { var, body, .. } = *single.as_ptr();
                        let tag = &(*single.as_ptr()).tag;
                        input = body;
                        spine.push((var, tag));
                      }
                      DAG::Branch(branch) => {
                        let Branch { left, right, .. } = *branch.as_ptr();
                        let new_branch = new_branch(branch, left, right);
                        top_branch = Some(branch);
                        for parent in DLL::iter_option(var_parents) {
                          upcopy(DAG::Var(new_var), *parent);
                        }
                        result = DAG::Branch(new_branch);
                        break;
                      }
                      // Otherwise it must be `var`, since `var` necessarily appears inside
                      // `body`
                      _ => break,
                    }
                  }
                  if top_branch.is_none() && spine.is_empty() {
                    panic!("Infinite loop found");
                  }
                  while let Some((var, tag)) = spine.pop() {
                    result = DAG::Single(new_single(var, result, tag.clone()));
                  }
                  top_branch
                    .map_or((), |mut app| clear_copies(link.as_ref(), app.as_mut()));

                  // Create a new fix node with the result of the copy
                  let fix_ref = alloc_uninit();
                  let new_fix = alloc_val(Single {
                    tag: SingleTag::Fix,
                    var: Some(new_var),
                    body: result,
                    body_ref: fix_ref,
                    parents: None
                  });
                  *fix_ref.as_ptr() = DLL::singleton(ParentCell::Body(new_fix));
                  add_to_parents(result, fix_ref);
                  replace_child(DAG::Var(var), DAG::Single(new_fix));
                }
                free_dead_node(node);
                node = body;
              },
            };
          }
          _ => break,
        }
      },
      DAG::Leaf(link) => unsafe {
        let Leaf { tag, .. } = link.as_ref();
        match tag {
          LeafTag::Ref(nam, def_link, _anon_link) => {
            if let Some(def) = defs.get(def_link) {
              // Using Fix:
              let new_var = new_var(nam.clone(), 0);
              let new_node = DAG::from_subterm(None, 0, &def.clone().term, Vector::unit(DAG::Var(new_var)), None);
              let fix_ref = alloc_uninit();
              let new_fix = alloc_val(Single {
                tag: SingleTag::Fix,
                var: Some(new_var),
                body: new_node,
                body_ref: fix_ref,
                parents: None
              });
              *fix_ref.as_ptr() = DLL::singleton(ParentCell::Body(new_fix));
              add_to_parents(new_node, fix_ref);
              replace_child(node, DAG::Single(new_fix));
              free_dead_node(node);
              node = DAG::Single(new_fix);
              
              // Without using Fix:
              // let new_ref = &Term::Ref(None, nam.clone(), *def_link, *anon_link);
              // let new_ref = DAG::from_subterm(0, new_ref, Vector::new(), None);
              // let new_node = DAG::from_subterm(0, &def.clone().term, Vector::unit(new_ref), None);
              // replace_child(node, new_node);
              // free_dead_node(node);
              // node = new_node;

            }
            else {
              panic!("undefined runtime reference: {}, {}", nam, def_link);
            }
          }
          LeafTag::Opr(opr) => {
            let len = trail.len();
            if len >= 1 && opr.arity() == 1 {
              let arg = whnf(defs, &mut (*trail[len - 1].as_ptr()).right);
              match arg {
                DAG::Leaf(x) => {
                  let x = (*x.as_ptr()).tag.clone();
                  match x {
                    LeafTag::Lit(x) => {
                      let res = apply_una_op(*opr, x);
                      if let Some(res) = res {
                        trail.pop();
                        node = DAG::Leaf(new_leaf(LeafTag::Lit(res)));
                        replace_child(arg, node);
                        free_dead_node(arg);
                      }
                      else {
                        break;
                      }
                    }

                    _ => break,
                  }
                }
                _ => break,
              }
            }
            else if len >= 2 && opr.arity() == 2 {
              let arg1 = whnf(defs, &mut (*trail[len - 2].as_ptr()).right);
              let arg2 = whnf(defs, &mut (*trail[len - 1].as_ptr()).right);
              match (arg1, arg2) {
                (DAG::Leaf(x), DAG::Leaf(y)) => {
                  let x = (*x.as_ptr()).tag.clone();
                  let y = (*y.as_ptr()).tag.clone();
                  match (x, y) {
                    (LeafTag::Lit(x), LeafTag::Lit(y)) => {
                      let res = apply_bin_op(*opr, y, x);
                      if let Some(res) = res {
                        trail.pop();
                        trail.pop();
                        node = DAG::Leaf(new_leaf(LeafTag::Lit(res)));
                        replace_child(arg1, node);
                        free_dead_node(arg1);
                      }
                      else {
                        break;
                      }
                    }
                    _ => break,
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
      },

      // TODO: All primitive operations
      // DAG::Opr(link) => unsafe {
      //         // TODO: (#cst (Nat 256) 0d1)
      //         //(DAG::App(x), DAG::Lit(y)) => {
      //         //
      //         //}
      //         else {
      //           break;
      //         }
      //       }
      //       _ => break,
      //     }
      //   }
      //   break;
      // },
      _ => break,
    }
  }
  if trail.is_empty() {
    *dag = node;
  }
  else {
    *dag = DAG::Branch(trail[0]);
  }
  *dag
}

// Reduce term to its normal form
pub fn norm(defs: &HashMap<Link, Def>, dag: &mut DAG) -> DAG {
  let top_node = whnf(defs, dag);
  let mut trail = vec![top_node];
  while let Some(node) = trail.pop() {
    match node {
      DAG::Branch(mut link) => unsafe {
        let branch = link.as_mut();
        trail.push(whnf(defs, &mut branch.left));
        trail.push(whnf(defs, &mut branch.right));
      },
      DAG::Single(mut link) => unsafe {
        let single = link.as_mut();
        trail.push(whnf(defs, &mut single.body));
      },
      _ => (),
    }
  }
  top_node
}

#[cfg(test)]
mod test {
  use super::{
    norm,
    DAG,
  };
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
  pub fn parser() {
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

  #[test]
  pub fn reducer() {
    fn norm_assert(input: &str, result: &str) {
      match parse(&input) {
        Ok((_, mut dag)) => {
          assert_eq!(format!("{}", norm(&HashMap::new(), &mut dag)), result)
        }
        Err(_) => panic!("Did not parse."),
      }
    }
    // Already normalized
    norm_assert("λ x => x", "λ x => x");
    norm_assert("λ x y => x y", "λ x y => x y");
    // Not normalized cases
    norm_assert("λ y => (λ x => x) y", "λ y => y");
    norm_assert("λ y => (λ z => z z) ((λ x => x) y)", "λ y => y y");
    // // Church arithmetic
    let zero = "λ s z => z";
    let three = "λ s z => s (s (s z))";
    let four = "λ s z => s (s (s (s z)))";
    let seven = "λ s z => s (s (s (s (s (s (s z))))))";
    let add = "λ m n s z => m s (n s z)";
    let is_three = format!("(({}) ({}) {})", add, zero, three);
    let is_seven = format!("(({}) ({}) {})", add, four, three);
    norm_assert(&is_three, three);
    norm_assert(&is_seven, seven);
    let id = "λ x => x";
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
