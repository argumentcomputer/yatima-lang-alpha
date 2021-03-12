use core::ptr::NonNull;

use crate::{
  core::{
    dag::*,
    upcopy::*,
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
pub fn subst(lam: NonNull<Single>, arg: DAGPtr) -> DAGPtr {
  unsafe {
    let Single { var, body, parents: lam_parents, .. } = *lam.as_ptr();
    let var = match var {
      Some(var) => var,
      None => panic!("Misuse of substitution"),
    };
    let Var { parents: var_parents, .. } = *var.as_ptr();
    let ans = if DLL::is_singleton(lam_parents) {
      replace_child(DAGPtr::Var(var), arg);
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
          DAGPtr::Single(single) => {
            let Single { var, body, .. } = *single.as_ptr();
            let tag = &(*single.as_ptr()).tag;
            input = body;
            spine.push((var, tag));
          }
          DAGPtr::Branch(branch) => {
            let Branch { left, right, .. } = *branch.as_ptr();
            let new_branch = upcopy_branch(branch, left, right);
            top_branch = Some(branch);
            for parent in DLL::iter_option(var_parents) {
              upcopy(arg, *parent);
            }
            result = DAGPtr::Branch(new_branch);
            break;
          }
          // Otherwise it must be `var`, since `var` necessarily appears inside
          // `body`
          _ => break,
        }
      }
      while let Some((var, tag)) = spine.pop() {
        result = DAGPtr::Single(upcopy_single(var, result, tag.clone()));
      }
      top_branch
        .map_or((), |mut app| clear_copies(lam.as_ref(), app.as_mut()));
      result
    };
    ans
  }
}

// Contract a lambda redex, return the body.
pub fn reduce_lam(redex: NonNull<Branch>, lam: NonNull<Single>) -> DAGPtr {
  let Branch { right: arg, .. } = unsafe { redex.as_ref() };
  let top_node = subst(lam, *arg);
  replace_child(DAGPtr::Branch(redex), top_node);
  free_dead_node(DAGPtr::Branch(redex));
  top_node
}

impl DAG {
  // Reduce term to its weak head normal form
  pub fn whnf(&mut self, defs: &HashMap<Link, Def>) {
    let mut node = self.head;
    let mut trail = vec![];
    loop {
      match node {
        DAGPtr::Branch(link) => unsafe {
          let Branch { left, tag, .. } = *link.as_ptr();
          match tag {
            BranchTag::App => {
              trail.push(link);
              node = left;
            }
            _ => break,
          }
        },
        
        DAGPtr::Single(link) => unsafe {
          let Single { tag, var, .. } = *link.as_ptr();
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
              let mut body = DAG::new((*link.as_ptr()).body);
              body.whnf(defs);
              match body.head {
                DAGPtr::Single(body_link) => {
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
              let body = (*link.as_ptr()).body;
              match var {
                None => panic!("Malformed Fix"),
                Some(var) => {
                  let Var { parents: var_parents, depth: var_depth, .. } = *var.as_ptr();
                  let var_name = &(*var.as_ptr()).name;
                  replace_child(node, body);
                  if !var_parents.is_none() {
                    let new_var = alloc_val(Var {name: var_name.clone(), depth: var_depth, parents: None});
                    let mut input = body;
                    let mut top_branch = None;
                    let mut result = DAGPtr::Var(new_var);
                    let mut spine = vec![];
                    loop {
                      match input {
                        DAGPtr::Single(single) => {
                          let Single { var, body, .. } = *single.as_ptr();
                          let tag = &(*single.as_ptr()).tag;
                          input = body;
                          spine.push((var, tag));
                        }
                        DAGPtr::Branch(branch) => {
                          let Branch { left, right, .. } = *branch.as_ptr();
                          let new_branch = upcopy_branch(branch, left, right);
                          top_branch = Some(branch);
                          for parent in DLL::iter_option(var_parents) {
                            upcopy(DAGPtr::Var(new_var), *parent);
                          }
                          result = DAGPtr::Branch(new_branch);
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
                      result = DAGPtr::Single(upcopy_single(var, result, tag.clone()));
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
                    *fix_ref.as_ptr() = DLL::singleton(ParentPtr::Body(new_fix));
                    add_to_parents(result, fix_ref);
                    replace_child(DAGPtr::Var(var), DAGPtr::Single(new_fix));
                  }
                  free_dead_node(node);
                  node = body;
                },
              };
            }
            _ => break,
          }
        },
        
        DAGPtr::Leaf(link) => unsafe {
          let Leaf { tag, .. } = link.as_ref();
          match tag {
            LeafTag::Ref(nam, def_link, _anon_link) => {
              if let Some(def) = defs.get(def_link) {
                // Using Fix:
                let new_var = alloc_val(Var {name: nam.clone(), depth: 0, parents: None});
                let new_node = DAG::from_subterm(&def.clone().term, 0, &Vector::new(), Vector::unit(DAGPtr::Var(new_var)), None).head;
                let fix_ref = alloc_uninit();
                let new_fix = alloc_val(Single {
                  tag: SingleTag::Fix,
                  var: Some(new_var),
                  body: new_node,
                  body_ref: fix_ref,
                  parents: None
                });
                *fix_ref.as_ptr() = DLL::singleton(ParentPtr::Body(new_fix));
                add_to_parents(new_node, fix_ref);
                replace_child(node, DAGPtr::Single(new_fix));
                free_dead_node(node);
                node = DAGPtr::Single(new_fix);
        
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
                let mut arg = DAG::new((*trail[len - 1].as_ptr()).right);
                arg.whnf(defs);
                match arg.head {
                  DAGPtr::Leaf(x) => {
                    let x = (*x.as_ptr()).tag.clone();
                    match x {
                      LeafTag::Lit(x) => {
                        let res = apply_una_op(*opr, x);
                        if let Some(res) = res {
                          trail.pop();
                          node = DAGPtr::Leaf(alloc_val(Leaf{ tag: LeafTag::Lit(res), parents: None }));
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
                  _ => break,
                }
              }
              else if len >= 2 && opr.arity() == 2 {
                let mut arg1 = DAG::new((*trail[len - 2].as_ptr()).right);
                let mut arg2 = DAG::new((*trail[len - 1].as_ptr()).right);
                arg1.whnf(defs);
                arg2.whnf(defs);
                match (arg1.head, arg2.head) {
                  (DAGPtr::Leaf(x), DAGPtr::Leaf(y)) => {
                    let x = (*x.as_ptr()).tag.clone();
                    let y = (*y.as_ptr()).tag.clone();
                    match (x, y) {
                      (LeafTag::Lit(x), LeafTag::Lit(y)) => {
                        let res = apply_bin_op(*opr, y, x);
                        if let Some(res) = res {
                          trail.pop();
                          trail.pop();
                          node = DAGPtr::Leaf(alloc_val(Leaf{ tag: LeafTag::Lit(res), parents: None }));
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
        _ => break,
      }
    }
    if trail.is_empty() {
      self.head = node;
    }
    else {
      self.head = DAGPtr::Branch(trail[0]);
    }
  }
  
  // Reduce term to its normal form
  pub fn norm(&mut self, defs: &HashMap<Link, Def>) {
    self.whnf(defs);
    let top_node = self.head;
    let mut trail = vec![top_node];
    while let Some(node) = trail.pop() {
      match node {
        DAGPtr::Branch(link) => unsafe {
          let branch = link.as_ptr();
          let mut left = DAG::new((*branch).left);
          let mut right = DAG::new((*branch).right);
          left.whnf(defs);
          right.whnf(defs);
          trail.push(left.head);
          trail.push(right.head);
        },
        DAGPtr::Single(link) => unsafe {
          let single = link.as_ptr();
          let mut body = DAG::new((*single).body);
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
  use super::{
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
          dag.norm(&HashMap::new());
          assert_eq!(format!("{}", dag), result)
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
