use core::ptr::NonNull;

use crate::core::{
  dag::{
    clear_copies,
    free_dead_node,
    new_branch,
    new_leaf,
    new_single,
    replace_child,
    Branch,
    BranchTag,
    Leaf,
    LeafTag,
    ParentCell,
    Single,
    SingleTag,
    DAG,
  },
  dll::*,
  primop::{
    apply_bin_op,
    apply_una_op,
  },
};

// The core up-copy function.
pub fn upcopy(new_child: DAG, cc: ParentCell) {
  unsafe {
    match cc {
      ParentCell::Body(parent) => {
        let Single { var, parents: grandparents, .. } = *parent.as_ptr();
        let tag = &(*parent.as_ptr()).tag;
        let new_single = new_single(var, new_child, tag.clone());
        for grandparent in DLL::iter_option(grandparents) {
          upcopy(DAG::Single(new_single), *grandparent)
        }
      }
      ParentCell::Left(parent) => {
        let Branch { var, copy, right, parents: grandparents, .. } =
          *parent.as_ptr();
        let tag = &(*parent.as_ptr()).tag;
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).left = new_child;
          }
          None => {
            let new_branch = new_branch(var, new_child, right, tag.clone());
            (*parent.as_ptr()).copy = Some(new_branch);
            for grandparent in DLL::iter_option(grandparents) {
              upcopy(DAG::Branch(new_branch), *grandparent)
            }
          }
        }
      }
      ParentCell::Right(parent) => {
        let Branch { var, copy, left, parents: grandparents, .. } =
          *parent.as_ptr();
        let tag = &(*parent.as_ptr()).tag;
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).right = new_child;
          }
          None => {
            let new_branch = new_branch(var, left, new_child, tag.clone());
            (*parent.as_ptr()).copy = Some(new_branch);
            for grandparent in DLL::iter_option(grandparents) {
              upcopy(DAG::Branch(new_branch), *grandparent)
            }
          }
        }
      }
      ParentCell::Root => (),
    }
  }
}

// Contract a lambda redex, return the body.
pub fn reduce_lam(redex: NonNull<Branch>, lam: NonNull<Single>) -> DAG {
  unsafe {
    let Branch { right: arg, .. } = *redex.as_ptr();
    let Single { var, body, parents: lam_parents, .. } = *lam.as_ptr();
    let var = match var {
      Some(var) => var,
      None => return DAG::Branch(redex),
    };
    let Leaf { parents: var_parents, .. } = *var.as_ptr();
    let ans = if DLL::is_singleton(lam_parents) {
      replace_child(DAG::Leaf(var), arg);
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
            let Branch { var, left, right, .. } = *branch.as_ptr();
            let tag = &(*branch.as_ptr()).tag;
            let new_branch = new_branch(var, left, right, tag.clone());
            (*branch.as_ptr()).copy = Some(new_branch);
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
        .map_or((), |app| clear_copies(lam.as_ref(), &mut *app.as_ptr()));
      result
    };
    replace_child(DAG::Branch(redex), ans);
    free_dead_node(DAG::Branch(redex));
    ans
  }
}

// Reduce term to its weak head normal form
pub fn whnf(mut node: DAG) -> DAG {
  let mut trail = vec![];
  loop {
    match node {
      DAG::Branch(link) => unsafe {
        let Branch { left, tag, .. } = &*link.as_ptr();
        match tag {
          BranchTag::App => {
            trail.push(link);
            node = *left;
          }
          // TODO: Add the `Ann` and `Let` cases
          _ => break,
        }
      },
      DAG::Single(link) => unsafe {
        let Single { tag, .. } = &*link.as_ptr();
        match tag {
          SingleTag::Lam => {
            if let Some(app_link) = trail.pop() {
              node = reduce_lam(app_link, link);
            }
            else {
              break;
            }
          }
          // TODO: Add the `Fix` case.
          _ => break,
        }
      },
      DAG::Leaf(link) => unsafe {
        let Leaf { tag, .. } = &*link.as_ptr();
        match tag {
          // LeafTag::Ref(nam, def) =>
          LeafTag::Opr(opr) => {
            let len = trail.len();
            if len >= 1 && opr.arity() == 1 {
              let arg = whnf((*trail[len - 1].as_ptr()).right);
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
              let arg1 = whnf((*trail[len - 2].as_ptr()).right);
              let arg2 = whnf((*trail[len - 1].as_ptr()).right);
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
      _ => break,
    }
  }
  if trail.is_empty() {
    return node;
  }
  DAG::Branch(trail[0])
}

// Reduce term to its normal form
pub fn norm(mut top_node: DAG) -> DAG {
  top_node = whnf(top_node);
  let mut trail = vec![top_node];
  while let Some(node) = trail.pop() {
    match node {
      DAG::Branch(link) => unsafe {
        let branch = &mut *link.as_ptr();
        trail.push(whnf(branch.left));
        trail.push(whnf(branch.right));
      },
      DAG::Single(link) => unsafe {
        let single = &mut *link.as_ptr();
        trail.push(whnf(single.body));
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

  pub fn parse(
    i: &str,
  ) -> nom::IResult<Span, DAG, crate::parse::error::ParseError<Span>> {
    let (i, tree) = crate::parse::term::parse(i)?;
    let (i, _) = nom::character::complete::multispace0(i)?;
    let (i, _) = nom::combinator::eof(i)?;
    let dag = DAG::from_term(tree);
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
        Ok((_, dag)) => assert_eq!(format!("{}", norm(dag)), result),
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
    let one = "λ s z => (s z)";
    let two = "λ s z => s (s z)";
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
