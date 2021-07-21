use crate::{
  name::Name,
  machine::machine::*,
};

use sp_std::{
  vec::Vec,
  collections::{
    btree_set::BTreeSet,
  },
};

pub fn equal(
  globals: &Vec<DefCell>,
  fun_defs: &Vec<FunCell>,
  dep: usize,
  a: Link<Graph>,
  b: Link<Graph>,
) -> bool {
  let a = reduce(globals, fun_defs, a);
  let b = reduce(globals, fun_defs, b);
  let mut triples = vec![(a, b, dep)];
  let mut set = BTreeSet::new();
  while let Some((a, b, dep)) = triples.pop() {
    let hash_a = copy_u8_hash(&a.borrow());
    let hash_b = copy_u8_hash(&b.borrow());
    let eq =
      hash_a == hash_b || set.contains(&(hash_a, hash_b)) || set.contains(&(hash_b, hash_a));
    set.insert((hash_a, hash_b));
    if !eq {
      match (&*a.borrow(), &*b.borrow()) {
        (Graph::Lam(_, a_clos), Graph::Lam(_, b_clos)) => {
          let new_var = new_var(dep, Name::from(""));
          let Closure { idx: a_idx, env: a_env } = a_clos;
          let Closure { idx: b_idx, env: b_env } = b_clos;
          let a_bod = build_graph(true, globals, fun_defs, *a_idx, a_env, new_var.clone());
          let b_bod = build_graph(true, globals, fun_defs, *b_idx, b_env, new_var);
          triples.push((a_bod, b_bod, dep + 1));
        },
        (Graph::Slf(_, a_clos), Graph::Slf(_, b_clos)) => {
          let new_var = new_var(dep, Name::from(""));
          let Closure { idx: a_idx, env: a_env } = a_clos;
          let Closure { idx: b_idx, env: b_env } = b_clos;
          let a_bod = build_graph(true, globals, fun_defs, *a_idx, a_env, new_var.clone());
          let b_bod = build_graph(true, globals, fun_defs, *b_idx, b_env, new_var);
          triples.push((a_bod, b_bod, dep + 1));
        },
        (Graph::Cse(_, a_bod), Graph::Cse(_, b_bod)) => {
          let a_bod = reduce(globals, fun_defs, a_bod.clone());
          let b_bod = reduce(globals, fun_defs, b_bod.clone());
          triples.push((a_bod, b_bod, dep));
        },
        (Graph::Dat(_, a_bod), Graph::Dat(_, b_bod)) => {
          let a_bod = reduce(globals, fun_defs, a_bod.clone());
          let b_bod = reduce(globals, fun_defs, b_bod.clone());
          triples.push((a_bod.clone(), b_bod.clone(), dep));
        },
        (Graph::All(_, a_uses, a_dom, a_clos), Graph::All(_, b_uses, b_dom, b_clos)) => {
          if a_uses != b_uses {
            return false;
          }
          let new_var = new_var(dep, Name::from(""));
          let Closure { idx: a_idx, env: a_env } = a_clos;
          let Closure { idx: b_idx, env: b_env } = b_clos;
          let a_dom = reduce(globals, fun_defs, a_dom.clone());
          let b_dom = reduce(globals, fun_defs, b_dom.clone());
          let a_img = build_graph(true, globals, fun_defs, *a_idx, a_env, new_var.clone());
          let b_img = build_graph(true, globals, fun_defs, *b_idx, b_env, new_var);
          triples.push((a_dom, b_dom, dep));
          triples.push((a_img, b_img, dep + 1));
        },
        (Graph::App(_, a_fun, a_arg), Graph::App(_, b_fun, b_arg)) => {
          // No need to reduce the functions, since we assume the parent nodes are in whnf
          triples.push((a_fun.clone(), b_fun.clone(), dep));
          let a_arg = reduce(globals, fun_defs, a_arg.clone());
          let b_arg = reduce(globals, fun_defs, b_arg.clone());
          triples.push((a_arg, b_arg, dep));
        },
        _ => return false,
      }
    }
  }
  true
}
