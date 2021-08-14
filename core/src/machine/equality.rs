use crate::{
  name::Name,
  machine::machine::*,
  literal::LitType,
};

use alloc::string::String;
use sp_std::{
  mem,
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
    let hash_a = hash_graph(globals, fun_defs, a.clone(), dep, dep);
    let hash_b = hash_graph(globals, fun_defs, b.clone(), dep, dep);
    let pair = (hash_a, hash_b);
    let eq = pair.0 == pair.1 || set.contains(&pair) || set.contains(&pair);
    set.insert(pair);
    if !eq {
      match (&*a.borrow(), &*b.borrow()) {
        (Graph::Lam(a_clos), Graph::Lam(b_clos)) => {
          let new_var = new_link(Graph::Var(dep, Name::from("")));
          let Closure { idx: a_idx, env: a_env } = a_clos;
          let Closure { idx: b_idx, env: b_env } = b_clos;
          let a_bod = build_graph(true, globals, fun_defs, *a_idx, a_env, new_var.clone());
          let b_bod = build_graph(true, globals, fun_defs, *b_idx, b_env, new_var);
          triples.push((a_bod, b_bod, dep + 1));
        },
        (Graph::Slf(a_clos), Graph::Slf(b_clos)) => {
          let new_var = new_link(Graph::Var(dep, Name::from("")));
          let Closure { idx: a_idx, env: a_env } = a_clos;
          let Closure { idx: b_idx, env: b_env } = b_clos;
          let a_bod = build_graph(true, globals, fun_defs, *a_idx, a_env, new_var.clone());
          let b_bod = build_graph(true, globals, fun_defs, *b_idx, b_env, new_var);
          triples.push((a_bod, b_bod, dep + 1));
        },
        (Graph::Cse(a_bod), Graph::Cse(b_bod)) => {
          let a_bod = reduce(globals, fun_defs, a_bod.clone());
          let b_bod = reduce(globals, fun_defs, b_bod.clone());
          triples.push((a_bod, b_bod, dep));
        },
        (Graph::Dat(a_bod), Graph::Dat(b_bod)) => {
          let a_bod = reduce(globals, fun_defs, a_bod.clone());
          let b_bod = reduce(globals, fun_defs, b_bod.clone());
          triples.push((a_bod.clone(), b_bod.clone(), dep));
        },
        (Graph::All(a_uses, a_dom, a_clos), Graph::All(b_uses, b_dom, b_clos)) => {
          if a_uses != b_uses {
            return false;
          }
          let new_var = new_link(Graph::Var(dep, Name::from("")));
          let Closure { idx: a_idx, env: a_env } = a_clos;
          let Closure { idx: b_idx, env: b_env } = b_clos;
          let a_dom = reduce(globals, fun_defs, a_dom.clone());
          let b_dom = reduce(globals, fun_defs, b_dom.clone());
          let a_img = build_graph(true, globals, fun_defs, *a_idx, a_env, new_var.clone());
          let b_img = build_graph(true, globals, fun_defs, *b_idx, b_env, new_var);
          triples.push((a_dom, b_dom, dep));
          triples.push((a_img, b_img, dep + 1));
        },
        (Graph::App(a_fun, a_arg), Graph::App(b_fun, b_arg)) => {
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

pub fn hash_graph(
  globals: &Vec<DefCell>,
  fun_defs: &Vec<FunCell>,
  graph: Link<Graph>,
  dep: usize,
  ini: usize,
) -> String {
  let mut stack = vec![graph];
  let mut string = String::new();
  while let Some(graph) = stack.pop() {
    match &*graph.borrow() {
      Graph::App(fun, arg) => {
        string.push('@');
        stack.push(fun.clone());
        stack.push(arg.clone());
      }
      Graph::Lam(Closure { idx, env }) => {
        let code_str = hash_code(globals, fun_defs, *idx, env, dep+1, ini);
        string.push('λ');
        string.push_str(&code_str);
      }
      Graph::Var(idx, _) => {
        if *idx >= ini {
          string.push_str(&format!("^{}", dep-idx-1));
        }
        else {
          string.push_str(&format!("#{}", idx));
        }
      }
      Graph::Ref(idx) => {
        string.push_str(&format!("R{}", idx));
      },
      Graph::Typ => {
        string.push('*')
      },
      Graph::All(uses, dom, Closure { idx, env }) => {
        let code_str = hash_code(globals, fun_defs, *idx, env, dep+1, ini);
        string.push_str(&format!("∀{}{}", uses, code_str));
        stack.push(dom.clone());
      },
      Graph::Slf(Closure { idx, env }) => {
        let code_str = hash_code(globals, fun_defs, *idx, env, dep+1, ini);
        string.push('$');
        string.push_str(&code_str);
      },
      Graph::Dat(bod) => {
        string.push('D');
        stack.push(bod.clone());
      },
      Graph::Cse(bod) => {
        string.push('C');
        stack.push(bod.clone());
      },
      Graph::Ann(_, exp) => {
        stack.push(exp.clone())
      },
      Graph::Let(_, _, exp, Closure { idx, env }) => {
        let code_str = hash_code(globals, fun_defs, *idx, env, dep+1, ini);
        string.push('L');
        string.push_str(&code_str);
        stack.push(exp.clone());
      },
      Graph::Fix(Closure { idx, env }) => {
        let code_str = hash_code(globals, fun_defs, *idx, env, dep+1, ini);
        string.push('μ');
        string.push_str(&code_str);
      },
      Graph::Lit(lit) => {
        string.push_str(&format!("I{}", lit))
      },
      Graph::LTy(lty) => {
        string.push_str(&format!("Y{}", lty))
      },
      Graph::Opr(opr) => {
        string.push_str(&format!("O{}", opr))
      },
      Graph::Unr(_, exp) => {
        stack.push(exp.clone())
      },
    }
  }
  string
}

pub fn hash_code(
  globals: &Vec<DefCell>,
  fun_defs: &Vec<FunCell>,
  idx: usize,
  env: &Vec<Link<Graph>>,
  dep: usize,
  ini: usize,
) -> String {
  let code = &fun_defs[idx].code;
  let mut pc = 0;
  let mut args = vec![];
  loop {
    match code[pc] {
      MK_APP => {
        // Function comes last, so it is popped first
        let fun = args.pop().unwrap();
        let arg = args.pop().unwrap();
        args.push(format!("@{}{}", fun, arg));
      },
      MK_LAM => {
        let (fun_index, fun_env) = hash_env(code, &mut pc, &env, dep);
        let code_str = hash_code(globals, fun_defs, fun_index, &fun_env, dep+1, ini);
        args.push(format!("λ{}", code_str));
      }
      REF_GBL => {
        let bytes = &code[pc+1..pc+1+ENV_SIZE];
        let index = bytes_to_usize(bytes);
        args.push(format!("R{}", index));
        pc = pc+ENV_SIZE;
      },
      REF_ARG => {
        args.push(format!("^0"));
      },
      REF_ENV => {
        let bytes = &code[pc+1..pc+1+ENV_SIZE];
        let index = bytes_to_usize(bytes);
        let hash = hash_graph(globals, fun_defs, env[index].clone(), dep, ini);
        args.push(hash);
        pc = pc+ENV_SIZE;
      },
      MK_VAR => {
        let bytes = &code[pc+1..pc+9];
        let index = bytes_to_usize(bytes);
        args.push(format!("#{}", index));
        pc = pc+8;
      },
      MK_TYP => {
        args.push(format!("*"));
      },
      MK_ALL => {
        let uses = code[pc+1];
        pc = pc+1;
        let (fun_index, fun_env) = hash_env(code, &mut pc, &env, dep);
        let dom = args.pop().unwrap();
        let code_str = hash_code(globals, fun_defs, fun_index, &fun_env, dep+1, ini);
        args.push(format!("∀{}{}{}", code_to_uses(uses), code_str, dom));
      },
      MK_SLF => {
        let (fun_index, fun_env) = hash_env(code, &mut pc, &env, dep);
        let code_str = hash_code(globals, fun_defs, fun_index, &fun_env, dep+1, ini);
        args.push(format!("${}", code_str));
      },
      MK_DAT => {
        let bod = args.pop().unwrap();
        args.push(format!("D{}", bod));
      },
      MK_CSE => {
        let bod = args.pop().unwrap();
        args.push(format!("C{}", bod));
      },
      MK_ANN => {
        args.pop().unwrap();
        let exp = args.pop().unwrap();
        args.push(format!("{}", exp));
      },
      MK_LET => {
        // Skip uses
        pc = pc+1;
        let (fun_index, fun_env) = hash_env(code, &mut pc, &env, dep);
        args.pop().unwrap();
        let exp = args.pop().unwrap();
        let code_str = hash_code(globals, fun_defs, fun_index, &fun_env, dep+1, ini);
        args.push(format!("L{}{}", code_str, exp));
      },
      MK_FIX => {
        let (fun_index, fun_env) = hash_env(code, &mut pc, &env, dep);
        let code_str = hash_code(globals, fun_defs, fun_index, &fun_env, dep+1, ini);
        args.push(format!("μ{}", code_str));
      },
      MK_LIT => {
        todo!()
      }
      MK_LTY => {
        let lty = code[pc+1];
        pc = pc+1;
        let lty: LitType = unsafe { mem::transmute(lty) };
        args.push(format!("Y{}", lty));
      }
      MK_OPR => {
        let typ_code = code[pc+1];
        let opr_code = code[pc+2];
        pc = pc+2;
        let opr = code_to_opr(typ_code, opr_code);
        args.push(format!("O{}", opr));
      }
      EVAL => {
      }
      END => break,
      _ => panic!("Operation does not exist"),
    }
    pc = pc+1;
  }
  format!("{}", args.pop().unwrap())
}

#[inline]
pub fn hash_env(
  code: &Vec<CODE>,
  pc: &mut usize,
  env: &Vec<Link<Graph>>,
  dep: usize,
) -> (usize, Vec<Link<Graph>>) {
  let bytes = &code[*pc+1..*pc+1+MAP_SIZE];
  let fun_index = bytes_to_usize(bytes);
  *pc = *pc+MAP_SIZE;
  let bytes = &code[*pc+1..*pc+1+ENV_SIZE];
  let env_length = bytes_to_usize(bytes);
  *pc = *pc+ENV_SIZE;
  let mut fun_env = vec![];
  for _ in 0..env_length {
    let bytes = &code[*pc+1..*pc+1+ENV_SIZE];
    let index = bytes_to_usize(bytes);
    if index == 0 {
      fun_env.push(new_link(Graph::Var(dep-1, Name::from("x"))));
    }
    else {
      fun_env.push(env[index-1].clone());
    }
    *pc = *pc+ENV_SIZE;
  }
  (fun_index, fun_env)
}
