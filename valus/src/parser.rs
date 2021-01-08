#![allow(unused_variables)]
#![allow(unused_imports)]

use crate::{
  dag::*,
  dll::*,
  term,
  term::Term,
};
use core::ptr::NonNull;
use nom::IResult;
use std::collections::HashMap;

// Consumes a term Tree and produces a DAG
pub fn tree_to_dag(tree: Term) -> DAG {
  pub fn go(
    tree: Term,
    mut map: HashMap<String, NonNull<Var>>,
    parents: NonNull<DLL<ParentCell>>,
  ) -> DAG {
    match tree {
      Term::Lam(name, body) => {
        // Allocate nodes
        let var = alloc_val(Var { name: name.clone(), parents: None });
        let sons_parents = alloc_uninit();
        let lam = alloc_val(Lam {
          var,
          // Temporary, dangling DAG pointer
          body: DAG::Lam(NonNull::dangling()),
          body_ref: sons_parents,
          parents: Some(parents),
        });

        // Update `sons_parents` to refer to current node
        unsafe {
          *sons_parents.as_ptr() = DLL::singleton(ParentCell::LamBod(lam));
        }

        // Map `name` to `var` node
        map.insert(name.clone(), var);
        let body = go(*body, map, sons_parents);

        // Update `lam` with the correct body
        unsafe {
          (*lam.as_ptr()).body = body;
        }
        DAG::Lam(lam)
      }

      Term::App(boxed) => {
        // Allocation and updates
        let arg_parents = alloc_uninit();
        let func_parents = alloc_uninit();
        let app = alloc_val(App {
          // Temporary, dangling DAG pointers
          func: DAG::Lam(NonNull::dangling()),
          arg: DAG::Lam(NonNull::dangling()),
          func_ref: func_parents,
          arg_ref: arg_parents,
          copy: None,
          parents: Some(parents),
        });
        unsafe {
          *arg_parents.as_ptr() = DLL::singleton(ParentCell::AppArg(app));
          *func_parents.as_ptr() = DLL::singleton(ParentCell::AppFun(app));
        }

        let (func, arg) = *boxed;
        let func = go(func, map.clone(), func_parents);
        let arg = go(arg, map, arg_parents);

        // Update `app` with the correct fields
        unsafe {
          (*app.as_ptr()).arg = arg;
          (*app.as_ptr()).func = func;
        }
        DAG::App(app)
      }

      Term::Var(name, _) => {
        let var = match map.get(&name.clone()) {
          Some(var) => unsafe {
            DLL::concat(parents, (*var.as_ptr()).parents);
            (*var.as_ptr()).parents = Some(parents);
            *var
          },
          None => alloc_val(Var { name: name.clone(), parents: Some(parents) }),
        };
        DAG::Var(var)
      }
    }
  }
  let root = alloc_val(DLL::singleton(ParentCell::Root));
  go(tree, HashMap::new(), root)
}

pub fn parse(i: &str) -> IResult<&str, DAG, term::TermParseError<&str>> {
  let (i, tree) = term::parse(i)?;
  let (i, _) = nom::character::complete::multispace0(i)?;
  let (i, _) = nom::combinator::eof(i)?;
  let dag = tree_to_dag(tree);
  Ok((i, dag))
}
