use crate::core::{
  dag::*,
  dll::*,
};

use core::ptr::NonNull;
use std::{
  alloc::{
    dealloc,
    Layout,
  },
  mem,
};

// // Resets the cache slots of the app nodes.
// pub fn clear_copies(mut spine: &Single, top_branch: &mut Branch) {
//   #[inline]
//   fn clean_up_var(var: Option<NonNull<Var>>) {
//     match var {
//       Some(mut var) => {
//         let var = unsafe { var.as_mut() };
//         for var_parent in DLL::iter_option((*var).parents) {
//           clean_up(var_parent);
//         }
//       }
//       None => (),
//     };
//   }
//   fn clean_up(cc: &ParentPtr) {
//     match cc {
//       ParentPtr::Left(mut parent) => unsafe {
//         let parent = parent.as_mut();
//         parent.copy.map_or((), |branch| {
//           parent.copy = None;
//           let Branch { var, left, left_ref, right, right_ref, .. } =
//             *branch.as_ptr();
//           add_to_parents(left, left_ref);
//           add_to_parents(right, right_ref);
//           clean_up_var(var);
//           for grandparent in DLL::iter_option(parent.parents) {
//             clean_up(grandparent);
//           }
//         })
//       },
//       ParentPtr::Right(mut parent) => unsafe {
//         let parent = parent.as_mut();
//         parent.copy.map_or((), |branch| {
//           parent.copy = None;
//           let Branch { var, left, left_ref, right, right_ref, .. } =
//             *branch.as_ptr();
//           add_to_parents(left, left_ref);
//           add_to_parents(right, right_ref);
//           clean_up_var(var);
//           for grandparent in DLL::iter_option(parent.parents) {
//             clean_up(grandparent);
//           }
//         })
//       },
//       ParentPtr::Body(parent) => unsafe {
//         let Single { parents, var, .. } = &*parent.as_ptr();
//         clean_up_var(*var);
//         for grandparent in DLL::iter_option(*parents) {
//           clean_up(grandparent);
//         }
//       },
//       ParentPtr::Root => (),
//     }
//   }
//   // Clears the top app cache and adds itself to its children's list of
// parents   top_branch.copy.map_or((), |ptr| unsafe {
//     top_branch.copy = None;
//     let Branch { var, left, left_ref, right, right_ref, .. } = *ptr.as_ptr();
//     add_to_parents(left, left_ref);
//     add_to_parents(right, right_ref);
//     clean_up_var(var);
//   });
//   loop {
//     clean_up_var(spine.var);
//     match spine.body {
//       DAGPtr::Single(single) => unsafe { spine = &*single.as_ptr() },
//       _ => break,
//     }
//   }
// }

// // Free parentless nodes.
pub fn free_dead_node(node: DAGPtr) {
  unsafe {
    match node {
      DAGPtr::Lam(link) => {
        let Lam { bod, bod_ref, .. } = &*link.as_ptr();
        let new_bod_parents = bod_ref.unlink_node();
        set_parents(*bod, new_bod_parents);
        if new_bod_parents.is_none() {
          free_dead_node(*bod)
        }
        dealloc(link.as_ptr() as *mut u8, Layout::new::<Lam>());
      }
      DAGPtr::Slf(link) => {
        let Slf { bod, bod_ref, .. } = &*link.as_ptr();
        let new_bod_parents = bod_ref.unlink_node();
        set_parents(*bod, new_bod_parents);
        if new_bod_parents.is_none() {
          free_dead_node(*bod)
        }
        dealloc(link.as_ptr() as *mut u8, Layout::new::<Slf>());
      }
      DAGPtr::Cse(link) => {
        let Cse { bod, bod_ref, .. } = &*link.as_ptr();
        let new_bod_parents = bod_ref.unlink_node();
        set_parents(*bod, new_bod_parents);
        if new_bod_parents.is_none() {
          free_dead_node(*bod)
        }
        dealloc(link.as_ptr() as *mut u8, Layout::new::<Cse>());
      }
      DAGPtr::Dat(link) => {
        let Dat { bod, bod_ref, .. } = &*link.as_ptr();
        let new_bod_parents = bod_ref.unlink_node();
        set_parents(*bod, new_bod_parents);
        if new_bod_parents.is_none() {
          free_dead_node(*bod)
        }
        dealloc(link.as_ptr() as *mut u8, Layout::new::<Dat>());
      }
      DAGPtr::All(link) => {
        let All { dom, img, dom_ref, img_ref, .. } = &*link.as_ptr();
        let new_dom_parents = dom_ref.unlink_node();
        set_parents(*dom, new_dom_parents);
        if new_dom_parents.is_none() {
          free_dead_node(*dom)
        }
        let new_img_parents = img_ref.unlink_node();
        set_parents(*img, new_img_parents);
        if new_img_parents.is_none() {
          free_dead_node(*img)
        }
        dealloc(link.as_ptr() as *mut u8, Layout::new::<All>());
      }
      DAGPtr::App(link) => {
        let App { fun, arg, fun_ref, arg_ref, .. } = &*link.as_ptr();
        let new_fun_parents = fun_ref.unlink_node();
        set_parents(*fun, new_fun_parents);
        if new_fun_parents.is_none() {
          free_dead_node(*fun)
        }
        let new_arg_parents = arg_ref.unlink_node();
        set_parents(*arg, new_arg_parents);
        if new_arg_parents.is_none() {
          free_dead_node(*arg)
        }
        dealloc(link.as_ptr() as *mut u8, Layout::new::<App>());
      }
      DAGPtr::Ann(link) => {
        let Ann { exp, typ, exp_ref, typ_ref, .. } = &*link.as_ptr();
        let new_exp_parents = exp_ref.unlink_node();
        set_parents(*exp, new_exp_parents);
        if new_exp_parents.is_none() {
          free_dead_node(*exp)
        }
        let new_typ_parents = typ_ref.unlink_node();
        set_parents(*typ, new_typ_parents);
        if new_typ_parents.is_none() {
          free_dead_node(*typ)
        }
        dealloc(link.as_ptr() as *mut u8, Layout::new::<Ann>());
      }
      DAGPtr::Let(link) => {
        let Let { exp, typ, exp_ref, typ_ref, bod, bod_ref, .. } =
          &*link.as_ptr();
        let new_exp_parents = exp_ref.unlink_node();
        set_parents(*exp, new_exp_parents);
        if new_exp_parents.is_none() {
          free_dead_node(*exp)
        }
        let new_typ_parents = typ_ref.unlink_node();
        set_parents(*typ, new_typ_parents);
        if new_typ_parents.is_none() {
          free_dead_node(*typ)
        }
        let new_bod_parents = bod_ref.unlink_node();
        set_parents(*bod, new_bod_parents);
        if new_bod_parents.is_none() {
          free_dead_node(*bod)
        }
        dealloc(link.as_ptr() as *mut u8, Layout::new::<Let>());
      }
      DAGPtr::Var(link) => {
        dealloc(link.as_ptr() as *mut u8, Layout::new::<Var>());
      }
      DAGPtr::Ref(link) => {
        dealloc(link.as_ptr() as *mut u8, Layout::new::<Ref>());
      }
      DAGPtr::Typ(link) => {
        dealloc(link.as_ptr() as *mut u8, Layout::new::<Typ>());
      }
      DAGPtr::Lit(link) => {
        dealloc(link.as_ptr() as *mut u8, Layout::new::<Lit>());
      }
      DAGPtr::LTy(link) => {
        dealloc(link.as_ptr() as *mut u8, Layout::new::<LTy>());
      }
      DAGPtr::Opr(link) => {
        dealloc(link.as_ptr() as *mut u8, Layout::new::<Opr>());
      }
    }
  }
}

// Replace one child w/another in the tree.
pub fn replace_child(oldchild: DAGPtr, newchild: DAGPtr) {
  #[inline]
  fn install_child(parent: &mut ParentPtr, newchild: DAGPtr) {
    unsafe {
      match parent {
        ParentPtr::LamBod(parent) => (*parent.as_ptr()).bod = newchild,
        ParentPtr::SlfBod(parent) => (*parent.as_ptr()).bod = newchild,
        ParentPtr::DatBod(parent) => (*parent.as_ptr()).bod = newchild,
        ParentPtr::CseBod(parent) => (*parent.as_ptr()).bod = newchild,
        ParentPtr::AppFun(parent) => (*parent.as_ptr()).fun = newchild,
        ParentPtr::AppArg(parent) => (*parent.as_ptr()).arg = newchild,
        ParentPtr::AllDom(parent) => (*parent.as_ptr()).dom = newchild,
        ParentPtr::AllImg(parent) => (*parent.as_ptr()).img = newchild,
        ParentPtr::AnnExp(parent) => (*parent.as_ptr()).exp = newchild,
        ParentPtr::AnnTyp(parent) => (*parent.as_ptr()).typ = newchild,
        ParentPtr::LetExp(parent) => (*parent.as_ptr()).exp = newchild,
        ParentPtr::LetTyp(parent) => (*parent.as_ptr()).typ = newchild,
        ParentPtr::LetBod(parent) => (*parent.as_ptr()).bod = newchild,
        ParentPtr::Root => (),
      }
    }
  }
  unsafe {
    let oldpref = get_parents(oldchild);
    if let Some(old_parents) = oldpref {
      let mut iter = (*old_parents.as_ptr()).iter();
      let newpref = get_parents(newchild);
      let mut last_old = None;
      let first_new = newpref.map(|dll| DLL::first(dll));
      while let Some(parent) = iter.next() {
        if iter.is_last() {
          last_old = iter.this();
          last_old.map_or((), |last_old| (*last_old.as_ptr()).next = first_new);
        }
        install_child(parent, newchild);
      }
      first_new.map_or((), |first_new| (*first_new.as_ptr()).prev = last_old);
      set_parents(newchild, oldpref);
    }
    set_parents(oldchild, None);
  }
}

// The core up-copy function.
pub fn upcopy(new_child: DAGPtr, cc: ParentPtr) {
  unsafe {
    match cc {
      ParentPtr::LamBod(lam) => {
        let Lam { var, parents, .. } = lam.as_ref();
        let Var { nam, dep, parents: var_parents } = var;
        let new_var = Var { nam: nam.clone(), dep: *dep, parents: None };
        let new_lam = alloc_val(Lam {
          var: new_var,
          bod: new_child,
          bod_ref: mem::zeroed(),
          parents: None,
        });
        (*new_lam.as_ptr()).bod_ref =
          DLL::singleton(ParentPtr::LamBod(new_lam));
        let ptr: *mut Parents = &mut (*new_lam.as_ptr()).bod_ref;
        add_to_parents(new_child, NonNull::new(ptr).unwrap());
        let ptr: *mut Var = &mut (*new_lam.as_ptr()).var;
        for parent in DLL::iter_option(*var_parents) {
          upcopy(DAGPtr::Var(NonNull::new(ptr).unwrap()), *parent)
        }
        for parent in DLL::iter_option(*parents) {
          upcopy(DAGPtr::Lam(new_lam), *parent)
        }
      }
      ParentPtr::SlfBod(slf) => {
        let Slf { var, parents, .. } = slf.as_ref();
        let Var { nam, dep, parents: var_parents } = var;
        let new_var = Var { nam: nam.clone(), dep: *dep, parents: None };
        let new_slf = alloc_val(Slf {
          var: new_var,
          bod: new_child,
          bod_ref: mem::zeroed(),
          parents: None,
        });
        (*new_slf.as_ptr()).bod_ref =
          DLL::singleton(ParentPtr::SlfBod(new_slf));
        let ptr: *mut Parents = &mut (*new_slf.as_ptr()).bod_ref;
        add_to_parents(new_child, NonNull::new(ptr).unwrap());
        let ptr: *mut Var = &mut (*new_slf.as_ptr()).var;
        for parent in DLL::iter_option(*var_parents) {
          upcopy(DAGPtr::Var(NonNull::new(ptr).unwrap()), *parent)
        }
        for parent in DLL::iter_option(*parents) {
          upcopy(DAGPtr::Slf(new_slf), *parent)
        }
      }
      ParentPtr::DatBod(dat) => {
        let Dat { parents, .. } = dat.as_ref();
        let new_dat = alloc_val(Dat {
          bod: new_child,
          bod_ref: mem::zeroed(),
          parents: None,
        });
        (*new_dat.as_ptr()).bod_ref =
          DLL::singleton(ParentPtr::DatBod(new_dat));
        let ptr: *mut Parents = &mut (*new_dat.as_ptr()).bod_ref;
        add_to_parents(new_child, NonNull::new(ptr).unwrap());
        for parent in DLL::iter_option(*parents) {
          upcopy(DAGPtr::Dat(new_dat), *parent)
        }
      }
      ParentPtr::CseBod(cse) => {
        let Cse { parents, .. } = cse.as_ref();
        let new_cse = alloc_val(Cse {
          bod: new_child,
          bod_ref: mem::zeroed(),
          parents: None,
        });
        (*new_cse.as_ptr()).bod_ref =
          DLL::singleton(ParentPtr::CseBod(new_cse));
        let ptr: *mut Parents = &mut (*new_cse.as_ptr()).bod_ref;
        add_to_parents(new_child, NonNull::new(ptr).unwrap());
        for parent in DLL::iter_option(*parents) {
          upcopy(DAGPtr::Cse(new_cse), *parent)
        }
      }
      ParentPtr::AppFun(app) => {
        let App { copy, arg, parents, .. } = app.as_ref();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).fun = new_child;
          }
          None => {
            let new_app = alloc_val(App {
              copy: None,
              fun: new_child,
              arg: *arg,
              fun_ref: mem::zeroed(),
              arg_ref: mem::zeroed(),
              parents: None,
            });
            (*new_app.as_ptr()).fun_ref =
              DLL::singleton(ParentPtr::AppFun(new_app));
            (*new_app.as_ptr()).arg_ref =
              DLL::singleton(ParentPtr::AppArg(new_app));
            (*app.as_ptr()).copy = Some(new_app);
            for parent in DLL::iter_option(*parents) {
              upcopy(DAGPtr::App(new_app), *parent)
            }
          }
        }
      }
      ParentPtr::AppArg(app) => {
        let App { copy, fun, parents, .. } = app.as_ref();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).arg = new_child;
          }
          None => {
            let new_app = alloc_val(App {
              copy: None,
              fun: *fun,
              arg: new_child,
              fun_ref: mem::zeroed(),
              arg_ref: mem::zeroed(),
              parents: None,
            });
            (*new_app.as_ptr()).fun_ref =
              DLL::singleton(ParentPtr::AppFun(new_app));
            (*new_app.as_ptr()).arg_ref =
              DLL::singleton(ParentPtr::AppArg(new_app));
            (*app.as_ptr()).copy = Some(new_app);
            for parent in DLL::iter_option(*parents) {
              upcopy(DAGPtr::App(new_app), *parent)
            }
          }
        }
      }
      ParentPtr::AnnTyp(ann) => {
        let Ann { copy, exp, parents, .. } = ann.as_ref();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).typ = new_child;
          }
          None => {
            let new_ann = alloc_val(Ann {
              copy: None,
              typ: new_child,
              exp: *exp,
              typ_ref: mem::zeroed(),
              exp_ref: mem::zeroed(),
              parents: None,
            });
            (*new_ann.as_ptr()).typ_ref =
              DLL::singleton(ParentPtr::AnnTyp(new_ann));
            (*new_ann.as_ptr()).exp_ref =
              DLL::singleton(ParentPtr::AnnExp(new_ann));
            (*ann.as_ptr()).copy = Some(new_ann);
            for parent in DLL::iter_option(*parents) {
              upcopy(DAGPtr::Ann(new_ann), *parent)
            }
          }
        }
      }
      ParentPtr::AnnExp(ann) => {
        let Ann { copy, typ, parents, .. } = ann.as_ref();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).exp = new_child;
          }
          None => {
            let new_ann = alloc_val(Ann {
              copy: None,
              typ: *typ,
              exp: new_child,
              typ_ref: mem::zeroed(),
              exp_ref: mem::zeroed(),
              parents: None,
            });
            (*new_ann.as_ptr()).typ_ref =
              DLL::singleton(ParentPtr::AnnTyp(new_ann));
            (*new_ann.as_ptr()).exp_ref =
              DLL::singleton(ParentPtr::AnnExp(new_ann));
            (*ann.as_ptr()).copy = Some(new_ann);
            for parent in DLL::iter_option(*parents) {
              upcopy(DAGPtr::Ann(new_ann), *parent)
            }
          }
        }
      }
      ParentPtr::AllDom(all) => {
        let All { copy, uses, var, img, parents, .. } = all.as_ref();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).dom = new_child;
          }
          None => {
            let Var { nam, dep, parents: var_parents } = var;
            let new_var = Var { nam: nam.clone(), dep: *dep, parents: None };
            let new_all = alloc_val(All {
              copy: None,
              uses: *uses,
              var: new_var,
              dom: new_child,
              img: *img,
              dom_ref: mem::zeroed(),
              img_ref: mem::zeroed(),
              parents: None,
            });
            (*new_all.as_ptr()).dom_ref =
              DLL::singleton(ParentPtr::AllDom(new_all));
            (*new_all.as_ptr()).img_ref =
              DLL::singleton(ParentPtr::AllImg(new_all));
            (*all.as_ptr()).copy = Some(new_all);
            let ptr: *mut Var = &mut (*new_all.as_ptr()).var;
            for parent in DLL::iter_option(*var_parents) {
              upcopy(DAGPtr::Var(NonNull::new(ptr).unwrap()), *parent)
            }
            for parent in DLL::iter_option(*parents) {
              upcopy(DAGPtr::All(new_all), *parent)
            }
          }
        }
      }
      ParentPtr::AllImg(all) => {
        let All { copy, uses, var, dom, parents, .. } = all.as_ref();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).img = new_child;
          }
          None => {
            let Var { nam, dep, parents: var_parents } = var;
            let new_var = Var { nam: nam.clone(), dep: *dep, parents: None };
            let new_all = alloc_val(All {
              copy: None,
              uses: *uses,
              var: new_var,
              dom: *dom,
              img: new_child,
              dom_ref: mem::zeroed(),
              img_ref: mem::zeroed(),
              parents: None,
            });
            (*new_all.as_ptr()).dom_ref =
              DLL::singleton(ParentPtr::AllDom(new_all));
            (*new_all.as_ptr()).img_ref =
              DLL::singleton(ParentPtr::AllImg(new_all));
            (*all.as_ptr()).copy = Some(new_all);
            let ptr: *mut Var = &mut (*new_all.as_ptr()).var;
            for parent in DLL::iter_option(*var_parents) {
              upcopy(DAGPtr::Var(NonNull::new(ptr).unwrap()), *parent)
            }
            for parent in DLL::iter_option(*parents) {
              upcopy(DAGPtr::All(new_all), *parent)
            }
          }
        }
      }
      ParentPtr::LetExp(let_) => {
        let Let { copy, uses, var, typ, bod, parents, .. } = let_.as_ref();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).exp = new_child;
          }
          None => {
            let Var { nam, dep, parents: var_parents } = var;
            let new_var = Var { nam: nam.clone(), dep: *dep, parents: None };
            let new_let = alloc_val(Let {
              copy: None,
              uses: *uses,
              var: new_var,
              exp: new_child,
              typ: *typ,
              bod: *bod,
              exp_ref: mem::zeroed(),
              typ_ref: mem::zeroed(),
              bod_ref: mem::zeroed(),
              parents: None,
            });
            (*new_let.as_ptr()).exp_ref =
              DLL::singleton(ParentPtr::LetExp(new_let));
            (*new_let.as_ptr()).typ_ref =
              DLL::singleton(ParentPtr::LetTyp(new_let));
            (*new_let.as_ptr()).bod_ref =
              DLL::singleton(ParentPtr::LetBod(new_let));
            (*let_.as_ptr()).copy = Some(new_let);
            let ptr: *mut Var = &mut (*new_let.as_ptr()).var;
            for parent in DLL::iter_option(*var_parents) {
              upcopy(DAGPtr::Var(NonNull::new(ptr).unwrap()), *parent)
            }
            for parent in DLL::iter_option(*parents) {
              upcopy(DAGPtr::Let(new_let), *parent)
            }
          }
        }
      }
      ParentPtr::LetTyp(let_) => {
        let Let { copy, uses, var, exp, bod, parents, .. } = let_.as_ref();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).typ = new_child;
          }
          None => {
            let Var { nam, dep, parents: var_parents } = var;
            let new_var = Var { nam: nam.clone(), dep: *dep, parents: None };
            let new_let = alloc_val(Let {
              copy: None,
              uses: *uses,
              var: new_var,
              exp: *exp,
              typ: new_child,
              bod: *bod,
              exp_ref: mem::zeroed(),
              typ_ref: mem::zeroed(),
              bod_ref: mem::zeroed(),
              parents: None,
            });
            (*new_let.as_ptr()).exp_ref =
              DLL::singleton(ParentPtr::LetExp(new_let));
            (*new_let.as_ptr()).typ_ref =
              DLL::singleton(ParentPtr::LetTyp(new_let));
            (*new_let.as_ptr()).bod_ref =
              DLL::singleton(ParentPtr::LetBod(new_let));
            (*let_.as_ptr()).copy = Some(new_let);
            let ptr: *mut Var = &mut (*new_let.as_ptr()).var;
            for parent in DLL::iter_option(*var_parents) {
              upcopy(DAGPtr::Var(NonNull::new(ptr).unwrap()), *parent)
            }
            for parent in DLL::iter_option(*parents) {
              upcopy(DAGPtr::Let(new_let), *parent)
            }
          }
        }
      }
      ParentPtr::LetBod(let_) => {
        let Let { copy, uses, var, typ, exp, parents, .. } = let_.as_ref();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).bod = new_child;
          }
          None => {
            let Var { nam, dep, parents: var_parents } = var;
            let new_var = Var { nam: nam.clone(), dep: *dep, parents: None };
            let new_let = alloc_val(Let {
              copy: None,
              uses: *uses,
              var: new_var,
              exp: *exp,
              typ: *typ,
              bod: new_child,
              exp_ref: mem::zeroed(),
              typ_ref: mem::zeroed(),
              bod_ref: mem::zeroed(),
              parents: None,
            });
            (*new_let.as_ptr()).exp_ref =
              DLL::singleton(ParentPtr::LetExp(new_let));
            (*new_let.as_ptr()).typ_ref =
              DLL::singleton(ParentPtr::LetTyp(new_let));
            (*new_let.as_ptr()).bod_ref =
              DLL::singleton(ParentPtr::LetBod(new_let));
            (*let_.as_ptr()).copy = Some(new_let);
            let ptr: *mut Var = &mut (*new_let.as_ptr()).var;
            for parent in DLL::iter_option(*var_parents) {
              upcopy(DAGPtr::Var(NonNull::new(ptr).unwrap()), *parent)
            }
            for parent in DLL::iter_option(*parents) {
              upcopy(DAGPtr::Let(new_let), *parent)
            }
          }
        }
      }
      ParentPtr::Root => (),
    }
  }
}
