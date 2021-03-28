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

pub fn clean_up(cc: &ParentPtr) {
  match cc {
    ParentPtr::LamBod(link) => unsafe {
      let Lam { parents, var, .. } = link.as_ref();
      for parent in DLL::iter_option(var.parents) {
        clean_up(parent);
      }
      for parent in DLL::iter_option(*parents) {
        clean_up(parent);
      }
    },
    ParentPtr::SlfBod(link) => unsafe {
      let Slf { parents, var, .. } = link.as_ref();
      for parent in DLL::iter_option(var.parents) {
        clean_up(parent);
      }
      for parent in DLL::iter_option(*parents) {
        clean_up(parent);
      }
    },
    ParentPtr::DatBod(link) => unsafe {
      let Dat { parents, .. } = link.as_ref();
      for parent in DLL::iter_option(*parents) {
        clean_up(parent);
      }
    },
    ParentPtr::CseBod(link) => unsafe {
      let Cse { parents, .. } = link.as_ref();
      for parent in DLL::iter_option(*parents) {
        clean_up(parent);
      }
    },
    ParentPtr::AppFun(mut link) | ParentPtr::AppArg(mut link) => unsafe {
      let app = link.as_mut();
      match app.copy {
        Some(app_copy) => {
          let App { fun, arg, .. } = app_copy.as_ref();
          app.copy = None;
          let ptr: *mut Parents = &mut (*app_copy.as_ptr()).fun_ref;
          add_to_parents(*fun, NonNull::new(ptr).unwrap());
          let ptr: *mut Parents = &mut (*app_copy.as_ptr()).arg_ref;
          add_to_parents(*arg, NonNull::new(ptr).unwrap());
          for parent in DLL::iter_option(app.parents) {
            clean_up(parent);
          }
        }
        None => (),
      }
    },
    ParentPtr::AnnExp(mut link) | ParentPtr::AnnTyp(mut link) => unsafe {
      let ann = link.as_mut();
      match ann.copy {
        Some(ann_copy) => {
          let Ann { typ, exp, .. } = ann_copy.as_ref();
          ann.copy = None;
          let ptr: *mut Parents = &mut (*ann_copy.as_ptr()).typ_ref;
          add_to_parents(*typ, NonNull::new(ptr).unwrap());
          let ptr: *mut Parents = &mut (*ann_copy.as_ptr()).exp_ref;
          add_to_parents(*exp, NonNull::new(ptr).unwrap());
          for parent in DLL::iter_option(ann.parents) {
            clean_up(parent);
          }
        }
        None => (),
      }
    },
    ParentPtr::AllDom(mut link) | ParentPtr::AllImg(mut link) => unsafe {
      let all = link.as_mut();
      match all.copy {
        Some(all_copy) => {
          let All { var, dom, img, .. } = all_copy.as_ref();
          all.copy = None;
          let ptr: *mut Parents = &mut (*all_copy.as_ptr()).dom_ref;
          add_to_parents(*dom, NonNull::new(ptr).unwrap());
          let ptr: *mut Parents = &mut (*all_copy.as_ptr()).img_ref;
          add_to_parents(*img, NonNull::new(ptr).unwrap());
          for parent in DLL::iter_option(var.parents) {
            clean_up(parent);
          }
          for parent in DLL::iter_option(all.parents) {
            clean_up(parent);
          }
        }
        None => (),
      }
    },
    ParentPtr::LetTyp(mut link)
    | ParentPtr::LetExp(mut link)
    | ParentPtr::LetBod(mut link) => unsafe {
      let let_ = link.as_mut();
      match let_.copy {
        Some(let_copy) => {
          let Let { var, typ, exp, bod, .. } = let_copy.as_ref();
          let_.copy = None;
          let ptr: *mut Parents = &mut (*let_copy.as_ptr()).typ_ref;
          add_to_parents(*typ, NonNull::new(ptr).unwrap());
          let ptr: *mut Parents = &mut (*let_copy.as_ptr()).exp_ref;
          add_to_parents(*exp, NonNull::new(ptr).unwrap());
          let ptr: *mut Parents = &mut (*let_copy.as_ptr()).bod_ref;
          add_to_parents(*bod, NonNull::new(ptr).unwrap());
          for parent in DLL::iter_option(var.parents) {
            clean_up(parent);
          }
          for parent in DLL::iter_option(let_.parents) {
            clean_up(parent);
          }
        }
        None => (),
      }
    },
    ParentPtr::Root => (),
  }
}

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
      ParentPtr::LamBod(link) => {
        let Lam { var, parents, .. } = link.as_ref();
        let Var { nam, dep, parents: var_parents } = var;
        let new_var = Var { nam: nam.clone(), dep: *dep, parents: None };
        let new_lam = alloc_lam(new_var, new_child, None);
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
      ParentPtr::SlfBod(link) => {
        let Slf { var, parents, .. } = link.as_ref();
        let Var { nam, dep, parents: var_parents } = var;
        let new_var = Var { nam: nam.clone(), dep: *dep, parents: None };
        let new_slf = alloc_slf(new_var, new_child, None);
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
      ParentPtr::DatBod(link) => {
        let Dat { parents, .. } = link.as_ref();
        let new_dat = alloc_dat(new_child, None);
        let ptr: *mut Parents = &mut (*new_dat.as_ptr()).bod_ref;
        add_to_parents(new_child, NonNull::new(ptr).unwrap());
        for parent in DLL::iter_option(*parents) {
          upcopy(DAGPtr::Dat(new_dat), *parent)
        }
      }
      ParentPtr::CseBod(link) => {
        let Cse { parents, .. } = link.as_ref();
        let new_cse = alloc_cse(new_child, None);
        let ptr: *mut Parents = &mut (*new_cse.as_ptr()).bod_ref;
        add_to_parents(new_child, NonNull::new(ptr).unwrap());
        for parent in DLL::iter_option(*parents) {
          upcopy(DAGPtr::Cse(new_cse), *parent)
        }
      }
      ParentPtr::AppFun(link) => {
        let App { copy, arg, parents, .. } = link.as_ref();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).fun = new_child;
          }
          None => {
            let new_app = alloc_app(new_child, *arg, None);
            (*link.as_ptr()).copy = Some(new_app);
            for parent in DLL::iter_option(*parents) {
              upcopy(DAGPtr::App(new_app), *parent)
            }
          }
        }
      }
      ParentPtr::AppArg(link) => {
        let App { copy, fun, parents, .. } = link.as_ref();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).arg = new_child;
          }
          None => {
            let new_app = alloc_app(*fun, new_child, None);
            (*link.as_ptr()).copy = Some(new_app);
            for parent in DLL::iter_option(*parents) {
              upcopy(DAGPtr::App(new_app), *parent)
            }
          }
        }
      }
      ParentPtr::AnnTyp(link) => {
        let Ann { copy, exp, parents, .. } = link.as_ref();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).typ = new_child;
          }
          None => {
            let new_ann = alloc_ann(new_child, *exp, None);
            (*link.as_ptr()).copy = Some(new_ann);
            for parent in DLL::iter_option(*parents) {
              upcopy(DAGPtr::Ann(new_ann), *parent)
            }
          }
        }
      }
      ParentPtr::AnnExp(link) => {
        let Ann { copy, typ, parents, .. } = link.as_ref();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).exp = new_child;
          }
          None => {
            let new_ann = alloc_ann(*typ, new_child, None);
            (*link.as_ptr()).copy = Some(new_ann);
            for parent in DLL::iter_option(*parents) {
              upcopy(DAGPtr::Ann(new_ann), *parent)
            }
          }
        }
      }
      ParentPtr::AllDom(link) => {
        let All { copy, uses, var, img, parents, .. } = link.as_ref();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).dom = new_child;
          }
          None => {
            let Var { nam, dep, parents: var_parents } = var;
            let new_var = Var { nam: nam.clone(), dep: *dep, parents: None };
            let new_all = alloc_all(new_var, *uses, new_child, *img, None);
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
      ParentPtr::AllImg(link) => {
        let All { copy, uses, var, dom, parents, .. } = link.as_ref();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).img = new_child;
          }
          None => {
            let Var { nam, dep, parents: var_parents } = var;
            let new_var = Var { nam: nam.clone(), dep: *dep, parents: None };
            let new_all = alloc_all(new_var, *uses, *dom, new_child, None);
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
      ParentPtr::LetExp(link) => {
        let Let { copy, uses, var, typ, bod, parents, .. } = link.as_ref();
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
            (*link.as_ptr()).copy = Some(new_let);
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
      ParentPtr::LetTyp(link) => {
        let Let { copy, uses, var, exp, bod, parents, .. } = link.as_ref();
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
            (*link.as_ptr()).copy = Some(new_let);
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
      ParentPtr::LetBod(link) => {
        let Let { copy, uses, var, typ, exp, parents, .. } = link.as_ref();
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
            (*link.as_ptr()).copy = Some(new_let);
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
