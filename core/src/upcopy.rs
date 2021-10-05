use crate::{
  dag::*,
  dll::*,
};

use core::{
  ptr::NonNull,
  sync::atomic::{
    AtomicUsize,
    Ordering,
  },
};

/// Counts the number of upcopy calls for benchmarking
pub static UPCOPY_COUNT: AtomicUsize = AtomicUsize::new(0);

/// Frees copied nodes between upcopy operations
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
    ParentPtr::FixBod(link) => unsafe {
      let Fix { parents, var, .. } = link.as_ref();
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
      if let Some(app_copy) = app.copy {
        let App { fun, arg, fun_ref, arg_ref, .. } = &mut *app_copy.as_ptr();
        app.copy = None;
        add_to_parents(*fun, NonNull::new(fun_ref).unwrap());
        add_to_parents(*arg, NonNull::new(arg_ref).unwrap());
        for parent in DLL::iter_option(app.parents) {
          clean_up(parent);
        }
      }
    },
    ParentPtr::AnnExp(mut link) | ParentPtr::AnnTyp(mut link) => unsafe {
      let ann = link.as_mut();
      if let Some(ann_copy) = ann.copy {
        let Ann { typ, exp, typ_ref, exp_ref, .. } = &mut *ann_copy.as_ptr();
        ann.copy = None;
        add_to_parents(*typ, NonNull::new(typ_ref).unwrap());
        add_to_parents(*exp, NonNull::new(exp_ref).unwrap());
        for parent in DLL::iter_option(ann.parents) {
          clean_up(parent);
        }
      }
    },
    ParentPtr::AllDom(mut link) | ParentPtr::AllImg(mut link) => unsafe {
      let all = link.as_mut();
      if let Some(all_copy) = all.copy {
        let All { dom, img, dom_ref, img_ref, .. } = &mut *all_copy.as_ptr();
        all.copy = None;
        add_to_parents(*dom, NonNull::new(dom_ref).unwrap());
        add_to_parents(DAGPtr::Lam(*img), NonNull::new(img_ref).unwrap());
        for parent in DLL::iter_option(all.parents) {
          clean_up(parent);
        }
      }
    },
    ParentPtr::LetTyp(mut link)
    | ParentPtr::LetExp(mut link)
    | ParentPtr::LetBod(mut link) => unsafe {
      let let_ = link.as_mut();
      if let Some(let_copy) = let_.copy {
        let Let { typ, exp, bod, typ_ref, exp_ref, bod_ref, .. } =
          &mut *let_copy.as_ptr();
        let_.copy = None;
        add_to_parents(*typ, NonNull::new(typ_ref).unwrap());
        add_to_parents(*exp, NonNull::new(exp_ref).unwrap());
        add_to_parents(DAGPtr::Lam(*bod), NonNull::new(bod_ref).unwrap());
        for parent in DLL::iter_option(let_.parents) {
          clean_up(parent);
        }
      }
    },
    ParentPtr::Root => (),
  }
}

/// The core up-copy function
pub fn upcopy(new_child: DAGPtr, cc: ParentPtr, should_count: bool) {
  if should_count {
    UPCOPY_COUNT.fetch_add(1, Ordering::SeqCst);
  }
  unsafe {
    match cc {
      ParentPtr::LamBod(link) => {
        let Lam { var, parents, .. } = link.as_ref();
        let Var { nam, dep, parents: var_parents, .. } = var;
        let new_lam = alloc_lam(nam.clone(), *dep, new_child, None);
        let ptr: *mut Parents = &mut (*new_lam.as_ptr()).bod_ref;
        add_to_parents(new_child, NonNull::new(ptr).unwrap());
        let ptr: *mut Var = &mut (*new_lam.as_ptr()).var;
        for parent in DLL::iter_option(*var_parents) {
          upcopy(DAGPtr::Var(NonNull::new(ptr).unwrap()), *parent, should_count)
        }
        for parent in DLL::iter_option(*parents) {
          upcopy(DAGPtr::Lam(new_lam), *parent, should_count)
        }
      }
      ParentPtr::SlfBod(link) => {
        let Slf { var, parents, .. } = link.as_ref();
        let Var { nam, dep, parents: var_parents, .. } = var;
        let new_slf = alloc_slf(nam.clone(), *dep, new_child, None);
        let ptr: *mut Parents = &mut (*new_slf.as_ptr()).bod_ref;
        add_to_parents(new_child, NonNull::new(ptr).unwrap());
        let ptr: *mut Var = &mut (*new_slf.as_ptr()).var;
        for parent in DLL::iter_option(*var_parents) {
          upcopy(DAGPtr::Var(NonNull::new(ptr).unwrap()), *parent, should_count)
        }
        for parent in DLL::iter_option(*parents) {
          upcopy(DAGPtr::Slf(new_slf), *parent, should_count)
        }
      }
      ParentPtr::FixBod(link) => {
        let Fix { var, parents, .. } = link.as_ref();
        let Var { nam, dep, parents: var_parents, .. } = var;
        let new_fix = alloc_fix(nam.clone(), *dep, new_child, None);
        let ptr: *mut Parents = &mut (*new_fix.as_ptr()).bod_ref;
        add_to_parents(new_child, NonNull::new(ptr).unwrap());
        let ptr: *mut Var = &mut (*new_fix.as_ptr()).var;
        for parent in DLL::iter_option(*var_parents) {
          upcopy(DAGPtr::Var(NonNull::new(ptr).unwrap()), *parent, should_count)
        }
        for parent in DLL::iter_option(*parents) {
          upcopy(DAGPtr::Fix(new_fix), *parent, should_count)
        }
      }
      ParentPtr::DatBod(link) => {
        let Dat { parents, .. } = link.as_ref();
        let new_dat = alloc_dat(new_child, None);
        let ptr: *mut Parents = &mut (*new_dat.as_ptr()).bod_ref;
        add_to_parents(new_child, NonNull::new(ptr).unwrap());
        for parent in DLL::iter_option(*parents) {
          upcopy(DAGPtr::Dat(new_dat), *parent, should_count)
        }
      }
      ParentPtr::CseBod(link) => {
        let Cse { parents, .. } = link.as_ref();
        let new_cse = alloc_cse(new_child, None);
        let ptr: *mut Parents = &mut (*new_cse.as_ptr()).bod_ref;
        add_to_parents(new_child, NonNull::new(ptr).unwrap());
        for parent in DLL::iter_option(*parents) {
          upcopy(DAGPtr::Cse(new_cse), *parent, should_count)
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
              upcopy(DAGPtr::App(new_app), *parent, should_count)
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
              upcopy(DAGPtr::App(new_app), *parent, should_count)
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
              upcopy(DAGPtr::Ann(new_ann), *parent, should_count)
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
              upcopy(DAGPtr::Ann(new_ann), *parent, should_count)
            }
          }
        }
      }
      ParentPtr::AllDom(link) => {
        let All { copy, uses, img, parents, .. } = link.as_ref();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).dom = new_child;
          }
          None => {
            let new_all = alloc_all(*uses, new_child, *img, None);
            (*link.as_ptr()).copy = Some(new_all);
            for parent in DLL::iter_option(*parents) {
              upcopy(DAGPtr::All(new_all), *parent, should_count)
            }
          }
        }
      }
      ParentPtr::AllImg(link) => {
        let All { copy, uses, dom, parents, .. } = link.as_ref();
        let new_child = match new_child {
          DAGPtr::Lam(link) => link,
          _ => panic!("Cannot install a non-lambda node as image"),
        };
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).img = new_child;
          }
          None => {
            let new_all = alloc_all(*uses, *dom, new_child, None);
            (*link.as_ptr()).copy = Some(new_all);
            for parent in DLL::iter_option(*parents) {
              upcopy(DAGPtr::All(new_all), *parent, should_count)
            }
          }
        }
      }
      ParentPtr::LetTyp(link) => {
        let Let { copy, uses, exp, bod, parents, .. } = link.as_ref();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).typ = new_child;
          }
          None => {
            let new_let = alloc_let(*uses, new_child, *exp, *bod, None);
            (*link.as_ptr()).copy = Some(new_let);
            for parent in DLL::iter_option(*parents) {
              upcopy(DAGPtr::Let(new_let), *parent, should_count)
            }
          }
        }
      }
      ParentPtr::LetExp(link) => {
        let Let { copy, uses, typ, bod, parents, .. } = link.as_ref();
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).exp = new_child;
          }
          None => {
            let new_let = alloc_let(*uses, *typ, new_child, *bod, None);
            (*link.as_ptr()).copy = Some(new_let);
            for parent in DLL::iter_option(*parents) {
              upcopy(DAGPtr::Let(new_let), *parent, should_count)
            }
          }
        }
      }
      ParentPtr::LetBod(link) => {
        let Let { copy, uses, typ, exp, parents, .. } = link.as_ref();
        let new_child = match new_child {
          DAGPtr::Lam(link) => link,
          _ => panic!("Cannot install a non-lambda node as image"),
        };
        match copy {
          Some(cache) => {
            (*cache.as_ptr()).bod = new_child;
          }
          None => {
            let new_let = alloc_let(*uses, *typ, *exp, new_child, None);
            (*link.as_ptr()).copy = Some(new_let);
            for parent in DLL::iter_option(*parents) {
              upcopy(DAGPtr::Let(new_let), *parent, should_count)
            }
          }
        }
      }
      ParentPtr::Root => (),
    }
  }
}
