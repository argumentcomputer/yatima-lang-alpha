use crate::core::{
  dag::*,
  dll::*,
};

use core::ptr::NonNull;

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
          let All { var, dom, img, dom_ref, img_ref, .. } =
            &mut *all_copy.as_ptr();
          all.copy = None;
          add_to_parents(*dom, NonNull::new(dom_ref).unwrap());
          add_to_parents(*img, NonNull::new(img_ref).unwrap());
          for parent in DLL::iter_option(var.parents) {
            clean_up(parent);
          }
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
          let Let { var, typ, exp, bod, typ_ref, exp_ref, bod_ref, .. } =
            &mut *let_copy.as_ptr();
          let_.copy = None;
          add_to_parents(*typ, NonNull::new(typ_ref).unwrap());
          add_to_parents(*exp, NonNull::new(exp_ref).unwrap());
          add_to_parents(*bod, NonNull::new(bod_ref).unwrap());
          for parent in DLL::iter_option(var.parents) {
            clean_up(parent);
          }
          for parent in DLL::iter_option(let_.parents) {
            clean_up(parent);
          }
        }
    },
    ParentPtr::Root => (),
  }
}

// The core up-copy function.
pub fn upcopy(new_child: DAGPtr, cc: ParentPtr) {
  unsafe {
    match cc {
      ParentPtr::LamBod(link) => {
        let Lam { var, parents, .. } = link.as_ref();
        let Var { nam, dep, parents: var_parents, .. } = var;
        let new_lam = alloc_lam(nam.clone(), *dep, None, new_child, None);
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
        let Var { nam, dep, parents: var_parents, .. } = var;
        let new_slf = alloc_slf(nam.clone(), *dep, None, new_child, None);
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
            let Var { nam, dep, parents: var_parents, .. } = var;
            let new_all =
              alloc_all(nam.clone(), *dep, None, *uses, new_child, *img, None);
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
            let Var { nam, dep, parents: var_parents, .. } = var;
            let new_all =
              alloc_all(nam.clone(), *dep, None, *uses, *dom, new_child, None);
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
            let Var { nam, dep, parents: var_parents, .. } = var;
            let new_let = alloc_let(
              nam.clone(),
              *dep,
              None,
              *uses,
              new_child,
              *typ,
              *bod,
              None,
            );
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
            let Var { nam, dep, parents: var_parents, .. } = var;
            let new_let = alloc_let(
              nam.clone(),
              *dep,
              None,
              *uses,
              *exp,
              new_child,
              *bod,
              None,
            );
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
            let Var { nam, dep, parents: var_parents, .. } = var;
            let new_let = alloc_let(
              nam.clone(),
              *dep,
              None,
              *uses,
              *exp,
              *typ,
              new_child,
              None,
            );
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
