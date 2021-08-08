use crate::unify::dag::*;
use yatima_core::dll::*;

use core::{
  ptr::NonNull,
  sync::atomic::{
    AtomicUsize,
    Ordering,
  },
};

pub static UPCOPY_COUNT: AtomicUsize = AtomicUsize::new(0);

pub fn clean_up(cc: &ParentPtr) {
  let mut stack = vec![cc];
  while let Some(cc) = stack.pop() {
    match cc {
      ParentPtr::Bound(link) => unsafe {
        let Bind { parents, var, .. } = link.as_ref();
        for parent in DLL::iter_option(var.parents) {
          stack.push(parent);
        }
        for parent in DLL::iter_option(*parents) {
          stack.push(parent);
        }
      },
      ParentPtr::SlfBod(link) => unsafe {
        let Slf { parents, var, .. } = link.as_ref();
        for parent in DLL::iter_option(var.parents) {
          stack.push(parent);
        }
        for parent in DLL::iter_option(*parents) {
          stack.push(parent);
        }
      },
      ParentPtr::FixBod(link) => unsafe {
        let Fix { parents, var, .. } = link.as_ref();
        for parent in DLL::iter_option(var.parents) {
          stack.push(parent);
        }
        for parent in DLL::iter_option(*parents) {
          stack.push(parent);
        }
      },
      ParentPtr::DatTyp(mut link) | ParentPtr::DatBod(mut link) => unsafe {
        let dat = link.as_mut();
        if let Some(dat_copy) = dat.copy {
          let Dat { typ, bod, typ_ref, bod_ref, .. } = &mut *dat_copy.as_ptr();
          dat.copy = None;
          add_to_parents(
            *typ,
            NonNull::new(typ_ref.as_mut().unwrap()).unwrap(),
          );
          add_to_parents(
            *bod,
            NonNull::new(bod_ref.as_mut().unwrap()).unwrap(),
          );
          for parent in DLL::iter_option(dat.parents) {
            stack.push(parent);
          }
        }
      },
      ParentPtr::CseBod(link) => unsafe {
        let Cse { parents, .. } = link.as_ref();
        for parent in DLL::iter_option(*parents) {
          stack.push(parent);
        }
      },
      ParentPtr::AppFun(mut link)
      | ParentPtr::AppTyp(mut link)
      | ParentPtr::AppArg(mut link) => unsafe {
        let app = link.as_mut();
        if let Some(app_copy) = app.copy {
          let App { fun, arg, typ, fun_ref, typ_ref, arg_ref, .. } =
            &mut *app_copy.as_ptr();
          app.copy = None;
          add_to_parents(
            *fun,
            NonNull::new(fun_ref.as_mut().unwrap()).unwrap(),
          );
          add_to_parents(
            *typ,
            NonNull::new(typ_ref.as_mut().unwrap()).unwrap(),
          );
          add_to_parents(
            *arg,
            NonNull::new(arg_ref.as_mut().unwrap()).unwrap(),
          );
          for parent in DLL::iter_option(app.parents) {
            stack.push(parent);
          }
        }
      },
      ParentPtr::LamTyp(mut link) | ParentPtr::LamBod(mut link) => unsafe {
        let lam = link.as_mut();
        if let Some(lam_copy) = lam.copy {
          let Lam { typ, bod, typ_ref, bod_ref, .. } = &mut *lam_copy.as_ptr();
          lam.copy = None;
          add_to_parents(
            *typ,
            NonNull::new(typ_ref.as_mut().unwrap()).unwrap(),
          );
          add_to_parents(
            DAGPtr::Bind(*bod),
            NonNull::new(bod_ref.as_mut().unwrap()).unwrap(),
          );
          for parent in DLL::iter_option(lam.parents) {
            stack.push(parent);
          }
        }
      },
      ParentPtr::AllDom(mut link) | ParentPtr::AllImg(mut link) => unsafe {
        let all = link.as_mut();
        if let Some(all_copy) = all.copy {
          let All { dom, img, dom_ref, img_ref, .. } = &mut *all_copy.as_ptr();
          all.copy = None;
          add_to_parents(
            *dom,
            NonNull::new(dom_ref.as_mut().unwrap()).unwrap(),
          );
          add_to_parents(
            DAGPtr::Bind(*img),
            NonNull::new(img_ref.as_mut().unwrap()).unwrap(),
          );
          for parent in DLL::iter_option(all.parents) {
            stack.push(parent);
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
          add_to_parents(
            *typ,
            NonNull::new(typ_ref.as_mut().unwrap()).unwrap(),
          );
          add_to_parents(
            *exp,
            NonNull::new(exp_ref.as_mut().unwrap()).unwrap(),
          );
          add_to_parents(
            DAGPtr::Bind(*bod),
            NonNull::new(bod_ref.as_mut().unwrap()).unwrap(),
          );
          for parent in DLL::iter_option(let_.parents) {
            stack.push(parent);
          }
        }
      },
      ParentPtr::Root => (),
    }
  }
}

// The core up-copy function.
pub fn upcopy(new_child: DAGPtr, cc: ParentPtr, should_count: bool) {
  unsafe {
    let mut stack = vec![(new_child, cc)];
    while let Some((new_child, cc)) = stack.pop() {
      if should_count {
        UPCOPY_COUNT.fetch_add(1, Ordering::SeqCst);
      }
      match cc {
        ParentPtr::Bound(link) => {
          let Bind { var, parents, .. } = link.as_ref();
          let Var { nam, dep, parents: var_parents, .. } = var;
          let new_bind = alloc_bind(nam.clone(), *dep, new_child, None);
          let ptr: *mut Parents =
            &mut *(*new_bind.as_ptr()).bod_ref.as_mut().unwrap();
          add_to_parents(new_child, NonNull::new(ptr).unwrap());
          let ptr: *mut Var = &mut (*new_bind.as_ptr()).var;
          for parent in DLL::iter_option(*var_parents) {
            stack.push((DAGPtr::Var(NonNull::new(ptr).unwrap()), *parent))
          }
          for parent in DLL::iter_option(*parents) {
            stack.push((DAGPtr::Bind(new_bind), *parent))
          }
        }
        ParentPtr::SlfBod(link) => {
          let Slf { var, parents, .. } = link.as_ref();
          let Var { nam, dep, parents: var_parents, .. } = var;
          let new_slf = alloc_slf(nam.clone(), *dep, new_child, None);
          let ptr: *mut Parents =
            &mut *(*new_slf.as_ptr()).bod_ref.as_mut().unwrap();
          add_to_parents(new_child, NonNull::new(ptr).unwrap());
          let ptr: *mut Var = &mut (*new_slf.as_ptr()).var;
          for parent in DLL::iter_option(*var_parents) {
            stack.push((DAGPtr::Var(NonNull::new(ptr).unwrap()), *parent))
          }
          for parent in DLL::iter_option(*parents) {
            stack.push((DAGPtr::Slf(new_slf), *parent))
          }
        }
        ParentPtr::FixBod(link) => {
          let Fix { var, parents, .. } = link.as_ref();
          let Var { nam, dep, parents: var_parents, .. } = var;
          let new_fix = alloc_fix(nam.clone(), *dep, new_child, None);
          let ptr: *mut Parents =
            &mut *(*new_fix.as_ptr()).bod_ref.as_mut().unwrap();
          add_to_parents(new_child, NonNull::new(ptr).unwrap());
          let ptr: *mut Var = &mut (*new_fix.as_ptr()).var;
          for parent in DLL::iter_option(*var_parents) {
            stack.push((DAGPtr::Var(NonNull::new(ptr).unwrap()), *parent))
          }
          for parent in DLL::iter_option(*parents) {
            stack.push((DAGPtr::Fix(new_fix), *parent))
          }
        }
        ParentPtr::CseBod(link) => {
          let Cse { parents, .. } = link.as_ref();
          let new_cse = alloc_cse(new_child, None);
          let ptr: *mut Parents =
            &mut *(*new_cse.as_ptr()).bod_ref.as_mut().unwrap();
          add_to_parents(new_child, NonNull::new(ptr).unwrap());
          for parent in DLL::iter_option(*parents) {
            stack.push((DAGPtr::Cse(new_cse), *parent))
          }
        }
        ParentPtr::DatTyp(link) => {
          let Dat { copy, bod, parents, .. } = link.as_ref();
          match copy {
            Some(cache) => {
              (*cache.as_ptr()).typ = new_child;
            }
            None => {
              let new_dat = alloc_dat(new_child, *bod, None);
              (*link.as_ptr()).copy = Some(new_dat);
              for parent in DLL::iter_option(*parents) {
                stack.push((DAGPtr::Dat(new_dat), *parent))
              }
            }
          }
        }
        ParentPtr::DatBod(link) => {
          let Dat { copy, typ, parents, .. } = link.as_ref();
          match copy {
            Some(cache) => {
              (*cache.as_ptr()).bod = new_child;
            }
            None => {
              let new_dat = alloc_dat(*typ, new_child, None);
              (*link.as_ptr()).copy = Some(new_dat);
              for parent in DLL::iter_option(*parents) {
                stack.push((DAGPtr::Dat(new_dat), *parent))
              }
            }
          }
        }
        ParentPtr::AppFun(link) => {
          let App { uses, copy, typ, arg, parents, .. } = link.as_ref();
          match copy {
            Some(cache) => {
              (*cache.as_ptr()).fun = new_child;
            }
            None => {
              let new_app =
                alloc_app(uses.clone(), new_child, *typ, *arg, None);
              (*link.as_ptr()).copy = Some(new_app);
              for parent in DLL::iter_option(*parents) {
                stack.push((DAGPtr::App(new_app), *parent))
              }
            }
          }
        }
        ParentPtr::AppTyp(link) => {
          let App { uses, copy, fun, arg, parents, .. } = link.as_ref();
          match copy {
            Some(cache) => {
              (*cache.as_ptr()).typ = new_child;
            }
            None => {
              let new_app =
                alloc_app(uses.clone(), *fun, new_child, *arg, None);
              (*link.as_ptr()).copy = Some(new_app);
              for parent in DLL::iter_option(*parents) {
                stack.push((DAGPtr::App(new_app), *parent))
              }
            }
          }
        }
        ParentPtr::AppArg(link) => {
          let App { uses, copy, fun, typ, parents, .. } = link.as_ref();
          match copy {
            Some(cache) => {
              (*cache.as_ptr()).arg = new_child;
            }
            None => {
              let new_app =
                alloc_app(uses.clone(), *fun, *typ, new_child, None);
              (*link.as_ptr()).copy = Some(new_app);
              for parent in DLL::iter_option(*parents) {
                stack.push((DAGPtr::App(new_app), *parent))
              }
            }
          }
        }
        ParentPtr::LamTyp(link) => {
          let Lam { copy, uses, bod, parents, .. } = link.as_ref();
          match copy {
            Some(cache) => {
              (*cache.as_ptr()).typ = new_child;
            }
            None => {
              let new_lam = alloc_lam(uses.clone(), new_child, *bod, None);
              (*link.as_ptr()).copy = Some(new_lam);
              for parent in DLL::iter_option(*parents) {
                stack.push((DAGPtr::Lam(new_lam), *parent))
              }
            }
          }
        }
        ParentPtr::LamBod(link) => {
          let Lam { copy, uses, typ, parents, .. } = link.as_ref();
          let new_child = match new_child {
            DAGPtr::Bind(link) => link,
            _ => panic!("Cannot install a non-bind node as lambda body"),
          };
          match copy {
            Some(cache) => {
              (*cache.as_ptr()).bod = new_child;
            }
            None => {
              let new_lam = alloc_lam(uses.clone(), *typ, new_child, None);
              (*link.as_ptr()).copy = Some(new_lam);
              for parent in DLL::iter_option(*parents) {
                stack.push((DAGPtr::Lam(new_lam), *parent))
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
              let new_all = alloc_all(uses.clone(), new_child, *img, None);
              (*link.as_ptr()).copy = Some(new_all);
              for parent in DLL::iter_option(*parents) {
                stack.push((DAGPtr::All(new_all), *parent))
              }
            }
          }
        }
        ParentPtr::AllImg(link) => {
          let All { copy, uses, dom, parents, .. } = link.as_ref();
          let new_child = match new_child {
            DAGPtr::Bind(link) => link,
            _ => panic!("Cannot install a non-binder node as forall image"),
          };
          match copy {
            Some(cache) => {
              (*cache.as_ptr()).img = new_child;
            }
            None => {
              let new_all = alloc_all(uses.clone(), *dom, new_child, None);
              (*link.as_ptr()).copy = Some(new_all);
              for parent in DLL::iter_option(*parents) {
                stack.push((DAGPtr::All(new_all), *parent))
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
              let new_let =
                alloc_let(uses.clone(), new_child, *exp, *bod, None);
              (*link.as_ptr()).copy = Some(new_let);
              for parent in DLL::iter_option(*parents) {
                stack.push((DAGPtr::Let(new_let), *parent))
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
              let new_let =
                alloc_let(uses.clone(), *typ, new_child, *bod, None);
              (*link.as_ptr()).copy = Some(new_let);
              for parent in DLL::iter_option(*parents) {
                stack.push((DAGPtr::Let(new_let), *parent))
              }
            }
          }
        }
        ParentPtr::LetBod(link) => {
          let Let { copy, uses, typ, exp, parents, .. } = link.as_ref();
          let new_child = match new_child {
            DAGPtr::Bind(link) => link,
            _ => panic!("Cannot install a non-binder node as let body"),
          };
          match copy {
            Some(cache) => {
              (*cache.as_ptr()).bod = new_child;
            }
            None => {
              let new_let =
                alloc_let(uses.clone(), *typ, *exp, new_child, None);
              (*link.as_ptr()).copy = Some(new_let);
              for parent in DLL::iter_option(*parents) {
                stack.push((DAGPtr::Let(new_let), *parent))
              }
            }
          }
        }
        ParentPtr::Root => (),
      }
    }
  }
}
