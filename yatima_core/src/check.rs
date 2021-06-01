pub mod ctx;
pub mod error;

use ctx::*;
use error::CheckError;

use crate::{
  dag::*,
  defs::Defs,
  dll::*,
  literal::Literal,
  position::Pos,
  term::Term,
  uses::*,
  yatima,
};

use std::collections::{
  HashMap,
  VecDeque,
};

use libipld::Cid;

use core::ptr::NonNull;
use std::collections::BTreeSet;

pub fn hash(dag: DAGPtr, dep: u64) -> Cid {
  let mut map = HashMap::new();
  DAG::dag_ptr_to_term(&dag, &mut map, dep, true).embed().0.cid()
}

pub fn equal(defs: &Defs, a: &mut DAG, b: &mut DAG, dep: u64) -> bool {
  a.whnf(defs);
  b.whnf(defs);
  let mut triples = vec![(a.head, b.head, dep)];
  let mut set: BTreeSet<(Cid, Cid)> = BTreeSet::new();
  while let Some((a, b, dep)) = triples.pop() {
    let mut a = DAG::new(a);
    let mut b = DAG::new(b);
    a.whnf(defs);
    b.whnf(defs);
    let hash_a = hash(a.head, dep);
    let hash_b = hash(b.head, dep);
    let eq = hash_a == hash_b
      || set.contains(&(hash_a, hash_b))
      || set.contains(&(hash_b, hash_a));
    set.insert((hash_a, hash_b));
    if !eq {
      match (a.head, b.head) {
        (DAGPtr::Lam(a_link), DAGPtr::Lam(b_link)) => unsafe {
          let Lam { bod: a_bod, .. } = *a_link.as_ptr();
          let Lam { bod: b_bod, .. } = *b_link.as_ptr();
          triples.push((a_bod, b_bod, dep + 1));
        },
        (DAGPtr::Slf(a_link), DAGPtr::Slf(b_link)) => unsafe {
          let Slf { bod: a_bod, .. } = *a_link.as_ptr();
          let Slf { bod: b_bod, .. } = *b_link.as_ptr();
          triples.push((a_bod, b_bod, dep + 1));
        },
        (DAGPtr::Cse(a_link), DAGPtr::Cse(b_link)) => unsafe {
          let Cse { bod: a_bod, .. } = *a_link.as_ptr();
          let Cse { bod: b_bod, .. } = *b_link.as_ptr();
          triples.push((a_bod, b_bod, dep));
        },
        (DAGPtr::Dat(a_link), DAGPtr::Dat(b_link)) => unsafe {
          let Dat { bod: a_bod, .. } = *a_link.as_ptr();
          let Dat { bod: b_bod, .. } = *b_link.as_ptr();
          triples.push((a_bod, b_bod, dep));
        },
        (DAGPtr::All(a_link), DAGPtr::All(b_link)) => unsafe {
          let All { uses: a_uses, dom: a_dom, img: a_img, .. } =
            *a_link.as_ptr();
          let All { uses: b_uses, dom: b_dom, img: b_img, .. } =
            *b_link.as_ptr();
          let a_img = DAGPtr::Lam(a_img);
          let b_img = DAGPtr::Lam(b_img);
          if a_uses != b_uses {
            return false;
          }
          triples.push((a_dom, b_dom, dep));
          triples.push((a_img, b_img, dep + 1));
        },
        (DAGPtr::App(a_link), DAGPtr::App(b_link)) => unsafe {
          let App { fun: a_fun, arg: a_arg, .. } = *a_link.as_ptr();
          let App { fun: b_fun, arg: b_arg, .. } = *b_link.as_ptr();
          triples.push((a_fun, b_fun, dep));
          triples.push((a_arg, b_arg, dep));
        },
        _ => return false,
      }
    }
  }
  true
}

pub fn check(
  rec: &Option<(String, Cid, Cid)>,
  defs: &Defs,
  ctx: &mut Ctx,
  uses: Uses,
  term: &Term,
  typ: &mut DAG,
) -> Result<(), CheckError> {
  match term {
    Term::Lam(pos, _, lam_bod) => {
      let lam_bod = &**lam_bod;
      typ.whnf(defs);
      match typ.head {
        DAGPtr::All(all_link) => {
          let All { uses: lam_uses, dom, img, .. } =
            unsafe { &mut *all_link.as_ptr() };
          let Lam { var: all_var, bod: img, .. } =
            unsafe { &mut *img.as_ptr() };
          // Annotate the depth of the node that binds each variable
          (*all_var).dep = ctx.len() as u64;
          // Add the domain of the function to the context
          let mut img = DAG::new(*img);
          let mut bod_ctx = ctx.clone();
          div_use_ctx(uses, &mut bod_ctx);
          bod_ctx.push((all_var.nam.to_string(), *lam_uses, dom));
          check(rec, defs, &mut bod_ctx, Uses::Once, lam_bod, &mut img)?;
          let (_, rest, _) = bod_ctx.last().unwrap();
          // Have to check whether the rest 'contains' zero (i.e., zero is less
          // than or equal to the rest), otherwise the variable was
          // not used enough
          if !Uses::lte(Uses::None, *rest) {
            Err(CheckError::QuantityTooLittle(
              *pos,
              error_context(&bod_ctx),
              all_var.nam.clone(),
              *lam_uses,
              *rest,
            ))
          }
          else {
            lam_rule(uses, ctx, &bod_ctx);
            Ok(())
          }
        }
        _ => {
          let checked = term.clone();
          let against = typ.to_term(false);
          Err(CheckError::LamAllMismatch(
            *pos,
            error_context(&ctx),
            checked,
            against,
          ))
        }
      }
    }
    Term::Dat(pos, dat_bod) => {
      let dat_bod = &**dat_bod;
      typ.whnf(defs);
      match typ.head {
        DAGPtr::Slf(slf_link) => {
          let Slf { var, bod: slf_bod, .. } =
            unsafe { &mut *slf_link.as_ptr() };
          let mut map = HashMap::new();
          if var.parents.is_some() {
            map.insert(
              DAGPtr::Var(NonNull::new(var).unwrap()),
              DAG::from_term_inner(
                term,
                ctx.len() as u64,
                VecDeque::new(),
                None,
                rec.clone(),
              ),
            );
          }
          let root = alloc_val(DLL::singleton(ParentPtr::Root));
          let mut new_bod =
            DAG::new(DAG::from_subdag(*slf_bod, &mut map, Some(root)));
          check(rec, defs, ctx, uses, dat_bod, &mut new_bod)?;
          new_bod.free();
          Ok(())
        }
        _ => {
          let checked = term.clone();
          let against = typ.to_term(false);
          Err(CheckError::DatSlfMismatch(
            *pos,
            error_context(&ctx),
            checked,
            against,
          ))
        }
      }
    }
    Term::Let(pos, ..) => Err(CheckError::GenericError(
      *pos,
      error_context(&ctx),
      "TODO: Let typecheck".to_string(),
    )),
    _ => {
      let depth = ctx.len();
      // TODO Should we clone ctx?
      let mut infer_typ = infer(rec, defs, ctx, uses, term)?;
      let eq = equal(defs, typ, &mut infer_typ, depth as u64);
      if eq {
        infer_typ.free();
        Ok(())
      }
      else {
        let expected = typ.to_term(false);
        let detected = infer_typ.to_term(false);
        infer_typ.free();
        Err(CheckError::TypeMismatch(
          term.pos(),
          error_context(&ctx),
          expected,
          detected,
        ))
      }
    }
  }
}

pub fn infer(
  rec: &Option<(String, Cid, Cid)>,
  defs: &Defs,
  ctx: &mut Ctx,
  uses: Uses,
  term: &Term,
) -> Result<DAG, CheckError> {
  match term {
    Term::Rec(_) => {
      if let Some((nam, exp, _)) = rec {
        if let Some(def) = defs.defs.get(exp) {
          Ok(DAG::from_term(&def.typ_))
        }
        else {
          panic!("undefined runtime reference: {}, {}", nam, exp);
        }
      }
      else {
        panic!("undefined recursion");
      }
    }
    Term::Var(pos, nam, idx) => {
      let dep = ctx.len() - 1 - (*idx as usize);
      let bind = ctx.get(dep).ok_or_else(|| {
        CheckError::UnboundVariable(
          *pos,
          error_context(&ctx),
          nam.clone(),
          dep as u64,
        )
      })?;
      let subtract_use = (bind.1 - uses).ok_or_else(|| {
        CheckError::QuantityTooMuch(
          *pos,
          error_context(&ctx),
          nam.clone(),
          bind.1,
          uses,
        )
      })?;
      let bind = &mut ctx[dep];
      bind.1 = subtract_use;
      let typ = unsafe {
        let dag = DAG::new(*bind.2);
        #[allow(clippy::redundant_clone)]
        dag.clone()
      };
      Ok(typ)
    }
    Term::Ref(pos, nam, def_link, _) => {
      let def = defs
        .defs
        .get(def_link)
        .ok_or_else(|| CheckError::UndefinedReference(*pos, nam.clone()))?;
      let typ = DAG::from_term(&def.typ_);
      Ok(typ)
    }
    Term::App(pos, fun_arg) => {
      let (fun, arg) = &**fun_arg;
      let mut fun_typ = infer(rec, defs, ctx, uses, fun)?;
      fun_typ.whnf(defs);
      match fun_typ.head {
        DAGPtr::All(link) => {
          let All { uses: lam_uses, dom, img, .. } =
            unsafe { &mut *link.as_ptr() };
          let Lam { var, bod: img, .. } = unsafe { &mut *img.as_ptr() };
          check(rec, defs, ctx, *lam_uses * uses, arg, &mut DAG::new(*dom))?;
          let mut map = HashMap::new();
          if var.parents.is_some() {
            map.insert(
              DAGPtr::Var(NonNull::new(var).unwrap()),
              DAG::from_term_inner(
                arg,
                ctx.len() as u64,
                VecDeque::new(),
                None,
                rec.clone(),
              ),
            );
          }
          let root = alloc_val(DLL::singleton(ParentPtr::Root));
          let new_img = DAG::from_subdag(*img, &mut map, Some(root));
          fun_typ.free();
          Ok(DAG::new(new_img))
        }
        _ => Err(CheckError::AppFunMismatch(
          *pos,
          error_context(&ctx),
          fun.clone(),
          fun_typ.to_term(false),
        )),
      }
    }
    Term::Cse(pos, exp) => {
      let exp = &**exp;
      let mut exp_typ = infer(rec, defs, ctx, uses, exp)?;
      exp_typ.whnf(defs);
      match exp_typ.head {
        DAGPtr::Slf(link) => {
          let Slf { var, bod, .. } = unsafe { &mut *link.as_ptr() };
          let mut map = HashMap::new();
          if var.parents.is_some() {
            map.insert(
              DAGPtr::Var(NonNull::new(var).unwrap()),
              DAG::from_term_inner(
                exp,
                ctx.len() as u64,
                VecDeque::new(),
                None,
                rec.clone(),
              ),
            );
          }
          let root = alloc_val(DLL::singleton(ParentPtr::Root));
          let new_bod = DAG::from_subdag(*bod, &mut map, Some(root));
          exp_typ.free();
          Ok(DAG::new(new_bod))
        }
        DAGPtr::LTy(link) => {
          let LTy { lty, .. } = unsafe { &mut *link.as_ptr() };
          let root = alloc_val(DLL::singleton(ParentPtr::Root));
          match lty.induction(exp.clone()) {
            None => Err(CheckError::NonInductiveLitType(
              *pos,
              error_context(&ctx),
              *lty,
            )),
            Some(ind) => {
              let induction = DAG::from_term_inner(
                &ind,
                0,
                VecDeque::new(),
                Some(root),
                None,
              );
              Ok(DAG::new(induction))
            }
          }
        }
        _ => Err(CheckError::CseDatMismatch(
          *pos,
          error_context(&ctx),
          exp.clone(),
          exp_typ.to_term(false),
        )),
      }
    }
    Term::All(_, _, nam, dom_img) => {
      let (dom, img) = &**dom_img;
      let mut typ = DAG::from_term(&Term::Typ(Pos::None));
      check(rec, defs, ctx, Uses::None, dom, &mut typ)?;
      let mut dom_dag = DAG::from_term_inner(
        dom,
        ctx.len() as u64,
        VecDeque::new(),
        None,
        rec.clone(),
      );
      ctx.push((nam.to_string(), Uses::None, &mut dom_dag));
      check(rec, defs, ctx, Uses::None, img, &mut typ)?;
      ctx.pop();
      free_dead_node(dom_dag);
      Ok(typ)
    }
    Term::Slf(_, nam, bod) => {
      let bod = &**bod;
      let mut typ = DAG::from_term(&Term::Typ(Pos::None));
      let mut term_dag = DAG::from_term_inner(
        term,
        ctx.len() as u64,
        VecDeque::new(),
        None,
        rec.clone(),
      );
      ctx.push((nam.to_string(), Uses::None, &mut term_dag));
      check(rec, defs, ctx, Uses::None, bod, &mut typ)?;
      ctx.pop();
      free_dead_node(term_dag);
      Ok(typ)
    }
    Term::Typ(_) => {
      let typ = DAG::from_term(&Term::Typ(Pos::None));
      Ok(typ)
    }
    Term::Ann(_, typ_exp) => {
      let (typ, exp) = &**typ_exp;
      let root = alloc_val(DLL::singleton(ParentPtr::Root));
      let mut typ_dag = DAG::new(DAG::from_term_inner(
        typ,
        ctx.len() as u64,
        VecDeque::new(),
        Some(root),
        rec.clone(),
      ));
      check(rec, defs, ctx, uses, exp, &mut typ_dag)?;
      Ok(typ_dag)
    }
    Term::Let(..) => {
      panic!("TODO: Let inference")
    }
    Term::Lit(_, lit) => Ok(DAG::from_term(&infer_lit(lit.to_owned()))),
    Term::LTy(..) => Ok(DAG::from_term(&yatima!("Type"))),
    Term::Opr(_, opr) => Ok(DAG::from_term(&opr.type_of())),
    Term::Lam(..) => {
      Err(CheckError::UntypedLambda(term.pos(), error_context(&ctx)))
    }
    Term::Dat(..) => {
      Err(CheckError::UntypedData(term.pos(), error_context(&ctx)))
    }
  }
}

pub fn infer_lit(lit: Literal) -> Term {
  match lit {
    Literal::Nat(_) => yatima!("#Nat"),
    Literal::Int(_) => yatima!("#Int"),
    Literal::Bits(_) => yatima!("#Bits"),
    Literal::Bytes(_) => yatima!("#Bytes"),
    Literal::Text(_) => yatima!("#Text"),
    Literal::Char(_) => yatima!("#Char"),
    Literal::Bool(_) => yatima!("#Bool"),
    Literal::U8(_) => yatima!("#U8"),
    Literal::U16(_) => yatima!("#U16"),
    Literal::U32(_) => yatima!("#U32"),
    Literal::U64(_) => yatima!("#U64"),
    Literal::U128(_) => yatima!("#U128"),
    Literal::I8(_) => yatima!("#I8"),
    Literal::I16(_) => yatima!("#I16"),
    Literal::I32(_) => yatima!("#I32"),
    Literal::I64(_) => yatima!("#I64"),
    Literal::I128(_) => yatima!("#I128"),
  }
}

pub fn infer_term(defs: &Defs, term: Term) -> Result<Term, CheckError> {
  let typ_dag = infer(&None, &defs, &mut vec![].into(), Uses::Once, &term)?;
  let typ = DAG::to_term(&typ_dag, true);
  typ_dag.free();
  Ok(typ)
}

pub fn check_def(defs: &Defs, name: &str) -> Result<Term, CheckError> {
  let def = defs.get(&name.to_string()).ok_or_else(|| {
    CheckError::UndefinedReference(Pos::None, name.to_owned())
  })?;
  let (d, _, a) = def.embed();
  let def_cid = d.cid();
  let ast_cid = a.cid();
  let rec = Some((name.to_owned(), def_cid, ast_cid));
  let mut typ = DAG::from_term(&def.typ_);
  check(&rec, &defs, &mut vec![].into(), Uses::Once, &def.term, &mut typ)?;
  typ.free();
  Ok(def.typ_.clone())
}
