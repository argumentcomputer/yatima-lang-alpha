pub mod ctx;
pub mod error;

use ctx::*;
use error::CheckError;

use crate::{
  dag::*,
  defs,
  defs::Defs,
  dll::*,
  literal::Literal,
  name::Name,
  parse,
  position::Pos,
  term::Term,
  uses::*,
  yatima,
};

use sp_cid::Cid;

use core::ptr::NonNull;

use alloc::string::ToString;
use sp_std::{
  borrow::ToOwned,
  collections::{
    btree_map::BTreeMap,
    btree_set::BTreeSet,
  },
  mem,
  rc::Rc,
};

/// Generates a content id for a DAG pointer's anonymous term
pub fn hash(dag: DAGPtr, dep: u64) -> Cid {
  let mut map = BTreeMap::new();
  DAG::dag_ptr_to_term(&dag, &mut map, dep, true).embed().0.cid()
}

/// Lazily checks if two DAGs are beta equivalent. 
pub fn equal(
  defs: &Defs,
  a: &mut DAG,
  b: &mut DAG,
  dep: u64,
  should_count: bool,
) -> bool {
  a.whnf(defs, should_count);
  b.whnf(defs, should_count);
  let mut triples = vec![(a.head, b.head, dep)];
  let mut set: BTreeSet<(Cid, Cid)> = BTreeSet::new();
  while let Some((a, b, dep)) = triples.pop() {
    let mut a = DAG::new(a);
    let mut b = DAG::new(b);
    a.whnf(defs, should_count);
    b.whnf(defs, should_count);
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

/// Typechecks a term, relying on type inference as needed
pub fn check(
  rec: &Option<(Name, Cid, Cid)>,
  defs: &Defs,
  ctx: &mut Ctx,
  uses: Uses,
  term: &Term,
  typ: &mut DAG,
  should_count: bool,
) -> Result<(), CheckError> {
  match term {
    Term::Lam(pos, _, bod) => {
      check_lam(rec, defs, ctx, uses, term, typ, pos, &**bod, should_count)
    }
    Term::Dat(pos, bod) => {
      check_dat(rec, defs, ctx, uses, term, typ, pos, &**bod, should_count)
    }
    _ => {
      let depth = ctx.len();
      // TODO Should we clone ctx?
      let mut detected_typ = infer(rec, defs, ctx, uses, term, should_count)?;
      if equal(defs, typ, &mut detected_typ, depth as u64, should_count) {
        detected_typ.free();
        Ok(())
      }
      else {
        let expected = typ.to_term(false);
        let detected = detected_typ.to_term(false);
        detected_typ.free();
        Err(CheckError::TypeMismatch(
          term.pos(),
          error_context(ctx),
          expected,
          detected,
        ))
      }
    }
  }
}

/// Typechecks a λ term
#[inline]
pub fn check_lam(
  rec: &Option<(Name, Cid, Cid)>,
  defs: &Defs,
  ctx: &mut Ctx,
  uses: Uses,
  term: &Term,
  typ: &mut DAG,
  pos: &Pos,
  bod: &Term,
  should_count: bool,
) -> Result<(), CheckError> {
  // To check whether a lambda is well typed, its type must reduce to a forall;
  // otherwise we fail
  typ.whnf(defs, should_count);
  match typ.head {
    DAGPtr::All(all_link) => {
      // Extract the domain and image of the function and also the variable that
      // is dependently bound.
      let All { uses: lam_uses, dom, img, .. } =
        unsafe { &mut *all_link.as_ptr() };
      let Lam { var: all_var, bod: img, .. } = unsafe { &mut *img.as_ptr() };
      // Annotate the depth of the node that binds each variable. This is needed
      // to decide whether two variables of distinct DAGs are actually the
      // same.
      (*all_var).dep = ctx.len() as u64;
      // Adjust the context multiplicity, add the argument to the context and
      // check the body
      let rest_ctx = div_ctx(uses, ctx);
      ctx.push((all_var.nam.to_string(), *lam_uses, dom));
      let mut img = DAG::new(*img);
      check(rec, defs, ctx, Uses::Once, bod, &mut img, should_count)?;
      // Check whether the rest 'contains' zero (i.e., zero is less than or
      // equal to the rest), otherwise the variable was not used enough
      let (_, rest, _) = ctx.last().unwrap();
      if !Uses::lte(Uses::None, *rest) {
        Err(CheckError::QuantityTooLittle(
          *pos,
          error_context(ctx),
          all_var.nam.to_string(),
          *lam_uses,
          *rest,
        ))
      }
      else {
        // Remove the argument from the context, readjust the context
        // multiplicity
        ctx.pop();
        add_mul_ctx(uses, ctx, rest_ctx);
        Ok(())
      }
    }
    _ => {
      let checked = term.clone();
      let against = typ.to_term(false);
      Err(CheckError::LamAllMismatch(
        *pos,
        error_context(ctx),
        checked,
        against,
      ))
    }
  }
}

/// Typechecks data as a self type
#[inline]
pub fn check_dat(
  rec: &Option<(Name, Cid, Cid)>,
  defs: &Defs,
  ctx: &mut Ctx,
  uses: Uses,
  term: &Term,
  typ: &mut DAG,
  pos: &Pos,
  bod: &Term,
  should_count: bool,
) -> Result<(), CheckError> {
  // To check whether data is well typed, its type must reduce to a self type;
  // otherwise we fail
  typ.whnf(defs, should_count);
  match typ.head {
    DAGPtr::Slf(slf_link) => {
      // Extract the body of the self type
      let Slf { var, bod: slf_bod, .. } = unsafe { &mut *slf_link.as_ptr() };
      // The type of the body of the data must be the body of the self with term
      // substituted for its variable. To do this we copy the body of the
      // self with a mapping from the variable to the term as a DAG. The
      // copied type must be rooted
      let mut map = BTreeMap::new();
      if var.parents.is_some() {
        map.insert(
          DAGPtr::Var(NonNull::new(var).unwrap()),
          DAG::from_term_inner(
            term,
            ctx.len() as u64,
            BTreeMap::new(),
            None,
            rec.clone(),
          ),
        );
      }
      let root = alloc_val(DLL::singleton(ParentPtr::Root));
      let mut unrolled_typ =
        DAG::new(DAG::from_subdag(*slf_bod, &mut map, Some(root)));
      check(rec, defs, ctx, uses, bod, &mut unrolled_typ, should_count)?;
      // We must free the newly created type as to not leak
      unrolled_typ.free();
      Ok(())
    }
    _ => {
      let checked = term.clone();
      let against = typ.to_term(false);
      Err(CheckError::DatSlfMismatch(
        *pos,
        error_context(ctx),
        checked,
        against,
      ))
    }
  }
}

/// Infers the type of a term and returns the type DAG
pub fn infer(
  rec: &Option<(Name, Cid, Cid)>,
  defs: &Defs,
  ctx: &mut Ctx,
  uses: Uses,
  term: &Term,
  should_count: bool,
) -> Result<DAG, CheckError> {
  match term {
    Term::Rec(_) => infer_rec(rec, defs),
    Term::Var(pos, nam, idx) => infer_var(rec, defs, ctx, uses, pos, nam, idx),
    Term::Ref(pos, nam, def_link, _) => infer_ref(defs, pos, nam, def_link),
    Term::App(pos, fun_arg) => {
      infer_app(rec, defs, ctx, uses, pos, &fun_arg.0, &fun_arg.1, should_count)
    }
    Term::Cse(pos, exp) => {
      infer_cse(rec, defs, ctx, uses, pos, exp, should_count)
    }
    Term::All(_, _, nam, dom_img) => {
      infer_all(rec, defs, ctx, nam, &dom_img.0, &dom_img.1, should_count)
    }
    Term::Slf(_, nam, bod) => {
      infer_slf(rec, defs, ctx, term, nam, bod, should_count)
    }
    Term::Ann(_, typ_exp) => {
      infer_ann(rec, defs, ctx, uses, &typ_exp.0, &typ_exp.1, should_count)
    }
    Term::Let(pos, false, exp_uses, nam, triple) => infer_let(
      rec,
      defs,
      ctx,
      uses,
      pos,
      *exp_uses,
      nam,
      &triple.0,
      &triple.1,
      &triple.2,
      should_count,
    ),
    Term::Let(pos, true, exp_uses, nam, triple) => infer_letrec(
      rec,
      defs,
      ctx,
      uses,
      pos,
      *exp_uses,
      nam,
      &triple.0,
      &triple.1,
      &triple.2,
      should_count,
    ),
    Term::Typ(_) => {
      let typ = DAG::from_term(&Term::Typ(Pos::None));
      Ok(typ)
    }
    Term::Lit(_, lit) => Ok(DAG::from_term(&infer_lit(lit.to_owned()))),
    Term::LTy(..) => Ok(DAG::from_term(&yatima!("Type"))),
    Term::Opr(_, opr) => Ok(DAG::from_term(&opr.type_of())),
    Term::Lam(..) => {
      Err(CheckError::UntypedLambda(term.pos(), error_context(ctx)))
    }
    Term::Dat(..) => {
      Err(CheckError::UntypedData(term.pos(), error_context(ctx)))
    }
  }
}

/// Infers the type of a `Rec` recursive marker
#[inline]
pub fn infer_rec(
  rec: &Option<(Name, Cid, Cid)>,
  defs: &Defs,
) -> Result<DAG, CheckError> {
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

/// Infers the type of a variable
#[inline]
pub fn infer_var(
  _rec: &Option<(Name, Cid, Cid)>,
  _defs: &Defs,
  ctx: &mut Ctx,
  uses: Uses,
  pos: &Pos,
  nam: &Name,
  idx: &u64,
) -> Result<DAG, CheckError> {
  let dep = ctx.len() - 1 - (*idx as usize);
  let bind = ctx.get(dep).ok_or_else(|| {
    CheckError::UnboundVariable(
      *pos,
      error_context(ctx),
      nam.to_string(),
      dep as u64,
    )
  })?;
  let subtract_use = (bind.1 - uses).ok_or_else(|| {
    CheckError::QuantityTooMuch(
      *pos,
      error_context(ctx),
      nam.to_string(),
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

/// Infers the type of an global content-addressed reference
#[inline]
pub fn infer_ref(
  defs: &Defs,
  pos: &Pos,
  nam: &Name,
  def_link: &Cid,
) -> Result<DAG, CheckError> {
  let def = defs
    .defs
    .get(def_link)
    .ok_or_else(|| CheckError::UndefinedReference(*pos, nam.to_string()))?;
  let typ = DAG::from_term(&def.typ_);
  Ok(typ)
}

/// Infers the type of a function application
#[inline]
pub fn infer_app(
  rec: &Option<(Name, Cid, Cid)>,
  defs: &Defs,
  ctx: &mut Ctx,
  uses: Uses,
  pos: &Pos,
  fun: &Term,
  arg: &Term,
  should_count: bool,
) -> Result<DAG, CheckError> {
  let mut fun_typ = infer(rec, defs, ctx, uses, fun, should_count)?;
  fun_typ.whnf(defs, should_count);
  match fun_typ.head {
    DAGPtr::All(link) => {
      let All { uses: lam_uses, dom, img, .. } = unsafe { &mut *link.as_ptr() };
      let Lam { var, bod: img, .. } = unsafe { &mut *img.as_ptr() };
      check(
        rec,
        defs,
        ctx,
        *lam_uses * uses,
        arg,
        &mut DAG::new(*dom),
        should_count,
      )?;
      let mut map = BTreeMap::new();
      if var.parents.is_some() {
        map.insert(
          DAGPtr::Var(NonNull::new(var).unwrap()),
          DAG::from_term_inner(
            arg,
            ctx.len() as u64,
            BTreeMap::new(),
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
      error_context(ctx),
      fun.clone(),
      fun_typ.to_term(false),
    )),
  }
}

/// Infers the type of a self type destructor
#[inline]
pub fn infer_cse(
  rec: &Option<(Name, Cid, Cid)>,
  defs: &Defs,
  ctx: &mut Ctx,
  uses: Uses,
  pos: &Pos,
  exp: &Term,
  should_count: bool,
) -> Result<DAG, CheckError> {
  let mut exp_typ = infer(rec, defs, ctx, uses, exp, should_count)?;
  exp_typ.whnf(defs, should_count);
  match exp_typ.head {
    DAGPtr::Slf(link) => {
      let Slf { var, bod, .. } = unsafe { &mut *link.as_ptr() };
      let mut map = BTreeMap::new();
      if var.parents.is_some() {
        map.insert(
          DAGPtr::Var(NonNull::new(var).unwrap()),
          DAG::from_term_inner(
            exp,
            ctx.len() as u64,
            BTreeMap::new(),
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
        None => {
          Err(CheckError::NonInductiveLitType(*pos, error_context(ctx), *lty))
        }
        Some(ind) => {
          let induction = DAG::from_term_inner(
            &ind,
            ctx.len() as u64,
            BTreeMap::new(),
            Some(root),
            None,
          );
          Ok(DAG::new(induction))
        }
      }
    }
    _ => Err(CheckError::CseDatMismatch(
      *pos,
      error_context(ctx),
      exp.clone(),
      exp_typ.to_term(false),
    )),
  }
}

/// Infers the type of a forall (∀)
#[inline]
pub fn infer_all(
  rec: &Option<(Name, Cid, Cid)>,
  defs: &Defs,
  ctx: &mut Ctx,
  nam: &Name,
  dom: &Term,
  img: &Term,
  should_count: bool,
) -> Result<DAG, CheckError> {
  let mut typ = DAG::from_term(&Term::Typ(Pos::None));
  check(rec, defs, ctx, Uses::None, dom, &mut typ, should_count)?;
  let mut dom_dag = DAG::from_term_inner(
    dom,
    ctx.len() as u64,
    BTreeMap::new(),
    None,
    rec.clone(),
  );
  ctx.push((nam.to_string(), Uses::None, &mut dom_dag));
  check(rec, defs, ctx, Uses::None, img, &mut typ, should_count)?;
  ctx.pop();
  free_dead_node(dom_dag);
  Ok(typ)
}

/// Infers the type of a self type
#[inline]
pub fn infer_slf(
  rec: &Option<(Name, Cid, Cid)>,
  defs: &Defs,
  ctx: &mut Ctx,
  term: &Term,
  nam: &Name,
  bod: &Term,
  should_count: bool,
) -> Result<DAG, CheckError> {
  let mut typ = DAG::from_term(&Term::Typ(Pos::None));
  let mut term_dag = DAG::from_term_inner(
    term,
    ctx.len() as u64,
    BTreeMap::new(),
    None,
    rec.clone(),
  );
  ctx.push((nam.to_string(), Uses::None, &mut term_dag));
  check(rec, defs, ctx, Uses::None, bod, &mut typ, should_count)?;
  ctx.pop();
  free_dead_node(term_dag);
  Ok(typ)
}

/// Infers the type of a local definition
#[inline]
pub fn infer_let(
  rec: &Option<(Name, Cid, Cid)>,
  defs: &Defs,
  ctx: &mut Ctx,
  uses: Uses,
  pos: &Pos,
  exp_uses: Uses,
  nam: &Name,
  exp_typ: &Term,
  exp: &Term,
  bod: &Term,
  should_count: bool,
) -> Result<DAG, CheckError> {
  let exp_dag = &mut DAG::new(DAG::from_term_inner(
    exp,
    ctx.len() as u64,
    BTreeMap::new(),
    None,
    rec.clone(),
  ));
  let root = alloc_val(DLL::singleton(ParentPtr::Root));
  let exp_typ_dag = &mut DAG::new(DAG::from_term_inner(
    exp_typ,
    ctx.len() as u64,
    BTreeMap::new(),
    Some(root),
    rec.clone(),
  ));
  check(rec, defs, ctx, exp_uses * uses, exp, exp_typ_dag, should_count)?;
  let rest_ctx = div_ctx(uses, ctx);
  ctx.push((nam.to_string(), exp_uses, &mut exp_typ_dag.head));
  let mut bod_typ = infer(rec, defs, ctx, Uses::Once, bod, should_count)?;
  let (_, rest, _) = ctx.last().unwrap();
  // Have to check whether the rest 'contains' zero (i.e., zero is less than or
  // equal to the rest), otherwise the variable was not used enough
  if !Uses::lte(Uses::None, *rest) {
    Err(CheckError::QuantityTooLittle(
      *pos,
      error_context(ctx),
      nam.to_string(),
      exp_uses,
      *rest,
    ))
  }
  else {
    ctx.pop();
    DAG::new(exp_typ_dag.head).free();
    add_mul_ctx(uses, ctx, rest_ctx);
    bod_typ.subst(ctx.len() as u64, exp_dag.head);
    Ok(bod_typ)
  }
}

/// Infers the type of a recursive local definition
#[inline]
pub fn infer_letrec(
  rec: &Option<(Name, Cid, Cid)>,
  defs: &Defs,
  ctx: &mut Ctx,
  uses: Uses,
  pos: &Pos,
  exp_uses: Uses,
  nam: &Name,
  exp_typ: &Term,
  exp: &Term,
  bod: &Term,
  should_count: bool,
) -> Result<DAG, CheckError> {
  unsafe {
    // Allocates exp as a DAG, must be rootless
    let fix = alloc_fix(nam.clone(), 0, mem::zeroed(), None);
    let Fix { var: fix_var, bod_ref: fix_bod_ref, .. } = &mut *fix.as_ptr();
    let mut exp_map = BTreeMap::new();
    exp_map.insert(ctx.len(), DAGPtr::Var(NonNull::new_unchecked(fix_var)));
    let exp_dag = &mut DAG::new(DAG::from_term_inner(
      exp,
      ctx.len() as u64 + 1,
      exp_map,
      NonNull::new(fix_bod_ref),
      rec.clone(),
    ));
    // Allocates exp_typ as a DAG, must be rooted
    let root = alloc_val(DLL::singleton(ParentPtr::Root));
    let exp_typ_dag = &mut DAG::new(DAG::from_term_inner(
      exp_typ,
      ctx.len() as u64,
      BTreeMap::new(),
      Some(root),
      rec.clone(),
    ));
    // Check exp, noting it is a recursive definition
    let rest_ctx = div_ctx(Uses::Many, ctx);
    ctx.push((nam.to_string(), Uses::Many, &mut exp_typ_dag.head));
    check(rec, defs, ctx, Uses::Many, exp, exp_typ_dag, should_count)?; // TODO better error message
    ctx.pop();
    // Check bod
    add_ctx(ctx, rest_ctx);
    let rest_ctx = div_ctx(uses, ctx);
    ctx.push((nam.to_string(), exp_uses, &mut exp_typ_dag.head));
    let mut bod_typ = infer(rec, defs, ctx, Uses::Once, bod, should_count)?;
    let (_, rest, _) = ctx.last().unwrap();
    // Have to check whether the rest 'contains' zero (i.e., zero is less than
    // or equal to the rest), otherwise the variable was not used enough
    if !Uses::lte(Uses::None, *rest) {
      Err(CheckError::QuantityTooLittle(
        *pos,
        error_context(ctx),
        nam.to_string(),
        exp_uses,
        *rest,
      ))
    }
    else {
      ctx.pop();
      DAG::new(exp_typ_dag.head).free();
      add_mul_ctx(uses, ctx, rest_ctx);
      bod_typ.subst(ctx.len() as u64, exp_dag.head);
      Ok(bod_typ)
    }
  }
}

/// Infers the type of a type annotation
#[inline]
pub fn infer_ann(
  rec: &Option<(Name, Cid, Cid)>,
  defs: &Defs,
  ctx: &mut Ctx,
  uses: Uses,
  exp: &Term,
  typ: &Term,
  should_count: bool,
) -> Result<DAG, CheckError> {
  let root = alloc_val(DLL::singleton(ParentPtr::Root));
  let mut typ_dag = DAG::new(DAG::from_term_inner(
    typ,
    ctx.len() as u64,
    BTreeMap::new(),
    Some(root),
    rec.clone(),
  ));
  check(rec, defs, ctx, uses, exp, &mut typ_dag, should_count)?;
  Ok(typ_dag)
}

/// Infers the type of a literal
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

/// Infers the type of a term and returns a term
pub fn infer_term(
  defs: &Defs,
  term: &Term,
  should_count: bool,
) -> Result<Term, CheckError> {
  let typ_dag =
    infer(&None, defs, &mut vec![], Uses::Once, term, should_count)?;
  let typ = DAG::to_term(&typ_dag, true);
  typ_dag.free();
  Ok(typ)
}

/// Typechecks a definition
pub fn check_def(
  defs: Rc<Defs>,
  name: &str,
  should_count: bool,
) -> Result<Term, CheckError> {
  let def = defs.get(&Name::from(name)).ok_or_else(|| {
    CheckError::UndefinedReference(Pos::None, name.to_owned())
  })?;
  let (d, _, a) = def.embed();
  let def_cid = d.cid();
  let ast_cid = a.cid();
  let rec = Some((Name::from(name), def_cid, ast_cid));
  let mut typ = DAG::from_term(&def.typ_);
  check(
    &rec,
    &defs,
    &mut vec![],
    Uses::Once,
    &def.term,
    &mut typ,
    should_count,
  )?;
  typ.free();
  Ok(def.typ_.clone())
}
