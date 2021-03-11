// use std::error::Error;
// use std::fmt;

// use crate::{
//   term::{
//     Term,
//     Defs,
//     Refs,
//   },
//   core::{
//     dll::*,
//     dag::*,
//     eval::{
//       whnf,
//     },
//     uses::*
//   },
// };

// use im::{
//   Vector,
// };
// use core::ptr::NonNull;
// use std::collections::HashSet;

// type PreCtx = Vec<(String, DAG)>;
// type Ctx    = Vec<(String, Uses, DAG)>;

// pub fn to_ctx(pre: PreCtx) -> Ctx {
//   let mut ctx = vec![];
//   for (nam, typ) in pre {
//     ctx.push((nam, Uses::None, typ));
//   }
//   ctx
// }

// #[inline]
// pub fn add_ctx(ctx: &mut Ctx, ctx2: Ctx) {
//   for i in 0..ctx.len() {
//     ctx[i].1 = Uses::add(ctx[i].1, ctx2[i].1)
//   }
// }

// #[inline]
// pub fn mul_ctx(uses: Uses, ctx: &mut Ctx) {
//   for mut bind in ctx {
//     bind.1 = Uses::mul(bind.1, uses)
//   }
// }

// #[derive(Debug)]
// pub enum CheckError {
//   GenericError(String),
// }

// impl fmt::Display for CheckError {
//   fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//     match self {
//       CheckError::GenericError(msg) => write!(f,"{}",msg),
//     }
//   }
// }

// impl Error for CheckError {
//   fn description(&self) -> &str {
//     match self {
//       CheckError::GenericError(msg) => &msg,
//     }
//   }
// }


// // TODO: quickly return the values of nodes that were previously computed
// pub fn stringify(a: DAG, ini: u64, dep: u64) -> String {
//   #[inline]
//   fn update_index(var: Option<NonNull<Var>>, dep: u64) {
//     match var {
//       Some(link) => unsafe {
//         (*link.as_ptr()).depth = dep;
//       },
//       _ => (),
//     }
//   }
//   match a {
//     DAG::Var(link) => unsafe {
//       let Var { depth, .. } = *link.as_ptr();
//       if depth < ini {
//         format!("#{}", depth)
//       }
//       else {
//         format!("^{}", dep-1-depth)
//       }
//     },
//     DAG::Leaf(link) => unsafe {
//       let Leaf { tag, .. } = &*link.as_ptr();
//       match tag {
//         LeafTag::Typ => String::from("*"),
//         LeafTag::Ref(name, _, _) => format!("<{}>", name),
//         _ => panic!("TODO: complete stringify"),
//       }
//     },
//     DAG::Single(link) => unsafe {
//       let Single { tag, body, var, .. } = *link.as_ptr();
//       update_index(var, dep);
//       let prefix = match tag {
//         SingleTag::Lam => "λ",
//         SingleTag::Slf => "$",
//         SingleTag::Cse => "C",
//         SingleTag::Dat => "D",
//         SingleTag::Fix => "μ",
//       };
//       let dep = if var.is_some() { dep+1 } else { dep };
//       format!("{}{}", prefix, stringify(body, ini, dep))
//     },
//     DAG::Branch(link) => unsafe {
//       let Branch { tag, left, right, var, .. } = *link.as_ptr();
//       update_index(var, dep);
//       let prefix = match tag {
//         BranchTag::App => "@",
//         BranchTag::All(uses) => {
//           match uses {
//             Uses::None => "∀0",
//             Uses::Once => "∀1",
//             Uses::Affi => "∀&",
//             Uses::Many => "∀ω",
//           }
//         },
//         BranchTag::Let => "L",
//       };
//       let right_dep = if var.is_some() { dep+1 } else { dep };
//       format!("{}{}{}", prefix, stringify(left, ini, dep), stringify(right, ini, right_dep))
//     },
//   }
// }

// pub fn equal(defs:&Defs, a: DAG, b: DAG, dep: u64) -> bool {
//   let mut triples = vec![(a, b, dep)];
//   let mut set = HashSet::new();
//   while let Some ((mut a, mut b, dep)) = triples.pop() {
//     whnf(defs, &mut a);
//     whnf(defs, &mut b);
//     let hash_a = stringify(a, dep, dep);
//     let hash_b = stringify(b, dep, dep);
//     let hash_ab = format!("{}{}", hash_a, hash_b);
//     let eq = hash_a == hash_b || set.contains(&hash_ab);
//     set.insert(hash_ab);
//     if !eq {
//       match (a, b) {
//         (DAG::Single(a_link),DAG::Single(b_link)) => unsafe {
//           let Single { tag: a_tag, body: a_body, var, .. } = *a_link.as_ptr();
//           let Single { tag: b_tag, body: b_body, .. } = *b_link.as_ptr();
//           if a_tag != b_tag {
//             return false
//           }
//           let dep = if var.is_some() { dep+1 } else { dep };
//           triples.push((a_body, b_body, dep));
//         },
//         (DAG::Branch(a_link),DAG::Branch(b_link)) => unsafe {
//           let Branch { tag: a_tag, left: a_left, right: a_right, var, .. } = *a_link.as_ptr();
//           let Branch { tag: b_tag, left: b_left, right: b_right, .. } = *b_link.as_ptr();
//           if a_tag != b_tag {
//             return false
//           }
//           let right_dep = if var.is_some() { dep+1 } else { dep };
//           triples.push((a_left, b_left, dep));
//           triples.push((a_right, b_right, right_dep));
//         },
//         _ => return false,
//       }
//     }
//   }
//   true
// }

// pub fn check(refr: DAG, defs: &Defs, mut pre: PreCtx, uses: Uses, term: &Term, typ: &mut DAG) -> Result<Ctx, CheckError> {
//   match &term {
//     Term::Lam(_, name, term_body) => {
//       // A potential problem is that `term` might also mutate, which is an unwanted side-effect,
//       // since `term` and `typ` might be coupled. To deal with this we will need to make copies
//       // of `term` in the self rule
//       whnf(defs, typ);
//       match typ {
//         DAG::Branch(link) => {
//           let Branch { tag, var, left: dom, right: img, .. } = unsafe { link.as_mut() };
//           match tag {
//             BranchTag::All(lam_uses) => {
//               // Annotate the depth of the node that binds var
//               match var {
//                 None => panic!("Malformed DAG"),
//                 Some(link) => unsafe {
//                   (*link.as_ptr()).depth = pre.len() as u64;
//                 }
//               }
//               // Add the domain of the function to the precontext
//               pre.push((name.to_string(), *dom));
//               let mut ctx = check(refr, defs, pre, Uses::Once, &**term_body, img)?;
//               let (_, infer_uses, infer_typ) = ctx
//                 .pop()
//                 .ok_or(CheckError::GenericError(String::from("Empty context.")))?;
//               // Discard the popped type if it is not connected to another DAG
//               if infer_typ.is_root() {
//                 infer_typ.uproot();
//                 free_dead_node(infer_typ);
//               }
//               if Uses::gth(infer_uses, *lam_uses) {
//                 Err(CheckError::GenericError(String::from("Quantity mismatch.")))
//               }
//               else {
//                 mul_ctx(uses, &mut ctx);
//                 Ok(ctx)
//               }
//             }
//             _ => Err(CheckError::GenericError(String::from("The type of a lambda must be a forall."))),
//           }
//         },
//         _ => Err(CheckError::GenericError(String::from("The type of a lambda must be a forall."))),
//       }
//     },

//     Term::Dat(_, dat_body) => {
//       whnf(defs, typ);
//       match typ {
//         DAG::Single(link) => {
//           let Single { tag, .. } = unsafe { &*link.as_ptr() };
//           match tag {
//             SingleTag::Slf => {
//               // Copy the self type
//               let new_link = match typ.full_copy() {
//                 DAG::Single(link) => link,
//                 _ => panic!("Error in the full_copy implementation")
//               };
//               let Single { var: new_var, body: new_body, .. } = unsafe { &mut *new_link.as_ptr() };
//               // Substitute the term for its variable
//               new_var.map(|var| {
//                 if get_parents(DAG::Var(var)).is_some() {
//                   // Create the term DAG
//                   let term_dag = DAG::from_subterm(Some(refr), pre.len() as u64, &term, Vector::new(), None);
//                   replace_child(DAG::Var(var), term_dag);
//                 }
//               });
//               // Replace the copied self type with its body and free the dead nodes
//               replace_child(DAG::Single(new_link), *new_body);
//               free_dead_node(DAG::Single(new_link));

//               // Check against `new_body` and free it later
//               let ctx = check(refr, defs, pre, uses, dat_body, new_body)?;
//               new_body.uproot();
//               free_dead_node(*new_body);
//               Ok(ctx)
//             }
//             _ => Err(CheckError::GenericError(String::from("The type of data must be a self."))),
//           }
//         },
//         _ => Err(CheckError::GenericError(String::from("The type of data must be a self."))),
//       }
//     },

//     Term::Let(_, _, _, _, _, _, _) => {
//       panic!("TODO")
//     }
//     _ => {
//       let depth = pre.len();
//       let (ctx, infer_typ) = infer(refr, defs, pre, uses, term)?;
//       let eq = equal(defs, *typ, infer_typ, depth as u64);
//       infer_typ.uproot();
//       free_dead_node(infer_typ);
//       if eq {
//         Ok(ctx)
//       }
//       else {
//         Err(CheckError::GenericError(String::from("Type mismatch.")))
//       }
//     },
//   }
// }

// pub fn infer(refr: DAG, defs: &Defs, mut pre: PreCtx, uses: Uses, term: &Term) -> Result<(Ctx, DAG), CheckError> {
//   match term {
//     Term::Var(_, _, idx) => {
//       let level = pre.len() - (*idx as usize) - 1;
//       let bind = pre
//         .get(level)
//         .ok_or(CheckError::GenericError(String::from("Unbound variable.")))?;
//       let typ = bind.1;
//       let mut ctx = to_ctx(pre);
//       ctx[*idx as usize].1 = uses;
//       Ok((ctx, typ.full_copy()))
//     }
//     Term::Ref(_, nam, def_link, _) => {
//       if let Some(def) = defs.get(def_link) {
//         let ctx = to_ctx(pre);
//         let root = alloc_val(DLL::singleton(ParentCell::Root));
//         let var = new_var(nam.clone(), 0);
//         let body = DAG::from_subterm(Some(refr), 0, &def.clone().typ_, Vector::unit(DAG::Var(var)), Some(root));
//         let typ_ref = alloc_uninit();
//         let typ = alloc_val(Single {
//           tag: SingleTag::Fix,
//           var: Some(var),
//           body,
//           body_ref: typ_ref,
//           parents: None
//         });
//         unsafe {
//           *typ_ref.as_ptr() = DLL::singleton(ParentCell::Body(typ));
//         }
//         add_to_parents(body, typ_ref);
//         Ok((ctx, DAG::Single(typ)))
//       }
//       else {
//         panic!("Undefined reference: {}, {}", nam, def_link);
//       }
//     }
//     Term::App(_, func, argm) => {
//       let depth = pre.len() as u64;
//       let (mut func_ctx, mut func_typ) = infer(refr, defs, pre.clone(), uses, func)?;
//       whnf(defs, &mut func_typ);
//       match func_typ {
//         DAG::Branch(mut link) => {
//           let Branch { tag, left: dom, .. } = unsafe { link.as_mut() };
//           match tag {
//             BranchTag::All(lam_uses) => {
//               let argm_ctx = check(refr, defs, pre, Uses::mul(*lam_uses, uses), argm, dom)?;
//               add_ctx(&mut func_ctx, argm_ctx);

//               // Copy the All type (in a efficient implementation, only image is copied)
//               let new_link = match func_typ.full_copy() {
//                 DAG::Branch(link) => link,
//                 _ => panic!("Error in the full_copy implementation")
//               };
//               let Branch { var: new_var, right: new_img, .. } = unsafe { &*new_link.as_ptr() };
//               // Substitute the argument for the image's variable
//               new_var.map(|var| {
//                 if get_parents(DAG::Var(var)).is_some() {
//                   // Create the argument DAG
//                   let argm_dag = DAG::from_subterm(Some(refr), depth, &argm, Vector::new(), None);
//                   replace_child(DAG::Var(var), argm_dag);
//                 }
//               });
//               // Replace the copied type with the new image and free the dead nodes
//               replace_child(DAG::Branch(new_link), *new_img);
//               free_dead_node(DAG::Branch(new_link));

//               Ok((func_ctx, *new_img))
//             }
//             _ => Err(CheckError::GenericError(String::from("Tried to apply something that isn't a lambda."))),
//           }
//         },
//         _ => Err(CheckError::GenericError(String::from("Tried to apply something that isn't a lambda."))),
//       }

//     }
//     Term::Cse(_, expr) => {
//       let depth = pre.len() as u64;
//       let (expr_ctx, mut expr_typ) = infer(refr, defs, pre, uses, expr)?;
//       whnf(defs, &mut expr_typ);
//       match expr_typ {
//         DAG::Single(link) => {
//           let Single { tag, .. } = unsafe { &*link.as_ptr() };
//           match tag {
//             SingleTag::Slf => {
//               let new_link = match expr_typ.full_copy() {
//                 DAG::Single(link) => link,
//                 _ => panic!("Error in the full_copy implementation")
//               };
//               let Single { var: new_var, body: new_body, .. } = unsafe { &*new_link.as_ptr() };
//               new_var.map(|var| {
//                 if get_parents(DAG::Var(var)).is_some() {
//                   let expr_dag = DAG::from_subterm(Some(refr), depth, &expr, Vector::new(), None);
//                   replace_child(DAG::Var(var), expr_dag);
//                 }
//               });
//               replace_child(DAG::Single(new_link), *new_body);
//               free_dead_node(DAG::Single(new_link));

//               Ok((expr_ctx, *new_body))
//             }
//             _ => Err(CheckError::GenericError(String::from("Tried to case on something that isn't a datatype."))),
//           }
//         },
//         _ => Err(CheckError::GenericError(String::from("Tried to case on something that isn't a datatype."))),
//       }

//     }
//     Term::All(_, _, name, dom, img) => {
//       let mut typ = DAG::from_term(&Term::Typ(None));
//       let _ = check(refr, defs, pre.clone(), Uses::None, dom, &mut typ)?;
//       let root = alloc_val(DLL::singleton(ParentCell::Root));
//       let dom_dag = DAG::from_subterm(Some(refr), pre.len() as u64, &dom, Vector::new(), Some(root));
//       pre.push((name.to_string(), dom_dag));
//       let ctx = check(refr, defs, pre, Uses::None, img, &mut typ)?;
//       Ok((ctx, typ))
//     }
//     Term::Slf(_, name, body) => {
//       let mut typ = DAG::from_term(&Term::Typ(None));
//       let root = alloc_val(DLL::singleton(ParentCell::Root));
//       let term_dag = DAG::from_subterm(Some(refr), pre.len() as u64, &term, Vector::new(), Some(root));
//       pre.push((name.to_string(), term_dag));
//       let ctx = check(refr, defs, pre, Uses::None, body, &mut typ)?;
//       Ok((ctx, typ))
//     }
//     Term::Let(_, _, _, _, _, _, _) => {
//       panic!("TODO")
//     }
//     Term::Typ(_) => {
//       Ok((to_ctx(pre), DAG::from_term(&Term::Typ(None))))
//     }
//     Term::Ann(_, typ, expr) => {
//       let root = alloc_val(DLL::singleton(ParentCell::Root));
//       let mut typ_dag = DAG::from_subterm(Some(refr), pre.len() as u64, &typ, Vector::new(), Some(root));
//       let ctx = check(refr, defs, pre, uses, expr, &mut typ_dag)?;
//       Ok((ctx, typ_dag))
//     }
//     Term::Lit(_, _) => {
//       panic!("TODO")
//     }
//     Term::LTy(_, _) => {
//       panic!("TODO")
//     }
//     Term::Opr(_, _) => {
//       panic!("TODO")
//     }
//     Term::Lam(_, _, _) => {
//       Err(CheckError::GenericError(String::from("Untyped lambda.")))
//     }
//     _ => {
//       Err(CheckError::GenericError(String::from("Cannot infer type.")))
//     }
//   }
// }

// pub fn check_def(defs: &Defs, refs: &Refs, name: &String) -> Result<(), CheckError> {
//   #[inline]
//   fn free(a: DAG) {
//     a.uproot();
//     free_dead_node(a);
//   }
//   let (def_link, ast_link) = refs
//     .get(name)
//     .ok_or(CheckError::GenericError(String::from("Undefined reference.")))?;
//   let def = defs
//     .get(def_link)
//     .ok_or(CheckError::GenericError(String::from("Undefined reference.")))?;
//   let refr = DAG::Leaf(new_leaf(LeafTag::Ref(name.clone(), *def_link, *ast_link)));
//   let mut typ = DAG::from_term(&def.typ_);
//   let ctx = check(refr, &defs, vec![(name.clone(), typ)], Uses::Once, &def.term, &mut typ)?;
//   for (_, _, dag) in ctx {
//     if dag.is_root(){
//       free(dag);
//     }
//   }
//   free(typ);
//   Ok(())
// }
