use hashexpr::{
  position::Pos,
  Link,
};
use std::collections::HashMap;

pub mod anon_term;
pub mod name_meta;
pub mod position_meta;

use crate::term::{
  Term,
  Term::*,
};

use anon_term::AnonTerm;
use name_meta::NameMeta;
use position_meta::PositionMeta;

pub fn term_to_dag(term: Term) -> (AnonTerm, NameMeta, PositionMeta) {
  match term {
    Var(pos, _, idx) => (
      AnonTerm::Ctor(String::from("var"), vec![AnonTerm::Vari(idx)]),
      NameMeta::Leaf,
      PositionMeta::Leaf(pos),
    ),
    Ref(pos, name, def_link, anon_link) => (
      AnonTerm::Ctor(String::from("ref"), vec![AnonTerm::Link(anon_link)]),
      NameMeta::Ctor(vec![NameMeta::Link(name, def_link)]),
      PositionMeta::Leaf(pos),
    ),
    Lit(pos, lit) => (
      AnonTerm::Ctor(String::from("lit"), vec![AnonTerm::Data(
        lit.encode().serialize(),
      )]),
      NameMeta::Ctor(vec![NameMeta::Leaf]),
      PositionMeta::Leaf(pos),
    ),
    LTy(pos, lty) => (
      AnonTerm::Ctor(String::from("lty"), vec![AnonTerm::Data(
        lty.encode().serialize(),
      )]),
      NameMeta::Ctor(vec![NameMeta::Leaf]),
      PositionMeta::Leaf(pos),
    ),
    Opr(pos, opr) => (
      AnonTerm::Ctor(String::from("opr"), vec![AnonTerm::Data(
        opr.encode().serialize(),
      )]),
      NameMeta::Ctor(vec![NameMeta::Leaf]),
      PositionMeta::Leaf(pos),
    ),
    Typ(pos) => (
      AnonTerm::Ctor(String::from("typ"), vec![]),
      NameMeta::Ctor(vec![]),
      PositionMeta::Leaf(pos),
    ),
    Lam(pos, name, body) => {
      let (anon, meta, posi) = term_to_dag(*body);
      (
        AnonTerm::Ctor(String::from("lam"), vec![AnonTerm::Bind(Box::new(
          anon,
        ))]),
        NameMeta::Ctor(vec![NameMeta::Bind(name.to_owned(), Box::new(meta))]),
        PositionMeta::Ctor(pos, vec![posi]),
      )
    }
    Slf(pos, name, body) => {
      let (anon, meta, posi) = term_to_dag(*body);
      (
        AnonTerm::Ctor(String::from("slf"), vec![AnonTerm::Bind(Box::new(
          anon,
        ))]),
        NameMeta::Ctor(vec![NameMeta::Bind(name.to_owned(), Box::new(meta))]),
        PositionMeta::Ctor(pos, vec![posi]),
      )
    }
    App(pos, fun, arg) => {
      let (fun_anon, fun_meta, fun_posi) = term_to_dag(*fun);
      let (arg_anon, arg_meta, arg_posi) = term_to_dag(*arg);
      (
        AnonTerm::Ctor(String::from("app"), vec![fun_anon, arg_anon]),
        NameMeta::Ctor(vec![fun_meta, arg_meta]),
        PositionMeta::Ctor(pos, vec![fun_posi, arg_posi]),
      )
    }
    Ann(pos, val, typ) => {
      let (val_anon, val_meta, val_posi) = term_to_dag(*val);
      let (typ_anon, typ_meta, typ_posi) = term_to_dag(*typ);
      (
        AnonTerm::Ctor(String::from("app"), vec![val_anon, typ_anon]),
        NameMeta::Ctor(vec![val_meta, typ_meta]),
        PositionMeta::Ctor(pos, vec![val_posi, typ_posi]),
      )
    }
    Dat(pos, body) => {
      let (anon, meta, posi) = term_to_dag(*body);
      (
        AnonTerm::Ctor(String::from("dat"), vec![anon]),
        NameMeta::Ctor(vec![meta]),
        PositionMeta::Ctor(pos, vec![posi]),
      )
    }
    Cse(pos, body) => {
      let (anon, meta, posi) = term_to_dag(*body);
      (
        AnonTerm::Ctor(String::from("cse"), vec![anon]),
        NameMeta::Ctor(vec![meta]),
        PositionMeta::Ctor(pos, vec![posi]),
      )
    }
    All(pos, uses, name, typ_, body) => {
      let (typ_anon, typ_meta, typ_posi) = term_to_dag(*typ_);
      let (bod_anon, bod_meta, bod_posi) = term_to_dag(*body);
      (
        AnonTerm::Ctor(String::from("all"), vec![
          AnonTerm::Data(uses.encode().serialize()),
          typ_anon,
          bod_anon,
        ]),
        NameMeta::Ctor(vec![
          NameMeta::Leaf,
          typ_meta,
          NameMeta::Bind(name, Box::new(bod_meta)),
        ]),
        PositionMeta::Ctor(pos, vec![
          PositionMeta::Leaf(None),
          typ_posi,
          bod_posi,
        ]),
      )
    }
    Let(pos, rec, uses, name, typ_, expr, body) => {
      let (typ_anon, typ_meta, typ_posi) = term_to_dag(*typ_);
      let (exp_anon, exp_meta, exp_posi) = term_to_dag(*expr);
      let (bod_anon, bod_meta, bod_posi) = term_to_dag(*body);
      (
        AnonTerm::Ctor(String::from("let"), vec![
          AnonTerm::Data(uses.encode().serialize()),
          AnonTerm::Data(if rec { vec![1] } else { vec![0] }),
          typ_anon,
          if rec { AnonTerm::Bind(Box::new(exp_anon)) } else { exp_anon },
          bod_anon,
        ]),
        NameMeta::Ctor(vec![
          NameMeta::Leaf,
          NameMeta::Leaf,
          typ_meta,
          if rec { NameMeta::Bind(name, Box::new(exp_meta)) } else { exp_meta },
          bod_meta,
        ]),
        PositionMeta::Ctor(pos, vec![
          PositionMeta::Leaf(None),
          PositionMeta::Leaf(None),
          typ_posi,
          exp_posi,
          bod_posi,
        ]),
      )
    }
  }
}

// Todo: Factor the dag module into its own generic library so it can be used by
// other languages than Yatima.

// pub struct Imports {
//  imports: Vec<(Link, String)>,
//}
// pub struct Index {
//  entries: HashMap<String, (Link, Link, Link)>,
//}
// pub struct Source {
//  name: String,
//  filetype: String,
//  text: String,
//}
// pub struct Package {
//  name: String,
//  docs: String,
//  source: Link,
//  imports: Imports,
//  index: Index,
//}
