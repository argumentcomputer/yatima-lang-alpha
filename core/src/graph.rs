use cid::Cid;
use petgraph::{
  dot::Dot,
  graph::{
    DefaultIx,
    DiGraph,
    NodeIndex,
  },
};

use crate::{
  dag::*,
  dll::DLL,
  literal::{
    LitType,
    Literal,
  },
  name::Name,
  prim::Op,
  uses::Uses,
};
use core::ptr::NonNull;
use std::collections::HashMap;

use std::fmt;

pub enum Node {
  Var { name: Name, rec: bool, dep: u64 },
  Lam { name: Name },
  App,
  All { uses: Uses },
  Slf { name: Name },
  Fix { name: Name },
  Dat,
  Cse,
  Ref { name: Name, rec: bool, exp: Cid, ast: Cid },
  Let { uses: Uses },
  Typ,
  Ann,
  Lit { lit: Literal },
  LTy { lty: LitType },
  Opr { opr: Op },
}

impl fmt::Display for Node {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Var { name, rec: false, .. } => write!(f, "{}", name),
      Self::Var { name, .. } => write!(f, "^{}", name),
      Self::Lam { name } => write!(f, "λ {}", name),
      Self::All { uses: Uses::None } => write!(f, "∀0"),
      Self::All { uses: Uses::Once } => write!(f, "∀1"),
      Self::All { uses: Uses::Affi } => write!(f, "∀&"),
      Self::All { uses: Uses::Many } => write!(f, "∀ω"),
      Self::Slf { name } => write!(f, "@{}", name),
      Self::Fix { name } => write!(f, "μ{}", name),
      Self::Dat => write!(f, "data"),
      Self::Cse => write!(f, "case"),
      Self::Ref { name, rec: false, .. } => write!(f, "#{}", name),
      Self::Ref { name, rec: true, .. } => write!(f, "#^{}", name),
      Self::Let { uses: Uses::None } => write!(f, "let0"),
      Self::Let { uses: Uses::Once } => write!(f, "let1"),
      Self::Let { uses: Uses::Affi } => write!(f, "let&"),
      Self::Let { uses: Uses::Many } => write!(f, "letω"),
      Self::Typ => write!(f, "Type"),
      Self::Ann => write!(f, "::"),
      Self::App => write!(f, "( )"),
      Self::Lit { lit } => write!(f, "{}", lit),
      Self::LTy { lty } => write!(f, "{}", lty),
      Self::Opr { opr } => write!(f, "{}", opr),
    }
  }
}

#[derive(Debug)]
pub enum Edge {
  Root,
  Copy,
  Downlink,
  LamBod,
  SlfBod,
  FixBod,
  DatBod,
  CseBod,
  AppFun,
  AppArg,
  AllDom,
  AllImg,
  AnnTyp,
  AnnExp,
  LetTyp,
  LetExp,
  LetBod,
  BindFree,
  BindLam,
  BindSlf,
  BindFix,
}

impl fmt::Display for Edge {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Root => write!(f, "Root"),
      Self::Copy => write!(f, "Copy"),
      Self::Downlink => write!(f, ""),
      Self::LamBod => write!(f, "LamBod"),
      Self::SlfBod => write!(f, "SlfBod"),
      Self::FixBod => write!(f, "FixBod"),
      Self::DatBod => write!(f, "DatBod"),
      Self::CseBod => write!(f, "CseBod"),
      Self::AppFun => write!(f, "AppFun"),
      Self::AppArg => write!(f, "AppArg"),
      Self::AllDom => write!(f, "AllDom"),
      Self::AllImg => write!(f, "AllImg"),
      Self::AnnTyp => write!(f, "AnnTyp"),
      Self::AnnExp => write!(f, "AnnExp"),
      Self::LetTyp => write!(f, "LetTyp"),
      Self::LetExp => write!(f, "LetExp"),
      Self::LetBod => write!(f, "LetBod"),
      Self::BindFree => write!(f, "BindFree"),
      Self::BindLam => write!(f, "BindLam"),
      Self::BindSlf => write!(f, "BindSlf"),
      Self::BindFix => write!(f, "BindFix"),
    }
  }
}

pub type Graph = DiGraph<Node, Edge>;

pub fn from_dag(dag: &DAG) -> Graph {
  let mut graph = Graph::new();
  let mut map: HashMap<DAGPtr, NodeIndex<DefaultIx>> = HashMap::new();
  from_dag_ptr(&dag.head, &mut map, &mut graph);
  graph
}

pub fn add_parent_edges(
  ix: NodeIndex<DefaultIx>,
  map: &mut HashMap<DAGPtr, NodeIndex<DefaultIx>>,
  graph: &mut Graph,
  parents: Option<NonNull<Parents>>,
) {
  for p in DLL::iter_option(parents) {
    match *p {
      ParentPtr::Root => {
        graph.add_edge(ix, ix, Edge::Root);
      }
      ParentPtr::LamBod(link) => {
        let p_ix = from_dag_ptr(&DAGPtr::Lam(link), map, graph);
        graph.add_edge(ix, p_ix, Edge::LamBod);
      }
      ParentPtr::SlfBod(link) => {
        let p_ix = from_dag_ptr(&DAGPtr::Slf(link), map, graph);
        graph.add_edge(ix, p_ix, Edge::SlfBod);
      }
      ParentPtr::FixBod(link) => {
        let p_ix = from_dag_ptr(&DAGPtr::Fix(link), map, graph);
        graph.add_edge(ix, p_ix, Edge::FixBod);
      }
      ParentPtr::DatBod(link) => {
        let p_ix = from_dag_ptr(&DAGPtr::Dat(link), map, graph);
        graph.add_edge(ix, p_ix, Edge::DatBod);
      }
      ParentPtr::CseBod(link) => {
        let p_ix = from_dag_ptr(&DAGPtr::Cse(link), map, graph);
        graph.add_edge(ix, p_ix, Edge::CseBod);
      }
      ParentPtr::AppFun(link) => {
        let p_ix = from_dag_ptr(&DAGPtr::App(link), map, graph);
        graph.add_edge(ix, p_ix, Edge::AppFun);
      }
      ParentPtr::AppArg(link) => {
        let p_ix = from_dag_ptr(&DAGPtr::App(link), map, graph);
        graph.add_edge(ix, p_ix, Edge::AppArg);
      }
      ParentPtr::AllDom(link) => {
        let p_ix = from_dag_ptr(&DAGPtr::All(link), map, graph);
        graph.add_edge(ix, p_ix, Edge::AllDom);
      }
      ParentPtr::AllImg(link) => {
        let p_ix = from_dag_ptr(&DAGPtr::All(link), map, graph);
        graph.add_edge(ix, p_ix, Edge::AllImg);
      }
      ParentPtr::AnnTyp(link) => {
        let p_ix = from_dag_ptr(&DAGPtr::Ann(link), map, graph);
        graph.add_edge(ix, p_ix, Edge::AnnTyp);
      }
      ParentPtr::AnnExp(link) => {
        let p_ix = from_dag_ptr(&DAGPtr::Ann(link), map, graph);
        graph.add_edge(ix, p_ix, Edge::AnnExp);
      }
      ParentPtr::LetTyp(link) => {
        let p_ix = from_dag_ptr(&DAGPtr::Let(link), map, graph);
        graph.add_edge(ix, p_ix, Edge::LetTyp);
      }
      ParentPtr::LetExp(link) => {
        let p_ix = from_dag_ptr(&DAGPtr::Let(link), map, graph);
        graph.add_edge(ix, p_ix, Edge::LetExp);
      }
      ParentPtr::LetBod(link) => {
        let p_ix = from_dag_ptr(&DAGPtr::Let(link), map, graph);
        graph.add_edge(ix, p_ix, Edge::LetBod);
      }
    }
  }
}

pub fn from_dag_ptr(
  node: &DAGPtr,
  map: &mut HashMap<DAGPtr, NodeIndex<DefaultIx>>,
  graph: &mut Graph,
) -> NodeIndex<DefaultIx> {
  match node {
    DAGPtr::Var(link) => {
      let Var { nam, dep, rec, binder, parents, .. } = unsafe { link.as_ref() };
      if let Some(ix) = map.get(node) {
        *ix
      }
      else {
        let ix =
          graph.add_node(Node::Var { name: nam.clone(), dep: *dep, rec: *rec });
        map.insert(*node, ix);
        match binder {
          BinderPtr::Free => {
            graph.add_edge(ix, ix, Edge::BindFree);
          }
          BinderPtr::Lam(link) => {
            let bind_ix = from_dag_ptr(&DAGPtr::Lam(*link), map, graph);
            graph.add_edge(ix, bind_ix, Edge::BindLam);
          }
          BinderPtr::Slf(link) => {
            let bind_ix = from_dag_ptr(&DAGPtr::Slf(*link), map, graph);
            graph.add_edge(ix, bind_ix, Edge::BindSlf);
          }
          BinderPtr::Fix(link) => {
            let bind_ix = from_dag_ptr(&DAGPtr::Fix(*link), map, graph);
            graph.add_edge(ix, bind_ix, Edge::BindFix);
          }
        }
        add_parent_edges(ix, map, graph, *parents);
        ix
      }
    }
    DAGPtr::Lam(link) => {
      if let Some(ix) = map.get(node) {
        *ix
      }
      else {
        let Lam { var, bod, parents, .. } = unsafe { &mut *link.as_ptr() };
        let ix = graph.add_node(Node::Lam { name: var.nam.clone() });
        map.insert(*node, ix);
        add_parent_edges(ix, map, graph, *parents);
        let bod_ix = from_dag_ptr(bod, map, graph);
        graph.add_edge(ix, bod_ix, Edge::Downlink);
        ix
      }
    }
    DAGPtr::Slf(link) => {
      if let Some(ix) = map.get(node) {
        *ix
      }
      else {
        let Slf { var, bod, parents, .. } = unsafe { &mut *link.as_ptr() };
        let ix = graph.add_node(Node::Slf { name: var.nam.clone() });
        map.insert(*node, ix);
        add_parent_edges(ix, map, graph, *parents);
        let bod_ix = from_dag_ptr(bod, map, graph);
        graph.add_edge(ix, bod_ix, Edge::Downlink);
        ix
      }
    }
    DAGPtr::Fix(link) => {
      if let Some(ix) = map.get(node) {
        *ix
      }
      else {
        let Fix { var, bod, parents, .. } = unsafe { &mut *link.as_ptr() };
        let ix = graph.add_node(Node::Fix { name: var.nam.clone() });
        map.insert(*node, ix);
        add_parent_edges(ix, map, graph, *parents);
        let bod_ix = from_dag_ptr(bod, map, graph);
        graph.add_edge(ix, bod_ix, Edge::Downlink);
        ix
      }
    }
    DAGPtr::Cse(link) => {
      if let Some(ix) = map.get(node) {
        *ix
      }
      else {
        let Cse { bod, parents, .. } = unsafe { &mut *link.as_ptr() };
        let ix = graph.add_node(Node::Cse);
        map.insert(*node, ix);
        add_parent_edges(ix, map, graph, *parents);
        let bod_ix = from_dag_ptr(bod, map, graph);
        graph.add_edge(ix, bod_ix, Edge::Downlink);
        ix
      }
    }
    DAGPtr::Dat(link) => {
      if let Some(ix) = map.get(node) {
        *ix
      }
      else {
        let Dat { bod, parents, .. } = unsafe { &mut *link.as_ptr() };
        let ix = graph.add_node(Node::Dat);
        map.insert(*node, ix);
        add_parent_edges(ix, map, graph, *parents);
        let bod_ix = from_dag_ptr(bod, map, graph);
        graph.add_edge(ix, bod_ix, Edge::Downlink);
        ix
      }
    }
    DAGPtr::App(link) => {
      if let Some(ix) = map.get(node) {
        *ix
      }
      else {
        let App { fun, arg, parents, .. } = unsafe { &mut *link.as_ptr() };
        let ix = graph.add_node(Node::App);
        map.insert(*node, ix);
        add_parent_edges(ix, map, graph, *parents);
        let fun_ix = from_dag_ptr(fun, map, graph);
        graph.add_edge(ix, fun_ix, Edge::Downlink);
        let arg_ix = from_dag_ptr(arg, map, graph);
        graph.add_edge(ix, arg_ix, Edge::Downlink);
        ix
      }
    }
    DAGPtr::Ann(link) => {
      if let Some(ix) = map.get(node) {
        *ix
      }
      else {
        let Ann { typ, exp, parents, .. } = unsafe { &mut *link.as_ptr() };
        let ix = graph.add_node(Node::Ann);
        map.insert(*node, ix);
        add_parent_edges(ix, map, graph, *parents);
        let typ_ix = from_dag_ptr(typ, map, graph);
        graph.add_edge(ix, typ_ix, Edge::Downlink);
        let exp_ix = from_dag_ptr(exp, map, graph);
        graph.add_edge(ix, exp_ix, Edge::Downlink);
        ix
      }
    }
    DAGPtr::All(link) => {
      if let Some(ix) = map.get(node) {
        *ix
      }
      else {
        let All { uses, dom, img, parents, .. } =
          unsafe { &mut *link.as_ptr() };
        let ix = graph.add_node(Node::All { uses: *uses });
        map.insert(*node, ix);
        add_parent_edges(ix, map, graph, *parents);
        let dom_ix = from_dag_ptr(dom, map, graph);
        graph.add_edge(ix, dom_ix, Edge::Downlink);
        let img_ix = from_dag_ptr(&DAGPtr::Lam(*img), map, graph);
        graph.add_edge(ix, img_ix, Edge::Downlink);
        ix
      }
    }
    DAGPtr::Let(link) => {
      if let Some(ix) = map.get(node) {
        *ix
      }
      else {
        let Let { uses, typ, exp, bod, parents, .. } =
          unsafe { &mut *link.as_ptr() };
        let ix =
          graph.add_node(Node::Let { uses: *uses });
        map.insert(*node, ix);
        add_parent_edges(ix, map, graph, *parents);
        let typ_ix = from_dag_ptr(typ, map, graph);
        graph.add_edge(ix, typ_ix, Edge::Downlink);
        let exp_ix = from_dag_ptr(exp, map, graph);
        graph.add_edge(ix, exp_ix, Edge::Downlink);
        let bod_ix = from_dag_ptr(&DAGPtr::Lam(*bod), map, graph);
        graph.add_edge(ix, bod_ix, Edge::Downlink);
        ix
      }
    }
    DAGPtr::Typ(link) => {
      if let Some(ix) = map.get(node) {
        *ix
      }
      else {
        let Typ { parents, .. } = unsafe { &mut *link.as_ptr() };
        let ix = graph.add_node(Node::Typ);
        map.insert(*node, ix);
        add_parent_edges(ix, map, graph, *parents);
        ix
      }
    }
    DAGPtr::Lit(link) => {
      if let Some(ix) = map.get(node) {
        *ix
      }
      else {
        let Lit { lit, parents, .. } = unsafe { &mut *link.as_ptr() };
        let ix = graph.add_node(Node::Lit { lit: lit.clone() });
        map.insert(*node, ix);
        add_parent_edges(ix, map, graph, *parents);
        ix
      }
    }
    DAGPtr::LTy(link) => {
      if let Some(ix) = map.get(node) {
        *ix
      }
      else {
        let LTy { lty, parents, .. } = unsafe { &mut *link.as_ptr() };
        let ix = graph.add_node(Node::LTy { lty: *lty });
        map.insert(*node, ix);
        add_parent_edges(ix, map, graph, *parents);
        ix
      }
    }
    DAGPtr::Opr(link) => {
      if let Some(ix) = map.get(node) {
        *ix
      }
      else {
        let Opr { opr, parents, .. } = unsafe { &mut *link.as_ptr() };
        let ix = graph.add_node(Node::Opr { opr: opr.clone() });
        map.insert(*node, ix);
        add_parent_edges(ix, map, graph, *parents);
        ix
      }
    }
    DAGPtr::Ref(link) => {
      if let Some(ix) = map.get(node) {
        *ix
      }
      else {
        let Ref { nam, exp, ast, rec, parents, .. } =
          unsafe { &mut *link.as_ptr() };
        let ix = graph.add_node(Node::Ref {
          name: nam.clone(),
          exp: *exp,
          ast: *ast,
          rec: *rec,
        });
        map.insert(*node, ix);
        add_parent_edges(ix, map, graph, *parents);
        ix
      }
    }
  }
}

pub fn to_dot<'a>(graph: &'a Graph) -> Dot<'a, &'a Graph> {
  Dot::with_attr_getters(
    graph,
    &[],
    &|_, e| match e.weight() {
      Edge::Root => format!(", weight = 2] {{rankdir = TB}} ["),
      Edge::Downlink => format!(", weight = 2"),
      Edge::Copy => format!(", weight = 0"),
      Edge::BindFree
      | Edge::BindLam
      | Edge::BindSlf
      | Edge::BindFix => format!(", weight = 0"),
      Edge::LamBod
      | Edge::SlfBod
      | Edge::FixBod
      | Edge::DatBod
      | Edge::CseBod
      | Edge::AppFun
      | Edge::AppArg
      | Edge::AllDom
      | Edge::AllImg
      | Edge::AnnTyp
      | Edge::AnnExp
      | Edge::LetTyp
      | Edge::LetExp
      | Edge::LetBod => format!(", weight = 1"),
    },
    &|_, n| match n.1 {
      Node::Var { .. } | Node::Ref { .. } => {
        format!("] {{rank = max; {}}} [", n.0.index())
      }
      _ => "".to_string(),
    },
  )
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::eval::test::parse;

  #[test]
  fn test() {
    let graph = from_dag(
      &parse("λ f g x y => let x: Type = ∀ (A: Type) -> A; f x (g x)")
        .unwrap()
        .1,
    );
    println!("{}", to_dot(&graph));
    // assert_eq!(true, false)
  }
}
