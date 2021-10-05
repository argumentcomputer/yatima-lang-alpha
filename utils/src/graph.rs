use petgraph::{
  dot::{
    Config,
    Dot,
  },
  graph::{
    DefaultIx,
    DiGraph,
    NodeIndex,
  },
};
use sp_cid::Cid;

use core::ptr::NonNull;
use std::collections::{
  HashMap,
  HashSet,
};
use yatima_core::{
  dag::*,
  dll::DLL,
  literal::{
    LitType,
    Literal,
  },
  name::Name,
  package::Package,
  prim::Op,
  uses::Uses,
};

use std::fmt;

#[derive(Debug, Clone, Default)]
pub struct PackageGraph {
  inner: DiGraph<Name, String>,
}

impl PackageGraph {
  pub fn new() -> Self { Self::default() }

  pub fn add_package<F>(&mut self, resolver: F, package: Package) -> bool
  where F: FnMut(Cid) -> Option<Package> + Clone {
    if let Some((nodes, edges)) = self.traverse_package(resolver, package) {
      let mut map = HashMap::new();
      for name in nodes {
        let node = self.inner.add_node(name.clone());
        map.insert(name.clone(), node);
      }
      for t in
        edges.iter().map(|(f, t)| (map.get(f).cloned(), map.get(t).cloned()))
      {
        if let (Some(from), Some(to)) = t {
          self.inner.add_edge(from, to, String::new());
        }
        else {
          return false;
        }
      }
      true
    }
    else {
      false
    }
  }

  pub fn to_dot<'a>(&'a self) -> Dot<'a, &'a DiGraph<Name, String>> {
    Dot::with_config(&self.inner, &[Config::EdgeNoLabel])
  }

  fn traverse_package<F>(
    &self,
    resolver: F,
    package: Package,
  ) -> Option<(HashSet<Name>, HashSet<(Name, Name)>)>
  where
    F: FnMut(Cid) -> Option<Package> + Clone,
  {
    let mut nodes = HashSet::new();
    let mut edges = HashSet::new();
    nodes.insert(package.name.clone());
    for import in
      package.imports.iter().flat_map(|import| resolver.clone()(import.cid))
    {
      if let Some((new_nodes, new_edges)) =
        self.traverse_package(resolver.clone(), package.clone())
      {
        nodes = nodes.union(&new_nodes).cloned().collect();
        edges = edges.union(&new_edges).cloned().collect();
        edges.insert((import.name.clone(), package.name.clone()));
      }
      else {
        return None;
      }
    }
    Some((nodes, edges))
  }
}

pub struct DagGraph {
  inner: DiGraph<DagNode, DagEdge>,
}

impl DagGraph {
  pub fn from_dag(dag: &DAG) -> Self {
    let mut graph = Self { inner: DiGraph::new() };
    graph.from_dag_ptr(&dag.head, &mut HashMap::new());
    graph
  }

  pub fn from_dag_ptr(
    &mut self,
    node: &DAGPtr,
    map: &mut HashMap<DAGPtr, NodeIndex<DefaultIx>>,
  ) -> NodeIndex<DefaultIx> {
    match node {
      DAGPtr::Var(link) => {
        let Var { nam, dep, rec, binder, parents, .. } =
          unsafe { link.as_ref() };
        if let Some(ix) = map.get(node) {
          *ix
        }
        else {
          let ix = self.inner.add_node(DagNode::Var {
            name: nam.clone(),
            dep: *dep,
            rec: *rec,
          });
          map.insert(*node, ix);
          match binder {
            BinderPtr::Free => {
              self.inner.add_edge(ix, ix, DagEdge::BindFree);
            }
            BinderPtr::Lam(link) => {
              let bind_ix = self.from_dag_ptr(&DAGPtr::Lam(*link), map);
              self.inner.add_edge(ix, bind_ix, DagEdge::BindLam);
            }
            BinderPtr::Slf(link) => {
              let bind_ix = self.from_dag_ptr(&DAGPtr::Slf(*link), map);
              self.inner.add_edge(ix, bind_ix, DagEdge::BindSlf);
            }
            BinderPtr::Fix(link) => {
              let bind_ix = self.from_dag_ptr(&DAGPtr::Fix(*link), map);
              self.inner.add_edge(ix, bind_ix, DagEdge::BindFix);
            }
          }
          self.add_parent_edges(ix, map, *parents);
          ix
        }
      }
      DAGPtr::Lam(link) => {
        if let Some(ix) = map.get(node) {
          *ix
        }
        else {
          let Lam { var, bod, parents, .. } = unsafe { &mut *link.as_ptr() };
          let ix = self.inner.add_node(DagNode::Lam { name: var.nam.clone() });
          map.insert(*node, ix);
          self.add_parent_edges(ix, map, *parents);
          let bod_ix = self.from_dag_ptr(bod, map);
          self.inner.add_edge(ix, bod_ix, DagEdge::Downlink);
          ix
        }
      }
      DAGPtr::Slf(link) => {
        if let Some(ix) = map.get(node) {
          *ix
        }
        else {
          let Slf { var, bod, parents, .. } = unsafe { &mut *link.as_ptr() };
          let ix = self.inner.add_node(DagNode::Slf { name: var.nam.clone() });
          map.insert(*node, ix);
          self.add_parent_edges(ix, map, *parents);
          let bod_ix = self.from_dag_ptr(bod, map);
          self.inner.add_edge(ix, bod_ix, DagEdge::Downlink);
          ix
        }
      }
      DAGPtr::Fix(link) => {
        if let Some(ix) = map.get(node) {
          *ix
        }
        else {
          let Fix { var, bod, parents, .. } = unsafe { &mut *link.as_ptr() };
          let ix = self.inner.add_node(DagNode::Fix { name: var.nam.clone() });
          map.insert(*node, ix);
          self.add_parent_edges(ix, map, *parents);
          let bod_ix = self.from_dag_ptr(bod, map);
          self.inner.add_edge(ix, bod_ix, DagEdge::Downlink);
          ix
        }
      }
      DAGPtr::Cse(link) => {
        if let Some(ix) = map.get(node) {
          *ix
        }
        else {
          let Cse { bod, parents, .. } = unsafe { &mut *link.as_ptr() };
          let ix = self.inner.add_node(DagNode::Cse);
          map.insert(*node, ix);
          self.add_parent_edges(ix, map, *parents);
          let bod_ix = self.from_dag_ptr(bod, map);
          self.inner.add_edge(ix, bod_ix, DagEdge::Downlink);
          ix
        }
      }
      DAGPtr::Dat(link) => {
        if let Some(ix) = map.get(node) {
          *ix
        }
        else {
          let Dat { bod, parents, .. } = unsafe { &mut *link.as_ptr() };
          let ix = self.inner.add_node(DagNode::Dat);
          map.insert(*node, ix);
          self.add_parent_edges(ix, map, *parents);
          let bod_ix = self.from_dag_ptr(bod, map);
          self.inner.add_edge(ix, bod_ix, DagEdge::Downlink);
          ix
        }
      }
      DAGPtr::App(link) => {
        if let Some(ix) = map.get(node) {
          *ix
        }
        else {
          let App { fun, arg, parents, .. } = unsafe { &mut *link.as_ptr() };
          let ix = self.inner.add_node(DagNode::App);
          map.insert(*node, ix);
          self.add_parent_edges(ix, map, *parents);
          let fun_ix = self.from_dag_ptr(fun, map);
          self.inner.add_edge(ix, fun_ix, DagEdge::Downlink);
          let arg_ix = self.from_dag_ptr(arg, map);
          self.inner.add_edge(ix, arg_ix, DagEdge::Downlink);
          ix
        }
      }
      DAGPtr::Ann(link) => {
        if let Some(ix) = map.get(node) {
          *ix
        }
        else {
          let Ann { typ, exp, parents, .. } = unsafe { &mut *link.as_ptr() };
          let ix = self.inner.add_node(DagNode::Ann);
          map.insert(*node, ix);
          self.add_parent_edges(ix, map, *parents);
          let typ_ix = self.from_dag_ptr(typ, map);
          self.inner.add_edge(ix, typ_ix, DagEdge::Downlink);
          let exp_ix = self.from_dag_ptr(exp, map);
          self.inner.add_edge(ix, exp_ix, DagEdge::Downlink);
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
          let ix = self.inner.add_node(DagNode::All { uses: *uses });
          map.insert(*node, ix);
          self.add_parent_edges(ix, map, *parents);
          let dom_ix = self.from_dag_ptr(dom, map);
          self.inner.add_edge(ix, dom_ix, DagEdge::Downlink);
          let img_ix = self.from_dag_ptr(&DAGPtr::Lam(*img), map);
          self.inner.add_edge(ix, img_ix, DagEdge::Downlink);
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
          let ix = self.inner.add_node(DagNode::Let { uses: *uses });
          map.insert(*node, ix);
          self.add_parent_edges(ix, map, *parents);
          let typ_ix = self.from_dag_ptr(typ, map);
          self.inner.add_edge(ix, typ_ix, DagEdge::Downlink);
          let exp_ix = self.from_dag_ptr(exp, map);
          self.inner.add_edge(ix, exp_ix, DagEdge::Downlink);
          let bod_ix = self.from_dag_ptr(&DAGPtr::Lam(*bod), map);
          self.inner.add_edge(ix, bod_ix, DagEdge::Downlink);
          ix
        }
      }
      DAGPtr::Typ(link) => {
        if let Some(ix) = map.get(node) {
          *ix
        }
        else {
          let Typ { parents, .. } = unsafe { &mut *link.as_ptr() };
          let ix = self.inner.add_node(DagNode::Typ);
          map.insert(*node, ix);
          self.add_parent_edges(ix, map, *parents);
          ix
        }
      }
      DAGPtr::Lit(link) => {
        if let Some(ix) = map.get(node) {
          *ix
        }
        else {
          let Lit { lit, parents, .. } = unsafe { &mut *link.as_ptr() };
          let ix = self.inner.add_node(DagNode::Lit { lit: lit.clone() });
          map.insert(*node, ix);
          self.add_parent_edges(ix, map, *parents);
          ix
        }
      }
      DAGPtr::LTy(link) => {
        if let Some(ix) = map.get(node) {
          *ix
        }
        else {
          let LTy { lty, parents, .. } = unsafe { &mut *link.as_ptr() };
          let ix = self.inner.add_node(DagNode::LTy { lty: *lty });
          map.insert(*node, ix);
          self.add_parent_edges(ix, map, *parents);
          ix
        }
      }
      DAGPtr::Opr(link) => {
        if let Some(ix) = map.get(node) {
          *ix
        }
        else {
          let Opr { opr, parents, .. } = unsafe { &mut *link.as_ptr() };
          let ix = self.inner.add_node(DagNode::Opr { opr: opr.clone() });
          map.insert(*node, ix);
          self.add_parent_edges(ix, map, *parents);
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
          let ix = self.inner.add_node(DagNode::Ref {
            name: nam.clone(),
            exp: *exp,
            ast: *ast,
            rec: *rec,
          });
          map.insert(*node, ix);
          self.add_parent_edges(ix, map, *parents);
          ix
        }
      }
    }
  }

  pub fn add_parent_edges(
    &mut self,
    ix: NodeIndex<DefaultIx>,
    map: &mut HashMap<DAGPtr, NodeIndex<DefaultIx>>,
    parents: Option<NonNull<Parents>>,
  ) {
    for p in DLL::iter_option(parents) {
      match *p {
        ParentPtr::Root => {
          self.inner.add_edge(ix, ix, DagEdge::Root);
        }
        ParentPtr::LamBod(link) => {
          let p_ix = self.from_dag_ptr(&DAGPtr::Lam(link), map);
          self.inner.add_edge(ix, p_ix, DagEdge::LamBod);
        }
        ParentPtr::SlfBod(link) => {
          let p_ix = self.from_dag_ptr(&DAGPtr::Slf(link), map);
          self.inner.add_edge(ix, p_ix, DagEdge::SlfBod);
        }
        ParentPtr::FixBod(link) => {
          let p_ix = self.from_dag_ptr(&DAGPtr::Fix(link), map);
          self.inner.add_edge(ix, p_ix, DagEdge::FixBod);
        }
        ParentPtr::DatBod(link) => {
          let p_ix = self.from_dag_ptr(&DAGPtr::Dat(link), map);
          self.inner.add_edge(ix, p_ix, DagEdge::DatBod);
        }
        ParentPtr::CseBod(link) => {
          let p_ix = self.from_dag_ptr(&DAGPtr::Cse(link), map);
          self.inner.add_edge(ix, p_ix, DagEdge::CseBod);
        }
        ParentPtr::AppFun(link) => {
          let p_ix = self.from_dag_ptr(&DAGPtr::App(link), map);
          self.inner.add_edge(ix, p_ix, DagEdge::AppFun);
        }
        ParentPtr::AppArg(link) => {
          let p_ix = self.from_dag_ptr(&DAGPtr::App(link), map);
          self.inner.add_edge(ix, p_ix, DagEdge::AppArg);
        }
        ParentPtr::AllDom(link) => {
          let p_ix = self.from_dag_ptr(&DAGPtr::All(link), map);
          self.inner.add_edge(ix, p_ix, DagEdge::AllDom);
        }
        ParentPtr::AllImg(link) => {
          let p_ix = self.from_dag_ptr(&DAGPtr::All(link), map);
          self.inner.add_edge(ix, p_ix, DagEdge::AllImg);
        }
        ParentPtr::AnnTyp(link) => {
          let p_ix = self.from_dag_ptr(&DAGPtr::Ann(link), map);
          self.inner.add_edge(ix, p_ix, DagEdge::AnnTyp);
        }
        ParentPtr::AnnExp(link) => {
          let p_ix = self.from_dag_ptr(&DAGPtr::Ann(link), map);
          self.inner.add_edge(ix, p_ix, DagEdge::AnnExp);
        }
        ParentPtr::LetTyp(link) => {
          let p_ix = self.from_dag_ptr(&DAGPtr::Let(link), map);
          self.inner.add_edge(ix, p_ix, DagEdge::LetTyp);
        }
        ParentPtr::LetExp(link) => {
          let p_ix = self.from_dag_ptr(&DAGPtr::Let(link), map);
          self.inner.add_edge(ix, p_ix, DagEdge::LetExp);
        }
        ParentPtr::LetBod(link) => {
          let p_ix = self.from_dag_ptr(&DAGPtr::Let(link), map);
          self.inner.add_edge(ix, p_ix, DagEdge::LetBod);
        }
      }
    }
  }

  pub fn to_dot(&self) -> Dot<&DiGraph<DagNode, DagEdge>> {
    Dot::with_attr_getters(
      &self.inner,
      &[],
      &|_, e| match e.weight() {
        DagEdge::Root => ", weight = 2] {{rankdir = TB}} [".to_string(),
        DagEdge::Downlink => ", weight = 2".to_string(),
        DagEdge::Copy => ", weight = 0".to_string(),
        DagEdge::BindFree
        | DagEdge::BindLam
        | DagEdge::BindSlf
        | DagEdge::BindFix => ", weight = 0".to_string(),
        DagEdge::LamBod
        | DagEdge::SlfBod
        | DagEdge::FixBod
        | DagEdge::DatBod
        | DagEdge::CseBod
        | DagEdge::AppFun
        | DagEdge::AppArg
        | DagEdge::AllDom
        | DagEdge::AllImg
        | DagEdge::AnnTyp
        | DagEdge::AnnExp
        | DagEdge::LetTyp
        | DagEdge::LetExp
        | DagEdge::LetBod => ", weight = 1".to_string(),
      },
      &|_, n| match n.1 {
        DagNode::Var { .. } | DagNode::Ref { .. } => {
          format!("] {{rank = max; {}}} [", n.0.index())
        }
        _ => "".to_string(),
      },
    )
  }
}

pub enum DagNode {
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

impl fmt::Display for DagNode {
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
pub enum DagEdge {
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

impl fmt::Display for DagEdge {
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

#[cfg(test)]
mod test {
  use super::*;
  use yatima_core::eval::test::parse;

  #[test]
  fn test() {
    let graph = DagGraph::from_dag(
      &parse("λ f g x y => let x: Type = ∀ (A: Type) -> A; f x (g x)")
        .unwrap()
        .1,
    );
    println!("{}", graph.to_dot());
    // assert_eq!(true, false)
  }
}
