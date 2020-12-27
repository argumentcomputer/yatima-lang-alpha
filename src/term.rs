use crate::uses::Uses;
use hashexpr::{
  atom::{
    Atom,
    Link,
  },
  position::Pos,
};
use num_bigint::{
  BigInt,
  BigUint,
};
use std::collections::HashMap;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Literal {
  Natural(BigUint),
  Integer(BigInt),
  Bits(Vec<u8>),
  Text(String),
  Char(char),
  Link(Link),
  Exception,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum LitType {
  Natural,
  Integer,
  Bits,
  Text,
  Char,
  Link,
  Exception,
}

#[derive(Clone, Debug)]
pub enum Term {
  Var(Pos, String, u64),
  All(Pos, String, Uses, Box<Term>, Box<Term>),
  Lam(Pos, String, Box<Term>),
  App(Pos, Box<(Term, Term)>),
  Slf(Pos, String, Box<Term>),
  Dat(Pos, Box<Term>),
  Cse(Pos, Box<Term>),
  Ref(Pos, String, Link, Link),
  Let(Pos, bool, String, Uses, Box<(Term, Term, Term)>),
  Ann(Pos, Box<(Term, Term)>),
  Lit(Pos, Literal),
  LTy(Pos, LitType),
}

pub struct Def {
  pos: Pos,
  name: String,
  doc: String,
  term: Term,
  typ_: Term,
}

pub type Defs = HashMap<String, Def>;
