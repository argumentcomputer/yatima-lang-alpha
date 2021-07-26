use yatima_core::{
  literal::{
    LitType,
    Literal,
  },
  name::Name,
  position::Pos,
  prim::Op,
};

use sp_cid::Cid;

use sp_std::{
  boxed::Box,
  fmt,
  vec::Vec,
};

use crate::pre_uses::PreUses;

#[derive(Clone, Debug)]
pub enum Expr {
  Variable(Pos, Name),
  MetaVariable(Pos, Name),
  Application(Pos, Box<Expr>, Vec<(PreUses, Expr, Expr)>),
  Lambda(Pos, Vec<(PreUses, Name, Expr)>, Box<Expr>),
  // Forall(Pos, Vec<(PreUses, Name, Expr)>, Box<Expr>),
  // SelfType(Pos, Name, Box<Expr>),
  // SelfData(Pos, Box<(Expr, Expr)>),
  // SelfCase(Pos, Box<Expr>),
  Reference(Pos, Name),
  // Let(Pos, Box<(bool, PreUses, Name, Expr, Expr)>, Box<Expr>),
  Type(Pos),
  Literal(Pos, Literal),
  LitType(Pos, LitType),
  /* LitOpr(Pos, Op),
   * Tuple
   * ConsList
   * Number
   * DoNotation
   * Match(Pos, Match),
   * ModuleSig(Pos, ModuleSig),
   * Module(Pos, Module),
   * ModuleOpr(Pos, ModuleOpr), */
}

impl PartialEq for Expr {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::Variable(_, na), Self::Variable(_, nb)) => na == nb,
      (Self::MetaVariable(_, na), Self::MetaVariable(_, nb)) => na == nb,
      (Self::Type(_), Self::Type(_)) => true,
      (Self::Application(_, fa, aa), Self::Application(_, fb, ab)) => {
        fa == fb && aa == ab
      }
      (Self::Reference(_, na), Self::Reference(_, nb)) => na == nb,
      (Self::Literal(_, a), Self::Literal(_, b)) => a == b,
      (Self::LitType(_, a), Self::LitType(_, b)) => a == b,
      (Self::Lambda(_, aa, ba), Self::Lambda(_, ab, bb)) => {
        aa == ab && ba == bb
      }
      //(Self::All(_, ua, na, da, ia), Self::All(_, ub, nb, db, ib)) => {
      //  ua == ub && na == nb && da == db && ia == ib
      //}
      //(Self::Slf(_, na, ba), Self::Slf(_, nb, bb)) => na == nb && ba == bb,
      //(Self::Dat(_, ta, ba), Self::Dat(_, tb, bb)) => ta == tb && ba == bb,
      //(Self::Cse(_, ba), Self::Cse(_, bb)) => ba == bb,
      //(
      //  Self::Let(_, ra, ua, na, ta, xa, ba),
      //  Self::Let(_, rb, ub, nb, tb, xb, bb),
      //) => ra == rb && ua == ub && na == nb && ta == tb && xa == xb && ba == bb,
      //(Self::Rec(_), Self::Rec(_)) => true,
      //(Self::Opr(_, a), Self::Opr(_, b)) => a == b,
      _ => false,
    }
  }
}

//// TypDecl List
//// DatDecl Cons
//// DatDecl Nat
//
//// match x -> Nat {
////  Cons x xs => 1,
////  Nil => 0,
//// }
//
