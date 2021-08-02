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

#[derive(Clone, Debug, PartialEq)]
pub struct LetDef {
  rec: bool,
  uses: PreUses,
  name: Name,
  typ_: Expr,
  term: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Binder {
  pub uses: PreUses,
  pub name: Name,
  pub typ_: Expr,
}

#[derive(Clone, Debug)]
pub enum Expr {
  Variable(Pos, Name),
  MetaVariable(Pos, Name),
  Application(Pos, Box<Expr>, Vec<(PreUses, Expr, Expr)>),
  Lambda(Pos, Vec<Binder>, Box<Expr>),
  Forall(Pos, Vec<Binder>, Box<Expr>),
  SelfType(Pos, Name, Box<Expr>),
  SelfData(Pos, Box<(Expr, Expr)>),
  SelfCase(Pos, Box<Expr>),
  Reference(Pos, Name),
  // Let(Pos, Vec<LetDef>, Box<Expr>),
  Type(Pos),
  Literal(Pos, Literal),
  LitOpr(Pos, Op),
  // Tuple (x,y,z)
  // ConsList [x,y,z] => List.Cons ?a x (List.Cons ?a y (List.Cons ?a z))
  // Number 42 => Nat, U64,
  // DoNotation do IO { .. }
  // Match(Pos, Match),
  //
  // match (f x) -> P self
  // | Cons x xs => 1,
  // | Nil       => Bool.True,
  //
  // ModuleSig(Pos, ModuleSig),
  // sig Foo { x : Nat, y: Int }
  // Module(Pos, Module),
  // module foo { x : Nat = 1, y: Int= +1 }
  // module foo : Foo { x = 1, y = +1 }
  // ModuleOpr(Pos, ModuleOpr),
  // foo.x => Mod_get "x" foo
  LitType(Pos, LitType),
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
      (Self::Forall(_, aa, ba), Self::Forall(_, ab, bb)) => {
        aa == ab && ba == bb
      }
      (Self::SelfType(_, na, ba), Self::SelfType(_, nb, bb)) => {
        na == nb && ba == bb
      }
      (Self::SelfData(_, ba), Self::SelfData(_, bb)) => ba == bb,
      (Self::SelfCase(_, ba), Self::SelfCase(_, bb)) => ba == bb,
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
