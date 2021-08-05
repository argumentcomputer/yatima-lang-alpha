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

use crate::{
  pre_term::PreTerm,
  pre_uses::PreUses,
};

#[derive(Clone, Debug, PartialEq)]
pub struct LetDef {
  pub rec: bool,
  pub uses: PreUses,
  pub name: Name,
  pub typ_: Expr,
  pub term: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Binder {
  pub uses: PreUses,
  pub name: Name,
  pub typ_: Expr,
}

impl Binder {
  pub fn new(uses: PreUses, name: Name, typ_: Expr) -> Self {
    Binder { uses, name, typ_ }
  }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Argument {
  pub uses: PreUses,
  pub typ_: Expr,
  pub term: Expr,
}
impl Argument {
  pub fn new(uses: PreUses, typ_: Expr, term: Expr) -> Self {
    Argument { uses, typ_, term }
  }
}

#[derive(Clone, Debug)]
pub enum Expr {
  Variable(Pos, Name),
  MetaVariable(Pos, Name),
  Application(Pos, Box<Expr>, Vec<Argument>),
  Lambda(Pos, Vec<Binder>, Box<Expr>),
  Forall(Pos, Vec<Binder>, Box<Expr>),
  SelfType(Pos, Name, Box<Expr>),
  SelfData(Pos, Box<Expr>, Box<Expr>),
  SelfCase(Pos, Box<Expr>),
  Reference(Pos, Name),
  Let(Pos, Vec<LetDef>, Box<Expr>),
  LetReference(Pos, Name),
  Type(Pos),
  Literal(Pos, Literal),
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
  LitOpr(Pos, Op),
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
      (Self::SelfData(_, ta, ba), Self::SelfData(_, tb, bb)) => {
        ta == tb && ba == bb
      }
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

impl Expr {
  pub fn elaborate(self) -> PreTerm {
    match self {
      Self::Variable(pos, name) => PreTerm::Var(pos, name),
      Self::MetaVariable(pos, name) => PreTerm::Hol(pos, name),
      Self::Type(pos) => PreTerm::Typ(pos),
      Self::Literal(pos, lit) => PreTerm::Lit(pos, lit),
      Self::LitType(pos, typ) => PreTerm::LTy(pos, typ),
      Self::LitOpr(pos, opr) => PreTerm::Opr(pos, opr),
      Self::SelfType(pos, nam, bod) => {
        PreTerm::Slf(pos, nam, Box::new(bod.elaborate()))
      }
      Self::SelfCase(pos, bod) => PreTerm::Cse(pos, Box::new(bod.elaborate())),
      Self::SelfData(pos, typ, bod) => {
        PreTerm::Dat(pos, Box::new(typ.elaborate()), Box::new(bod.elaborate()))
      }
      Self::Application(pos, fun, args) => args.into_iter().fold(
        fun.elaborate(),
        |acc, Argument { uses, typ_, term }| {
          PreTerm::App(
            pos,
            uses,
            Box::new(acc),
            Box::new(typ_.elaborate()),
            Box::new(term.elaborate()),
          )
        },
      ),
      Self::Lambda(pos, binds, bod) => binds.into_iter().rev().fold(
        bod.elaborate(),
        |acc, Binder { uses, name, typ_ }| {
          PreTerm::Lam(
            pos,
            uses,
            name,
            Box::new(typ_.elaborate()),
            Box::new(acc),
          )
        },
      ),
      Self::Forall(pos, binds, bod) => binds.into_iter().rev().fold(
        bod.elaborate(),
        |acc, Binder { uses, name, typ_ }| {
          PreTerm::All(
            pos,
            uses,
            name,
            Box::new(typ_.elaborate()),
            Box::new(acc),
          )
        },
      ),
      _ => todo!(),
    }
  }
}
