use crate::ipld_error::IpldError;
use sp_cid::Cid;
use sp_ipld::{
  dag_cbor::cid,
  Ipld,
};

use crate::{
  literal::{
    LitType,
    Literal,
  },
  prim::Op,
  uses::Uses,
};

use sp_std::{
  borrow::ToOwned,
  boxed::Box,
  convert::TryInto,
};

#[derive(PartialEq, Clone, Debug)]
pub enum Anon {
  Var(u64),
  Lam(Uses, Box<(Anon, Anon)>),
  App(Uses, Box<(Anon, Anon, Anon)>),
  All(Uses, Box<(Anon, Anon)>),
  Slf(Box<Anon>),
  Dat(Box<(Anon, Anon)>),
  Cse(Box<Anon>),
  Ref(Cid),
  Let(bool, Uses, Box<(Anon, Anon, Anon)>),
  Typ,
  Lit(Literal),
  LTy(LitType),
  Opr(Op),
  Rec,
}

/// var: [0, idx]
/// lam: [1, <body>]
/// app: [2, <fun>, <arg>]

impl Anon {
  pub fn to_ipld(&self) -> Ipld {
    match self {
      Self::Var(idx) => {
        Ipld::List(vec![Ipld::Integer(0), Ipld::Integer(*idx as i128)])
      }
      Self::Lam(uses, typ_bod) => {
        let (typ, bod) = (*typ_bod).as_ref();
        Ipld::List(vec![
          Ipld::Integer(1),
          uses.to_ipld(),
          typ.to_ipld(),
          bod.to_ipld(),
        ])
      }
      Self::App(uses, fun_typ_arg) => {
        let (fun, typ, arg) = (*fun_typ_arg).as_ref();
        Ipld::List(vec![
          Ipld::Integer(2),
          uses.to_ipld(),
          fun.to_ipld(),
          typ.to_ipld(),
          arg.to_ipld(),
        ])
      }
      Self::All(uses, dom_img) => {
        let (dom, img) = (*dom_img).as_ref();
        Ipld::List(vec![
          Ipld::Integer(3),
          uses.to_ipld(),
          dom.to_ipld(),
          img.to_ipld(),
        ])
      }
      Self::Slf(bod) => Ipld::List(vec![Ipld::Integer(4), bod.to_ipld()]),
      Self::Dat(typ_bod) => {
        let (typ, bod) = (*typ_bod).as_ref();
        Ipld::List(vec![Ipld::Integer(5), typ.to_ipld(), bod.to_ipld()])
      }

      Self::Cse(bod) => Ipld::List(vec![Ipld::Integer(6), bod.to_ipld()]),
      Self::Ref(cid) => Ipld::List(vec![Ipld::Integer(7), Ipld::Link(*cid)]),
      Self::Let(rec, uses, typ_exp_bod) => {
        let (typ, exp, bod) = (*typ_exp_bod).as_ref();
        Ipld::List(vec![
          Ipld::Integer(8),
          Ipld::Bool(*rec),
          uses.to_ipld(),
          typ.to_ipld(),
          exp.to_ipld(),
          bod.to_ipld(),
        ])
      }
      Self::Typ => Ipld::List(vec![Ipld::Integer(9)]),
      Self::Lit(lit) => {
        Ipld::List(vec![Ipld::Integer(10), lit.clone().to_ipld()])
      }
      Self::LTy(lty) => Ipld::List(vec![Ipld::Integer(11), lty.to_ipld()]),
      Self::Opr(opr) => Ipld::List(vec![Ipld::Integer(12), opr.to_ipld()]),
      Self::Rec => Ipld::List(vec![Ipld::Integer(13)]),
    }
  }

  pub fn cid(&self) -> Cid { cid(&self.to_ipld()) }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::List(xs) => match xs.as_slice() {
        [Ipld::Integer(0), Ipld::Integer(x)] => {
          let idx: u64 = (*x).try_into().map_err(IpldError::U64)?;
          Ok(Anon::Var(idx))
        }
        [Ipld::Integer(1), uses, typ, bod] => {
          let uses = Uses::from_ipld(uses)?;
          let typ = Anon::from_ipld(typ)?;
          let bod = Anon::from_ipld(bod)?;
          Ok(Anon::Lam(uses, Box::new((typ, bod))))
        }
        [Ipld::Integer(2), uses, fun, typ, arg] => {
          let uses = Uses::from_ipld(uses)?;
          let fun = Anon::from_ipld(fun)?;
          let typ = Anon::from_ipld(typ)?;
          let arg = Anon::from_ipld(arg)?;
          Ok(Anon::App(uses, Box::new((fun, typ, arg))))
        }
        [Ipld::Integer(3), uses, dom, img] => {
          let uses = Uses::from_ipld(uses)?;
          let dom = Anon::from_ipld(dom)?;
          let img = Anon::from_ipld(img)?;
          Ok(Anon::All(uses, Box::new((dom, img))))
        }
        [Ipld::Integer(4), bod] => {
          let bod = Anon::from_ipld(bod)?;
          Ok(Anon::Slf(Box::new(bod)))
        }
        [Ipld::Integer(5), typ, bod] => {
          let typ = Anon::from_ipld(typ)?;
          let bod = Anon::from_ipld(bod)?;
          Ok(Anon::Dat(Box::new((typ, bod))))
        }
        [Ipld::Integer(6), bod] => {
          let bod = Anon::from_ipld(bod)?;
          Ok(Anon::Cse(Box::new(bod)))
        }
        [Ipld::Integer(7), Ipld::Link(cid)] => Ok(Anon::Ref(*cid)),
        [Ipld::Integer(8), Ipld::Bool(rec), uses, typ, exp, bod] => {
          let uses = Uses::from_ipld(uses)?;
          let typ = Anon::from_ipld(typ)?;
          let exp = Anon::from_ipld(exp)?;
          let bod = Anon::from_ipld(bod)?;
          Ok(Anon::Let(*rec, uses, Box::new((typ, exp, bod))))
        }
        [Ipld::Integer(9)] => Ok(Anon::Typ),
        [Ipld::Integer(10), lit] => {
          let lit = Literal::from_ipld(&lit)?;
          Ok(Self::Lit(lit))
        }
        [Ipld::Integer(11), lty] => {
          let lty = LitType::from_ipld(&lty)?;
          Ok(Self::LTy(lty))
        }
        [Ipld::Integer(12), opr] => {
          let opr = Op::from_ipld(&opr)?;
          Ok(Self::Opr(opr))
        }
        [Ipld::Integer(13)] => Ok(Self::Rec),
        xs => Err(IpldError::Anon(Ipld::List(xs.to_owned()))),
      },
      xs => Err(IpldError::Anon(xs.to_owned())),
    }
  }
}

#[cfg(test)]
pub mod tests {
  use super::*;
  use quickcheck::{
    Arbitrary,
    Gen,
  };

  use crate::term::Term;

  impl Arbitrary for Anon {
    fn arbitrary(g: &mut Gen) -> Self {
      let term: Term = Arbitrary::arbitrary(g);
      let (anon, _) = term.embed();
      anon
    }
  }

  #[quickcheck]
  fn anon_ipld(x: Anon) -> bool {
    match Anon::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}
