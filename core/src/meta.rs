use crate::{
  ipld_error::IpldError,
  name::Name,
  position::Pos,
};
use sp_cid::Cid;
use sp_ipld::Ipld;

use sp_im::{
  OrdMap,
  Vector,
};

use sp_std::{
  borrow::ToOwned,
  boxed::Box,
  collections::btree_map::BTreeMap,
  vec::Vec,
};

use alloc::string::ToString;

#[derive(PartialEq, Clone, Debug)]
pub enum Meta {
  Var(Pos, Name),
  Lam(Pos, Name, Box<Meta>),
  App(Pos, Box<(Meta, Meta)>),
  All(Pos, Name, Box<(Meta, Meta)>),
  Slf(Pos, Name, Box<Meta>),
  Dat(Pos, Box<Meta>),
  Cse(Pos, Box<Meta>),
  Ref(Pos, Name, Cid),
  Let(Pos, Name, Box<(Meta, Meta, Meta)>),
  Typ(Pos),
  Ann(Pos, Box<(Meta, Meta)>),
  Lit(Pos),
  LTy(Pos),
  Opr(Pos),
  Rec(Pos),
  Vec(Pos, Box<Meta>, Vector<Meta>),
  HVc(Pos, Vector<(Meta, Meta)>),
  Map(Pos, Box<Meta>, OrdMap<Name, Meta>),
  Mod(Pos, OrdMap<Name, (Meta, Meta)>),
}

impl Meta {
  pub fn to_ipld(&self) -> Ipld {
    match self {
      Self::Var(pos, nam) => Ipld::List(vec![
        Ipld::Integer(0),
        pos.to_ipld(),
        Ipld::String(nam.to_string()),
      ]),
      Self::Lam(pos, nam, bod) => Ipld::List(vec![
        Ipld::Integer(1),
        pos.to_ipld(),
        Ipld::String(nam.to_string()),
        bod.to_ipld(),
      ]),
      Self::App(pos, fun_arg) => {
        let (fun, arg) = (*fun_arg).as_ref();
        Ipld::List(vec![
          Ipld::Integer(2),
          pos.to_ipld(),
          fun.to_ipld(),
          arg.to_ipld(),
        ])
      }
      Self::All(pos, nam, dom_img) => {
        let (dom, img) = (*dom_img).as_ref();
        Ipld::List(vec![
          Ipld::Integer(3),
          pos.to_ipld(),
          Ipld::String(nam.to_string()),
          dom.to_ipld(),
          img.to_ipld(),
        ])
      }
      Self::Slf(pos, nam, bod) => Ipld::List(vec![
        Ipld::Integer(4),
        pos.to_ipld(),
        Ipld::String(nam.to_string()),
        bod.to_ipld(),
      ]),
      Self::Dat(pos, bod) => {
        Ipld::List(vec![Ipld::Integer(5), pos.to_ipld(), bod.to_ipld()])
      }
      Self::Cse(pos, bod) => {
        Ipld::List(vec![Ipld::Integer(6), pos.to_ipld(), bod.to_ipld()])
      }
      Self::Ref(pos, nam, cid) => Ipld::List(vec![
        Ipld::Integer(7),
        pos.to_ipld(),
        Ipld::String(nam.to_string()),
        Ipld::Link(*cid),
      ]),
      Self::Let(pos, nam, typ_exp_bod) => {
        let (typ, exp, bod) = (*typ_exp_bod).as_ref();
        Ipld::List(vec![
          Ipld::Integer(8),
          pos.to_ipld(),
          Ipld::String(nam.to_string()),
          typ.to_ipld(),
          exp.to_ipld(),
          bod.to_ipld(),
        ])
      }
      Self::Typ(pos) => Ipld::List(vec![Ipld::Integer(9), pos.to_ipld()]),
      Self::Ann(pos, typ_exp) => {
        let (typ, exp) = (*typ_exp).as_ref();
        Ipld::List(vec![
          Ipld::Integer(10),
          pos.to_ipld(),
          typ.to_ipld(),
          exp.to_ipld(),
        ])
      }
      Self::Lit(pos) => Ipld::List(vec![Ipld::Integer(11), pos.to_ipld()]),
      Self::LTy(pos) => Ipld::List(vec![Ipld::Integer(12), pos.to_ipld()]),
      Self::Opr(pos) => Ipld::List(vec![Ipld::Integer(13), pos.to_ipld()]),
      Self::Rec(pos) => Ipld::List(vec![Ipld::Integer(14), pos.to_ipld()]),
      Self::Vec(pos, typ, vec) => Ipld::List(vec![
        Ipld::Integer(15),
        pos.to_ipld(),
        typ.to_ipld(),
        Ipld::List(vec.into_iter().map(|x| x.to_ipld()).collect()),
      ]),
      Self::HVc(pos, arr) => {
        let mut typs = Vec::new();
        let mut trms = Vec::new();
        for (t, x) in arr {
          typs.push(t.to_ipld());
          trms.push(x.to_ipld());
        }
        Ipld::List(vec![
          Ipld::Integer(16),
          pos.to_ipld(),
          Ipld::List(typs),
          Ipld::List(trms),
        ])
      }
      Self::Map(pos, typ, mty) => Ipld::List(vec![
        Ipld::Integer(17),
        pos.to_ipld(),
        typ.to_ipld(),
        Ipld::StringMap(
          mty.into_iter().map(|(k, v)| (k.to_string(), v.to_ipld())).collect(),
        ),
      ]),
      Self::Mod(pos, mod_) => {
        let mut typs = BTreeMap::new();
        let mut trms = BTreeMap::new();
        for (n, (t, x)) in mod_ {
          typs.insert(n.to_string(), t.to_ipld());
          trms.insert(n.to_string(), x.to_ipld());
        }
        Ipld::List(vec![
          Ipld::Integer(18),
          pos.to_ipld(),
          Ipld::StringMap(typs),
          Ipld::StringMap(trms),
        ])
      }
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::List(xs) => match xs.as_slice() {
        [Ipld::Integer(0), pos, Ipld::String(nam)] => {
          let pos = Pos::from_ipld(pos)?;
          Ok(Meta::Var(pos, Name::from(nam.clone())))
        }
        [Ipld::Integer(1), pos, Ipld::String(nam), bod] => {
          let pos = Pos::from_ipld(pos)?;
          let bod = Meta::from_ipld(bod)?;
          Ok(Meta::Lam(pos, Name::from(nam.clone()), Box::new(bod)))
        }
        [Ipld::Integer(2), pos, fun, arg] => {
          let pos = Pos::from_ipld(pos)?;
          let fun = Meta::from_ipld(fun)?;
          let arg = Meta::from_ipld(arg)?;
          Ok(Meta::App(pos, Box::new((fun, arg))))
        }
        [Ipld::Integer(3), pos, Ipld::String(nam), dom, img] => {
          let pos = Pos::from_ipld(pos)?;
          let dom = Meta::from_ipld(dom)?;
          let img = Meta::from_ipld(img)?;
          Ok(Meta::All(pos, Name::from(nam.clone()), Box::new((dom, img))))
        }
        [Ipld::Integer(4), pos, Ipld::String(nam), bod] => {
          let pos = Pos::from_ipld(pos)?;
          let bod = Meta::from_ipld(bod)?;
          Ok(Meta::Slf(pos, Name::from(nam.clone()), Box::new(bod)))
        }
        [Ipld::Integer(5), pos, bod] => {
          let pos = Pos::from_ipld(pos)?;
          let bod = Meta::from_ipld(bod)?;
          Ok(Meta::Dat(pos, Box::new(bod)))
        }
        [Ipld::Integer(6), pos, bod] => {
          let pos = Pos::from_ipld(pos)?;
          let bod = Meta::from_ipld(bod)?;
          Ok(Meta::Cse(pos, Box::new(bod)))
        }
        [Ipld::Integer(7), pos, Ipld::String(nam), Ipld::Link(cid)] => {
          let pos = Pos::from_ipld(pos)?;
          Ok(Meta::Ref(pos, Name::from(nam.clone()), *cid))
        }
        [Ipld::Integer(8), pos, Ipld::String(nam), typ, exp, bod] => {
          let pos = Pos::from_ipld(pos)?;
          let typ = Meta::from_ipld(typ)?;
          let exp = Meta::from_ipld(exp)?;
          let bod = Meta::from_ipld(bod)?;
          Ok(Meta::Let(pos, Name::from(nam.clone()), Box::new((typ, exp, bod))))
        }
        [Ipld::Integer(9), pos] => {
          let pos = Pos::from_ipld(pos)?;
          Ok(Meta::Typ(pos))
        }
        [Ipld::Integer(10), pos, typ, exp] => {
          let pos = Pos::from_ipld(pos)?;
          let typ = Meta::from_ipld(typ)?;
          let exp = Meta::from_ipld(exp)?;
          Ok(Meta::Ann(pos, Box::new((typ, exp))))
        }
        [Ipld::Integer(11), pos] => {
          let pos = Pos::from_ipld(pos)?;
          Ok(Self::Lit(pos))
        }
        [Ipld::Integer(12), pos] => {
          let pos = Pos::from_ipld(pos)?;
          Ok(Self::LTy(pos))
        }
        [Ipld::Integer(13), pos] => {
          let pos = Pos::from_ipld(pos)?;
          Ok(Self::Opr(pos))
        }
        [Ipld::Integer(14), pos] => {
          let pos = Pos::from_ipld(pos)?;
          Ok(Self::Rec(pos))
        }
        [Ipld::Integer(15), pos, typ, Ipld::List(vec)] => {
          let pos = Pos::from_ipld(pos)?;
          let typ = Meta::from_ipld(typ)?;
          let mut res = Vector::new();
          for x in vec {
            let x = Meta::from_ipld(x)?;
            res.push_back(x);
          }
          Ok(Self::Vec(pos, Box::new(typ), res))
        }
        [Ipld::Integer(16), pos, Ipld::List(typs), Ipld::List(trms)] => {
          let pos = Pos::from_ipld(pos)?;
          let mut res = Vector::new();
          for (t, x) in typs.iter().zip(trms.iter()) {
            let t = Meta::from_ipld(t)?;
            let x = Meta::from_ipld(x)?;
            res.push_back((t, x));
          }
          Ok(Self::HVc(pos, res))
        }
        [Ipld::Integer(17), pos, typ, Ipld::StringMap(map)] => {
          let pos = Pos::from_ipld(pos)?;
          let typ = Meta::from_ipld(typ)?;
          let mut res = OrdMap::new();
          for (k, x) in map {
            let x = Meta::from_ipld(x)?;
            res.insert(Name::from(k.clone()), x);
          }
          Ok(Self::Map(pos, Box::new(typ), res))
        }
        [Ipld::Integer(18), pos, Ipld::StringMap(typs), Ipld::StringMap(trms)] =>
        {
          let pos = Pos::from_ipld(pos)?;
          let mut res = OrdMap::new();
          for (n, t) in typs.iter() {
            let t = Meta::from_ipld(t)?;
            let x = trms.get(n).ok_or(IpldError::ModTerm)?;
            let x = Meta::from_ipld(x)?;
            res.insert(Name::from(n.clone()), (t, x));
          }
          Ok(Self::Mod(pos, res))
        }
        xs => Err(IpldError::Meta(Ipld::List(xs.to_owned()))),
      },
      xs => Err(IpldError::Meta(xs.to_owned())),
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

  impl Arbitrary for Meta {
    fn arbitrary(g: &mut Gen) -> Self {
      let term: Term = Arbitrary::arbitrary(g);
      let (_, meta) = term.embed();
      meta
    }
  }

  #[quickcheck]
  fn meta_ipld(x: Meta) -> bool {
    match Meta::from_ipld(&x.to_ipld()) {
      Ok(y) => x == y,
      _ => false,
    }
  }
}
