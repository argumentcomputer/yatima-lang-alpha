use core::fmt;

use crate::{
  ipld_error::IpldError,
  name::Name,
  position::Pos,
};
use sp_cid::Cid;
use sp_ipld::Ipld;

use sp_std::{
  borrow::ToOwned,
  boxed::Box,
};

use alloc::string::ToString;

/// Metadata term containing only name, source position, and content id
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
}

impl Meta {
  /// Converts a metadata term into an IPLD object
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
    }
  }

  /// Converts an IPLD object into a metadata term
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
        xs => Err(IpldError::Meta(Ipld::List(xs.to_owned()))),
      },
      xs => Err(IpldError::Meta(xs.to_owned())),
    }
  }
}

impl fmt::Display for Meta {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use Meta::*;
    match self.clone() {
      Var(_, name) => write!(f, "Var({})", name),
      Lam(_, name, b) => write!(f, "Lam({}, {}", name, *b),
      App(_, b) => write!(f, "App({}, {})", (*b).0, (*b).1),
      All(_, name, b) => write!(f, "All({}, {}, {})", name, (*b).0, (*b).1),
      Slf(_, name, b) => writeln!(f, "Slf({}, {})", name, *b),
      Dat(_, b) => write!(f, "Dat({})", *b),
      Cse(_, b) => write!(f, "Cse({})", *b),
      Ref(_, name, cid) => write!(f, "Ref({}, {})", name, cid),
      Let(_, name, b) => {
        write!(f, "Let({}, {}, {}, {})", name, (*b).0, (*b).1, (*b).2)
      }
      Typ(_) => write!(f, "Typ"),
      Ann(_, b) => write!(f, "Ann({}, {})", (*b).0, (*b).1),
      Lit(_) => write!(f, "Lit"),
      LTy(_) => write!(f, "LTy"),
      Opr(_) => write!(f, "Opr"),
      Rec(_) => write!(f, "Rec"),
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
