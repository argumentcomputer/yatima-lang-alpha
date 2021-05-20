use std::{
  error::Error,
  fmt,
};

use crate::{
  check::ctx::*,
  position::Pos,
  term::Term,
  uses::Uses,
};

#[derive(Debug)]
pub enum CheckError {
  UndefinedReference(Pos, String),
  UnboundVariable(Pos, ErrCtx, String, u64),
  UntypedLambda(Pos, ErrCtx),
  QuantityTooLittle(Pos, ErrCtx, Uses, Uses),
  QuantityTooMuch(Pos, ErrCtx, Uses, Uses),
  TypeMismatch(Pos, ErrCtx, Term, Term),
  LamAllMismatch(Pos, ErrCtx, Term, Term),
  DatSlfMismatch(Pos, ErrCtx, Term, Term),
  AppFunMismatch(Pos, ErrCtx, Term, Term),
  CseDatMismatch(Pos, ErrCtx, Term, Term),
  GenericError(Pos, ErrCtx, String),
}

impl CheckError {
  pub fn pos(&self) -> Pos {
    match self {
      Self::TypeMismatch(pos, ..) => *pos,
      Self::GenericError(pos, ..) => *pos,
      _ => Pos::None,
    }
  }
}

pub fn pretty_pos(pos: Pos) -> String {
  if let Pos::Some(pos) = pos {
    format!(
      "from {}:{} to {}:{} in {}",
      pos.from_line, pos.from_column, pos.upto_line, pos.upto_column, pos.input
    )
  }
  else {
    "at unknown location".to_owned()
  }
}

impl fmt::Display for CheckError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      CheckError::UnboundVariable(pos, ctx, name, dep) => {
        write!(
          f,
          "Unbound free variable \"{}\" with depth {} {}",
          name,
          dep,
          pretty_pos(*pos)
        )?;
        if !ctx.is_empty() {
          writeln!(f, "• Context:")?;
          for (n, typ) in ctx {
            writeln!(f, "  - {}: {}", n, typ)?;
          }
        }
        Ok(())
      }
      CheckError::UndefinedReference(pos, name) => {
        write!(f, "Undefined reference \"{}\" {}", name, pretty_pos(*pos))
      }
      CheckError::UntypedLambda(pos, ctx) => {
        write!(f, "Untyped lambda {}", pretty_pos(*pos))?;
        if !ctx.is_empty() {
          writeln!(f, "• Context:")?;
          for (n, typ) in ctx {
            writeln!(f, "  - {}: {}", n, typ)?;
          }
        }
        Ok(())
      }
      CheckError::LamAllMismatch(pos, ctx, trm, typ) => {
        writeln!(
          f,
          "The type of a lambda (λ) is not a forall (∀) {}",
          pretty_pos(*pos)
        )?;
        if !ctx.is_empty() {
          writeln!(f, "• Context:")?;
          for (n, typ) in ctx {
            writeln!(f, "  - {}: {}", n, typ)?;
          }
        }
        writeln!(f, "• Checked: {}", trm)?;
        writeln!(f, "• Against: {}", typ)?;
        Ok(())
      }
      CheckError::AppFunMismatch(pos, ctx, fun, typ) => {
        writeln!(
          f,
          "Tried to apply an expression which is not a function {}",
          pretty_pos(*pos)
        )?;
        if !ctx.is_empty() {
          writeln!(f, "• Context:")?;
          for (n, typ) in ctx {
            writeln!(f, "  - {}: {}", n, typ)?;
          }
        }
        writeln!(f, "• Checked: {}", fun)?;
        writeln!(f, "• Against: {}", typ)?;
        Ok(())
      }
      CheckError::CseDatMismatch(pos, ctx, dat, typ) => {
        writeln!(
          f,
          "Tried to case match on an expression which is not an inductive \
           datatype or literal {}",
          pretty_pos(*pos)
        )?;
        if !ctx.is_empty() {
          writeln!(f, "• Context:")?;
          for (n, typ) in ctx {
            writeln!(f, "  - {}: {}", n, typ)?;
          }
        }
        writeln!(f, "• Checked: {}", dat)?;
        writeln!(f, "• Against: {}", typ)?;
        Ok(())
      }
      CheckError::DatSlfMismatch(pos, ctx, trm, typ) => {
        writeln!(
          f,
          "The type of a data constructor is not a self-type {}",
          pretty_pos(*pos)
        )?;
        if !ctx.is_empty() {
          writeln!(f, "• Context:")?;
          for (n, typ) in ctx {
            writeln!(f, "  - {}: {}", n, typ)?;
          }
        }
        writeln!(f, "• Checked: {}", trm)?;
        writeln!(f, "• Against: {}", typ)?;
        Ok(())
      }
      CheckError::QuantityTooLittle(pos, ctx, exp, det) => {
        writeln!(f, "Quantity not used enough {}", pretty_pos(*pos))?;
        if !ctx.is_empty() {
          writeln!(f, "• Context:")?;
          for (n, typ) in ctx {
            writeln!(f, "  - {}: {}", n, typ)?;
          }
        }
        writeln!(f, "• Expected: {}", exp)?;
        writeln!(f, "• Detected: {}", det)?;
        Ok(())
      }
      CheckError::QuantityTooMuch(pos, ctx, exp, det) => {
        writeln!(f, "Quantity used too much {}", pretty_pos(*pos))?;
        if !ctx.is_empty() {
          writeln!(f, "• Context:")?;
          for (n, typ) in ctx {
            writeln!(f, "  - {}: {}", n, typ)?;
          }
        }
        writeln!(f, "• Expected: {}", exp)?;
        writeln!(f, "• Detected: {}", det)?;
        Ok(())
      }
      CheckError::TypeMismatch(pos, ctx, exp, det) => {
        writeln!(f, "Type Mismatch {}", pretty_pos(*pos))?;
        if !ctx.is_empty() {
          writeln!(f, "• Context:")?;
          for (n, typ) in ctx {
            writeln!(f, "  - {}: {}", n, typ)?;
          }
        }
        writeln!(f, "• Expected: {}", exp)?;
        writeln!(f, "• Detected: {}", det)?;
        Ok(())
      }
      CheckError::GenericError(pos, ctx, msg) => {
        writeln!(f, "{} {} ", msg, pretty_pos(*pos))?;
        if !ctx.is_empty() {
          write!(f, "With context:\n{} ", pretty_context(ctx))?;
        }
        Ok(())
      }
    }
  }
}
