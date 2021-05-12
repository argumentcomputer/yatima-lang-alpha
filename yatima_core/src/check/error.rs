use std::{
  error::Error,
  fmt,
};

use crate::{
  check::ctx::*,
  position::Pos,
};

#[derive(Debug)]
pub enum CheckError {
  // QuantityMismatch(Pos, Pos, ErrCtx),
  GenericError(Pos, ErrCtx, String),
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
      CheckError::GenericError(pos, ctx, msg) => {
        writeln!(f, "{} {} ", msg, pretty_pos(*pos))?;
        if !ctx.is_empty() {
          writeln!(f, "With context:\n{} ", pretty_context(ctx))?;
        }
        Ok(())
      }
    }
  }
}

impl Error for CheckError {
  fn description(&self) -> &str {
    match self {
      CheckError::GenericError(_, _, msg) => &msg,
    }
  }
}
