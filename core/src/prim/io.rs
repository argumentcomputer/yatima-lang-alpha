use sp_ipld::Ipld;

use crate::{ipld_error::IpldError, literal::Literal, term::Term, yatima};

use std::io::Read;
use std::fs::File;
use std::io::Write;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum IoOp {
  ReadAll,
  ReadAllStdin,
  ReadByte,
  ReadByteStdin,
  Write,
  WriteStdout,
  WriteStderr,
}

impl IoOp {
  pub fn symbol(self) -> String {
    match self {
      Self::ReadAll => "read_all".to_string(),
      Self::ReadAllStdin => "read_all_stdin".to_string(),
      Self::ReadByte => "read_byte".to_string(),
      Self::ReadByteStdin => "read_byte_stdin".to_string(),
      Self::Write => "write".to_string(),
      Self::WriteStdout => "write_stdout".to_string(),
      Self::WriteStderr => "write_stderr".to_string(),
    }
  }

  pub fn type_of(self) -> Term {
    match self {
      Self::ReadAll => yatima!("∀ #Text -> #Bytes"),
      Self::ReadAllStdin => yatima!("#Bytes"),
      Self::ReadByte => yatima!("∀ #Text -> #U8"),
      Self::ReadByteStdin => yatima!("#U8"),
      IoOp::Write => yatima!("∀ #Text #Bytes -> #Bool"),
      IoOp::WriteStdout => yatima!("∀ #Bytes -> #Bool"),
      IoOp::WriteStderr => yatima!("∀ #Bytes -> #Bool"),
    }
  }

  pub fn to_ipld(self) -> Ipld {
    match self {
      IoOp::ReadAll => Ipld::Integer(0),
      IoOp::ReadAllStdin => Ipld::Integer(1),
      IoOp::ReadByte => Ipld::Integer(2),
      IoOp::ReadByteStdin => Ipld::Integer(3),
      IoOp::Write => Ipld::Integer(4),
      IoOp::WriteStdout => Ipld::Integer(5),
      IoOp::WriteStderr => Ipld::Integer(6),
    }
  }

  pub fn from_ipld(ipld: &Ipld) -> Result<Self, IpldError> {
    match ipld {
      Ipld::Integer(0) => Ok(IoOp::ReadAll),
      Ipld::Integer(1) => Ok(IoOp::ReadAllStdin),
      Ipld::Integer(2) => Ok(IoOp::ReadByte),
      Ipld::Integer(3) => Ok(IoOp::ReadByteStdin),
      Ipld::Integer(4) => Ok(IoOp::Write),
      Ipld::Integer(5) => Ok(IoOp::WriteStdout),
      Ipld::Integer(6) => Ok(IoOp::WriteStderr),
      xs => Err(IpldError::IoOp(xs.to_owned())),
    }
  }

  pub fn arity(self) -> u64 {
    match self {
      IoOp::ReadAll
      | IoOp::ReadByte
      | IoOp::WriteStdout
      | IoOp::WriteStderr => 1,
      IoOp::ReadAllStdin | IoOp::ReadByteStdin => 0,
      IoOp::Write => 2,
    }
  }

  pub fn apply0(self) -> Option<Literal> {
    match self {
      IoOp::ReadAllStdin => {
        let mut stdin = std::io::stdin();
        let mut buf = Vec::new();
        stdin.read_to_end(&mut buf).unwrap();
        Some(Literal::Bytes(buf))
      }
      IoOp::ReadByteStdin => {
        let mut stdin = std::io::stdin();
        let mut buf = [0];
        stdin.read_exact(&mut buf).unwrap();
        Some(Literal::U8(buf[0]))
      }
      _ => None
    }
  }

  pub fn apply1(self, x: &Literal) -> Option<Literal> {
    match (self, x) {
      (IoOp::ReadAll, Literal::Text(filename)) => {
        let mut file = File::create(filename.to_string()).unwrap();
        let mut buf = Vec::new();
        file.read_to_end(&mut buf).unwrap();
        Some(Literal::Bytes(buf))
      }
      (IoOp::ReadByte, Literal::Text(filename)) => {
        let mut file = File::create(filename.to_string()).unwrap();
        let mut buf = [0];
        file.read_exact(&mut buf).unwrap();
        Some(Literal::U8(buf[0]))
      }
      (IoOp::WriteStdout, Literal::Bytes(contents)) => {
        let mut stdout = std::io::stdout();
        let result = stdout.write_all(contents);
        Some(Literal::Bool(result.is_ok()))
      }
      (IoOp::WriteStderr, Literal::Bytes(contents)) => {
        let mut stderr = std::io::stderr();
        let result = stderr.write_all(contents);
        Some(Literal::Bool(result.is_ok()))
      }
      _ => None
    }
  }

  pub fn apply2(self, x: &Literal, y: &Literal) -> Option<Literal> {
    match (self, x, y) {
      (IoOp::Write, Literal::Text(filename), Literal::Bytes(contents)) => {
        let mut file = File::create(filename.to_string()).unwrap();
        let result = file.write_all(contents);
        Some(Literal::Bool(result.is_ok()))
      }
      _ => None
    }
  }
}
