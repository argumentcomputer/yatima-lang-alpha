use libipld::Cid;
use yatima_core::{
  self,
  ipld_error::IpldError,
  parse::{
    self,
    span::Span,
  },
};

use std::{
  cmp::Ordering,
  fmt,
  fmt::Write,
  string::String,
};

use nom::{
  error::ErrorKind,
  AsBytes,
  Err,
  IResult,
  InputLength,
};
use std::path::PathBuf;

#[derive(PartialEq, Debug, Clone)]
pub enum FileErrorKind {
  CoreError(parse::error::ParseErrorKind),
  UnknownLink(Cid),
  MisnamedPackage(String),
  MalformedPath,
  ImportCollision(String, Cid, String),
  MisnamedImport(String, Cid, String),
  ImportCycle(PathBuf),
  IpldError(IpldError),
  EmbedError(Box<yatima_core::embed_error::EmbedError>),
  Nom(ErrorKind),
}

impl FileErrorKind {
  pub fn from_core_error(x: parse::error::ParseErrorKind) -> Self {
    use parse::error::ParseErrorKind::*;
    match x {
      Nom(e) => Self::Nom(e),
      x => Self::CoreError(x),
    }
  }

  pub fn is_nom_err(&self) -> bool { matches!(self, Self::Nom(_)) }
}

impl<'a> fmt::Display for FileErrorKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::CoreError(e) => {
        write!(f, "{}", e)
      }
      Self::UnknownLink(link) => {
        write!(f, "Link {} not found in local store", link)
      }
      Self::MisnamedPackage(name) => {
        write!(
          f,
          "Package {} must be in a file named {}.ya",
          name.clone(),
          name
        )
      }
      Self::MisnamedImport(name, link, pack_name) => {
        write!(
          f,
          "Tried to import a package {} from {}, but package is actually \
           named {}",
          name, link, pack_name
        )
      }

      Self::MalformedPath => {
        write!(f, "malformed path")
      }
      Self::EmbedError(e) => {
        write!(f, "Error reading from local store: {:?}", e)
      }
      Self::IpldError(e) => {
        write!(f, "Error reading from local store: {:?}", e)
      }
      Self::ImportCycle(path) => {
        write!(
          f,
          "An `import` declaration creates an import cycle with file {:?}, \
           which is not allowed",
          path
        )
      }
      Self::ImportCollision(imp_name, cid, def_name) => {
        writeln!(
          f,
          "Cannot import {def} from {imp} because a definition named \
           \"{def}\" is already imported. Try adding a unique alias like \
           `import {imp} as {imp}`",
          def = def_name,
          imp = imp_name,
        )
      }
      e => write!(f, "internal parser error: {:?}", e),
    }
  }
}

#[derive(PartialEq, Debug, Clone)]
pub struct FileError<I: AsBytes> {
  pub input: I,
  pub expected: Option<&'static str>,
  pub errors: Vec<FileErrorKind>,
}

impl<I: AsBytes> FileError<I> {
  pub fn new(input: I, error: FileErrorKind) -> Self {
    FileError { input, expected: None, errors: vec![error] }
  }

  pub fn from_core_error(x: parse::error::ParseError<I>) -> Self {
    FileError {
      input: x.input,
      expected: None,
      errors: x
        .errors
        .iter()
        .map(|x| FileErrorKind::from_core_error(x.clone()))
        .collect(),
    }
  }
}

impl<'a> fmt::Display for FileError<Span<'a>> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut res = String::new();

    writeln!(
      &mut res,
      "at line {}:{}",
      self.input.location_line(),
      self.input.get_column()
    )?;
    let line = String::from_utf8_lossy(self.input.get_line_beginning());

    writeln!(&mut res, "{} | {}", self.input.location_line(), line)?;

    let cols = format!("{} | ", self.input.location_line()).len()
      + self.input.get_column();
    for _ in 0..(cols - 1) {
      write!(&mut res, " ")?;
    }
    writeln!(&mut res, "^")?;

    if let Some(exp) = self.expected {
      writeln!(&mut res, "Expected {}", exp)?;
    }

    let mut errs = self.errors.iter().filter(|x| !x.is_nom_err()).peekable();
    if errs.peek() == None {
      // TODO: Nom verbose mode
      writeln!(&mut res, "Internal parser error")?;
    }
    else {
      writeln!(&mut res, "Reported errors:")?;
      for kind in errs {
        writeln!(&mut res, "- {}", kind)?;
      }
    }

    write!(f, "{}", res)
  }
}

impl<I: AsBytes> nom::error::ParseError<I> for FileError<I>
where
  I: InputLength,
  I: Clone,
{
  fn from_error_kind(input: I, kind: ErrorKind) -> Self {
    FileError::new(input, FileErrorKind::Nom(kind))
  }

  fn append(input: I, kind: ErrorKind, mut other: Self) -> Self {
    match input.input_len().cmp(&other.input.input_len()) {
      Ordering::Less => FileError::new(input, FileErrorKind::Nom(kind)),
      Ordering::Equal => {
        other.errors.push(FileErrorKind::Nom(kind));
        other
      }
      Ordering::Greater => other,
    }
  }

  fn or(self, mut other: Self) -> Self {
    match self.input.input_len().cmp(&other.input.input_len()) {
      Ordering::Less => self,
      Ordering::Equal => {
        for x in self.errors {
          other.errors.push(x);
        }
        other
      }
      Ordering::Greater => other,
    }
  }
}

impl<I: AsBytes> nom::error::ContextError<I> for FileError<I>
where
  I: InputLength,
  I: Clone,
{
  fn add_context(input: I, ctx: &'static str, other: Self) -> Self {
    match input.input_len().cmp(&other.input.input_len()) {
      Ordering::Less => {
        FileError { input, expected: Some(ctx), errors: vec![] }
      }
      Ordering::Equal => match other.expected {
        None => FileError { input, expected: Some(ctx), errors: other.errors },
        _ => other,
      },
      Ordering::Greater => other,
    }
  }
}

pub fn convert<I: AsBytes>(
  x: Err<parse::error::ParseError<I>>,
) -> Err<FileError<I>> {
  match x {
    Err::Incomplete(n) => Err::Incomplete(n),
    Err::Error(e) => Err::Error(FileError::from_core_error(e)),
    Err::Failure(e) => Err::Failure(FileError::from_core_error(e)),
  }
}

pub fn throw_err<I: AsBytes, A, F: Fn(FileError<I>) -> FileError<I>>(
  x: IResult<I, A, FileError<I>>,
  f: F,
) -> IResult<I, A, FileError<I>> {
  match x {
    Ok(res) => Ok(res),
    Err(Err::Incomplete(n)) => Err(Err::Incomplete(n)),
    Err(Err::Error(e)) => Err(Err::Error(f(e))),
    Err(Err::Failure(e)) => Err(Err::Failure(f(e))),
  }
}
