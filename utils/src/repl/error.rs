pub enum ReplError {
  Interrupted,
  Eof,
  Other(String),
}
