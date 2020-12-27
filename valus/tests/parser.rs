#[cfg(test)]
mod test {
  use valus::parser::parse;

  #[test]
  pub fn parser() {
    fn parse_assert(input: &str) {
      match parse(&input) {
        Ok((_, dag)) => assert_eq!(format!("{}", dag), input),
        Err(_) => panic!("Did not parse."),
      }
    }
    parse_assert("λ x => x");
    parse_assert("λ x y => x y");
    parse_assert("λ y => (λ x => x) y");
    parse_assert("λ y => (λ z => z z) ((λ x => x) y)");
  }
}
