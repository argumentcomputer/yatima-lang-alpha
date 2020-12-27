#[cfg(test)]
mod test {
  use valus::dag::norm;
  use valus::parser::parse;

  #[test]
  pub fn reducer() {
    fn norm_assert(input: &str, result: &str) {
      match parse(&input) {
        Ok((_, dag)) => assert_eq!(format!("{}", norm(dag)), result),
        Err(_) => panic!("Did not parse."),
      }
    }
    // Already normalized
    norm_assert("λ x => x", "λ x => x");
    norm_assert("λ x y => x y", "λ x y => x y");
    // Not normalized cases
    norm_assert("λ y => (λ x => x) y", "λ y => y");
    norm_assert("λ y => (λ z => z z) ((λ x => x) y)", "λ y => y y");
    // // Church arithmetic
    let zero = "λ s z => z";
    let three = "λ s z => s (s (s z))";
    let four = "λ s z => s (s (s (s z)))";
    let seven = "λ s z => s (s (s (s (s (s (s z))))))";
    let add = "λ m n s z => m s (n s z)";
    let is_three = format!("(({}) ({}) {})", add, zero, three);
    let is_seven = format!("(({}) ({}) {})", add, four, three);
    norm_assert(&is_three, three);
    norm_assert(&is_seven, seven);
  }
}
