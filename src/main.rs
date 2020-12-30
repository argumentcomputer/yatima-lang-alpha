use im::Vector;
use std::collections::HashMap;
use yatima::term::{
  Term,
  Term::*,
};

fn main() {
  let inp = "(lambda x y)";
  println!(
    "{:?}",
    Term::decode(
      HashMap::new(),
      Vector::new(),
      hashexpr::parse(inp).unwrap().1
    )
  )
}
