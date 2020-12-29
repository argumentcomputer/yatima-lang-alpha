// use atom::Atom;
use hashexpr::*;
// use span::Span;

fn main() {
  println!("{}", parse("(lambda x y z => 0d12:int32 + 0d12:int32)").unwrap().1);
}
