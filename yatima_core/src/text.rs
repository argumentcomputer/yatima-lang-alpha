use im::Vector;

pub type Text = Vector<char>;

pub fn from_string(x: String) -> Text {
  let mut res = Vector::new();
  for c in x.chars() {
    res.push_back(c);
  }
  res
}

pub fn to_string(x: Text) -> String {
  let mut res = String::new();
  for c in x.iter() {
    res.push(*c);
  }
  res
}
