use yatima_core::name::Name;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum PreUses {
  None,
  Affi,
  Once,
  Many,
  Hol(Name),
}
