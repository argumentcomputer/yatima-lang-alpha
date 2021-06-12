// adapted from https://github.com/bodil/im-rs/blob/10.2.0/src/shared.rs

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

//! Automatic `Rc` wrapping.

use sp_std::rc::Rc;

/// # Automatic `Rc` wrapping
///
/// The `Shared` trait provides automatic wrapping for things which
/// take [`Rc`][sp_std::rc::Rc]s, meaning that anything which takes
/// an argument of type `Shared<A>` will accept either an `A` or an
/// `Rc<A>`.
///
/// Because everything stored in `im`'s persistent data structures is
/// wrapped in [`Rc`][sp_std::rc::Rc]s, `Shared` makes you have to
/// worry less about whether what you've got is an `A` or an `Rc<A>`
/// or a reference to such - the compiler will just figure it out for
/// you, which is as it should be.
///
/// [sp_std::rc::Rc]: https://docs.rs/sp-std/3.0.0/sp_std/rc/struct.Rc.html
pub trait Shared<A> {
  fn shared(self) -> Rc<A>;
}

impl<A> Shared<A> for A {
  fn shared(self) -> Rc<A> { Rc::new(self) }
}

impl<'a, A> Shared<A> for &'a A
where A: Clone
{
  fn shared(self) -> Rc<A> { Rc::new(self.clone()) }
}

impl<A> Shared<A> for Rc<A> {
  fn shared(self) -> Rc<A> { self }
}

impl<'a, A> Shared<A> for &'a Rc<A> {
  fn shared(self) -> Rc<A> { self.clone() }
}
