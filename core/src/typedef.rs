use crate::{
  defs::Def,
  name::Name,
  package::Entry,
  term::*,
};
use sp_std::{
  boxed::Box,
  iter::once,
  rc::Rc,
  vec::Vec,
};

/// A type declaration syntax that allows for convenient expression of Yatima's
/// lambda encoded datatypes.
///
/// ```yatima
/// type Vector (A: Type): forall (k: Nat) -> Type {
///    Nil: Vector A 0,
///    Cons (k: Nat) (a: A) (as: Vector A k): Vector A (Nat.suc k),
/// }
/// ```
///
/// This `type` declaration adds the following definitions into a Yatima
/// environment:
///
/// ```yatima
/// // `Vector` type
/// def Vector (A: Type) (k: Nat): Type =
///  @Vector.self ∀
///  (0 P : ∀ (k: Nat) (self: Vector A k) -> Type)
///  (& Nil : P 0 (data λ Vector.Motive Vector.Nil Vector.Cons => Vector.Nil))
///  (& Cons: ∀ (0 k: Nat) (x: A) (xs: Vector A k)
///    -> P (succ k) (data λ P Vector.Nil Vector.Cons => Vector.Cons k x xs))
///  -> P k Vector.self
///
/// // `Vector` constructors
/// def Vector.Nil (0 A: Type): Vector A zero
/// = data λ P Vector.Nil Vector.Cons => Vector.Nil
///
/// def Vector.Cons (0 A: Type) (0 k: Nat) (x: A) (xs: Vector A k)
/// : Vector A (succ k)
/// = (data λ P Vector.Nil Vector.Cons => Vector.Cons k x xs))
/// ```
#[derive(Clone, Debug)]
pub struct TypeDef {
  /// Source position information for better erroring
  pub pos: Pos,
  /// The name of the type, e.g. `Vector`
  pub name: Name,
  /// Type parameters which are constant across all constructors, e.g `(A:
  /// Type)` Syntactically these appear on the left-hand side of the `:` type
  /// signature in the declaration header.
  pub typ_params: Vec<(Uses, Name, Term)>,
  /// Type indices which can change per constructor, e.g `(k: Nat)`.
  /// Syntactically these appear on the right-hand side of the `:` type
  /// signature in the declaration header.
  pub typ_indices: Vec<(Uses, Name, Term)>,
  /// Datatype constructors which are parsed with the correct syntactic context
  /// for elaboration of the self-type definition of the `type` declaration.
  /// This contains substantially the same information as `cons_variants`, but
  /// is parsed with a different context and stored separately, so that we
  /// don't have to perform delicate and error-prone De Bruijn index shifting
  /// on the variables in the constructors
  pub typ_variants: Vec<Variant>,
  /// Datatype constructors which are parsed with the correct syntactic context
  /// for elaboration of the constructor definitions
  pub cons_variants: Vec<Variant>,
}

impl PartialEq for TypeDef {
  fn eq(&self, other: &Self) -> bool {
    self.name == other.name
      && self.typ_params == other.typ_params
      && self.typ_indices == other.typ_indices
      && self.typ_variants == other.typ_variants
      && self.cons_variants == other.cons_variants
  }
}

/// Datatype variants such as `Vector.Nil` or `Vector.Cons` which are elaborated
/// into either self-type parameters or constructor definitions
#[derive(PartialEq, Clone, Debug)]
pub struct Variant {
  pub name: Name,
  pub bind: Vec<(Uses, Name, Term)>,
  pub params: Vec<Term>,
  pub indices: Vec<Term>,
}

impl TypeDef {
  /// Constructs the type of the `type` declaration
  /// e.g. `Vector : ∀ (0 A: Type) (0 k: Nat) -> Type`
  pub fn type_of(&self) -> Term {
    self.typ_params.iter().chain(self.typ_indices.iter()).rev().fold(
      Term::Typ(Pos::None),
      |acc, (u, n, t)| {
        Term::All(Pos::None, *u, n.clone(), Box::new((t.clone(), acc)))
      },
    )
  }

  /// Constructs the elimination motive, e.g. in `Vector`:
  /// ```yatima
  /// (0 P : ∀ (k: Nat) (self: Vector A k) -> Type)
  /// ```
  pub fn motive(&self) -> Term {
    // We proceed by constructing terms from the inside out. The motive is
    // always a telescope of `∀` (a.k.a. `forall`) binders whose final image is
    // `Type`.
    let img: Term = Term::Typ(Pos::None);
    // We then proceed with the rightmost/innermost binder, which is always
    // the dependent `self` binder. This is structurally a recursion on the self
    // type saturated with variables binding to all the types parameters and
    // indices.
    //
    // Count the number of type indices
    let i_len = self.typ_indices.len();
    // Construct an iterator of variables pointing the the type parameters
    // (e.g. `(A: Type)`) with the correct De Bruijn offset, which is the sum
    // of:
    // - The relative De Bruijn index `i` of the parameter. For example, if the
    //   type had parameters `(A B C: Type)` then the `i` values would be 2 for
    //   `A`, 1 for `B`, 0 for `C`
    //- a constant `1` for the self-type binder (e.g. `@Vector.self`)
    //- 2 times `i_len` (the number of type indices), because
    //   each type index appears as a binder in both the outer term and in the
    //   motive (e.g. `(k: Nat)` appears twice in the `Vector` self-type)
    let params = self
      .typ_params
      .iter()
      .rev()
      .enumerate()
      .map(|(i, (_, n, _))| {
        Term::Var(Pos::None, n.clone(), (i + 1 + 2 * i_len) as u64)
      })
      .rev();
    // Construct an iterator of variables  pointing to the type indices (e.g.
    // `(k: Nat)`). No De Bruijn offset is required here because the motive
    // is parameterized on the type indices.
    let indices = self
      .typ_indices
      .iter()
      .rev()
      .enumerate()
      .map(|(i, (_, n, _))| Term::Var(Pos::None, n.clone(), i as u64))
      .rev();
    // Fold the `params` and `indices` iterators over the recursion term to
    // create an application telescope.
    let slf_ty: Term =
      params.chain(indices).fold(Term::Rec(Pos::None), |acc, arg| {
        Term::App(Pos::None, Box::new((acc, arg)))
      });
    // Build the full binder entry with usage multiplicity and name
    let slf: (Uses, Name, Term) = (Uses::Many, Name::from("self"), slf_ty);
    // Build an iterator for the rest of the type index binders in the motive,
    // shifting the free variables in the types by adding 1 (for the
    // `@{name}.self` outer binder) plus the number of outer type index binders.
    let index_binders = self
      .typ_indices
      .clone()
      .into_iter()
      .map(|(u, n, t)| (u, n, t.shift((1 + i_len) as u64, Some(0))));
    // Finally, fold the index binders plus the self binder over the `img` into
    // a telescope of `forall` quantifiers
    index_binders.chain(once(slf)).rev().fold(img, |acc, (u, n, t)| {
      Term::All(Pos::None, u, n, Box::new((t, acc)))
    })
  }

  /// Create the lambda-encoded data terms of the self type, one for each
  /// constructor. These are used in the actual constructor definitions, but are
  /// also inlined into the type definition to avoid mutual recursion.
  /// ```yatima
  /// Vector.Nil  = data λ P Vector.Nil Vector.Cons => Vector.Nil
  /// Vector.Cons = data λ P Vector.Nil Vector.Cons => Vector.Cons k x xs
  /// ```
  pub fn data_terms(&self) -> Vec<Term> {
    // Store the result in this vector
    let mut res = vec![];

    // Since the binders in `data λ P ... =>` are the same in each constructor
    // we can create a list of it up-front to use later in the loop.
    let lam_bind: Vec<Name> = once(Name::from("P"))
      .chain(
        self
          .typ_variants
          .iter()
          .map(|v| Name::from(format!("{}.{}", self.name, v.name))),
      )
      .collect();
    // The number of variants
    let n_variants = self.typ_variants.len();
    // Iterate over the variants with a count
    for (i, v) in self.typ_variants.iter().enumerate() {
      // The name of the variant
      let name = Name::from(format!("{}.{}", self.name, v.name));
      // Construct an iterator of free variables that point to the binders in
      // `v.bind`. The two places we inline the data terms are in the type
      // and the constructor, and in both places the free variables will be
      // immediately captured:
      // In the type:
      // ```
      // (& Cons: ∀ (0 k: Nat) (x: A) (xs: Vector A k) -> P (succ k)
      //    (data λ P Vector.Nil Vector.Cons => Vector.Cons k x xs))
      // ```
      // In the constructor:
      // def Vector.Cons (0 A: Type) (0 k: Nat) (x: A) (xs: Vector A k)
      // : Vector A (succ k)
      // = (data λ P Vector.Nil Vector.Cons => Vector.Cons k x xs))
      let bind_vars = v
        .bind
        .iter()
        .rev()
        .enumerate()
        .map(|(j, (_, n, _))| {
          Term::Var(Pos::None, n.clone(), (j + lam_bind.len()) as u64)
        })
        .rev();
      // Apply the outer binder variables to a variable pointing to the
      // lambda that corresponds to the datatype variant. Since the variants are
      // always in a fixed order, this is variables index is the complement of
      // the iteration index `i` relative to number of variants
      let bod: Term = bind_vars.fold(
        Term::Var(Pos::None, name, (n_variants - 1 - i) as u64),
        |acc, arg| Term::App(Pos::None, Box::new((acc, arg))),
      );
      // Fold the lambda body over the lambda binders
      let trm: Term = lam_bind
        .iter()
        .rev()
        .fold(bod, |acc, n| Term::Lam(Pos::None, n.clone(), Box::new(acc)));
      // Add the outer `data` term constructor and push onto the results
      res.push(Term::Dat(Pos::None, Box::new(trm)));
    }
    res
  }

  /// Construct the term of the `type` definition. For example, for `Vector`:
  /// ```yatima
  /// @Vector.self ∀
  /// (0 P : ∀ (k: Nat) (self: Vector A k) -> Type)
  /// (& Nil : P 0 (data λ Vector.Motive Vector.Nil Vector.Cons => Vector.Nil))
  /// (& Cons: ∀ (0 k: Nat) (x: A) (xs: Vector A k)
  ///   -> P (succ k) (data λ P Vector.Nil Vector.Cons => Vector.Cons k x xs))
  /// -> P k Vector.self
  /// ```
  /// A self type definition is always an `@self` self-type term containing a
  /// sequence of `forall` binders. The first binder is always the motive `P`
  /// and the final image of the foralls is always `P` applied to all the type
  /// indices and the self-type variable.
  pub fn term_of(&self) -> Term {
    // Staring from the outside in, storing the forall binders in a vector
    let mut alls: Vec<(Uses, Name, Term)> = vec![];
    // The motive is constructed by another function
    alls.push((Uses::None, Name::from("P"), self.motive()));
    // As are the data terms
    let data_terms = self.data_terms();
    // Iterate over the variants to build the types of their binders,
    for (i, v) in self.typ_variants.iter().enumerate() {
      // The image of the binder type is the motive `P` applied to the variant
      // indices and the data term. E.g.
      // ```
      // P (succ k) (data λ P Vector.Nil Vector.Cons => Vector.Cons k x xs))
      // ```
      let img = v.indices.iter().chain(once(&data_terms[i])).fold(
        Term::Var(Pos::None, Name::from("P"), (i + v.bind.len()) as u64),
        |acc, arg| Term::App(Pos::None, Box::new((acc, arg.clone()))),
      );
      // Then we fold the variant binders over the image as `foralls`. E.g.
      // ```
      //∀ (0 k: Nat) (x: A) (xs: Vector A k) ->
      // ```
      let typ = v.bind.iter().rev().fold(img, |acc, (u, n, t)| {
        Term::All(Pos::None, *u, n.clone(), Box::new((t.clone(), acc)))
      });
      // And then we push on to the vector with the affine usage multiplicty and
      // the variant name
      alls.push((Uses::Affi, Name::from(format!("{}", v.name)), typ));
    }
    // The final image is the motive variable `P` applied to all the type
    // indices and the self-type variable. The De Bruijn index of `P` is
    // always the number of variants
    let len = self.typ_variants.len() as u64;
    let mot = Term::Var(Pos::None, Name::from("P"), len);
    // The De Bruijn index of the self-variable is always 1 more than that of
    // the motive, since the `@self` binder is immediately outside the binder of
    // the motive.
    let slf_name = Name::from(format!("{}.self", self.name));
    let slf = Term::Var(Pos::None, slf_name.clone(), len + 1);
    // Next are the variables of type indices, whose binders are immediately
    // outside the `@self` binder. So we can then compute the De Bruijn indices
    // of the type index variables (This is potentially a confusing overloading
    // of the word "index", but a De Bruijn index is number, and type index is a
    // type) by taking its relative index and adding 2 plus the number of
    // variants.
    let indices = self
      .typ_indices
      .iter()
      .rev()
      .enumerate()
      .map(|(i, (_, n, _))| Term::Var(Pos::None, n.clone(), i as u64 + len + 2))
      .rev();
    // Then we apply the type index variables and the self variable to the
    // motive variable to construct the image
    let img = indices
      .chain(once(slf))
      .fold(mot, |acc, arg| Term::App(Pos::None, Box::new((acc, arg.clone()))));
    // And fold the foralls over that image
    let forall = alls.iter().rev().fold(img, |acc, (u, n, t)| {
      Term::All(Pos::None, *u, n.clone(), Box::new((t.clone(), acc)))
    });
    // Then add the outer `@self` binder
    let bod = Term::Slf(Pos::None, slf_name, Box::new(forall));
    // and finally add lambda binders for each type parameter and type index
    self
      .typ_params
      .iter()
      .chain(self.typ_indices.iter())
      .rev()
      .fold(bod, |acc, (_, n, _)| {
        Term::Lam(Pos::None, n.clone(), Box::new(acc))
      })
  }

  /// Construct the definition of the `type` declaration's type out of the
  /// `term_of()` and `type_of()` functions and the `name` field.
  pub fn type_def(&self) -> (Name, Def, Entry) {
    let (d, e) = Def::make(Pos::None, self.type_of(), self.term_of());
    (self.name.clone(), d, e)
  }

  /// Construct a `Term::Ref` reference to the `type_def` with correct
  /// content-identifiers
  pub fn type_ref(&self) -> Term {
    let (_, _, ty_entry) = self.type_def();
    Term::Ref(Pos::None, self.name.clone(), ty_entry.cid(), ty_entry.term_anon)
  }

  /// Create the `Def` definitions for each constructor of the datatype
  pub fn constructors(&self) -> Vec<(Name, Def, Entry)> {
    // Currently we use two separate parsing passes to build separate Variant
    // structs for the datatype type definitions versus the constructors. This
    // is because the different contexts for both cases cause the De Bruijn
    // indices to differ significantly.
    let mut res = Vec::new();
    let data = self.data_terms();
    for (i, v) in self.cons_variants.iter().enumerate() {
      // The term is the correspoding `data_term` wrapped in lambdas
      // for the `typ_params` and variant binders. E.g. for `Vector.Cons`:
      // ```yatima
      // def Vector.Cons : ... =
      // λ A k x xs => (data λ P Vector.Nil Vector.Cons => Vector.Cons k x xs))
      // ```
      let trm = self
        .typ_params
        .iter()
        .chain(v.bind.iter())
        .rev()
        .fold(data[i].clone(), |acc, (_, n, _)| {
          Term::Lam(Pos::None, n.clone(), Box::new(acc))
        });
      // The type image is the `type_ref` applied to the type parameters and
      // variant indices. The type then wraps the image in binders for the type
      // parameters and variant binders, e.g. for `Vector.Cons`:
      // ```yatima
      // def Vector.Cons:
      //  ∀ (0 A: Type) (0 k: Nat) (x: A) (xs: Vector A k) -> Vector A (succ k)
      // = ...
      // ```
      let img = v
        .params
        .iter()
        .chain(v.indices.iter())
        .fold(self.type_ref(), |acc, arg| {
          Term::App(Pos::None, Box::new((acc, arg.clone())))
        });
      let typ = self
        .typ_params
        .clone()
        .into_iter()
        // The multiplicities of the typ_params are set to 0 in constructors
        .map(|(u, n, t)| (Uses::None, n, t))
        .chain(v.bind.clone().into_iter())
        .rev()
        .fold(img, |acc, (u, n, t)| {
          Term::All(Pos::None, u, n, Box::new((t, acc)))
        });
      // Replace `Term::Rec` with the reference to the type definition
      let typ = typ.un_rec(Rc::new(self.type_ref()));
      // Build the constructor definition
      let (d, e) = Def::make(Pos::None, typ, trm);
      res.push((Name::from(format!("{}.{}", self.name, v.name)), d, e));
    }
    res
  }
}
