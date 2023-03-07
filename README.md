# Yatima: A programming language for the decentralized web

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

NOTE: This repository is no longer maintained. Development has moved to https://github.com/yatima-inc/yatima, a dependently typed and content addressed compiler from the [Lean theorem prover](https://github.com/leanprover/lean4) to the [Lurk zkSNARK language](https://github.com/lurk-lang/lurk-rs). Further information can be found in the [Yatima Wiki](https://github.com/yatima-inc/yatima/wiki).

---

> In one sense, the Truth Mines were just another indexscape. Hundreds of
> thousands of specialized selections of the library's contents were accessible
> in similar ways--and Yatima had climbed the Evolutionary Tree, hopscotched the
> Periodic Table, walked the avenue-like Timelines for the histories of
> fleshers, gleisners, and citizens. Half a megatau before, ve'd swum through
> the Eukaryotic Cell; every protein, every nucleotide, even carbohydrate
> drifting through the cytoplasm had broadcast gestalt tags with references to
> everything the library had to say about the molecule in question.
>
> In the Truth Mines, though, the tags weren't just references; they included
> complete statements of the particular definitions, axioms, or theorems the
> objects represented. The Mines were self-contained: every mathematical result
> that fleshers and their descendants had ever proven was on display in its
> entirety. The library's exegesis was helpful-but the truths themselves were
> all here.
>
> *Diaspora*, Greg Egan

---

Yatima is a pure functional programming language implemented in Rust with the following features:

- **Content-Addressing** powers reproducible builds, and peer-to-peer
  package management. A Yatima content-address represents an
  _immutable_ program and all its dependencies. That means if someones shares an
  address with you, you can perfectly replicate their computation (and in
  principle even their computing environment!). Since the program is immutable,
  the way it runs the first time is the way it runs everytime.
- **First-class types**. This lets you the programmer to tell the compiler what
  you _intend_ to do in your program. Then, like a helpful robot assistant, the
  compiler will check to make sure that what you're _actually doing_ matches
  those expressed intentions. Type-driven programming lets the compiler act as
  your "correctness exocortex", i.e. a cognitive augmentation that helps you
  catch your mistakes.
- **Linear, affine and erased types** give you fine-grained control over
  resource usage during execution. Substructural types allow you to get the
  memory safety benefits of using a high-level language, while also allowing you
  to work "close to the metal" when you want to.
- **Type-safe dependent metaprogramming** lets Yatima have the flexibility and
  extensibility of a dynamically-typed language, without sacrificing the safety
  of static-typing.

## Examples

Algebraic datatypes (ADTs):

```
type Maybe (A: Type) {
  None,
  Some A,
}

type List (A: Type) {
  Nil,
  Cons A (List A),
}

def List.head (0 A: Type) (a: List A): Maybe A
  = (case a) (λ _ => Maybe A) (Maybe.None A) (λ x _ => Maybe.Some A x)
```

Generalized algrebraic datatypes:

```
type Expr: ∀ Type -> Type {
  N Nat: Expr Nat,
  B Bool: Expr Bool,
  Add (Expr Nat) (Expr Nat): Expr Nat,
  Mut (Expr Nat) (Expr Nat): Expr Nat,
  Eql (Expr Nat) (Expr Nat): Expr Bool,
}

def Expr.checks : Expr Bool = Expr.Eql (Expr.N 1) (Expr.N 2)
```

Dependent types and proofs:

```
type Vector (A: Type): ∀ (ω k: Natural) -> Type {
   Nil: Vector A Natural.Z,
   Cons (0 k: Natural) (x: A) (xs: Vector A k): Vector A (Natural.S k),
}

def Vector.head (0 A: Type) (k: Natural) (a : Vector A (Natural.S k)): A
  = ((case a) (λ k' self => ∀ (Equal Natural (Natural.S k) k') -> A)
    (λ e => Empty.absurd A (Natural.Z_isnt_S k e))
    (λ k x xs e => x))
    (Equal.Refl Natural (Natural.S k))
```

For more examples of Yatima code please refer to the `introit` standard library: https://github.com/yatima-inc/introit

## Implementation

- Yatima's core reduction machine is based on the λ-DAG technique described in [Bottom-up β-reduction](https://www.ccs.neu.edu/home/wand/papers/shivers-wand-10.pdf).
- Yatima's approach to inductive datatypes is based on [Self Types for Dependently Typed Lambda Encodings](https://homepage.divms.uiowa.edu/~astump/papers/fu-stump-rta-tlca-14.pdf).
- Yatima's quantitative types are based on [Syntax and Semantics of Quantitative Type Theory](https://bentnib.org/quantitative-type-theory.pdf).
- Many aspects of the language, particularly its libraries and type-equality algorithm, are adapted from the authors' previous work on [The Formality proof language](https://github.com/moonad/Formality-tmp).


Come chat with us on Matrix: [#yatima:matrix.org](https://matrix.to/#/!bBgWgXJqeKuDuiUYWN:matrix.org?via=matrix.org) or on the [Yatima subreddit](https://www.reddit.com/r/yatima/)

## Build Instructions:

Clone this repository and `cd` into it:

```bash
git clone git@github.com:yatima-inc/yatima.git
...
cd yatima
```

### Using binary cache (optional):

To speed up builds use our binary cache from [cachix](https://github.com/cachix/cachix). Install cachix and run:

```bash
cachix use yatima
```

### With Nix flakes (default):

Assuming you have activated flakes for your nix, otherwise [see here](https://nixos.wiki/wiki/Flakes).

```bash
# Activate shell environment
direnv allow
# Run standalone
nix run
# Build
nix build
# Start dev shell. Handled automatically by direnv
nix develop
# Install into your environment
nix profile install
```

### Compiling to WASM

```bash
nix-shell
cd web
```

Then run the following command to install required dependencies:

```bash
npm install
```

Afterwards, the experimental web version can be hosted with:

```bash
npm start
```

### With cargo

Yatima requires nightly Rust:

```bash
rustup default nightly
```

To build yatima:

```bash
cargo build
```

To run the test-suite:
```bash
cargo test --all
```

To install the yatima binary:

```bash
cargo install --path cli
```

# Usage Instructions:

Parse a `.ya` file (like from https://github.com/yatima-inc/introit) with:

```bash
λ yatima parse bool.ya 
Package parsed: bafy2bzacedl5jeqjqvvykquxjy53xey2l2hvcye2bi2omddjdwjbfqkpagksi
...
```

Typecheck with:

```bash
λ yatima check bool.ya
Checking package bool at bafy2bzacedl5jeqjqvvykquxjy53xey2l2hvcye2bi2omddjdwjbfqkpagksi
Checking definitions:
✓ Bool: Type
✓ Bool.True: Bool
✓ Bool.False: Bool
✓ Bool.eql: ∀ (x: Bool) (y: Bool) -> Bool
✓ Bool.lte: ∀ (x: Bool) (y: Bool) -> Bool
✓ Bool.lth: ∀ (x: Bool) (y: Bool) -> Bool
✓ Bool.gte: ∀ (x: Bool) (y: Bool) -> Bool
✓ Bool.gth: ∀ (x: Bool) (y: Bool) -> Bool
✓ Bool.and: ∀ (x: Bool) (y: Bool) -> Bool
✓ Bool.or: ∀ (x: Bool) (y: Bool) -> Bool
✓ Bool.xor: ∀ (x: Bool) (y: Bool) -> Bool
✓ Bool.not: ∀ (x: Bool) -> Bool
✓ Bool.neq: ∀ (x: Bool) (y: Bool) -> Bool
✓ Bool.if: ∀ (A: Type) (bool: Bool) (t: A) (f: A) -> A
```

Run the `main` expression in a Yatima package with

```bash
yatima run HelloWorld.ya
```

Enter the interactive Yatima REPL with
```bash
yatima repl
```

## Motivation

We're still in the early days of the Computing Revolution. The first
general-purpose digital computers were only switched on about 75 years ago.
The living memory of your parents and grandparents extends into the past
*before* computers. These machines are shockingly new, and as a species we
really have no idea what they're for yet. We're in the middle of an epochal
transformation whose nearest precedent is the invention of *writing*.
There are a lot of prognostications of what that means for our future; lots 
of different, and sometimes contradictory, visions of how computing is going 
to continue to shape our minds, our bodies, and our relationships with one 
another.

Yatima, as a project, has an opinionated view of that future. We think computing
should belong to individual users rather than corporations or states. A
programming language is an empowering medium of *individual* expression, 
where the user encounters, and extends their mind through, a computing machine.
We believe "Programmer" shouldn't be a job description, anymore than "scribe" 
is a job description in a world with near-universal literacy. Computing belongs 
to everyone, and computer programming should therefore be maximally accessible 
to everyone.

Currently, it's not: There are about 5 billion internet users worldwide, but
only an estimated 25 million software developers. That's a "Programming Literacy
rate" of less than 1%. Furthermore, that population is not demographically
representative. It skews heavily toward men, the Global North, and those from
privileged socioeconomic or ethnic backgrounds. This is a disgrace.
It is if we live in some absurd dystopia where only people with green eyes 
play music.

A new programming language isn't going to be some panacea that solves that
problem on its own, but there are some ways in a programming language can help:

1. Build a simple, but powerful programming language. Yatima's
   core logic is under 500 lines of code, but is incredibly expressive in its
   type system, runtime and syntax. We want to reduce the language's conceptual
   overhead, without hindering the language learner's future growth and power.

2. Make explicit in the language the connection between computing and
   mathematics. These two seemingly separate fields are actually, in essence,
   the same: All proofs are programs, all programs are proofs. A student
   doing math homework *is* programming, even if they don't conceptualize at
   such.

   Many people dislike math due to the tedium of manual computation and the
   unclear relevance of the results. And many people dislike programming because
   the concrete mechanics often seem arbitrary and frustrating. These are are
   complementary complaints. Math is more fun when you have a computer to take
   care of the detail-work. And computing is much easier when you have a clear
   notion of the theory of what you're doing.

3. Be portable in execution. Run locally, in the browser, on mobile, in a 
   distributed process. People shouldn't have to worry about the details of 
   *where* they want to do something, only *what* they want to do.

4. Be portable in semantics. Pure semantics and reproducible builds let people
   focus on the actual content of their programs rather than the scut-work of
   configuring infrastructure.

5. Integrate with decentralized technologies to remove, as much as possible,
   social barriers and frictions. Having centralized services like
   most modern package managers raises the question "Who controls the package server?" 
   The famous [leftpad incident](https://qz.com/646467/how-one-programmer-broke-the-internet-by-deleting-a-tiny-piece-of-code)
   is commonly presented as a build system issue (which it absolutely is), but
   less frequently discussed is that what precipitated the incident was how the
   `npm` administrators transfered ownership of a package from an individual
   developer without their consent to a large company.

6. Have a clear code of conduct to combat the endemic toxicity of contemporary
   programming culture. Some might find this controverisial, but it shouldn't be.
   Computing is a social and cultural project as much as it is a technical one.
   Cultures which perpetuate cycles of trauma are less successful in the long
   run than ones which do not.

The future we want to build is one where billions of people use, understand and
love their mathematical computing machines, as natural extensions of
themselves. A future where users have autonomy and privacy over their own
systems and their own data. A future where reliable, type-checked,
formally-verified software is the norm, so you can rely on software engineering
with the same quotidian confidence you have for civil engineering whenever you 
drive your car over a bridge.

## Thank you to our Supporters!

<img align="left" width="500" src="img/web3_foundation_grants_badge_white.jpg"/>


<img align="left" width="275" src="img/iota_black.png"/>
