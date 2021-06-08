# Yatima: A programming language for the decentralized web

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

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

On an implementation level:

- Yatima's core reduction machine is based on the λ-DAG technique described in [Bottom-up β-reduction](https://www.ccs.neu.edu/home/wand/papers/shivers-wand-10.pdf).
- Yatima's approach to inductive datatypes is based on [Self Types for Dependently Typed Lambda Encodings](https://homepage.divms.uiowa.edu/~astump/papers/fu-stump-rta-tlca-14.pdf).
- Yatima's quantitative types are based on [Syntax and Semantics of Quantitative Type Theory](https://bentnib.org/quantitative-type-theory.pdf).
- Many aspects of the language, particularly its libraries and type-equality algorithm, are adapted from the authors' previous work on [The Formality proof language](https://github.com/moonad/Formality-tmp).
- The `hashexpr` content-addressing schema is an adaptation of IPFS' [Multiformats](https://multiformats.io/) and [IPLD](https://docs.ipld.io/) to align with [Rivest's Canonical S-exprssions](https://people.csail.mit.edu/rivest/Sexp.txt) rather than JSON.


Come chat with us on Matrix: [#yatima:matrix.org](https://matrix.to/#/!bBgWgXJqeKuDuiUYWN:matrix.org?via=matrix.org)


## Build Instructions:

Clone this repository and `cd` into it:

```bash
git clone git@github.com:yatima-inc/yatima.git
...
cd yatima
```

### With Nix:

Set up dev environment assuming [nix](https://nixos.org), [lorri](https://github.com/target/lorri) and direnv are installed.
```bash
direnv allow
```
This should load correct versions of dependencies into your shell. To build yatima using naersk:

```bash
nix-build yatima.nix
```

To install the yatima binary into your environment:

```bash
nix-env -i -f yatima.nix
```

To run the test-suite and CI:

```bash
nix-build default.nix
```
[Niv](https://github.com/nmattia/niv) fixes upgrading dependencies for development.

### Compiling to WASM

```bash
wasm-pack build --target web
```

Host the experimental web version with:

```bash
yatima hashspace server
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
cargo install --path .
```

# Usage Instructions:

Parse a `.ya` file (like from https://github.com/yatima-inc/introit) with:

```bash
yatima parse Bool.ya
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
to everyone, and computer programming should therefore be maximally accesible 
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
