# slick

## How to run in repl.it:

To run the Slick REPL, use the "Run" button, or type `./slick` in the terminal
panel.  To run a script, run `./slick <file>`.  To load a module from _within_
the REPL, use the command `:load "<file>"`.  We wrote a few demo scripts/modules
to show you what Slick code looks like.  Try running the following:

- From the Bash terminal: `./slick demos/hello.sl`

- From inside the REPL: `:load "demos/demo.sl"`, then try running `fibonacci`,
  `fibonacci 8`, `factorial`, `factorial 7`, etc.

---

## In a nutshell

Slick is a functional programming language developed initially for repl.it's
language jam.

In most existing languages, _safety_ and _ease of use_ are at opposite ends of a
spectrum.  Languages like Python and JS are easy to hack with, concise, and
beginner-friendly, but they allow all sorts of runtime errors (in particular,
_type_ errors), and [behavior can be consequently
unpredictable](https://www.destroyallsoftware.com/talks/wat).  Languages like
C++, Java, and Rust provide additional _type safety_, preventing invalid
expressions like `"hi" + 1` at compile time, but they require complicated type
annotations, are far more verbose, and pose a higher barrier to programming
beginners.

Slick tries to combine the best of both worlds: a concise, _dynamic-feeling_,
versatile coding style combined with the safety of static types.  We achieve
this goal using two concepts from functional-programming research: _type
inference_ and _row polymorphism_.  (If those phrases don't mean anything to
you, don't worry!  You don't need to understand them to use its features.)
These tools allow Slick's syntax to be nearly as simple as vanilla
Python--requiring no type-annotations whatsoever--while still providing a
mechanism for type-checking and type-safety.

The gist of how it works is this: when you write down an expression like `a + b`,
we can tell without _evaluating_ the code that `a` and `b` must be numbers--
since `+` only works on numbers.  Slick takes this idea to the next level,
allowing type inference to be done on arbitrary structures (a.k.a. "records")
as well as variants (a.k.a. "tagged unions", "sum types", "parametrized enums").
_Here_ is where Slick shines relative to other language with type-inference,
like Haskell and OCaml: you can write an expression like `x.my_field`, and the
type-checker can infer, without knowing what _value_ `x` is, that it must be
a _record type_ containing a field named `my_field`.  Of course, it may also
contain other fields--so we say `x` is an _open_ record type containing a field
named `my_field`.  That type is denoted by
```
{ ρ | my_field : α }
```
(the `α` denotes that `my_field` has a type `α`).  If you then write something
like `x.my_field + y.other_field`, it can combine different pieces of information
to infer that `x` has type `{ ρ1 | my_field : Int }` and
`y` has type `{ ρ2 | other_field : Int }`.

Try it out!  Type the following commands into the Slick REPL (the syntax `\x -> ...`
defines an anonymous function, like `(x) => ...` in JS or `lambda x: ...` in Python):

```
slick> \x -> x.my_field
<function> : ({ρ22 | my_field : α23} -> α23)

slick> \{x, y} -> x.my_field + y.other_field
<function> : ({x : {ρ26 | my_field : Int}, y : {ρ28 | other_field : Int}} -> Int)
```

(The function _type signature_ `a -> b` indicates that `f` _takes in_ a value of
type `a` and _outputs_ a value of type `b`.)

This ability to infer and check types on arbitrary _structures_ is what allows
Slick to feel as versatile as Python, while still providing the static-typing
guarantees that languages like Haskell, OCaml, and Rust provide.  Moreover, it
allows Slick to not only be a beginner _friendly_ language but also an excellent
_education_ language: it allows programming newcomers to write simple,
straightforward code (a la Python) while introducing them to the concepts of
type-safety, _without requiring them to understand types in the first place_.

Here are some more expressions you can try running in the REPL!  See if you can
guess what they do, and guess what their types are before you run them...

- `\x -> x` (this is a function--what happens if you apply it?  try `(\x -> x) 3`)

- `\x -> (\y -> x)` (what about `(\x -> (\y -> x)) 3`? `((\x -> \y -> x) "hi") 5`?)

- `\{a, b} -> a + b` (what about `\x -> x.a + x.b`?)

- `print "hello world!"`

- `\x -> case x: | True -> False | False -> True`

- `\x -> case x % 2: | 0 -> Even | 1 -> Odd`

- `\x -> {x | new_key="hi"}` (the `{a | k=v}` syntax is called _record
  extension_.  not sure what that means?  try `(\x -> {x | new_key="hi"})
  {a=3}`.)

To learn more, check out our 10-minute tutorial [here](/docs/tutorial.md).

Read more about Slick [here](/docs/about.md).

For more details, read the Slick language reference
[here](/docs/language-features.md).

For type-savvy enthusiasts and theorists, read about Slick's type system
[here](/docs/typing.pdf). This document is mostly up-to-date, but there are some
recent changes like patterns which aren't in it.

---

## How to build and run locally

### Fetch repository

```
git clone https://github.com/kwshi/slick
```

### Install opam (skip if using other package manager for OCaml)

- macOS (Homebrew):

  - `brew install opam`
  - `opam init -c 4.10.0`, follow instructions to setup

- Linux:

  - use your OS's package manager to install `opam`:

    - Ubuntu: `apt-get install opam`

    - Nix: `nix-env -i opam`.  

    - Fedora/CentOS/RHEL: `yum install opam` (I just guessed this one, might be
      wrong)

  - `opam init -c 4.10.0`, follow instructions

- Windows:

  - I honestly don't know.  As far as I can tell, you're on your own.  Why
    not use WSL?

### Install OCaml packages

- opam:

  - `opam install dune`

  - `dune external-lib-deps src` to list all of Slick's library dependencies.
    Then run `opam install <pkgs...>`.  As of writing, that looks like

    ```
    opam install cmdliner containers fmt linenoise ppx_deriving zarith
    ```

  - `opam pin containers 2.7`, because we're old and obsolete and using an
    outdated version of containers.  Sorry--we'll update soon.

- Nix shell:

  - Simply run `nix-shell` from the project root after cloning.

### Compile and run!

- `dune build` to build everything (libraries and executables)

- `dune build src/exe` to build the Slick REPL executable

- `dune exec slick` to build and run the Slick REPL
