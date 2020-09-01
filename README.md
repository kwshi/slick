# slick

## How to run in repl.it:

To run the Slick REPL, use the "Run" button, or type `./slick` in the terminal
panel.  To run a script, run `./slick <file>`.  To load a module from _within_
the REPL, use the command `:load "<file>"`.  We wrote a few demo scripts/modules
to show you what Slick code looks like.  Try running the following:

- From the Bash terminal: `./slick demos/hello.sl`

- From inside the REPL: `:load "demos/demo.sl"`, then try running `fibonacci`,
  `fibonacci 8`, `factorial`, `factorial 7`, etc.

## Slick in a nutshell

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
straightforward code (a la Python) while still introducing them to the concepts of
type-safety, _without requiring them to understand types in the first place_.

Here are some more expressions you can try running in the REPL!  See if you can
guess what they do, and guess what their types are before you run them...

- `\x -> x` (this is a function--what happens if you apply it?  try `(\x -> x) 3`)

- `\x -> (\y -> x)` (what about `(\x -> (\y -> x)) 3`? `((\x -> \y -> x) "hi") 5`?)

- `\{a, b} -> a + b` (what about `\x -> x.a + x.b`?)

- `{a=3, b="bar"}` (now try `{a=3, b="bar"}.b`)

- `True`, `False`, `Some 3`

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

---

## FAQs

### Help! I don't understand this error message!

Yeah... sorry about that.  We were so busy implementing other cool language
features we haven't gotten around to making the error messages very informative
yet.  For now, here's a rule-of-thumb to decipher what they mean, sort of:

- `Exception Not_found`: something went wrong during _runtime_, or _evaluation_.
  This kind of error should rarely occur, unless we did our type-checking wrong
  (which we may have--this project is very much a work-in-progress).

- `subsumes: unimplemented types`: a type error.  Most likely you are trying to
  apply a function to an argument of an incompatible type, e.g. `"hi" + 3` (`+`
  only acts on integers and cannot be applied to the string `"hi"`).

- `lookup_var: ...`: Most likely you are using a variable that hasn't been
  defined in a particular scope, e.g., in `\x -> y`, `y` has not been defined.

- `syntax error`: some sort of parsing/lexing-related error.  Try adding
  parentheses around certain expressions--in particular, around the
  right-hand-side of function expressions and `case` branches, e.g. (`\x -> ( ... )`,
  `| <pattern> -> ( ... )`).

We _will_, we promise, get to improving the error messages soon--and we assure
you, they will be some of the best you've ever seen.

### Why is Slick so slow?

Because we haven't gotten around to optimizing it yet, at all.  Currently, Slick
is an interpreted language, and its type-checker and evaluator/runtime are
largely a proof-of-concept, implemented in considerably inefficient ways.
Eventually, we will get around to rewriting those parts of Slick to be more
efficient, and we also plan to look into adding a _compilation_ target (most
likely via LLVM) to Slick.

### What do you mean by "static" safety?  Aren't the errors still occurring when I try to _run_ the code?



---

## Acknowledgements

- We take heavy inspiration from well-established functional programming
  languages like Haskell and OCaml.  We also take a ton of inspiration from
  Python for feature ideas and our syntax design.

- We stumbled on the idea of row polymorphism (and their benefits &
  implementation) from languages like Elm (polymorphic records) and OCaml
  (polymorphic variants).

- Much of the language's motivation and design philosophy was inspired by work
  during @kwshi's internship at Bloomberg.  (The code and implementation of
  Slick are completely original, independent of the code written at Bloomberg,
  but many of the ideas take after the Bloomberg "in spirit".)

- This cool bidirectional-type-inference paper:
  <https://www.cl.cam.ac.uk/~nk480/bidir.pdf>.