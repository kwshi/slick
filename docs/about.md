# About Slick

Slick is a statically-typed, interpreted programming language. It is designed to be
simple, concise, and safe. Below are some features that demonstrate these
values.

## First class functions

Most things in Slick are values, including functions.

```
slick> (\f -> f(3)) (\x -> x + 2)
5 : Int
```

Most of the code in Slick that you will write boils down to transforming one
value into another.

## Powerful type inference

You don't need to declare types, Slick will infer them for you and ensure that
your programs are free of type mismatches.

```
slick> \f -> \x -> f(x+1)
<function> : ∀ α. ((Int -> α) -> (Int -> α))
```

For pratical purposes, you can ignore the type signatures if your program passes
type checking.  _Types in Slick are just a mechanism for catching mistakes_.

```
slick> (\f -> \x -> f(x+1)) 5
! Type mismatch !
```

If you enjoy type signatures, you can check out how inference handles an example
like the strict Y combinator.

```
slick> \f -> (\x -> f (\v -> (x x) v)) (\x -> f (\v -> (x x) v))
<function> : ∀ α25. ∀ α26. (((α25 -> α26) -> (α25 -> α26)) -> (α25 -> α26))
```

Neat.

## Flexible records

Records are like Objects in Javascript: they are maps from labels to values. In
Slick, record access is guaranteed safe by the type system.

```
slick> {x=1}.x
1 : Int
```

```
slick> {x=1}.y
! Type mismatch !
```

Records in Slick are flexible, meaning they can be extended with new values

```
slick> { {x=1} | y=3 }
{x = 2, y = 3} : {x : Int, y : Int}
```

And functions which expect records are happy with any record that has the fields
they need (provided those fields have the right type). So even though the record
below has an additional field `y`, the type checker is fine with it.

```
slick> (\r -> r.x) {x=1,y=3}
1 : Int
```

## Tags

For additional type safety, you can give any value a tag. This allows you to
safely separate data into semantic categories.

```
slick> \t -> case t | Seconds s -> s | Minutes m -> 60 * m | Hours h -> 3600 * h
<function> : (⟦Seconds : Int, Minutes : Int, Hours : Int⟧ -> Int)
```

This guarantees before your code is run that `Seconds`, `Minutes`, and `Hours`
are all treated differently, and the type checker rejects your code if it isn't
one of these categories.

```
slick> (\t -> case t | Seconds s -> s | Minutes m -> 60 * m | Hours h -> 3600 * h) (Lightyears 30)
! Type mismatch !
```

# Known issues / areas of improvement

Slick has been in development for only about 2 weeks. As such, there are many
areas in which it can, and will be improved. This is not an exhaustive list, but
should give you a sense of where our priorities lie.

Things may also get solved (or less bad) as time progresses but not get
reflected in this list.

## Error messages

During parsing we collect a lot of provenance (information about where tokens
come from), but this isn't used to deliver syntax error messages or propagated 
in the type checking to deliver locations of type errors.

Type errors could be significantly improved, and in some cases we might be able
to catch and resume after a type error in order to check the rest of an
expression. At present, the type system usually knows what's gone wrong, but
reports a cryptic message like "subsumes: invalid case", sometimes followed by a
debug dump of type checking information.

Expect this to be greatly improved, especially on the type error front (since
those are typically harder for users to deal with than syntax errors).

## Pattern matching

The only form of "pattern matching" that Slick presently supports is
destructuring tags in a `case` (but the pattern match must strictly conform to
the syntax of the tags). Our goal is to eventually add pattern matching
everywhere a variable can occur.

## Type naming

Right now type variables get whatever unique names they were given during type
inference. This leads to types like

```
slick> \x -> \y -> x
<function> : ∀ α10. ∀ α15. (α10 -> (α15 -> α10))
```

Ideally, we will change this to be something easier on the eyes, like

```
slick> \x -> \y -> x
<function> : ∀ a. ∀ b. a -> b -> a
```

(We're undecided on what symbols to use, but we'll probably stray from α.)

## Record updates

Presently, extending a record with the same label will result in a record
containing two of that field. This is a reflection of our implementation of
records and should not be allowed. 
```
slick> { { x = 1 } | x = 2 }
{x = 2, x = 1} : {x : Int, x : Int}
```

Similarly, performing a `case` on two of the same tag will result in a type with
multiple labels. This will also not be allowed.

```
slick> \x -> case x: | True a -> {} | False b -> {} | True c -> {}
<function> : ∀ α18. ∀ α17. ∀ α16. (⟦True : α18, False : α17, True : α16⟧ -> {})
```

## Variable self-assignment

We plan on allowing variables to be reassigned; however, right now the type
system will either throw a fit or act wonky around self-assignment.
Self-assignment will shadow the previous value. This can lead to confusing type
inference.
