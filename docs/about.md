# About Slick

Slick is a statically-typed, interpreted programming language.

It melds the type safety of languages like Haskell, OCaml, and Rust with the
versatility of languages like Python and Swift.

Slick is designed to be simple, concise, and safe. Below are some features that
demonstrate these values.

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
slick> \t -> case t: | Seconds s -> s | Minutes m -> 60 * m | Hours h -> 3600 * h
<function> : (⟦Seconds : Int, Minutes : Int, Hours : Int⟧ -> Int)
```

This guarantees before your code is run that `Seconds`, `Minutes`, and `Hours`
are all treated differently, and the type checker rejects the input `t` if it
isn't one of these categories.

```
slick> (\t -> case t: | Seconds s -> s | Minutes m -> 60 * m | Hours h -> 3600 * h) (Lightyears 30)
! Type mismatch !
```
