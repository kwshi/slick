# Learn Slick in 10 minutes

What follows is a quick tutorial on how to use Slick. Every time you see a code block like

```
slick> "hello!"
"hello!" : String
```

that's an invocation of the Slick REPL. The code after `slick>` is executed to
give an output on the line below. The output consists of a value (`"hello!"`)
and its corresponding type (`String`), denoted by the colon.

## Functions

Slick is a functional language, so your first introduction will be to
functions!

This is how you write anonymous functions in Slick.

```
slick> \x -> x
<function> : ∀ α10. (α10 -> α10)
```

Anonymous functions are now prevalent in a lot of languages. Here's how you'd
write the same function in Python

```python
lambda x: x
```

or Javascript

```javascript
(x) => x
```

It's called an "anonymous" function because it has no name. It just exists as a
value. This is a somewhat strange concept for many programming languages, but in
Slick it's completely normal. Functions are values just like `Int`s.

## Integers

Slick supports arbitrary precision integers, which are written like normal numbers.

```
slick> 12
12 : Int
```

You can do operations on `Int`s using the following functions:

```
int_add : {a : Int, b : Int} -> Int
int_sub : {a : Int, b : Int} -> Int
int_mul : {a : Int, b : Int} -> Int
int_div : {a : Int, b : Int} -> Int
int_neg : Int -> Int
```

The `{a : Int, b : Int}` syntax means that it takes in a record as input. Right
now the names of the inputs must be specified. This syntax will also be made
nicer in the future.

```
slick> int_add{a=5,b=6}
11 : Int
```

You can do comparisons on `Int`s using the following functions:

```
int_le : {a : Int, b : Int} -> ⟦True : {}, False : {}⟧
int_ge : {a : Int, b : Int} -> ⟦True : {}, False : {}⟧
int_lt : {a : Int, b : Int} -> ⟦True : {}, False : {}⟧
int_gt : {a : Int, b : Int} -> ⟦True : {}, False : {}⟧
int_eq : {a : Int, b : Int} -> ⟦True : {}, False : {}⟧
```

The `⟦True : {}, False : {}⟧` means that the function will return `True {}` or
`False {}`. We'll discuss tags later, but for all intents and purposes this
means it returns a boolean. This syntax (specifically the `True {}` syntax) will
be made much nicer in the future.

```
slick> int_eq{a=3,b=4}
False {} : ⟦True : {}, False : {}⟧
```

## Other base types

Slick supports only `Int` and `String` right now (and bindings to `String` are
less complete than those to `Int`). However, that doesn't mean Slick is wholly
unusable, as you can make ad-hoc user-defined types very easily.
