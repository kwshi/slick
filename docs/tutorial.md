# Learn Slick in 10 minutes

What follows is a quick tutorial on how to use Slick. Every time you see a code block like

```
slick> "hello!"
"hello!" : String
```

that's an invocation of the Slick REPL. The code after `slick>` is executed to
give an output on the line below. The output consists of a value (`"hello!"`)
and its corresponding type (`String`), denoted by the colon.

## Introduction to functions

Slick is a functional language, so the first part of your introduction will be
to functions!

This is how you write anonymous functions in Slick.

```
\x -> x
```

Anonymous functions are now prevalent in a lot of languages. Perhaps you'd
recognize this same function written in Python

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

Here's what happens when we run it in a REPL.

```
slick> \x -> x
<function> : ∀ α10. (α10 -> α10)
```

Let's address what the output means, because it is a little strange.

First, the value Slick reports for the function `\x -> x` is simply
`<function>`. That's because we can't print the description of all functions, so
we don't bother even for the functions we could print (like `\x -> x`).

Second, the type assigned to the function is `∀ α10. (α10 -> α10)`. If you
aren't familiar with other functional programming languages like Haskell or
OCaml, this will seem intimidating. Let's break it down, piece-by-piece.

`∀ α10`. This is read as "for all `α10`". The `α10` is known as a type variable.
Its number is nothing special, it's just a unique identifier so if we have
multiple type variables (like `α10` and `α11`) we can differentiate them. Type
variables are like regular variables in a program, except their values range
over _types_. So `α10` could take on the value `String` or `Int`.

Putting this together, what this means is that in the type `∀ α10. (α10 -> α10)`,
`α10` can be any type.

`(α10 -> α10)`. This is where things get especially wonky. That arrow indicates
a function. The left side of the arrow is its input and the right side is its
output. So `\x -> x` takes a value of type `α10` and gives back a value of the
same type.

That makes sense, since `\x -> x` simply returns its input unchanged!

## Applying our knowledge of functions

A function without any arguments is kind of like a pen without any ink: you'll
have to squint pretty hard to see what it does.

Function application in Slick is pretty simple. Let's observe what the function
from the previous section does to the value `3`.

```
slick> (\x -> x) 3
3 : Int
```

Application consists of just separating our function and its argument by
whitespace. Well, mostly.

In order to apply `\x -> x`, we need to put it in parentheses. This is just
because if we don't, the parser will think that our function takes in an `x` and
then applies that `x` to `3`! The parentheses are only necessary to disambiguate
for the parser.

Since `\x -> x` can take in any type, why don't we try applying it to itself?

```
slick> (\x -> x) (\x -> x)
<function> : ∀ α16. (α16 -> α16)
```

Well, the output has the same type, so it seems like it should be the same
function. Recall two things: the name of the variable doesn't matter and the
parentheses are for the parser.

## How about multiple arguments?

So far we've seen functions that take only one argument. In fact, we've only
seen one function, and it's kind of boring. How can we deal with more
interesting functions?

Let's take a look at

```
\x -> \y -> x
```

There are two ways to read this: the simpler way is as a function that takes two
arguments, `x` and `y`, and returns `x`, ignoring `y`.

The more complicated, but technically correct way is as a function which takes
`x` and _returns_ a function `\y -> x`, which ignores its `y` and gives back the
original `x`. If that sounds weird, just don't think about it.

Here's how we give multiple arguments

```
slick> ((\x -> \y -> x) 1) 2
1 : Int
```

This syntax looks a lot weirder, and it's because of the technically correct way
to interpret taking multiple arguments. Fortunately, there's a simpler way to
take multiple arguments in Slick!

## Introducing: records



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

The `{a : Int, b : Int}` syntax means that it takes in a record as input. More
on those later. What you need to know is that you specify the input as
`{a=1,b=2}`, i.e. the first argument is bound to `a` and the second to `b`. This
syntax will also be made nicer in the future.

```
slick> int_add {a=5,b=6}
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
slick> int_eq {a=3,b=4}
False {} : ⟦True : {}, False : {}⟧
```

## Other base types

Slick supports only `Int` and `String` right now (and bindings to `String` are
less complete than those to `Int`). However, that doesn't mean Slick is wholly
unusable, as you can make ad-hoc user-defined types very easily.
