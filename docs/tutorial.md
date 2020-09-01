# Learn Slick in 10 minutes*

\* Duration dependent upon frame of reference.

This is a quick tutorial on all the major language features of Slick. Every time
you see a code block like

```
slick> 314
314 : Int
```

that's an invocation of the Slick REPL. The code after `slick>` is executed to
give an output on the line below. The output consists of a value (`314`)
and its corresponding type (`Int`), separated by a colon.

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
<function> : α10 -> α10
```

Let's address what the output means, because it is a little strange.

First, the value Slick reports for the function `\x -> x` is simply
`<function>`. That's because we can't print the description of all functions, so
we don't bother even for the functions we could print (like `\x -> x`).

Second, the type assigned to the function is `α10 -> α10`. If you
aren't familiar with other functional programming languages like Haskell or
OCaml, this will seem intimidating. Let's break it down, piece-by-piece.

`α10`. The `α10` is known as a type variable. Its number is nothing special,
it's just a unique identifier so if we have multiple type variables (like `α10`
and `α11`) we can differentiate them. Type variables are like regular variables
in a program, except their values range over _types_. So `α10` could take on the
value `String` or `Int`.

This means is that in the type `α10 -> α10`, `α10` can be any type.

`(α10 -> α10)`. This is where things get especially wonky. That arrow indicates
a function. The left side of the arrow is its input and the right side is its
output. So `\x -> x` takes a value of type `α10` and gives back a value of the
same type.

That makes sense, since `\x -> x` simply returns its input unchanged!

## Naming functions

You can define named functions using `def`. This defines a function bound to `f`
that functions just like `\x -> x`.

```
slick> def f x: x
<function> : α14 -> α14
```

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
<function> : α16 -> α16
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
slick> def f x y: x
<function> : α14 -> (α21 -> α14)
```

This defines a function that takes two arguments, `x`, and `y`. It returns only
`x`. 

Here's how we give multiple arguments

```
slick> f 1 2
1 : Int
```

This might seem a little weird to you, especially the syntax for defnining the
function. Fortunately, there's another way to take multiple arguments in Slick!

## Introducing: records

Records are kind of like Objects in Javascript, and they have a somewhat similar
syntax. The important difference is that in Slick, accessing a record is always
guaranteed to work! 

Let's dive right in with an example:

```
slick> { x = 12, y = "hello" }
{x = 12, y = "hello"} : {x : Int, y : String}
```

This is a record with two fields: `x` and `y`, both of type `Int`. We can access
the fields using `.`, like in `record.field_name`.

```
slick> { x = 12, y = "hello" }.x
12 : Int
```

```
slick> { x = 12, y = "hello" }.y
"hello" : String
```

Can we use records as inputs to our functions? We totally can. And we don't even
need to specify that we expect an input that is of the record type, we can just
use it like a record and Slick will infer its type for us.

```
slick> (\record -> record.x) {x = 12, y = "hello"}
12 : Int
```

So we can use records to obtain multiple arguments to our functions.

```
slick> (\args -> args.x + args.y) {x = 5, y = 12}
17 : Int
```

One important point to note is that Slick 

## Extending our knowledge of records

Records can be extended, which means adding other fields to them.

```
slick> {{x = 3} | y = 4, z = 5}
{z = 5, y = 4, x = 3} : {z : Int, y : Int, x : Int}
```

This extension will also (in the future) update any fields if the original
record has them. So if I extend `{x = 3}` with the field `x = \y -> y`, the type
and value of the record will be updated accordingly. Updating is currently
bugged, so we don't include an example of it in the REPL.

## Integers

Slick supports arbitrary precision integers, which are written like normal numbers.

```
slick> 12
12 : Int
```

You can do operations on `Int`s using the expected functions


## Other base types

Slick supports only `Int` and `String` right now. However, that doesn't mean Slick is wholly
unusable, as you can make ad-hoc types very easily.

## Tags

Slick lets you create Tags by writing an identifier that starts with a capital letter. Like this

```
slick> True
True : ⟦ρ13 | True⟧
```

You can think of Tags as ways of separating data into disjoint catagories. The
`ρ13` is another kind of type variable that stands for a set of other Tags. You
can read the type signature as saying "the Tag `True` is part of a set of tags
that at least contains `True`." This is a little unhelpful.

You might think, knowing that `True` often inhabits a type known as "Bool" or
"Boolean" in other languages, "why isn't `True` part of a set of tags containing
`True` and `False`?" This is a very reasonable question, but the answer is an
interesting feature of Slick: there is no pre-defined Boolean type! We'll see
why in a little bit.

You can use Tags by putting them into a `case` statement.

```
slick> case True: | True -> "it's true"
"it's true" : String
```

This isn't very helpful. But we can make our `case` more general by adding more
branches. This is easier to read on multiple lines, but we'll include a version
fit for the REPL below that.

```
case True
  | True  -> "it's true"
  | False -> "it's false"
```

```
slick> case True: | True  -> "it's true" | False -> "it's false"
“it's true” : String
slick> case False: | True  -> "it's true" | False -> "it's false"
“it's false” : String
```

Did we mention it's type safe?

```
slick> case 3: | True  -> "it's true" | False -> "it's false"
! Type error !
slick> case AnotherTag | True  -> "it's true" | False -> "it's false"
! Type error !
```

Going back to what we said earlier, the choice of `True` and `False` is only by convention!

```
slick> 1 == 3
False : ⟦True, False⟧
```

This could be `Banana : ⟦Apple, Banana⟧` -- what matters is that when we use
functions like `==`, we respect their types.

Tags can also tag data, rather than be used alone, like in

```
slick> MyNumber 3
MyNumber 3 : ⟦ρ13 | MyNumber : Int⟧
```

So we can for example define a safe division function:
```
\d -> \q -> case: (q == 0)
  | True  -> DivError
  | False -> DivResult (d / q)
```

```
slick> \d -> \q -> case: (q == 0) | True  -> DivError | False -> DivResult (d / q)
<function> : (Int -> (Int -> ⟦ρ49 | DivResult : Int, DivError⟧))
```

It "rejects" invalid inputs by returning the tag `DivError` and returns the
result wrapped in the `DivResult` tag.

## Fin

You now know enough to be dangerous with Slick! There are a few more features
not covered here. Check out the [language documentation here](/docs/language-features.md)
