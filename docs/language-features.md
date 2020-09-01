# Language Features

This document covers all of the features of Slick.

When you see

```
slick> 1 + 2
3 : Int
```

that is an invocation of the Slick REPL.

The `1 + 2` is the input, and `3 : Int` is the output.

`3` is the output value and `Int` is the output type.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Language Features](#language-features)
    - [Integers](#integers)
        - [Literals](#literals)
        - [Functions on integers](#functions-on-integers)
    - [Strings](#strings)
        - [Literals](#literals-1)
        - [Functions on strings](#functions-on-strings)
    - [Booleans](#booleans)
        - [Literals](#literals-2)
        - [Functions on booleans](#functions-on-booleans)
        - [A note on booleans](#a-note-on-booleans)
    - [Functions](#functions)
        - [Anonymous functions](#anonymous-functions)
        - [Function application](#function-application)
    - [Records](#records)
        - [Defining records](#defining-records)
        - [Getting elements](#getting-elements)
        - [Extending records](#extending-records)
        - [Record polymorphism](#record-polymorphism)
    - [Tags](#tags)
        - [Defining tags](#defining-tags)
        - [Using tags (case statements)](#using-tags-case-statements)
    - [Patterns](#patterns)
        - [Literal patterns](#literal-patterns)
        - [Variable patterns](#variable-patterns)
        - [Wildcard pattern](#wildcard-pattern)
        - [Tag patterns](#tag-patterns)
        - [Record patterns](#record-patterns)
        - [Nested pattern matches](#nested-pattern-matches)
    - [Assignments](#assignments)
    - [Definitions](#definitions)
    - [Types](#types)
        - [Base types](#base-types)
        - [Functions](#functions-1)
        - [Record types](#record-types)
        - [Tag types (variant types)](#tag-types-variant-types)
        - [Quantified types](#quantified-types)
        - [Recursive types](#recursive-types)
        - [More details](#more-details)

<!-- markdown-toc end -->

## Integers

### Literals

Write integer literals (`0`, `314`) to create integers.

```
slick> 314
314 : Int
```

### Functions on integers

Slick supports most standard options on integers. 

|Operator  |  Function  |
|          |            |
|----------|------------|
| `+`      | Addition   |
| `-`      | Subtraction |
| `*`      | Multiplication |
| `/`      | Integer division |
| `%`      | Modulus    |
| `**`       | Exponentiation|
| `<`        | Less than |
| `<=`       | Less than or equal |
| `>`        | Greater than  |
| `>=`       | Greater than or equal |
| `==`       | Equal |
| `!=`       | Not equal |
| `-`        | Negation            |

`-` can be used as a unary operator (takes one argument), in which case it is
negation or as a binary operator (takes two arguments), in which case it is
subtraction.

## Strings

### Literals

String literals are enclosed in quotes.

```
slick> "hello, slick!"
“hello, slick!” : String
```

### Functions on strings

| Function | Descripion |
|----------|------------|
| `print`    | Prints the string to the terminal. Returns `{}` (an empty record). |
| `++` | String concatenation |

## Booleans

### Literals

`True` and `False` are the boolean literals. There are aliases `true` and
`false` which map to the literals.

### Functions on booleans

| Function | Descripion |
|----------|------------|
| `&&`     | Logical "and"|
| `||`     | Logical "or"|

### A note on booleans

Booleans are not a primitive data type. By convention, Slick uses them, for
example, as the return type of functions like `==`. But they are just regular
[tags](#tags).

## Functions

### Anonymous functions

Function values (i.e. [anonymous
functions](https://en.wikipedia.org/wiki/Anonymous_function)) take the form
`\arg -> body` where `arg` is the argument (which can be a pattern) and `body`
is an expression.

```
slick> \x -> x + 1
<function> : (Int -> Int)
```

Take multiple arguments by defining functions that return functions (known as
[currying](https://en.wikipedia.org/wiki/Currying)). This is just a
technicality, so if that sounds confusing you can just read the below as an
anonymous function which takes two arguments.

```
slick> \x -> \y -> x
<function> : (α13 -> (α20 -> α13))
```

### Function application

Function application takes the form `f e` where both are expressions. Of course,
for it to type check, `f` must resolve to being a function and `e` must be of
its argument type.

In order for the parser to parse the correct application, it is usually
necessary to wrap your anonymous functions in parentheses.

Correct:

```
slick> (\x -> x) 3
3 : Int
```

Incorrect (unless you wanted to define a function):

```
slick> \x -> x 3
<function> : ((Int -> α17) -> α17)
```

## Records

Records are types which themselves contain multiple types, each indexed by a
unique label. You can think of them as maps whose keys are known before the
program is run. Slick's type system ensures that any time a record is accessed,
its value is guaranteed to exist.

### Defining records

You can create record values by defining them in the form `{field1=e1,
field2=e2,field3=e3}` with however many fields you want (including none).

```
slick> {x=3,y="hi"}
{x = 3, y = “hi”} : {x : Int, y : String}
```

```
slick> {}
{} : {}
```

### Getting elements

You can access record fields by using syntax of the form `record.field`.

```
slick> {x=3, y="hi"}.x
3 : Int
slick> {x=3, y="hi"}.y
“hi” : String
```

Multiple accesses work with nested records.

```
slick> {a={b={c=10}}}.a.b.c
10 : Int
```

### Extending records

You can extend a record by using the syntax `{record | field1=e1, field2=e2,
field3=e3}` with however fields you want (as long as there is at least one).

```
slick> { {x=1} | y = "hi" }
{y = “hi”, x = 1} : {y : String, x : Int}
```

Extending a record with a field already in that record overwrites that field,
potentially to a value of a different type. Right now, extension is bugged and
introduces two separate entries, but this will be fixed soon.

### Record polymorphism

Slick's type system supports record polymorphism. This is a fancy way of saying
that functions expecting a record won't care if the record has fields they don't
use.

```
slick> (\r -> r.x) {x = 1}
1 : Int
slick> (\r -> r.x) {x = 1, y = "hi"}
1 : Int
```

In many languages with records, the second function application wouldn't be
allowed.

## Tags

Tags (also known as variants) are a way of separating data into disjoint
categories. Because Slick is typed, if a function expect an `Int` and you give
it a `String`, you'll get a type error. With tags, you can have a function that
handles `Int`s and `String`s separately, but allows taking both as input.

### Defining tags

A tag is just an identifier that starts with an uppercase letter, followed by 1
or fewer values to tag.

```
slick> MyTag
MyTag : ⟦ρ19 | MyTag⟧
```

Here, the `ρ19` is a variable indicating that this tag can be a part of a larger
set of tags. Practically speaking, the type that `MyTag` has ensures that it can
always be given as input to a function that can deal with it.

```
slick> MyTaggedInt 3
MyTaggedInt 3 : ⟦ρ19 | MyTaggedInt : Int⟧
```

Note that the type that `MyTaggedInt` wraps is shown in its type signature.

### Using tags (case statements)

On their own, you can't do much with Tags. The `case` statement allows you to
separate your code into different branches based upon the given Tag. This is all
verified to be safe by the type checker.

```
slick> case MyTag: | MyTag -> "this is my tag" | AnotherTag -> "this is not my tag"
“this is my tag” : String
slick> case AnotherTag: | MyTag -> "this is my tag" | AnotherTag -> "this is not my tag"
“this is not my tag” : String
slick> case AThirdTag: | MyTag -> "this is my tag" | AnotherTag -> "this is not my tag"
!! Type error !! 
```

This lets you write functions that deal with different data separately, even if
it has the same type:

```
slick> \t -> case t: | Seconds s -> s | Minutes m -> 60 * m | Hours h -> 3600 * h
<function> : (⟦Hours : Int, Minutes : Int, Seconds : Int⟧ -> Int)
slick> (\t -> case t: | Seconds s -> s | Minutes m -> 60 * m | Hours h -> 3600 * h) Hours 2
7200 : Int
```

The type of this function (`⟦Hours : Int, Minutes : Int, Seconds : Int⟧ -> Int`)
indicates it expects a tag to be either `Hours`, `Minutes`, or `Seconds`, and
furthermore all of these tags must wrap an `Int`. It is a type error to give
anything else^* .

`case` statements can match on more than just Tags, [see the section on
patterns](#patterns). They evaluate the branches in order, looking for a match.

```
slick> case 5: | 3 -> "three" | 5 -> "five" | _ -> "i don't know"
“five” : String
```

\* Currently the system is bugged and will accept other Tags and error, but this
is being fixed. It's a regression caused by our switch to using patterns in
`case` statements. The same bug manifests itself with incomplete patterns
provided for `String`s and `Int`s.

## Patterns

Functions and `case` statements take as their parameters a pattern. In its
simplest form, a pattern is simply a variable that inputs match against.
However, patterns can be more complex than this.

### Literal patterns

A pattern consisting of a literal (right now, this means a `String` or an `Int`)
will match just that literal.

```
\20 -> "this is a dumb function"
<function> : (Int -> String)
```

The above function will error at runtime if given a value other than 20, due to
the way that pattern matching is implemented for functions. We are considering
removing these types of pattern matches from functions because they are unsafe.

### Variable patterns

The pattern `variable_name` will match any value given to it, binding it to the
name `variable_name`. In function arguments, this is the most common pattern,
and in `case` statements it has utility as a fall-through case (although often
times you will want the fall through to be `_` because you do not use its
value).

```
slick> (\str -> case str: | "hi" -> "hola" | other_string -> other_string) "hi"
“hola” : String
slick> (\str -> case str: | "hi" -> "hola" | other_string -> other_string) "blah"
“blah” : String
```

### Wildcard pattern

The pattern `_` is like a variable pattern in that it matches anything, but it
doesn't bind those things to a variable. You cannot define or use variables
named `_`.

### Tag patterns

The pattern `TagToMatch pat` is a pattern that matches a Tag called `TagToMatch
e` whose expresion `e` matches `pat`. Sometimes it is necessary to wrap this
pattern in parentheses in order to avoid ambiguities when parsing (such as when
it is the argument to a function).

A tag pattern can also be empty, like in `EmptyTagPattern`, in which case it
will match the Tag `EmptyTagPattern`.

### Record patterns

The pattern `{field1=pattern1,field2=pattern2,field3=pattern3}` is a pattern
that matches a record with fields `field1`, `field2`, and `field3` (and no other
fields) whose respective expressions match `pattern1`, `pattern2`, and
`pattern3`. Note that you can match on the pattern `{}` which expects the empty
record.

```
slick> \{x=arg1,y=arg2} -> arg1 + arg2
<function> : ({x : Int, y : Int} -> Int)
slick> (\{x=arg1,y=arg2} -> arg1 + arg2) {x=1,y=2}
3 : Int
slick> \{} -> 3
<function> : ({} -> Int)
slick> (\{} -> 3) {}
3 : Int
```

Often times, you'd like to reuse the names of the fields as variables in a
record. The pattern `{field1,field2}` does so.

```
slick> \{x,y} -> x + y
<function> : ({x : Int, y : Int} -> Int)
slick> (\{x,y} -> x + y) {x=1,y=2}
3 : Int
```

You can mix and match these patterns, too.

```
slick> \{x=arg1,y} -> arg1 + y
<function> : ({x : Int, y : Int} -> Int)
slick> (\{x=arg1,y} -> arg1 + y) {x=1,y=2}
3 : Int
```

### Nested pattern matches

Most pattern matches can be nested. Here's an example that combines a bunch of
the types of pattern matches:

```
def greet_person person: 
  case person:
  | (Adult {age,job,name="Kye"}) -> "burger time"
  | (Adult {age,job=(Programmer _),name}) -> "hello, world"
  | (Adult _) -> "hello, adult"
  | (Child _) -> "hello, child"
```

(REPL-friendly definition)
```
def greet_person person: case person | (Adult {age,job,name="Kye"}) -> "burger time" | (Adult {age,job=(Programmer _),name}) -> "hello, world" | (Adult _) -> "hello, adult" | (Child _) -> "hello, child"
```

And example invocations.

```
slick> greet_person (Adult {age=20,job=Worker,name="Kye"})
“burger time” : String
slick> greet_person (Adult {age=21,job=Programmer,name="cole"})
“hello, world” : String
slick> greet_person (Adult {age=30,job=Worker,name="Bob"})
“hello, adult” : String
slick> greet_person (Child {age=5,name="Joe"})
“hello, child” : String
```

## Assignments

You can assign variables to expressions using the syntax `var := expr1; expr2`.

`expr1` will be evaluated, bound to `var`, and then the value of `expr2` will be
returned.

```
slick> x := 3; x * 6
18 : Int
```

## Definitions

Definitions are like assignments but top-level. They use the syntax `def name
arg1 arg2: expr`, where there are 0 or more arguments `arg1`,`arg2`,...

0 arguments are allowed.

```
slick> def three: 3
3 : Int
slick> three == 3
True : ⟦True, False⟧
```

If a definition has arguments, it will define a function taking each of the
arguments in order.

```
slick> def plus_one x: x + 1
<function> : (Int -> Int)
slick> plus_one three
4 : Int
```

A definition can still be a function if it doesn't explicitly take arguments, of
course. The syntax is for convenience.

```
slick> def plus_two: \x -> x + 2
<function> : (Int -> Int)
slick> plus_two 3
5 : Int
```

Definitions at present cannot be recursive, but this is a known limitation and
is being worked on. You can use the helper function `fix` to convert your
recursive definitions to valid definitions. Suppose you define

```
slick> def f x: case x: | 0 -> 1 | n -> n * f (x - 1)
<will error if invoked>
```

First, make `f` into a definition that uses no explicit arguments.

```
def f: \x -> case x: | 0 -> 1 | n -> n * f (x - 1)
```

Then, wrap it in a function that takes its name (`f`) as the argument.

```
def f: \f -> \x -> case x: | 0 -> 1 | n -> n * f (x - 1)
```

Call `fix` on that in the definition, and it will be recursive.

```
def f: fix (\f -> \x -> case x: | 0 -> 1 | n -> n * f (x - 1))
```

```
slick> def f: fix (\f -> \x -> case x: | 0 -> 1 | n -> n * f (x - 1))
<function> : (Int -> Int)
slick> f 10
3628800 : Int
```

`fix` is the [strict Y
combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Strict_fixed-point_combinator),
for those curious.

## Types

### Base types

`Int` and `String` are the two base types.

### Functions

`t1 -> t2` is the type of functions from `t1` to `t2`.

### Record types

`{ field1: t1, field2: t2, field3: t3 }` is the type of a closed record with
three fields: `field1` with type `t1`, `field2` with type `t2`, and `field3`
with type `t3`. A record may have 0 or more fields.

A record may also be polymorphic over a variable `ρ` which stands for any fields
which might not be present in the record at whatever types they need to be.

`{ρ | field1: t1, field2: t2, field3: t3 }`

### Tag types (variant types)

`⟦ field1: t1, field2: t2, field3: t3 ⟧` is the type of a closed Tag with three
fields: `field1` with type `t1`, `field2` with type `t2`, and `field3` with type
`t3`. A Tag may have 0 or more fields (although the 0-field Tag, which is
uninhabitable, is not presently definable in Slick).

A Tag may also be polymorphic over a variable `ρ` which stands for any fields
which might not be present in the Tag at whatever types they need to be.

`⟦ ρ | field1: t1, field2: t2, field3: t3 ⟧`

### Quantified types

`forall α. t` is a type where every occurrence of `α` may be substituted for any
other type. In Slick at the moment, leading `forall`s are stripped, so any type
variable (indicated by `α`) can be assumed to be quantified.

### Recursive types

A recursive type is an infinite type which has been abbreviated, since printing
it would obviously be impossible (and dealing with it in type inference too).

It looks like `(t=type)`, where `type` references `t`. An example recursive type
is the one given for `\x -> x x`.

```
slick> \x -> x x
<function> : ((t22 = ((t22 -> α26)) -> α26) -> α26)
```

`(t22 = ((t22 -> α26)) -> α26)` is the recursive type. You can imagine `t22`
being substituted for _that whole expression_, which means it expands to

```
(((t22 = ((t22 -> α26)) -> α26) -> α26)) -> α26
```

after one expansion and

```
(((((((t22 = ((t22 -> α26)) -> α26) -> α26) -> α26)) -> α26) -> α26)) -> α26
```
after two.

### More details

A partially-complete outline of the full type system (especially details related
to type checking) can be found [here](/docs/typing.pdf).
