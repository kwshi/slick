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

## Expressions

### Integers

Write integer literals (`0`, `314`) to create integers.

```
slick> 314
314 : Int
```

### Strings

String literals are enclosed in quotes.

```
slick> "hello, slick!"
“hello, slick!” : String
```

### Functions

#### Anonymous functions

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

#### Function application

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

### Records

#### Defining records

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

#### Getting elements
