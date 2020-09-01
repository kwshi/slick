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

## Arbitrary-arity tags

Similarly, tags must all take a single argument. This means that `True` is
syntactic sugar for `True {}`. This is a fine substitution, but the way that
case statements handle 0-argument tags reveals that we are actually just
ignoring their implicit single argument.

```
slick> def f := \x -> case x | True -> "hi" | False -> "bye"
<function> : ∀ α18. ∀ α17. (⟦True : α18, False : α17⟧ -> String)
```

Ideally the type shown here would be `⟦True, False⟧ -> String`.

## Type printing

Types can be formatted nicer.

### Variable names

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

### Spurious foralls

Occasionally when inference is done, there will be unnecessary foralls added
onto types. These are technically correct types, but they should be removed.

```
slick> (\x -> case x: | Some n -> n | x -> 0) (Some 3)
3 : ꓤ ρ23. Int
```

This is a result of variables being quantified by looping over the context.

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

## Tuples

No tuples exist at present, although records can be used in lieu of them. We
imagine that we will add tuples which act as a wrapper around records to provide
a nice user interface for records without labels.

## Case variant closing

We should statically analyze case statements and close up the input variant if
there are no fallthroughs in a `case`.

E.g. the below should be closed

```
slick> (\x -> case x | A -> x | B -> x)
<function> : (⟦ρ47 | B, A⟧ -> ⟦ρ47 | B, A⟧)
```

but the following is OK

```
slick> (\x -> case x | A -> x | B -> x | _ -> x)
<function> : (⟦ρ47 | B, A⟧ -> ⟦ρ47 | B, A⟧)
```
