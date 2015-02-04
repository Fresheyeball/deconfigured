%Ambiguous Lambda Calc - 4
%Athan Clark
%2/3/2015

Ambiguously-Typed Lambda Calculus
=================================

## Introduction

The Ambiguously-Typed Lambda Calculus __encodes it's types in the Natural numbers__
polymorphically to denote variable parameter size, which may be constrained
depending on their use. All top-level terms can have the most general type,
`forall a. => a`, but it is more interesting to see how this plays out internally,
when we actually use the system.

To use the system productively, all you need are ATLC expressions; you don't
need to pay attention to type signatures or care about substitution mappings.
However, we will dive into these settings to prove at least the conceptual
interest :)

> Sorry, I can't write proofs yet.

We proceed by showing the translation from our shorthand syntax to
our closure - the substitution mapping.

## Notation

Our shorthand syntax has a grammar as follows:

```haskell
e = l -- literal "foo", instance of underlying monoid
  | t -- term `x`
  | \p -> e -- abstraction
  | e e -- ambiguous application
  | e ^e -- inner priority
  | ^e e -- outer priority
  | e <> e -- ambiguous monoid
  | e <>. e -- left priority
  | e .<> e -- right priority
```

The size-dependent type system _should_ be able to infer the type of every
shorthand expression:

```haskell
"foo" : 0
x : forall a. => a
\p -> e : forall a b. => b + 1 + X
    where X = if (occurs p e) then b
                              else 0
f x, f ^x, ^f x
  : forall a b. {a >= 1} => a - 1 + Y
    where Y = if uses (head f) then b
                               else 0
x <> y, x <>. y, x .<> y
  : forall a b. => a + b
```

Where `uses` checks to make sure that the `head` of `f` (the top of it's
parameter stack) is _used_ in it's body.

The types measure the increase or decrease of possible parameters commutatively
and generally, but is entirely dependent on if a closures uses it's parameter.

Lastly, our substitution mapping system. It provides a
"weak-head normal form"-like way of looking at parameters. Sometimes, however,
our resulting substitution mapping may have _both_ unbound variables and an
unsolved polymorphic arity. The notation looks like so:

__Bound version__:
```haskell
\ [_____] =: name : forall a. {a >= 1} => a -- "head"
  | a-1 | -- variant parameters, a "chunk"
  @-----@ :--> (name "bar") <> "foo"
            -- ^ "body"
```

__Unbound and immediately unknown__:
```haskell
x : forall a. => a

x =
  \ |  a  |
    @-----@ :--> B -- unknown
```

We usually reference the whole body of this substitution, like a pattern match,
via `B`, that is to say that `B` uses the parameters `a`.

__0-Ary__:
```haskell
"foo" : 0

@-----@ :--> "foo"
```

A key ingredient here is that a substitution's body should be "strongly normalizing",
in that we are not allowed to use lambdas, but we may have application. Also,
our precedence system does not matter here - it's taken care of in the `head`.
You could say that there are homomorphisms over the monoid operators and
apply operators from our shorthand syntax, into this simple and normalizing
syntax. Not sure what hole this rabbit goes down.

## Definitions

Here is what we have so far:

### Literal

```haskell
"foo" : 0
```

```haskell
  @-----@ :--> "foo"
```

### Term

```haskell
x : forall a. => a
```

```haskell
\ |  a  |
  @-----@ :--> x -- or B
```

`x` is a free variable in our substitution.

### Application

```haskell
(f : forall a. {a >= 1} => a) (x : forall b. => b)
  : forall a b. {a >= 1} => a - 1 + X

  where
    X = (if uses (head f) then b
                          else 0)
```

The `uses` is basically the inverse of our occurs
check with [abstraction](#abstraction). If the parameter we apply with is actually
used in the function, then we will need to handle it's parameters (as it's result
is retained), otherwise, ditch it.

```haskell
f = -- pattern match on the head of the parameters
  \ [_____] =: h : forall z. => z
    | a-1 |
    @-----@ :--> B h -- `a-1` is bound in `B`

x =
  \ |  b  |
    @-----@ :--> x -- could have been a different name,
                   -- left the same for reference

f x =
  \ | a-1 |
    | + X | -- note the commutativity
    @-----@ :--> B x

f ^x =
  \ |  X  |
    |-----|
    | a-1 |
    @-----@ :--> B x

^f x =
  \ | a-1 |
    |-----|
    |  X  |
    @-----@ :--> B x
```

### Abstraction

```haskell
\(p : forall a. => a) -> (e : forall b. => b)
  : b + 1 + X

  where
    X = (if (occurs p (body e)) then a
                                else 0)
```

```haskell
\ [_____] =: p
  |  b  |
  | + X |
  @-----@ :--> e
```

Note that the size of the stack adds the atomic parameter `p`, so the result
type is `b + 1 + X`.

### Monoid

```haskell
(x : forall a. => a) <> (y : forall b. => b)
  : forall a b. => a + b
```

```haskell
x =
  \ |  a  |
    @-----@ :--> x

y =
  \ |  b  |
    @-----@ :--> y

x <> y =
  \ |  b  |
    | + a |
    @-----@ :--> x <> y

x <>. y =
  \ |  a  |
    |-----|
    |  b  |
    @-----@ :--> x <> y

x .<> y =
  \ |  b  |
    |-----|
    |  a  |
    @-----@ :--> x <> y
```

## Examples

### Identity Function

```haskell
id : forall a. => a + 1
id = \x -> (x : forall a. => a)
```

```haskell
id =
  \ [_____] =: x
    |  a  |
    @-----@ :--> x
```

Note that `id : forall b. => b`, too, just like normal terms.

### (Ambiguous) Apply

```haskell
($) : forall a b. {a >= 1} => a - 1 + X
  where
    X = if (occurs x (body f)) then b
                               else 0

f $ x = f x
```

```haskell
$ =
  \ [_____] =: f
    [_____] =: x
    | a-1 |
    | + X |
    @-----@ :--> f x
```

> To force precedence, simply do `f ^x` or `^f x`.

### (Ambiguous) Composition

```haskell
(.) : forall a b c. {b >= 1, a >= 1} => (a - 1) + X
  where
    X = if uses (head f) then b - 1 + Y
                         else 0
    Y = if uses (head g) then c
                         else 0

(f . g) x = f (g x)
```

```haskell
. =
  \ [_____] =: f
    [_____] =: g
    [_____] =: x
    | a-1 |
    | + X |
    @-----@ :--> f (g x)
```

## Conclusion

The substitution system depends entirely on the actual use of parameters
each term has, at runtime - our occurs & uses checks have discretion for a type
_because_ this is a cascading system - if something isn't used, it's history
is forgotten.

I hope you enjoyed this, please ping me if you have any questions!
