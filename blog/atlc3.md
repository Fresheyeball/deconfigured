%Ambiguous Lambda Calc - 3
%Athan Clark
%1/31/2015

Ambiguously-Typed Lambda Calculus
=================================

> ...continued again

This post follows from both the [primer on ATLC](/blog/atlc) and the
[next post](/blog/atlc2) - attempting to give a working implementation

## Complete Type Inference

In the last post, we gave a bit of detail on how the AST should work.
Now, I would like to tie all of this together to hopefully make unification a
breeze.

First, we will analyze how much type inference we can make with just value terms -
`f x`, `\a -> ...`, and so on.

### Terms

All terms are assumed to have an unconstrained polymorphic type at first;
the use case of `foo` will be assumed to have a type `forall a. a`, where `a`
is a fresh type variable in our environment.

### Term Application

There are three "phases" to type manipulation when applying two terms:

1. Induce constraint on function
2. Decrement function's type
3. Monoidally append the input type and decremented function type through addition

Phase 2 and 3 should happen atomically in the same transaction - phase 3 is
actually the result of the application expression `$`.

Something like this:

#### Initial Setup

```haskell
f :: forall a. => a
x :: forall b. => b
```

#### 1

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
(f :: forall a. {a >= 1} => a) x
```

#### 2

```haskell
f x

> :t f in f x
f :: forall a. {a >= 1} => a - 1
```

#### 3

```haskell
(f x) :: forall a b. {a >= 1} => (a - 1) + b
```

In 2, we are doing a type query inside a closure. Also, note that in 3 the order
of type variable declarations `forall a b.` must match one-to-one with the terms
being described `f x`.

### Name Abstraction

Making a lambda will have a different type result depending on if the term name
abstracted occurs in the expression `E`:

```haskell
\p -> E
```

Type inference here is also pretty trivial. First and foremost, we can _add_
`1` to the sub expression `E` regardless of what happens. Then, depending on
if `p` exists in `E`, we can decide to add `p`'s type to `E + 1`:

```haskell
:t \(p :: forall a. => a) -> (E :: forall b. => b)
 | -- occurs p E ?
 |
 |\_Y_> (b + 1) + a
 |
  \_N_> (b + 1)
```

This is how we solidify constant functions in our type signatures.

### Monoid

This is actually the second most simple expression to infer, next to a term:

```haskell
(x :: forall a. => a) <> (y :: forall b. => b)
(x <> y) :: forall a b. => a + b
```

### Literals

Crud, I forgot monoid literals like `String`, which all have a type `0`.

## Conclusion

This should be strongly normalizing semantics for the (sized) type inference
for this lambda calculus / language / thing. You can see that I've covered
all possible values of type `Expr` from the previous post.
