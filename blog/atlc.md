%Ambiguous Lambda Calc
%Athan Clark
%1/30/2015

Ambiguously-Typed Lambda Calculus
=================================

...by Athan Clark.

## Abstract

I present the ambiguously-typed lambda calculus quickly and briefly, then expand
on it's appearance and use cases. Lastly, we close with it's design as an
implementation for the templating engine underlying the
[ltext project](https://github.com/athanclark).

## Background

### SLTC

If we recall the
[simply-typed lambda calculus](http://en.wikipedia.org/wiki/Simply_typed_lambda_calculus)
, we see it as a method for
prescribing functions with basic encodings of it's
[arity](http://en.wikipedia.org/wiki/Arity) -
we first have an
alphabet of type terms / type constants `B`, and a series of arity symbols `->`.
This is similar to Haskell, except there is no polymorphism, and therefore
no unification - just simple 1-to-1 mapping of a function's parameters and it's
type signature:

```haskell
-- SLTC Grammar
data Expr = Term
          | Lamb (Term,Type) Expr
          | App Expr Expr
          | Type
```

Where `Term` is just a reference to a variable, and `Type` is a reference to a
type constant in alphabet `B`. We can then create functions and terms like so:

```haskell
-- foo.sltc
x = -- someExpression
f (a :: A -> B) = a x
data B
```

That should be about all we have in terms of machinery. Notice how all types
in the type signature must match 1-for-1, and that each term can only have one
type.

### Size-Dependent Types

If you have messed around with
[cryptol](http://cryptol.net/), you should be with me so far. Basically, the
idea is this - you have some type-level natural number in your type system
denoting some constraint. In cryptol, they use it to create minimum bounds
for precision or bit depth necessary to fulfill a computation. It has a flavor
similar to the following:

```haskell
foo :: forall a t. {a >= 4} => [a| t |]
```

Imagine this as "a list of `t`s greater-than or equal-to `a` in length". We will
use this idea for encoding arity in our terms.

## ATLC Description

### Poly-arity Functions

In the ambiguously-typed lambda calculus, every term is assumed to have countably
infinite arity, and we place constraints on the functions to preserve minimum
bounds created from parameter declaration:

```haskell
f :: forall a. {a >= 1} => a
f x = -- ...
```

`a` can be seen as the _"amount of arity"_ for `f`.

### Heterogeneous Function Application

Our type system encodes the _magnitude_ of the arity for each term - how many
parameters it accepts. Therefore, our most-concrete function type is one with
`0` arity. This is our type for plaintext in ltext -

```haskell
"foo" :: 0
```

Note, however, that using this value for function application __still consumes
a parameter__:

```haskell
f "foo" :: forall a. {a >= 1} => a - 1
```

Now we diagnose the general case - what is the type of `f x`?

```haskell
x :: forall b. => b
f :: forall a. {a >= 1} => a
f x :: forall a b. {a >= 1} => a + b - 1
```

The critical thing to take from this is that we are resolving the remaining
parameters of `f` and `x` into the result, but we don't know how - we're just
looking at the magnitude.

### Overflow Parameter Resolution

Now we dive one level deeper - actual parameter application. From ATLC's perspective,
much like the untyped lambda calculus, all terms are functions, and can handle
application. This will fail when we populate our terms with insufficient values,
like applying plaintext into plaintext, but this all theoretically makes sense
in the type system. Therefore, we have to handle this potentially infinite
arity somehow - the __Application System__.

...which really only has two ideas - __covariant and contravariant parameter
nesting__. You can view the parameters as a stack (isomorphic to Nats) - Observe
the fancy diagram:

![](/images/application.png)

I use `0` to represent the result, after application is finished (so the left-most
side can now be seen as the head of the parameter list). I also split `f`'s
original type signature to show the different options for substitution.

So, `f x` has two different options for how the stack is resolved (for now, at
least. We might make interspersal an option if it was practical :\) - one where
`f`'s remaining arguments have precedence over `x`, and the opposite. I call
these covariant and contravariant in fifo-style parameter nesting, respectively.

### Bonus Monoid Instance

I will be using this system in
[ltext](https://github.com/athanclark/ltext) for parameterizing files, where
simple variable dereferencing in the text file will monoidally append it's
_result_ value to the text around it. For instance:

__before__:
```haskell
-- foo.ltext
Foo Bar
```

__after__:
```haskell
-- foo.ltext
// this :: forall a. {a >= 1} => a
// this x =
Foo Bar
{x}
```

> The syntax will vary in the final implementation.

As a result, `x` gets monoidally appended to `"Foo Bar"`, no matter it's arity,
such that:

```haskell
(<>) :: forall a. a -> 0 -> a
(<>) :: forall a. 0 -> a -> a
```

or, more generally:

```haskell
(<>) :: forall a b. a -> b -> a + b
```

...now, can we make `<>` a poly-arity function, too? Maybe `mconcat`? Not quite.

#### Countably Infinite Functions

The strange thing about `mconcat` in this case is how we would query the types
of parameters:

```haskell
mconcat :: forall a. sum $ map (::) a
```

...which doesn't really make sense. We would have to see the type of every
parameter, then sum them all together to see the magnitude of the resulting
type, which really can't work as _all_ parameters are modeled after `Nat`,
not some inspectable object. Also, `mconcat` in our case would have another
level of generality, compared with the "countably infinite"-parameter terms
it's dealing with; __all contents in a file are monoidally concatenated__.
This means that there is almost a universal automatic appending going on -
something less tangible than first-class function application.

## Formalization

### Abstraction

```haskell
f :: forall b. {b >= 1} => b
f = \p1 -> -- ...

g :: forall c. {c >= 2} => c
g = \p1 p2 -> -- ...
```

### Modus Ponens / Application

```haskell
x :: forall a. => a
f :: forall b. {b >= 1} => b
f x :: forall a b. {b >= 1} => a + b - 1
```

### Monoid

```haskell
x :: forall a. => a
y :: forall b. => b
x <> y :: forall a b. => a + b
```

## Conclusion

Basically, we now have a fairly well-typed lambda calculus for dealing with
poly-arity functions. It is dependently typed by it's argument length, and
infers constraints based on parameter declaration. Function application and
union (`<>`) are sensible in terms of arithmetic over natural numbers, and eta
reduction is built-in through parameter resolution.

I apologize ahead of time for my lack of proofs, I haven't gotten that far yet.
Soon this will all be in production! Thank you for reading :)
