%Ambiguous Lambda Calc - 2
%Athan Clark
%1/30/2015

Ambiguously-Typed Lambda Calculus
=================================

> ...continued

This post follows from the [primer on ATLC](/blog/atlc), and dives deeper into
the semantics and tries to add more formal structure to the system.

## Grammar

> I'm using pandoc with markdown, please be forgiving and replace `Expr` with
> `/sigma` :(. Also, these ASTs are __NOT__ properly recursive; please be patient.

### Terms

```haskell
Result = Term
       | Expr `$` Expr
       | Expr `<>` Expr
       | Literal

Expr = \(Term) -> Expr
     | Result
```

We split `Expr` into `Result` for clarity when considering substitution mappings -
the underlying machinery for lambdas. We also include `<>` to the working
vocabulary for _every_ lambda __result__ (not expression!).

### Types

```haskell
Decl = [Var]

TypeScheme = Decl [Var `>=` Nat]

TypeExpr = Nat
         | TypeExpr `+` TypeExpr
         | TypeExpr - Nat

Type = TypeScheme TypeExpr
     | Nat
```

A `Decl` is just a `forall a ...` listing - declaring variables. We map type
variables 1-for-1 with their respective function term name and abstracted
argument names. This is due to the lack of `->`. A (polymorphic) type scheme
is a delcaration, and an (optional) listing of inequalities, constraining
declared variables to their minimum bound.

The actual type of a term is then either a literal (for `String` and constant
functions, [see below](#fun-ideas-examples)),
or a type scheme with it's type expression -
where a type expression facilitates our dependent sized union.

### Parameters

Every term has it's finite set of parameters, set when the term was declared
with lambda expressions.
These parameters form a total order, and a lambda
term itself is a substitution mapping from this total order to it's monoid
result.

```haskell
data Term = Term Name Substitution

data Substitution = Substitution [Name] -- parameters
                                 Result
```

A `Term` is a variable name with it's substitution mapping, and a `Substitution`
is a total order of parameter names, and the result.

### Application

As stated before, application is dependently typed:

```haskell
($) :: forall a b. {a >= 1} => a + b - 1
```

And, moreso, substitutions are _merged_:

```haskell
-- covariant parameter nesting
(Substitution (n:fs) r1) $^ (Substitution xs r2) =
  Substitution (fs ++ xs) r1[n/r2]

-- contravariant parameter nesting
(Substitution (n:fs) r1) ^$ (Substitution xs r2) =
  Substitution (xs ++ fs) r1[n/r2]
```

The `^` denotes which side is _highest_ in nesting.

### Monoid

For completeness (yeah.. completeness...):

```haskell
-- covariant
(Substitution xs r1) <>. (Substitution ys r2) =
  Substitution (xs ++ ys) (r1 <> r2)

-- contravariant
(Substitution xs r1) .<> (Substitution ys r2) =
  Substitution (ys ++ xs) (r1 <> r2)
```

The `.` deonotes which side is _lowest_ or least priority. Note that this doesn't
mess with the result's monoid, but only how parameters are nested.

## Fun Ideas & Examples

(these might stay or go...)

- Constant functions have literal type signatures:

```haskell
\z -> z <> "foo" :: forall a. {a >= 1} => a
\z ->      "foo" ::                 1
```

...when you apply it, it's automatically `0`. I'm not sure if it's a legitimate
idea, but if feels nice :). It would eliminate the polymorphism & information
of the unused parameter, may be worth keeping.

- Monoid over lambdas:

```haskell
(\x -> x) <>. (\y -> y) = \x y -> x <> y
(\x -> x) .<> (\y -> y) = \y x -> x <> y
```

## Conclusion (so far)

So, I failed to make a complete idea. But, it's a start! If you would like to
contribute or adopt the idea, please use & abuse my free time, I would like to see
this project go far.

> Continue to the [next post](/blog/atlc3).
