%Ambiguous Lambda Calc - 4
%Athan Clark
%2/1/2015

Ambiguously-Typed Lambda Calculus
=================================

> Continuing the continuation

This post follows from the previous three, and gives an outlook on how
substitution / unification will be facilitated. We try our hand at unifying
a ltext expression, as well.

## Prior Cleanup

### Shapes Instead of Types

I decided to remove "types" from our terminology - we do not have certainty
in our function types. From now on, I will be describing a term's "__shape__",
rather than it's type, with our `forall a b. {a >= 1} => a` syntax.

### Term Expressions

For completeness, our term expressions are as follows:

1. Literals: `"foo"`, `"bar"`, etc.
2. Terms: `x`, `y`, `a`, etc.
3. Application: `^f x`, `f^x` - `^` shows which term's stack will be above the
   other - `^f x` will have `f`'s parameters prioritized before `x`'s.
4. Abstraction: `\p -> E` where `p` may or may not occour in `E`.
5. Monoid: `x <>. y`, `x .<> y` - `.` shows which term's stack will be below the
   other - `x <>. y` will have `x`'s parameters prioritized before `y`'s.

> __Note__: When using the same monoidal append in your expression, ie; `<>.`
> or `.<>`, things will stay associative. But, if they are different,
> we don't have associativity anymore (in terms of parameters. Sizes still are.)

Operator precedence is as follows:

| Operator    |
|:-----------:|
| application |
| `<>.`       |
| `.<>`       |

I try to imagine `.<>` like `&` from
[lens](https://hackage.haskell.org/package/lens)... but for... non-associative
monoids... :x

Basically, I think writing in a left-to-right style is how most small expressions
will go, and `.<>` will be used for the lazy, to put their additions above others.
In modular codebases, this might be a useful hack.

An interesting side effect - in a function application expression, if we have
one `^` for every term, then unambiguous parameter precedence is decidable:

```haskell
-- f x y z
^^f x ^y z ~ ^((^f x) ^y) z
```

> I realize I'm mixing the term _prescedence_ here, and it's not fair. I'll
> come up with a new term to describe parameter stacks.

An example term expression:

```haskell
(\x -> x <>. "foo") ^ (\x -> \y -> ^x y) .<> (\z -> ^z "bar")
```

> Something also kinda funny is that stack precedence does not matter when
> working with literals: `x <>. "foo"` ~ `x .<> "foo"`

### Substitution Model

Here are some more guidelines for the project:

1. We expect end-users to _only_ use the term expression syntax.
2. _Shape_ inference should be decidable, and _describes_ our terms under the
   assumption of infinite arity.
3. Reduction is facilitated through _substitution objects_ when run - basically
   manual lambdas where the body can only contain application, monoid, parameter
   terms and monoidal append (our `Result` type in [2](/blog/atlc2)).

Our substitutions are essentially lambdas, but with a stack of parameters. We
document... differently. Here is what `\y x -> ^y x <>. "foo"` turns into:

```haskell
\ [_____] := y
  [_____] := x
  | y-1 |
  |-----|
  |  x  |
  @-----@ :--> y x <> "foo"
```

And the monoid `x .<> y` turns into:

```haskell
\ [_____] := x
  [_____] := y
  |  y  |
  |-----|
  |  x  |
  @-----@ :--> x <> y
```

A couple things to see here:

- We first assign our first and second parameter to size variables.
- (Imagine this was built after proper size deduction) We handle the stacks in
  their proper order - `^f x` means "f's params before x's".
- Our result expressions are free of precedence __and__ lambdas.
- `@-----@` has a type `:: 0` - the "end".
- Each substitution body has scope only to it's parameters.

Something also pretty funny is that this "substitution object" still has variant
/ undecided arity (from `y-1` and `x`). In order to define these, we need to
apply either..

1. a literal
3. another substitution

Which are the same possabilities that term names can reduced/resolve to.

> __Note__: A monoid over two literals should automatically append them:
> "foo" <> "bar" ~ "foobar"
