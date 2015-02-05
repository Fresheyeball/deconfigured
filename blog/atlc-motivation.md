%Ambiguously-Typed Lambda Calculus Motivation
%Athan Clark
%2/5/2015

Ambiguously-Typed Lambda Calculus
=================================

> Problem Domain Exasperated

## What is going on?!?

Okay, okay, I'll back up. This has all been a whirlwind for me, I hope you
are forgiving.

Consider the issue - the pure, untyped lambda calculus does not designate what
evaluation order takes place, but merely provides intutitive structure for how
application and abstraction should take place.

Consider the construct, a simple application in the pure lambda calculus:

```haskell
f x
```

What would this give us? If we "peek" into `f`, we may have something like this:

```haskell
(\h0 h1 h2 h3... B h0 h1 h2 h3...) x
```

I abbreviate with a `head` pattern match on the first parameter like so:

```haskell
(\h ~. B h ~) x
```

> I abbreviate `~` for _"the rest of the parameters"_.

So, here is the kicker. When we apply `x`, obviously it is a one-for-one
substitution with `h`, correct? Wonderful. Now, what happens when we apply
another parameter?

```haskell
(\h ~. B h ~) x y
```

Where will `y` be applied? Only to the next "head" in `~`? Or can we apply it
to `B h`, directly?

This is where the untyped, pure lambda calculus is non-negotiably identical.
It's just a trivial Eta conversion, right?

```
(\h. B h) ~ B
```

This is where I've found a discrepency - if we apply not just a term, but a
lambda or a function into `f`, what would happen to the residual parameters to
the lambda?

More specifically, How would this evaluate?

```haskell
f := (\h ~. B h ~)
x := (\p ~. C p ~)

f x
(\h ~. B h ~) (\p ~. C p ~)
```

Imagine - we still have a ton of parameters to handle in `x`, what would happen
to them? Would they be packed away into the result, available only after fully applying
`f`? In the untyped lambda calculus, (theoretically) `f` would have _infinite_
parameters. So what else could happen?

This is where I went nuts. If you include parameter nesting into the untyped
lambda calculus, is it necessary for us to have some notion of _priority_ or
_precedence_ in how additional parameters are necessitated.

### Diagram

Let the following diagram serve as a guide:

![](/images/precedence2.png)

Each empty circle is a _necessary_ lambda term (that is, it must have a parameter),
and the filled circles represent terms that _may not be a lambda_, that is -
it is not necessary for them to have parameters, _but they may_.

So, we first apply `f` to `x`, and get our result `f x`. Then, we have an option
with `y`, we could either apply it to the next parameter available in `(f x) y`,
with `\alpha`, or we could apply... it... to the result... `\beta`, with...
well, `(f x) y`.

### Okay, seriously, wtf.

This is where we get complex, and I present the idea of a size-dependent type
theory to help differentiate the possible actions we could take.

In this size-dependent type theory, every lambda term has some positive number
of parameters, correct? With the untyped lambda calculus, we just assume they do.
But, what if they don't? Obviously, `\x. B x` has 1 parameter, then hands it off
to `B`, which, well, __we don't know__. This is why we quantify polymorphically
over the natural numbers, and also why we enforce minimum bound constraints, because
obviously `f x` would not work, if `f` did not have `>= 1` parameters available.

### Back to the diagram

Our notation is fairly trivial - in the system, every free term, without
syntactic inspection, is assumed to be purely polymorphic in it's number of
parameters:

```haskell
f : forall a. => a
```

If, however, we attempt to utilize an assumed-to-be-existing parameter in
`f`, we would obviously need to reciprocate that constraint:

```haskell
f : forall a. {a >= 1} => a
```

This type is inferable by inspecting the abstract syntax tree.

Now, let `f : forall a. {a >= 1} => a`, and `x : forall b. => b` in our diagram:

![](/images/precedence3.png)

What would be the type of `f x`? This is where we banter into "precedence".

## Precedence

In short, we can _merge_ `f`'s and `x`'s extra parameters very simply through
addition:

```haskell
f x : forall a b. {a >= 1} => (a - 1) + b
```

This doesn't give any concern to _how_ it's merged, but rather just the total
__size__ of the merge. This fits with the untyped lambda calculus - we know
nothing of how it's evaluated, but we do know that there is a result. This is
the size of the result.

Which result, though? Do we want to __prioritize__ `f`'s extra parameters, or
`x`'s? Lets look at the diagram once more:

![](/images/precedence4.png)

`\alpha` could be seen as the result of `f x` when `f`'s extra parameters are
prioritized, while `\beta` is what happens when we prioritize `x`'s _before_
`f`'s (strange idea, I know). You could almost see the arrow going from `x` into
`f`, to push the rest of `b` to the outer layer of `f x` (when assuming `\alpha`
is the next application).

### Implications

What does this mean? Well, we have _two_ forms of application, now:

- inner nested - `f ^x`
- outer nested - `^f x`

In terms of tractible semantics. What else does this mean...?

#### Constant Functions

Well, we actually messed up a bit. What happens if `f` throws away it's first
argument, or is the second parameter to a constant function? Well, of course,
`x`'s arguments would not be included in the result! Therefore, we need some kind
of occurs-check, such that if `x` is included in the _body_ of `f`, then
we include `b` to the result size. If not, then we add nothing (`0` - this is in
the space of the natural numbers). We represent this check with `|b|_a` - `b`'s
absolute value, in the _use_ of `a`:

```haskell
f x : forall a b. {a >= 1} => (a - 1) + |b|_a
```

Another note - what if we _replicate_ the use of the parameter in `f`? What
if `f` looks like `\h. B h (B' h)`, or something like this? Well, the way I see it,
the `h` references in the body of the lambda are still identical - therefore, the
parameter listing included with `h` should reciprocate across both use cases. Otherwise,
we would need to multiply `|b|_a` by the number of inclusions, and thus combinatorically
explode. This tries to keep things _identical_.

#### Monoid

If our terms are instances of a monoid, then __any lambda terms are also monoids__,
such that appending two lambdas simply merges their parameters (without reduction
/ application / substitution / `-1`), and is homomorphic through the respective
bodies:

```haskell
x : forall a. => a
y : forall b. => b
x <> y : forall a b. => a + b

(\h. B h) <> (\p. B' p) =
  (\{h p}. B h <> B' p)
```

Where `{h p}` denotes __unknown order__ - our monoid instance, as well, also has
precedence:

```haskell
(\h. B h) <>. (\p. B' p) =
  (\h p. B h <>. B' p)

(\h. B h) .<> (\p. B' p) =
  (\p h. B h .<> B' p)
```

Any term of type `0` is our `mempty` - in my project [ltext](https://github.com/athanclark/ltext),
this is just a raw string.

## Back to Discretionary Application

If we take another look at our first diagram:

![](/images/precedence2.png)

We can see that `(^f x) y ~ \alpha`, and `(f ^x) y ~ \beta`. Do we have discretionary
abstraction?

If we think sideways, we can see that precedence is _beyond the abstraction_ -
the body of `\h. B h` has an implied merge of `B`'s size and `h`'s, but our
abstraction itself is just `1` (...because it's just one parameter) -
therefore, the actual discretion of _how parameters
are merged_ is done through _application_.

## Short Examples

Just to be... practical, here are some examples of traditional lambda constructs,
but in our ambiguous style:

### Identity

```haskell
id : forall z. {z >= 1} => z
id := \x. x : forall a. => a + 1
```

such that, when we apply `(\x. x) (y : forall b. => b)`, our resulting type
unifies `a` with `b`, and decrements `1` due to the consumed parameter.
Also pretty crazy, is that `(z - 1) + |b|_z` ~ `b`, because `z` ~ `a + 1`, and
`x : a`. Ta dah.

### Constant

```haskell
const : forall z. {z >= 2} => z
const := \x _. x : forall a b. => a + 2
```

Notice that we don't include the second parameter's type in our type signature:

```haskell
const' := \x (y : forall b. => b). x
```

In this case, `|b|_z ~ 0`, because it does not occur.

### S-Combinator

```haskell
s : forall z. {z >= 3} => z
s := \f g x. f x (g x) : forall a b c. {a >= 2, b >= 1} =>
                           (a - 2) + |c|_a + (b - 1) + |c|_b + 3
```

### Monoid

```haskell
<> : forall z. {z >= 2} => z
<> := \x y. x <> y : forall a b. a + b + 2

mempty : 0
```

where `mempty`'s definition is beyond the scope of the calculus, and defined in
it's use / implementation.

### Beta Reduction and Eta Conversion

```haskell
f :   forall a.   {a >= 1} => a
x :   forall b.            => b
f x : forall a b. {a >= 1} => (a-1) + |b|_a

-- f ~ (\h. B h)
\h. B h :     forall j k.   {j >= 1} => (j-1) + |k|_j + 1
(\h. B h) x : forall j k b. {j >= 1} => ((j-1) + |k|_j + 1)
                                      - 1 + |b|_a
-- a ~ (j-1) + |k|_j + 1
-- h ~ x
-- b ~ k
-- |k|_j ~ k
-- therefore,
(\h. B h) x : forall j k b. {j >= 1} => (j-1) + |b|_j
B x :         forall j k b. {j >= 1} => (j-1) + |b|_j
-- (j-1) ~ (a-1)
-- |b|_j ~ |b|_a
-- Q.E.D. (I think)
```

## Legibility

It's pretty easy to see that we _add_ the constant `1` for each lambda, but
combine generally (and totally) terms we aren't aware of yet. Also, with application,
we decrement `1`.

## Conclusion

What will this be useful for? Having discretion on where you apply terms - you
can either apply to the historically applied term `f`, or apply to it's input
`x`. This divergence creates a seam between what we normally use as strongly-typed
Haskellers, and something of a tail-cascading application style - the PHP of
the semantic calculi, so to speak. Why would this be useful? Well, if we mandated
finite parameter counts in our implementation of this calculus, then we can let
the engine cascade our parameter precedence through a stack - if I apply
`f ^"foo"` - some `f` to a literal, but have the literal's parameter stack
prioritized... that's literally the same as `^f "foo"`, because `"foo" : 0`,
and thus doesn't need any. This seam would blur, and rather than having
total knowledge of where terms are going statically (as in type systems like
Haskell / ML / et. al), we would trust our precedence pragmas to handle them
sensibly.

I hope this gave more insight as to _why_ I'm wasting my time with this.
It's a seriously impacting conclusion, and I hope it gives you insight
to this style of "discretionary application evaluation".

Thank you, I hope to make ltext soon, so we can play around with this idea.
Love, peace and chicken grease!
