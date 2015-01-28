%Dag
%Athan Clark
%1-26-2015

DAG
===

> Directed Acyclic Graphs

So, I've been messing with haskell for some time, and I think graphs are pretty
easy to reason about; they give us a straightforward system for modeling
"connections" (whatever that means), dependence, optons, things like this.
However, in Haskell, it's not obvious how we would declare such a type. The most
we have is structural recursion, which mirrors tree structures far better than
sets or graphs. So, I took an inductive approach and made graph construction
inductive. There are major flaws in the project, but it is still a nice
experiment. You can find it on [github](https://github.com/athanclark/dag) and
on [hackage](https://hackage.haskell.org/package/dag).

## Overview

To get started, I just want to show how the graph works and it's capacities,
for some easy absorption for the reader.

### From Scratch

I split the graph into two components - a `String`-indexed map of nodes
(called a `NodeSchema`), and an inductive list of edges between keys (called
an `EdgeSchema`).

```haskell
{-# LANGUAGE DataKinds #-}

edges =
  ECons (Edge :: EdgeValue "foo" "bar") $
  ECons (Edge :: EdgeValue "bar" "baz") $
  ECons (Edge :: EdgeValue "foo" "baz") $
  unique
```

`edges` is our `EdgeSchema`. We use `GHC.TypeLits` for our key (making our
key type parametric gives a funny error :P), and we force the edges to be unique
with the `unique` value - just `ENil`, but type-coercing for unique schemas.

If we check out the type of `edges`, its nutty:

```haskell
λ> :t edges

-- Note these are promoted
edges ::
  EdgeSchema
    [EdgeType "foo" "bar", EdgeType "bar" "baz", EdgeType "foo" "baz"]
    [("foo", ["bar", "baz"]), ("bar", ["baz"])]
    True
```

The first type parameter to `EdgeSchema` is the list of edges verbatim, with
a different promoted type. We used `Edge` and `EdgeValue` before for user
friendliness and compatability with `(->)`, and now `EdgeType` and `EdgeKind`
serve to provide structure for our type-level functions.

The second parameter is a type-level map of the transitively reached nodes each
node has. This gives us a cut-and-dry method for exclusion.

The last parameter is just a switch for uniqueness.

For instance, check out what happens when we add another edge:

```haskell
λ> :t ECons (Edge :: EdgeValue "foo" "qux") edges

ECons (Edge :: EdgeValue "foo" "qux") edges ::
  EdgeSchema
    [EdgeType "foo" "qux", EdgeType "foo" "bar",
      EdgeType "bar" "baz", EdgeType "foo" "baz"]
    [("foo", ["qux", "bar", "baz"]), ("bar", ["baz"])]
    True
```

We achieve type-safety of acyclicity through existential constraints on GADT
data constructors and type families on resulting types. Check out `ECons`:

```haskell
λ> :t ECons

ECons ::
  Acceptable (EdgeType from to) oldLoops unique =>
    EdgeValue from to -- Edge to add
 -> EdgeSchema old oldLoops unique -- older edges
 -> EdgeSchema -- result
      (EdgeType from to : old)
      (DisallowIn (EdgeType from to) oldLoops 'False)
      unique
```

This is where the madness starts to fester. We rely on `Acceptable` to give us
an edge that's sensible, the construct the resulting type manually with
`DisallowIn` and the list prepend.
