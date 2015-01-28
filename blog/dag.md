%Dag
%Athan Clark
%1-26-2015

DAG
===

> Directed Acyclic Graphs

So, I've been messing with Haskell for a couple years, and I've always been
perplexed on how to model acyclic graphs in a _type safe_ way. However, in
Haskell, it's not obvious how we would declare such a type. Most of what we have
is structural recursion through (G)ADTs, which is closer tree structures than
sets or graphs. Since that day of disapointment in my perusal of
[hackage](https://hackage.haskell.org)
, I vowed in my heart to make a directed acyclic graph library worthy of
awe; my quest would cost me my sanct, my prosperity, nay, my eyeglass
perscription. Alas, ye shall see the fruits of dangerous obsession with type
safety - __uselessness__.

...kinda.

There are major flaws in the project, but it is still a nice
experiment. You can find it on [github](https://github.com/athanclark/dag) and
on [hackage](https://hackage.haskell.org/package/dag).

#### TL, DR;

> You can't even pattern match on our type-safe construction, but we can
> extract the spanning trees from an `EdgeSchema`. The most I could see this
> useful for is an authentication scheme for a website - having many-many (but
> acyclic) dependency chains of access groups. That should come shortly.

## Overview

To get started, I just want to show how the graph works and it's capacities,
for some easy absorption for the reader.

### From Scratch

I split the graph into two components - a `String`-indexed map of nodes
(called a `NodeSchema`), and an inductive list of edges between nodes (called
an `EdgeSchema`). We reference nodes throughout the code with it's respective
"key" - a `String` value.

```haskell
{-# LANGUAGE DataKinds #-}

data SomeType = Val1
              | Val2
              | Val3
  deriving (Show)

nodes = nadd "foo" Val1 $
        nadd "bar" Val2 $
        nadd "baz" Val3 $
        nempty

edges =
  ECons (Edge :: EdgeValue "foo" "bar") $
  ECons (Edge :: EdgeValue "bar" "baz") $
  ECons (Edge :: EdgeValue "foo" "baz") $
  unique
```

`nodes` is our `NodeSchema` map, and `edges` is our `EdgeSchema`. Note that we
use normal `String` values for `NodeSchema` maps, while `EdgeSchema`'s use
`GHC.TypeLits.Symbol` for keys. This is for... fun reasons :) (see below).
We force the edges to be unique in this example with the `unique` value - just
`ENil`, but type-coercing for unique schemas.

`nodes` looks pretty simple:

```haskell
> :t nodes

nodes :: NodeSchema SomeType
```

...but if we check out the type of `edges`, its nutty:

```haskell
> :t edges

-- Note the lists and tuples are promoted:
edges ::
  EdgeSchema
    [EdgeType "foo" "bar", EdgeType "bar" "baz",
     EdgeType "foo" "baz"]
    [("foo", ["bar", "baz"]),
     ("bar", ["baz"])]
    True
```

The first type parameter to `EdgeSchema` is the list of edges _verbatim_, with
a different promoted container type. We used `Edge` and `EdgeValue` before for
`ECons` (user friendliness and compatibility with `(->)`), and now `EdgeType`
and `EdgeKind` serve to provide structure for our type-level functions / type
families.

The second parameter is a type-level map of the transitively connected nodes,
for each node. This gives us a cut-and-dry method for exclusion.

The last parameter is just a switch that enforces uniqueness.

For instance, check out what happens when we add another edge:

```haskell
> :t ECons (Edge :: EdgeValue "foo" "qux") edges

ECons (Edge :: EdgeValue "foo" "qux") edges ::
  EdgeSchema
    [EdgeType "foo" "qux", EdgeType "foo" "bar",
     EdgeType "bar" "baz", EdgeType "foo" "baz"]
    [("foo", ["qux", "bar", "baz"]),
     ("bar", ["baz"])]
    True
```

We achieve type-safe acyclicity of inductive list of edges through existential
constraints on GADT data constructors for `ECons`, and type families to
properly construct resulting types for `EdgeSchmea`. Check out `ECons`:

```haskell
> :t ECons

ECons ::
  Acceptable (EdgeType from to) oldLoops unique =>
    EdgeValue from to              -- Edge to add
 -> EdgeSchema old oldLoops unique -- older edges
 -> EdgeSchema                     -- result
      (EdgeType from to : old)
      (DisallowIn (EdgeType from to) oldLoops 'False)
      unique
```

This is where the madness starts boiling. We rely on `Acceptable` to deduce only
edges that are sensible, then construct the resulting exclusion map manually
with `DisallowIn`, and tac on the edge to the list. Every `EdgeSchema` has
enough information to determine what additional `ECons` edges can hold -
`Acceptable` can be a context entirely dependent on the older types, and solve
for "acceptable" `from` and `to` instances.

## Okay!

So here's where I got to have fun, the implementation of `Acceptable`:

```haskell
> :i Acceptable

class Acceptable (a :: EdgeKind)
                 (oldLoops :: [(GHC.TypeLits.Symbol, [GHC.TypeLits.Symbol])])
                 (unique :: Bool)
        -- Defined at src/Data/Graph/DAG/Edge.hs:55:1
```

> ...not really sure how to look up instances in `ghci` :[

...and some instances...

```haskell
-- Defined at src/Data/Graph/DAG/Edge.hs:58:1
instance ( Excluding from (Lookup to excludeMap)
         , from =/= to ) =>
            Acceptable ('EdgeType from to) excludeMap 'False where
instance ( Excluding from (Lookup to excludeMap)
         , Excluding to (Lookup from excludeMap)
         , from =/= to ) =>
            Acceptable ('EdgeType from to) excludeMap 'True where
```

If we look sideways, all we're doing here is enforcing uniqueness by delegating
the complexity to it's instace head context - via `Excluding` and `=/=`. `=/=`
just makes sure no reflexive edges are present, and `excludeMap` is the same
`oldLoops` from before.

You read each exclusion as, "`from` should not be transitively connected to
`to`" (and vise-versa). The second exclusion for uniqueness simply states that
we can't add an edge `from -> to` if `to` is already touched by `from`.

### Exclusion Map

So, the theory is "if you know all the transitive connections of each node,
then you can evade a pair of nodes that connects the base and a touched
node", basically:

```haskell
-- Defined at src/Data/Graph/DAG/Edge.hs:35:1
type family Excluding (x :: k) (xs :: Maybe [k]) :: Constraint where
  Excluding a (Just []) = Deducible True -- Basis
  Excluding a Nothing   = Deducible True -- Basis
  Excluding a (Just (a : ts)) = Deducible False -- Reject & Refute
  Excluding a (Just (b : ts)) = Excluding a (Just ts) -- continue
```

This is just `not . elem`, implemented at the type level with structural
recursion over type lists. Notice that we're poly-kinded in `k`. Also...
not the whack `Deducilbe`... I kinda fudged to make it.

### Forged Non-Deducability

So, in the `Hask` category, the terminal object is `()` ("top"), and the initial
object is `undefined` ("bottom"). In the _category of Constraints_, the terminal
object is still `()` (that is `forall a. () => a` holds), but the initial
object _shouldn't exist_ (in my opinion), but bad coersions give you a hack.
Here's the implementation:

```haskell
-- Defined at src/Data/Graph/DAG/Edge.hs:35:1
type family Deducible (x :: Bool) :: Constraint where
  Deducible True = ()
```

Now you see why `Deducible False` _isn't deducible_! :V

### Degenerate Type Inequality

Now that we have a stapler, we can build a rocket ship:

```haskell
-- Defined at src/Data/Graph/DAG/Edge.hs:48:1
type family (x :: k1) =/= (y :: k2) :: Constraint where
  a =/= a = Deducible False
  a =/= b = Deducible True
```

The crazy part is that it actually works! Props / big-ups to `Hermit` on
`#haskell` for this one. It's also in the
[paper on type families](http://www.cis.upenn.edu/~eir/papers/2014/axioms/axioms-extended.pdf)
, had I not been plebby when I asked.

The important thing to take away from this is __when you need to reject
unification, you need a hack__. I dunno... maybe. Eh.

## Alright.

Phew! All done? __Hell no.__

What we need to peek at next is `DisallowIn`. It's pretty ratty:

```haskell
-- Defined at src/Data/Graph/DAG/Edge.hs:76:1
type family DisallowIn
              (new :: EdgeKind)
              ( oldLoops :: [(Symbol, [Symbol])] )
              (keyFoundYet :: Bool) :: [(Symbol, [Symbol])] where
  DisallowIn (EdgeType from to) ( (from, xs) : es) False =
    (from, (to : xs)) :                       -- add @to@ to transitively
                                              -- connected list
      (DisallowIn (EdgeType from to) es True) -- continue
  DisallowIn (EdgeType from to) ( (key, vs) : es ) keyFoundYet =
    (key, (PrependIfElem from to vs)) :              -- add `from` to `vs` if
                                                     -- `to` is an `elem`
      (DisallowIn (EdgeType from to) es keyFoundYet) -- continue
  DisallowIn a [] True                   = [] -- search over.
  DisallowIn (EdgeType from to) [] False = [(from, [to])]
```

So, first off `keyFoundYet` is like an accumulator for our manual recursive
fold. Basically, if we're in the map that matches `from` first, then we just
add `to` to it's connected list. But now we know that we've already visited
that particular key, and we don't need to add a new one to keep the map
up-to-date.

The second pattern match doesn't make us so lucky, but with `PrependIfElem`,
we can simply add `from` to any list that has `to` in it - perfect for adding
transitive connections.

We then handle exhausted searches, both when `from` was found and not. It has
simple code.

## Sub-Conclusion

With this tooling, we can now construct compile-time acyclic graphs, but...

1. Only at compile time
2. We can't pattern match

We've basically created an existential type for every `ECons`, this makes it
impossible to pattern match and undo some of the type families used to construct
`oldLoops`, for instance. Also, another bummer is that `GHC.TypeLits.Symbol`
types can't be used (easily, at least) at runtime (they are also wrapped in an
existential). So how do we get values out of the monster?


__Pure insanity.__

## Pure Insanity

Okay, so a graph's spanning trees can encode all it's connections (and acyclic
connections obviously terminate) in one value. It's simply a list of rose trees
(rose forest), like so:

```haskell
data RTree a = a :@-> [RTree a]
  deriving (Show, Eq)
```
