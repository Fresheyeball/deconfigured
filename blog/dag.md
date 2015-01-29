%Dag
%Athan Clark
%1/26/2015

DAG
===

> Directed Acyclic Graphs

So, I've been messing with Haskell for a couple years, and I've always been
perplexed on how to model acyclic graphs in a _type safe_ way - In
Haskell it's not obvious how one would declare such a type. Most of our tooling
comes from structural recursion with (G)ADTs, which more closely models tree
structures than sets or graphs. Since that day of disappointment in my perusal of
[hackage](https://hackage.haskell.org), I vowed in my heart to make a directed
acyclic graph library worthy of awe; my quest would cost me my sanct, my
prosperity, nay, my eyeglass prescription! Hark, thou shalt bear witness to the
fruits of dangerous obsession with type safety...

...__Uselessness__! (_kinda_)

There are major flaws in the project, but it is still a nice
experiment. You can find it on [github](https://github.com/athanclark/dag) and
on [hackage](https://hackage.haskell.org/package/dag).

##### TL;DR:

> You can't even pattern match on our type-safe construction, but we can
> extract the spanning trees from an `EdgeSchema`. The most I could see this
> useful for is an authentication scheme for a website - having many-to-many (yet
> acyclic) dependency chains of ACL groups. That lib should come shortly.

## Overview

To get started, I just want to show how the graph works and it's capacities,
for some easy absorption for the reader.

### Basic Example

I split the graph into two components - a `String`-indexed map of nodes
(called a `NodeSchema`), and an inductive list of edges between nodes (called
an `EdgeSchema`). We reference nodes throughout the code with it's respective
"key" - a `String` value.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
`GHC.TypeLits.Symbol` for keys. This is for... fun reasons :).
We force the edges to be unique with the `unique` term - just
`ENil`, but coercing the expression to be unique.

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

- The first type parameter to `EdgeSchema` is the list of edges _verbatim_, with
  a different promoted container type. Before, we used `Edge` and `EdgeValue` for
  `ECons` (user friendliness and compatibility with `(->)`), and now `EdgeType`
  and `EdgeKind` serve to provide structure for our type-level functions / type
  families.
- The second parameter is a type-level map of the transitively connected nodes,
  for each node. This gives us a cut-and-dry method for exclusion.
- The last parameter is just a switch that enforces uniqueness.

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

### Underlying Machinery

We achieve type-safe acyclicity of inductive list of edges through existential
constraints on GADT data constructors for `ECons`, and type families to
properly construct resulting types for `EdgeSchmea`. Let's look at `ECons`:

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
with `DisallowIn`, and tack-on the edge to the list. Every `EdgeSchema` has
enough information to determine what additional `ECons` edges can hold -
`Acceptable` can be a context entirely dependent on the older types, and solve
for "acceptable" `from` and `to` instances.

## Okay!

So here's where I got to have some fun, the implementation of `Acceptable`:

```haskell
> :i Acceptable

-- Defined at src/Data/Graph/DAG/Edge.hs:55:1
class Acceptable (a :: EdgeKind)
                 (oldLoops :: [(GHC.TypeLits.Symbol, [GHC.TypeLits.Symbol])])
                 (unique :: Bool)

instance ( Excluding from (Lookup to excludeMap)
         , from =/= to ) =>
            Acceptable ('EdgeType from to) excludeMap 'False where
instance ( Excluding from (Lookup to excludeMap)
         , Excluding to (Lookup from excludeMap)
         , from =/= to ) =>
            Acceptable ('EdgeType from to) excludeMap 'True where
```

If we look sideways, all we're doing here is enforcing uniqueness by delegating
the complexity to the instance's head context - via `Excluding` and `=/=`. `=/=`
just makes sure no reflexive edges are present, and `excludeMap` is the same
`oldLoops` from before.

You read each exclusion as, "`from` should not be _reached_ by `to`" (and
vise-versa). The second exclusion for uniqueness simply states that
we can't add an edge `from -> to` if `to` is already touched by `from`. :)

### Exclusion Map

So, the theory is "if you know all the transitive connections of each node,
then you can evade a pair of nodes that connects the base and a touched
node", basically. The main method to _"evasion"_ is done with testing for
existence (like in a list or something), the _rejecting_ it:

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
note the whack `Deducilbe`... I kinda fudged making it.

### Forged Non-Deducability

So, in the `Hask` category, the terminal object is `()` ("top"), and the initial
object is `undefined` ("bottom"). In the _category of Constraints_, the terminal
object is still `()` (that is `forall a. () => a` holds), but the initial
object _shouldn't exist_ (in my opinion) - but bad coercions give you a hack.
Here's its implementation:

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

The crazy part is that it actually works! Mad props / big-ups to `Hermit` on
`#haskell` for this one. It's also in the
[paper on type families](http://www.cis.upenn.edu/~eir/papers/2014/axioms/axioms-extended.pdf)
, had I not been plebb when I asked.

The important thing to take away from this is __when you need to "reject"
*unification*, you need a hack__.

> Note to self - get better at defining... stuff.

## Alright.

Phew! All done? __No.__

What we need to look at next is `DisallowIn`. It's pretty ratty:

```haskell
-- Defined at src/Data/Graph/DAG/Edge.hs:76:1
type family DisallowIn
              (new :: EdgeKind)
              ( oldLoops :: [(Symbol, [Symbol])] )
              (keyFoundYet :: Bool) :: [(Symbol, [Symbol])] where
  DisallowIn (EdgeType from to) ( (from, xs) : es) False =
    (from, (to : xs)) : -- add @to@ to transitively
                        -- connected list
      (DisallowIn (EdgeType from to) es True) -- continue
  DisallowIn (EdgeType from to) ( (key, vs) : es ) keyFoundYet =
    (key, (PrependIfElem from to vs)) : -- add `from` to `vs` if
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
simple code when you stare it in the eyes O_O.

---

## Sub-Conclusion

With this tooling, we can now construct acyclic graphs, but...

1. Only at compile time
2. And we can't pattern match

We've basically created an existential type for every `ECons`, this makes it
impossible to pattern match-out types, or undo some of the type families used
to construct `oldLoops`. Also, another bummer is that `GHC.TypeLits.Symbol`
types can't be used (easily, at least) at runtime (they are also wrapped in an
existential). So how do we get values out of the monster?

__Pure insanity.__

## Pure Insanity

Okay, so a graph's spanning trees can encode all it's connections (and, acyclic
connections obviously terminate) into one object. It's simply a list of rose trees
(rose forest), like so:

```haskell
-- Defined at src/Data/Graph/DAG/Edge/Utils.hs:29:1
data RTree a = a :@-> [RTree a]
  deriving (Show, Eq)
```

If we promote this with `-XDataKinds` like before, we get what we'd expect;
a kind `RTree` with inhabitant type `':@->`. I made a pretty
complicated `SpanningTrees` type function, it takes a list of edges and
returns a rose tree of `GHC.TypeLits.Symbol`s. I suggest trying to decipher
how it works as an exercise for the reader. Hint - manual folds everywhere.

Now, we have a pretty easy method of storing type-family results in values -
`Data.Proxy`. It gives us a dummy data constructor, but with a polymorphic
type parameter:

```haskell
data Proxy a = Proxy
```

An interesting thing about value-level functions is that even with `-XPolyKinds`,
`(->)` (at the value level) still uses `*`:

```haskell
> :set -XPolyKinds
> :k (->)

(->) :: * -> * -> *
```

I mean, how else could we pattern match on anything? Promoted types __don't have
any inhabitants__ (except with `singletons` ... hold on.). This is what `Proxy`
gives us - a way to wrap non-`*` types in a `*` value for runtime functions:

```haskell
-- Defined at src/Data/Graph/DAG/Edge/Utils.hs:120:1
getSpanningTrees :: EdgeSchema es x unique -> Proxy (SpanningTrees es)
getSpanningTrees _ = Proxy
```

Note that `SpanningTrees` is really doing all the work. Then we just
hand over the affiliated proxy.

### Reflection

So `-XDataKinds` gives us automatic reification, but how do we come back to
runtime-land? With Singletons, donchyaknow?

I recently asked a question
[on stack overflow](http://stackoverflow.com/questions/28030118/reflecting-heterogeneous-promoted-types-back-to-values-compositionally)
(while developing this lib...), and got a wonderful response from Andras Kovacs
(sorry, I can't get pandoc to handle unicode yet :c) on this idea.
Basically, we hand-off the `a` inside `Proxy  a` to a type-function `Demote`
that handles most of the hard work. Something very interesting about the
implementation of singletons is the `KProxy` - a kind-level proxy, which
is used to capture / constrain instances of promoted singletons _only_. Then
it was pretty straight forward:

```haskell
-- Defined at src/Data/Graph/DAG/Edge/Utils.hs:52:1
reflect ::
  forall (a :: k).
  (SingI a, SingKind ('KProxy :: KProxy k)) =>
  Proxy a -> Demote a
reflect _ = fromSing (sing :: Sing a)
```

Now we can come back down to runtime-land:

```haskell
-- Defined at src/Data/Graph/DAG/Edge/Utils.hs:125:1
espanningtrees :: SingI (SpanningTrees' es '[]) =>
                  EdgeSchema es x unique
               -> Demote (SpanningTrees' es '[])
espanningtrees = reflect . getSpanningTrees
```

Something also interesting - we basically have a type-level thunk
from `Demote`. This is an artifact of type families and flexible matching.
In summary, type families won't reduce unless if input types can be
matched uniquely (please read the paper for actual details).

If you look at the stack overflow question, you'll see that the definition of
`Demote` indeed is pretty ambiguous:

```haskell
type family Demote' (kparam :: KProxy k) :: *  
type Demote (a :: k) = Demote' ('KProxy :: KProxy k)  

-- User-level instances
type instance Demote'
                ('KProxy :: KProxy Symbol) =
                  String
type instance Demote'
                (KProxy :: KProxy (Tree a)) =
                  Tree (Demote' (KProxy :: KProxy a))
type instance Demote'
                (KProxy :: KProxy [a]) =
                  [Demote' (KProxy :: KProxy a)]
```

The important thing to realize that each of these `KProxy` types are __the same
type__. Super ambiguous. So how do we use the values?

...we... well.. just, do, heh.. :

```haskell
-- Defined at src/Data/Graph/DAG/Edge/Utils.hs:131:1
etree :: SingI (SpanningTrees' es '[]) =>
         String -> EdgeSchema es x unique -> Maybe (RTree String)
etree k es = getTree k $ espanningtrees es
  where
  getTree k1 ( n@(k2 :@-> xs) : ns ) | k1 == k2 = Just n
                                     | otherwise = getTree k1 ns
  getTree _ [] = Nothing
```

...aaand for some reason this type checks :)

We can flip back and build a list of edges out of values, to really
"reflect" the list of edges in our `EdgeSchema` to a list of pairs:

```haskell
-- Defined at src/Data/Graph/DAG/Edge/Utils.hs:174:1
fcEdges :: SingI (SpanningTrees' es '[]) =>
           EdgeSchema es x 'True -> [(String, String)]
fcEdges = eForestToEdges . espanningtrees
```

> If you need more code, github is your ally.

## Bonus

Remember how I said there would be something funny happening if we tried to
make the graph poly-kinded in it's key type? It seems like it should play nice
, right? Clone the repo and try it :)

---

### Moral Of The Story

_Closed_ type families and Constraints are wonderful tools for type-level
programming. However, if we get ahead of ourselves, we find that things
diverge and become un-unifiable pretty quickly. I hope this post gave you
something valuable!
