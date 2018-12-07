---
what: "Fold data structures down to a single value"
why: "Catamorphisms allow you to express recursive operations elegantly"
section: "Recursive"
title: "Basic Catamorphisms"
sortKey: 2
---

# Basic Catamorphisms

Catamorphisms are a great place to start on your journey towards understanding
recursion schemes as a whole; they aren't as scary as they sound, the prefix
`cata` comes from the Greek word meaning `downwards`, `morph` loosely means 'to
change from one thing to another' so if we squint a bit we can pretend that
`catamorphism` means to reduce something down. Catamorphisms take some _thing_
with some associated structure and reduce it down to some new _thing_ with **less**
structure.

If you've been using a functional programming language for a while you're
probably familiar with other ways of reducing structure such as operations from
the `Foldable` typeclass or perhaps using explicit recursion. Catamorphisms
closely resemble these operations, offering more power than `Foldable` and
consolidating the complexities of arbitrary recursion into a well-formed idea.
Let's get an intuition for how catamorphisms work by comparing them with `Foldable`!

```haskell
foldr :: Foldable t  => (a -> b -> b)   -> b -> t a -> b
cata ::  Recursive t => (Base t b -> b)      -> t   -> b
```

If we specialize both functions to lists we get the following:

```haskell
foldr :: (a -> b -> b)    -> b   -> [a] -> b
cata  :: (ListF a b -> b) ->        [b] -> b
```

There are few things we notice right off the bat: the function argument which
we pass to each of them is slightly different. The function you pass to `foldr`
combines the next element `a` and the accumulator `b` and `foldr` requires a
default `b` in case the foldable structure is empty. The albegra you pass to
`cata` must combine any accumulated `b`s from your structure with any `a` which
exist at the current level of the structure. It doesn't accept an explicit
default, you're required to ensure that your algebra isn't *partial* and will
typically provide your own default at the base case of your data-structure.

The first argument which cata takes (`Recursive t => (Base t b -> b)`) is
called an [**F-Algebra**](/articles/recursive/f-algebras), it's a function
which performs a single reduction step of a recursive operation. See the article
on [F-Algebras](/articles/recursive/f-algebras) to dive in deeper!

The algebra is how we gain more power than available in `Foldable`;
Inside the algebra we have knowledge of the structure of the
data we're folding at each step. Maybe the structure has multiple `a`s or multiple recursive bits in some constructors
and their place in the structure determines their significance? Perhaps there's more than one base case in the structure
and we should return a different default value for each. `Foldable` abstracts over the structure too far for us to make
these decisions.

Let's write the `sum :: [Int] -> Int` function in a few different ways so we
can compare approaches. We'll do one using `foldr`, one with explicit recursion
and finally one with `cata`. 

Here's the simple `foldr` version:

```{.haskell include=articles/src/Examples/Recursive/Cata.hs snippet=sumFoldr}
```

Here's one with explicit recursion:

```{.haskell include=articles/src/Examples/Recursive/Cata.hs snippet=sumRecursive}
```

Here's the `cata` version

```{.haskell include=articles/src/Examples/Recursive/Cata.hs snippet=sumCata}
```

So we can see here that the `cata` version is definitely longer for this
particular case, using `cata` doesn't gain us much when we're operating on
lists since they have no additional structure on top of what `Foldable`
provides us via `toList`; but as we continue onwards to more complex structures
we find that we have more power using an algebra than we do with the `Foldable`
instance of structures.

Let's upgrade our Lists to Trees to see how this plays out!

Here's a data structure representing a [Binary
Tree](https://en.wikipedia.org/wiki/Binary_tree) where each node contains a
value. Notice that we're also deriving a `Foldable` instance for the structure
here using `DeriveFoldable`.

```{.haskell include=articles/src/Data/BinaryTreeF.hs snippet=BinTree}
```

Now we'll write the recursive `Functor` version of the type for use with
recursion schemes where each recursive occurance of the structure is replaced
with a new type variable `r`. 

```{.haskell include=articles/src/Data/BinaryTreeF.hs snippet=BinTreeF}
```

Great! That's a bit more interesting!

To prove that our recursion schemes and algebras give new power over what
`Foldable` brings we'll attempt to implement some standard tree traversals! If
you're unfamiliar with the concept you can read up more about tree traversals
on the [Wiki
page](https://en.wikipedia.org/wiki/Tree_traversal#In-order_(LNR))!

In short, we want to reduce a tree to a list, but there are several possible
orderings to do it!

It turns out that when we derive the `Foldable` class, the order of the fields
in our constructor actually determines which traversal we get! Since we have
the structure `Branch a (BinTree a) (BinTree a)` the derived instance will fold
the `a` first, then the left branch followed by the right, corresponding to a
depth first traversal. That means our implementation of Depth-First-Search
using foldable is simply:

```{.haskell include=articles/src/Examples/Recursive/Cata.hs snippet=depthFirstFoldable}
```

Here's the equivalent version using `cata`

```{.haskell include=articles/src/Examples/Recursive/Cata.hs snippet=depthFirstCata}
```

Hopefully that's relatively stright forward! Now for the tricky bit, how do we
define an [*In
Order*](https://en.wikipedia.org/wiki/Tree_traversal#In-order_(LNR)) traversal
using our Foldable instance? Sorry to break it to ya, but we *can't*! Our
foldable instance only knows how to process all elements, there are no
guarantees on ordering and there's no way for us to talk about the **structure**
of the data as we fold! Using `cata` it's easy! Check it out:

```{.haskell include=articles/src/Examples/Recursive/Cata.hs snippet=inOrderCata}
```

Here's one more quick example of a simple algebra which **can't** be written
using `Foldable`, we'll collect a list of only the leftmost nodes of a binary
tree!

```{.haskell include=articles/src/Examples/Recursive/Cata.hs snippet=leftMostCata}
```

This shows how recursion-schemes and the use of **algebras** helps give us more
**power** by having knowledge of our data's **structure** at the cost of a
little boiler-plate. There are many many more complex things we can do with
`cata`, and some recursive helpers like `para`, `zygo`, `histo` and friends
which help make some common tasks easier!

