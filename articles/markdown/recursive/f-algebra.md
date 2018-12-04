---
what: "F-Algebras reduce data embedded in a structure (Functor) down to a single value"
why: "F-Algebras are typically used as arguments to Recursive recursion-schemes"
section: "Recursive"
title: "F-Algebras"
sortKey: 1
---

# F-Algebras

`Algebra` is unfortunately an extremely overloaded term, but here we're going
to specifically be talking about `F-Algebras`. In a nutshell, an `F-Algebra` is
simply any function of type `(f a -> a)`. Honestly, if that makes sense to you,
you can probably stop there and go back to whatever you were trying to do. 
If you want a bit more background on it feel free to keep reading!

In the context of the Recursion Schemes `F-Algebras` are passed to combinators to 
show them how you'd like to collapse down a single 'layer' of structure. The library
can then apply this reduction iteratively until the entire structure has been collapsed.

Here's an example of an `F-Algebra` over lists which shows how to fold a single
'layer' of the `ListF` functor which contains an `Int` as data, and to combine
it with the recursive result of summing the rest of the list. In the `Nil` case we must still
return an `Int` somehow so we use `0` as the sum of an empty list is `0`.

The signature of our algebra is `ListF Int Int -> Int` which means the `f` of
this `F-Algebra` is `ListF Int` . We provide a way to collapse this structure
containing `Int`s into a single `Int`

```{.haskell include=articles/src/Examples/Algebra/FAlgebra.hs snippet=list-sum-algebra}
```

The Recursion Schemes functions accept algebras as arguments to perform their
function, so you can avoid a lot of work by writing a simple algebra which
expresses a single step of your problem and let the library figure out the rest
for you.
