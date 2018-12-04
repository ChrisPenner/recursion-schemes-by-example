---
what: "ListF"
why: "ListF is the Base functor for the [] type"
section: "Data"
title: "ListF"
sortKey: 2
---

# ListF

`ListF` is one of the most common `Base` functors. It's relatively simple and
will be coming up in a lot of examples so it's worth taking a bit of time to
understand it well. It's also one of the few Base functors which comes for
included with the `recursion-schemes`, but for the sake of pedagogy we'll go
ahead and re-implement it here.

If you've read about the [`Base` type family](/articles/data/base) already
you'll have learned that in order to use `recursion-schemes` on a given
data-type we need a version of that data-type where the recursive *bits* of
that type have been replaced with a type parameter. Let's take a look at the
definition of the `[]` type in Haskell and see if we can write `ListF` on our
own:

```haskell
data [] a = [] | a : [a]
```

Lists get special treatment in Haskell since they have custom syntax, but
taking a look at the definition we can see that there are two constructors,
either the empty list (`[]`) or an element paired with another list! The `[a]`
is what we're looking for, it's an occurance of the data type we're defining
within the definition itself! That's the part we need to replace with a type
parameter. Let's write `ListF`, a type that's the same as (see 'isomorphic to')
the list type, but with the recursion **factored-out** into a type parameter
which we'll call `r` for **recursive**.

```{.haskell include=articles/src/Data/ListF.hs snippet=ListF}
```

And that's it! Note how we've also derived the `Functor` typeclass for our
`Base` type, which we'll need to do if we plan on using it with
`recursion-schemes`. If `ListF` wasn't already included for us we'd also want
to define it as the `Base` of `[]` by writing:

```haskell
type instance Base [a] = ListF a
```

Now whenever we write an algebra or coalgebra over `ListF` we have empty list
as our fixed-point, and can hold a piece of data (i.e. an `a`) alongside a
recursively computed result, or the seed for future computation in the `r`
slot! 
