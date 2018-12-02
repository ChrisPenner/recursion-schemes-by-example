---
what: "Base"
why: "Each recursive type has a 'Base' where recursion is factored out into a type parameter"
section: "Data"
title: "Base"
---

# Base

Reading through type signatures in the `recursion-schemes` library you'll see
many occurances of something like `Base t a`. `Base` is a *type family* (i.e. a
function on types which returns a result type based on its arguments). The
`Base` type of some data-type is a variant of the data-type where any slot in
the structure which contained a recursive occurance of the type is instead
replaced with a new type parameter, and a Functor instance is defined over the
new type parameter. This means that `Base t` will always resolve to a type of
kind `Type -> Type`; i.e. the `Base` type of a data structure always has at
*least* one type parameter, and has a `Functor` instance which maps over that
type parameter.

Defining this machinery is what allows the
`recursion-schemes` library to collapse or expand your data one *slice* at a
time. Since the recursive portion of the type is replaced by a type parameter,
the library can fill that slot with the result of applying your algebra,
allowing you to simply pass a single step of your
[algebra](/articles/recursive/f-algebra) and have the library handle the iteration for you.

For an easy-to-understand example of how to build a `Base` type for your
structure take a look at [ListF](/articles/data/listf)
