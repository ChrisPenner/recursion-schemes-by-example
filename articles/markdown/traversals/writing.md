---
what: "Learn to write and compose traversals"
why: "Traversals can access or alter zero or more elements from throughout a data structure"
section: "Traversals"
title: "Writing Traversals"
---

# Writing Traversals

Traversals are one of the most adaptable and useful optics. In short
they let you:

- Access and change deeply nested parts of state
- Access multiple parts of a structure at once
- 'Focus' on each of multiple elements 'pretending' you're only working with one element at a time

How do Traversals fit into the lens hierarchy?

- All `Lens`es are usable as `Traversal`s over a single element
- A `Traversal` can be treated as a `Prism`s by focusing on the first element of the traversal
- All traversals are also `Folds`
- Most traversals are `Indexable` and can contain location info. E.g. index in their list, key of a `Map`

## How

Most lens tutorials show you how to use and compose traversals, but most skim
over how to WRITE complex traversals.


## Simple Traversals

We'll start of with our imports and extensions:

```{.haskell include=articles/src/Examples/Traversals/Writing.hs snippet=imports}
```

Let's start with the simplest possible traversal! In fact this is the type
you'll probably use the most often. Let's say we work at a bank and we have a
type which represents a simple bank transaction:

```{.haskell include=articles/src/Examples/Traversals/Writing.hs snippet=transaction}
```

We have two constructors, one for withdrawals, one for deposits. Each have the
`amount` accessor which gets the amount of the transaction within **either** of
the constructors.

Great! Let's write our first simple traversal! Let's say we have a list of
Transactions and we want to focus on each element of that list using a
traversal. Any time we're writing a traversal over a data type which implements
`Traversable` over the focus we want then we can actually use the traversable
instance to write our traversal for us! Here's what it looks like:

```{.haskell include=articles/src/Examples/Traversals/Writing.hs snippet=simple-traversal}
```

What?? Yup it's that simple! turns out that the signature of a traversal is
**identical** to that of the actual `traverse` function! Any time something is
traversable we can just use `traverse`; and in fact there's no reason to define
this traversal ourselves unless we want an alias for clarity. In this case
we've actually defined a **worse** traversal than just using `traverse`
directly, can you see why? Let's learn how to read the type signatures of Traversals!

```haskell
Traversal' s a
-- a.k.a.
Traversal' structure focus
-- a.k.a.
Traversal' [Transaction] Transaction
```

The lens library has a convention where a suffix of `'` means to use a 'simple'
form of the provided type. For traversals this means that anything with a type
of `Traversal' s a` is **NOT** allowed to change the type of the structure or
focused value. In our example this means that when using the
`simpleTransactions` traversal we **can't change Transaction into some other
type**. If we try we'll see an error:

```haskell
someTransactions :: [Transaction]
someTransactions = [Deposit 100, Withdrawal 50]

λ> someTransactions & simpleTransactions .~ "a string"

error:
    • Couldn't match expected type ‘Transaction’
                  with actual type ‘[Char]’
    • In the second argument of ‘(.~)’, namely ‘"a string"’
      In the second argument of ‘(&)’, namely
        ‘simpleTransactions .~ "a string"’
      In the expression:
        someTransactions & simpleTransactions .~ "a string"
```

Let's fix this by relaxing our type signature to allow the traversal to change the type of the focus.

```{.haskell include=articles/src/Examples/Traversals/Writing.hs snippet=type-changing-traversal}
```

Now we're using the non-simplified type `Traversal s t a b`; as usual the `stab` types mean:

- `s`: Starting structure, e.g. `[Transaction]` in our case
- `t`: Ending structure, e.g. `[result]`
- `a`: Starting focus, e.g. `Transaction`
- `b`: Ending focus, e.g. `result`

Now that we've relaxed the type signature we can use it to change the type of our transactions if we like:

```haskell
λ> someTransactions & typeChangingTransactions .~ "a string"
["a string","a string"]
```

In practice you probably shouldn't bother writing these types of traversals
yourself, just use provided `traversed` traversal which depends on the
`Traversal` instance and also passes indexing information if you end up wanting
it.

## Selective Traversals

The default traversal works great if you want to select every element of a
traversable structure, but what if we only want to select withdrawal
transactions? Time to write our own custom traversal!

Remember how `traverse` is a valid traversal? That means we can write our own
traversal by matching the type signature of `traverse` itself!

```haskell 
traverse 
  :: (Traversable t, Applicative f) 
  => (a -> f b) -> t a -> f (t b)
```

Let's constrain the type so it matches what we want to do, in our case we want
to focus on the amount of withdrawal transactions, so given a list of
transactions we can access the `Int`s inside JUST the withdrawals.

Using the shorthand notation that gives us:

```haskell
withdrawals :: Traversal' [Transaction] Int
-- a.k.a.
withdrawals :: Traversal [Transaction] [Transaction] Int Int
```

Notice that this sort of operation does **not** allow us to change any of the
types, so we use the `Traversal'` notation, but that doesn't help us write our
actual function, so let's take a look at the signature of `traverse`
specialized to `withdrawals`:

```haskell
withdrawals :: (Applicative f) 
            => (Int -> f Int) 
            -> [ Transaction ] 
            -> f [ Transaction ]
```

This means that our `withdrawals` traversal must take a function as an
argument, that function *wraps* an `Int` into **some** Applicative structure,
but we're **NOT** allowed to pick which Applicative! `withdrawals` needs to use
that function to somehow wrap the whole list of `Transaction`s in the unknown
applicative. This should look pretty familiar if you've ever implemented
`Traversable` by hand, the difference is that here we're allowed to know a bit
of extra info about the types we're traversing!

Let's see if we can write something which at least type-checks with this signature:

```{.haskell include=articles/src/Examples/Traversals/Writing.hs snippet=all-amounts}
```

Here we're relying on the generic `traverse` function for our list type, but we
have to manually unpack and re-wrap each `Transaction` using `go` because it's
not generically traversable over the contained amount. Inside `go` we have to
rely on `<$>` (a.k.a. `fmap`) since we don't actually know which Applicative
will be chosen when the traversal is eventually used.

This type-checks, *BUT* it makes no discrimination between `Withdrawal`s and
`Deposit`s! In fact the traversal we've written selects the `Int` amount of
both withdrawals AND deposits. A quick way to debug your traversals is to
simply fold the selected elements into a list using `(^..)`. Let's see how
`allTransactions` behaves:

```haskell
someTransactions :: [Transaction]
someTransactions = [Deposit 100, Withdrawal 50]

λ> someTransactions ^.. allAmounts
[100,50]
```

So it's pretty clear we're focusing both deposits and withdrawals.

We need to somehow tell the traversal which `Int`s we care about and which we
don't! We do this by selecting the values we care about using the provided `f`
and ignore the values we don't by lifting them with `pure`. You can think of it
as though `f` **marks** focused values while `pure` **ignores** them. Let's alter our
traversal accordingly!

```{.haskell include=articles/src/Examples/Traversals/Writing.hs snippet=withdrawals}
```

And let's test out our new traversal, we'll check the focus with `^..` then try
adding `$10` to all of our withdrawals using `+~` which adds an amount to numbers selected by a lens or traversal.

```haskell
λ> someTransactions ^.. withdrawals
[50]

λ> someTransactions & withdrawals +~ 10
[Deposit {amount = 100},Withdrawal {amount = 60}]
```

Looks great!

One last example as a word of warning, it's tempting to write our `withdrawal`
traversal by `filter`ing our list down to the values we care about and
traversing over those, but don't fall into this trap! If we filter out values
from the list they'll be forgotten when we modify values using the traversal!

Here's an example of what **NOT** to do:

```{.haskell include=articles/src/Examples/Traversals/Writing.hs snippet=bad-withdrawals}
```

Notice how we lose the deposit entirely when we try to edit our withdrawals!

```haskell
λ> someTransactions & badWithdrawals +~ 10
[Withdrawal {amount = 60}]
```

## Traversals over embedded structures

So far we know how to write traversals for types which are already traversable,
but what if the focus we want is embedded deeper within a newtype or record?
Let's expand our example to handle transactions embedded within a bank account
data-type!

```{.haskell include=articles/src/Examples/Traversals/Writing.hs snippet=bank-account}
```

For a non-trivial example we'll show how to write a new traversal which uses
our existing `withdrawals` traversal to access the withdrawals nested within a
`BankAccount`!

```{.haskell include=articles/src/Examples/Traversals/Writing.hs snippet=account-withdrawals}
```

Here we simply reconstruct the unaffected parts of `BankAccount` as normal,
when we get to our transactions we need to wrap them in the given applicative
and **focus** the appropriate elements using the provided `f` function. We can
rely on the fact that traversals behave just like the regular ol' `traverse`
function! We'll run `withdrawals` over the list as though it were `traverse`,
passing it `f` as the function to select values within the structure. Pretty
nifty stuff! Now we have a traversal which can access withdrawals from within a bank account!

```haskell
account :: BankAccount
account = BankAccount Savings [Deposit 100, Withdrawal 50]

λ> account ^.. accountWithdrawals
[50]

λ> account & accountWithdrawals +~ 10
BankAccount 
  { accountType = Savings
  , transactions = [Deposit {amount = 100},Withdrawal {amount = 60}]}
```

Note that if we have lenses over our `BankAccount` we can save a bit of work
and define the same traversal by *composing* a lens with our traversal to build
the new traversal, assuming `transactions :: Lens' BankAccount [Transaction]` we get:

```haskell
accountWithdrawals :: Traversal' BankAccount Int
accountWithdrawals = transactions . withdrawals
```

That'll do it! Most complex traversals can be built using a combination of
these techniques!
