{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
module Data.ListF where

import           Data.Functor.Foldable             hiding ( ListF(..) )

data ListF a r = Cons a r | Nil
  deriving (Show, Eq, Functor)

-- type instance Base [a] = ListF a

-- instance Recursive [a] where
project :: [a] -> ListF a [a]
project []       = Nil
project (x : xs) = Cons x xs

-- instance Corecursive [a] where
embed :: ListF a [a] -> [a]
embed Nil         = []
embed (Cons x xs) = x : xs
