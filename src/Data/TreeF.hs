{-# LANGUAGE TypeFamilies #-}
module Data.TreeF where

import           Data.Functor.Foldable

data TreeF a r = BranchF r r | LeafF a
data Tree a = Branch (Tree a) (Tree a) | Leaf a

type instance Base (Tree a) = TreeF a
