{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
module Data.TreeF where

import           Data.Functor.Foldable

-- Recursive binary tree with data at leaf nodes
data Tree a = Branch (Tree a) (Tree a) | Leaf a
  deriving (Show, Eq)
data TreeF a r = BranchF r r | LeafF a
  deriving (Show, Eq, Functor)

type instance Base (Tree a) = TreeF a

instance Recursive (Tree a) where
  project (Leaf a) = LeafF a
  project (Branch l r) = BranchF l r

instance Corecursive (Tree a) where
  embed (LeafF a) = Leaf a
  embed (BranchF l r) = Branch l r
