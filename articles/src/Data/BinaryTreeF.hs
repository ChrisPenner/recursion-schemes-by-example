{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Data.BinaryTreeF where

import           Data.Functor.Foldable

-- start snippet BinTree
-- Recursive binary tree with data at branch nodes
data BinTree a = Branch a (BinTree a) (BinTree a) | Empty
  deriving (Show, Eq, Foldable)
-- end snippet BinTree

-- start snippet BinTreeF
data BinTreeF a r = BranchF a r r | EmptyF
  deriving (Show, Eq, Functor)

type instance Base (BinTree a) = BinTreeF a

instance Recursive (BinTree a) where
  project Empty = EmptyF
  project (Branch a left right) = BranchF a left right

instance Corecursive (BinTree a) where
  embed EmptyF = Empty
  embed (BranchF a left right) = Branch a left right
-- end snippet BinTreeF
