module Examples.Recursive.Cata where

import Data.Functor.Foldable
import Data.Foldable
import Data.BinaryTreeF

-- start snippet sumFoldr
sumFoldr :: [Int] -> Int
sumFoldr = foldr (+) 0
-- end snippet sumFoldr

-- start snippet sumCata
sumCata :: [Int] -> Int
sumCata = cata algebra
 where
  algebra :: ListF Int Int -> Int
  -- Here we provide a default value for our base case
  algebra Nil               = 0
  -- We collapse one step of the list by adding
  -- the next element to the running total
  algebra (Cons next total) = next + total
-- end snippet sumCata

-- start snippet depthFirstFoldable
depthFirstFoldable :: BinTree a -> [a]
depthFirstFoldable = toList
-- end snippet depthFirstFoldable

-- start snippet depthFirstCata
depthFirstCata :: BinTree a -> [a]
depthFirstCata = cata algebra
 where
  algebra :: BinTreeF a [a] -> [a]
  algebra EmptyF          = []
  algebra (BranchF a l r) = [a] ++ l ++ r
-- end snippet depthFirstCata

-- start snippet inOrderCata
inOrderCata :: BinTree a -> [a]
inOrderCata = cata algebra
 where
  algebra :: BinTreeF a [a] -> [a]
  algebra EmptyF          = []
  algebra (BranchF a l r) = l ++ [a] ++ r
-- end snippet inOrderCata
