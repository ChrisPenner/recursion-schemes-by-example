{-# LANGUAGE ScopedTypeVariables #-}
module Recursive where

import           Data.Functor.Foldable
import           Data.TreeF
import           Data.JSONF

------------------ Cata -----------------------


-- sum list

sumList :: [Int] -> Int
sumList = cata alg
 where
  alg :: ListF Int Int -> Int
  alg Nil               = 0
  alg (Cons next total) = next + total


sumTree :: Tree Int -> Int
sumTree = cata alg
 where
  alg :: TreeF Int Int -> Int
  alg (LeafF n    ) = n
  alg (BranchF a b) = a + b

sumJSON :: JSON -> Double
sumJSON = cata alg
 where
  alg :: JSONF Double -> Double
  alg NullF         = 0
  alg (ObjectF obj) = sum obj
  alg (ArrayF  arr) = sum arr
  alg (StringF s  ) = 0
  alg (NumberF n  ) = n
  alg (BoolF   b  ) = 0

mapList :: forall a b . (a -> b) -> [a] -> [b]
mapList f = cata alg
 where
  alg :: ListF a [b] -> [b]
  alg Nil         = []
  alg (Cons a bs) = f a : bs

mapTree :: forall a b . (a -> b) -> Tree a -> Tree b
mapTree f = cata alg
 where
  alg :: TreeF a (Tree b) -> Tree b
  alg (LeafF a    ) = Leaf (f a)
  alg (BranchF l r) = Branch l r

mapNumbers :: (Double -> Double) -> JSON -> JSON
mapNumbers f = cata alg
 where
  alg :: JSONF JSON -> JSON
  alg (NumberF n) = Number (f n)
  alg j           = embed j
