{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Examples.Corecursive where

import           Data.Functor.Foldable
import           Data.TreeF
import           Data.JSONF
import qualified Data.Map                      as M

------------------ Ana -----------------------

buildList :: Int -> [Int]
buildList = ana coalg
 where
  coalg :: Int -> ListF Int Int
  coalg 0 = Nil
  coalg n = Cons n (n - 1)

tails :: String -> [String]
tails = ana coalg
 where
  coalg :: String -> ListF String String
  coalg ""         = Nil
  coalg s@(_ : xs) = Cons s xs

mapList :: forall a b . (a -> b) -> [a] -> [b]
mapList f = ana coalg
 where
  coalg :: [a] -> ListF b [a]
  coalg []       = Nil
  coalg (x : xs) = Cons (f x) xs

mapTree :: forall a b . (a -> b) -> Tree a -> Tree b
mapTree f = ana coalg
 where
  coalg :: Tree a -> TreeF b (Tree a)
  coalg (Leaf a    ) = LeafF (f a)
  coalg (Branch l r) = BranchF l r

mapNumbers :: (Double -> Double) -> JSON -> JSON
mapNumbers f = ana alg
 where
  alg :: JSON -> JSONF JSON
  alg (Number n) = NumberF (f n)
  alg j          = project j

buildPath :: ([String], Double) -> JSON
buildPath = ana coalg
 where
  coalg :: ([String], Double) -> JSONF ([String], Double)
  coalg ([]       , d) = NumberF d
  coalg (p : paths, d) = ObjectF (M.singleton p (paths, d))


collatzBuilder :: Int -> [Int]
collatzBuilder = ana coalg
 where
  coalg :: Int -> ListF Int Int
  coalg 0 = Nil
  coalg 1 = Cons 1 0
  coalg n | even n    = Cons n (n `div` 2)
          | otherwise = Cons n ((n * 3) + 1)

anaGroupBy :: Int -> [a] -> [[a]]
anaGroupBy n = ana coalg
 where
  coalg :: [a] -> ListF [a] [a]
  coalg [] = Nil
  coalg xs = Cons (take n xs) (drop n xs)
